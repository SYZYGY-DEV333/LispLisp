import System.IO
import Text.ParserCombinators.Parsec
-- import Text.Parsec (future compatibility)
import System.Environment -- for the getArgs function
-- import qualified Data.ByteString.Lazy as B; to be used later if this is faster or makes sense

-- I, K, S, T, and Z are known as the Schönfinkel combinators
-- http://www.johndcook.com/blog/2014/02/06/schonfinkel-combinators/
data Atom = A Atom Atom | K | S | K1 Atom | S1 Atom | S2 Atom Atom | I | V String | Inc | N !Int | T2 Atom Atom | Z2 Atom Atom deriving (Show, Eq)

parseApply = do
	c <- oneOf "`*"
	blank
	a <- parseExpr
	b <- parseExpr
	let change_S2KK = if c == '*' then change_S2KK_to iota else change_S2KK_to I
		in return $ A (change_S2KK a) (change_S2KK b)

change_S2KK_to new_i (S2 K K) = new_i
change_S2KK_to _ a = a

parseCombinator = do
	combinator <- oneOf "ksiKSI"
	blank
	return (case combinator of
		'k' -> K
		's' -> S
		'i' -> S2 K K -- act as a marker for iota or unlambda interpretation, while being correct for unlambda
		'K' -> K
		'S' -> S
		'I' -> I
		_ -> undefined)

parseCCExpr = do
	e <- parseExpr
	parseCCExprRest e

-- used to remove left recursion, but we want left associative recursion anyway
-- hint: we could have just used chainl1
parseCCExprRest e = (eof >> return e)
	<|> (do
		e' <- parseExpr
		parseCCExprRest (A e e'))
	<|> return e -- happens right before a ')'

parseJot = do
	j <- oneOf "01"
	blank
	parseJotRest (buildJot I j)

-- [F0]    ->   [F]SK
buildJot a '0' = (A (A a S) K)
-- [F1]    ->   \xy.[F](xy) -> S(K[F])
buildJot a '1' = (S1 (K1 a))
buildJot _ _ = undefined

parseJotRest j = (eof >> return j)
	<|> (do
		j' <- oneOf "01"
		blank
		parseJotRest (buildJot j j'))
	<|> return j

parseExpr = parseApply <|> parseCombinator <|> parseJot <|>
	(do char '('
	    blank
	    e <- parseCCExpr
	    char ')'
	    blank
	    return e) <|> (eof >> return I)

parseProgram = blank >> parseCCExpr

blank = skipMany ((space >> return ()) <|> singleLineComment)
	where
		singleLineComment = char '#' >> manyTill anyChar (char '\n') >> return ()

main = do
	args <- getArgs
	result <- parseFromFile parseProgram (last args)
	case result of
		Left err -> putStrLn ("Error: " ++ (show err))
		Right a -> do
			-- (putStrLn . toUnlambda . unexecute) a
			programInput <- getContents
			if "-f" `elem` args
				then hSetBuffering stdout NoBuffering
				else return ()
			runLazyK (A (optimize a) (atomListFromChurchList (churchListFromInput programInput)))
				where
					churchListFromInput :: String -> [Atom]
					churchListFromInput [] = repeat n256
					churchListFromInput xs = map ((churchFromIntList !!) . fromEnum) xs ++ churchListFromInput []

					-- S(S(KS)K)[n]
					churchFromInt 0 = K1 I
					churchFromInt 1 = I
					churchFromInt i = if i < 0 || i >= 256
						then n256
						else (S2 (S2 (K1 S) K) (churchFromInt (pred i)))
					churchFromIntList = map churchFromInt [0..] -- memoize church numbers

					atomListFromChurchList :: [Atom] -> Atom
					atomListFromChurchList [] = error "Must return an infinite list"
					-- S(SI(K[x]))(K[xs])
					atomListFromChurchList (x: xs) = (S2 (S2 I (K1 x)) (K1 (atomListFromChurchList xs)))

					runLazyK :: Atom -> IO ()
					runLazyK = putStr . map toEnum . takeWhile (< 256) . map fromAtom . listFromAtomList
						where
							fromAtom :: Atom -> Int
							fromAtom a' = case execute (A (A a' Inc) zero) of
								N i -> i
								_ -> error "Element in output was not a number"

							listFromAtomList :: Atom -> [Atom]
							listFromAtomList las = (car las): listFromAtomList (cdr las)
								where
									car a' = execute (A a' K)
									cdr a' = execute (A a' (K1 I))

-- Show is intended for Haskell syntax, not other kinds of printing
toUnlambda (A a b) = "`" ++ (toUnlambda a) ++ (toUnlambda b)
toUnlambda K = "k"
toUnlambda S = "s"
toUnlambda (K1 a) = "{k|" ++ (toUnlambda a) ++ "}"
toUnlambda (S1 a) = "{s|" ++ (toUnlambda a) ++ "}"
toUnlambda (S2 a b) = "{s|" ++ (toUnlambda a) ++ "," ++ (toUnlambda b) ++ "}"
toUnlambda I = "i"
toUnlambda (V s) = "[" ++ s ++ "]"
toUnlambda Inc = "{+1}"
toUnlambda (N i) = show i

{- replace execute A() with direct application
 in order to reduce memory allocations -}
apply I a = a
apply K a = K1 a

execute (A I a) = a
execute (A K a) = K1 a
execute (A (K1 a) _b) = a
execute (A S a) = S1 a
execute (A (S1 a) b) = S2 a b
execute (A (S2 a b) c) = execute $ A (A a c) (A b c)
execute (A (V s) b) = A (V s) (execute b)
execute (A (T2 a b) c) = execute $ A (A a c) b
execute (A (Z2 a b) c) = execute $ A a (A b c)

-- Inc and N make Atom communicate things back to the Haskell world
execute (A Inc n) = N $ succ i
	where
		i = case execute n of -- this evaluator works in a strict language, e.g. ocaml
			N i -> i
			_ -> error "Can only increment a number"
{-execute (A Inc (N i)) = N $ succ i
execute (A Inc _) = error "Can only increment a number"-}
execute (A (N _) _) = error "Cannot apply a number"

execute (A a b) = execute $ A a' b -- if write b instead of b' do not execute b yet: applying an Inc will do it only when needed
	where
		a' = execute a
		b' = execute b
-- otherwise there is nothing to execute
execute a = a

optimize (A (A S (A K e)) (A K f)) = K1 (A (optimize e) (optimize f))
optimize (A (A S (A K e)) I) = optimize e
optimize (A (A S e) (A K f)) = T2 (optimize e) (optimize f)
optimize (A (A S (A K e)) f) = Z2 (optimize e) (optimize f)
optimize (A e f) = A (optimize e) (optimize f)
optimize a = a

unexecute (K1 a) = A K (unexecute a)
unexecute (S1 a) = A S (unexecute a)
unexecute (S2 a b) = A (unexecute (S1 a)) (unexecute b)
unexecute (A a b) = A (unexecute a) (unexecute b)
-- otherwise we cannot unexecute
unexecute a = a

reversal a b = (A (A (A (A S (A K (A S I))) K) a) b)

zero = N 0
-- SII(SII(S(S(KS)K)I))
n256 = (A (S2 I I) (A (S2 I I) (S2 (S2 (K1 S) K) I)))
-- eof = (A (A n256 Inc) zero)

-- S(SI(KS))(KK)
iota = S2 (S2 I (K1 S)) (K1 K)

