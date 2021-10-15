module WordFrequency
  ( Table,
    newTable,
    lookup,
    inferWords,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Foldable as Foldable
import qualified Data.Hashable as Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Maybe as Maybe
import Prelude hiding (lines, lookup, words)

-- | A word frequency table ranked according to Zipf\'s law.
--
-- The best way to proceed is to model the distribution of the output.
-- A good first approximation is to assume all words are independently
-- distributed. Then you only need to know the relative frequency of all
-- words. It is reasonable to assume that they follow Zipf's law, that is
-- the word with rank n in the list of words has probability roughly @1\/(n
-- log N)@ where @N@ is the number of words in the dictionary.
--
-- See: https://stackoverflow.com/a/11642687
data Table = Table
  { longest :: Int,
    frequencies :: IntMap Double
  }
  deriving stock (Show, Eq)

-- | Create a new table from an ordered newline delimited list of ASCII words.
newTable :: ByteString -> Either String Table
newTable input = do
  let lines = ByteString.Char8.lines input
      constant = log (fromIntegral (length lines))

      frequency i bytes =
        (hash bytes, log ((i + 1) * constant))

  if null lines
    then
      Left
        ( "readTable: failed creating word frequency table"
            <> " - input contains no lines."
        )
    else
      Right
        Table
          { longest = maximum (map ByteString.length lines),
            frequencies = IntMap.fromList (zipWith frequency [0 ..] lines)
          }

lookup :: ByteString -> Table -> Double
lookup bytes table =
  Maybe.fromMaybe 9e999 $
    IntMap.lookup (hash bytes) (frequencies table)

hash :: ByteString -> Int
hash = Hashable.hash

-- | Break an ASCII 'ByteString' on probabilistic word boundaries.
inferWords :: Table -> ByteString -> [ByteString]
inferWords table bytes =
  let total = ByteString.length bytes

      -- Create a slice of the input bytes.
      slice from to =
        ByteString.drop from (ByteString.take to bytes)

      -- We assume the costs are reversed due to construction via foldl + cons.
      candidates i costs =
        let len = length costs
            from = len - i
            to = len - max 0 (i - longest table)
         in zip [0 ..] $ drop from $ take to costs

      -- Find the best match for the i first characters, assuming cost has
      -- already been calculate for the i-1 first characters.
      --
      -- Returns a tuple of (match_cost, match_length)
      bestMatch i costs =
        minimum
          [ (c + lookup (slice (i - k - 1) i) table, k + 1)
            | (k, c) <- candidates i costs
          ]

      -- Build the cost array.
      finalCosts =
        Foldable.foldl'
          (\costs i -> fst (bestMatch i costs) : costs)
          [0]
          [1 .. total]

      -- Backtrack to recover the minimal-cost string.
      backtrack i
        | i <= 0 = []
        | actual /= expect = error ("Assertion failure " ++ show (actual, expect))
        | otherwise = slice (i - k) i : backtrack (i - k)
        where
          (actual, k) = bestMatch i finalCosts
          expect = reverse finalCosts !! i
   in reverse (backtrack total)
