-- Module      : Gen.Types.Notation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Notation where

import Control.Applicative
import Control.Monad qualified as Monad
import Data.Aeson
import Data.Attoparsec.Text qualified as A
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Gen.Types.Id

data Key a
  = Key {fromKey :: a}
  | Each {fromKey :: a}
  | Last {fromKey :: a}
  deriving (Eq, Show, Functor, Foldable)

data Notation a
  = Access (NonEmpty (Key a))
  | IsEmptyList (NonEmpty (Key a))
  | NonEmptyList (NonEmpty (Key a))
  | NonEmptyText (Key a)
  | Choice (Notation a) (Notation a)
  deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
  parseJSON = withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` A.parseOnly expr1 t
  where
    msg =
      "Failed parsing index notation: "
        ++ Text.unpack t
        ++ ", with: "

    expr0 = nonEmptyText <|> isEmptyList <|> nonEmptyList <|> fmap Access access
    expr1 = choice <|> expr0

    choice =
      Choice
        <$> expr0
          <* A.string "||"
        <*> expr1

    isEmptyList =
      IsEmptyList
        <$> (A.string "length(" *> access <* A.char ')')
        <* strip (A.string "==")
        <* strip (A.string "`0`")

    nonEmptyList =
      NonEmptyList
        <$> (A.string "length(" *> access <* A.char ')')
        <* strip (A.char '>')
        <* strip (A.string "`0`")

    nonEmptyText =
      NonEmptyText
        <$> (A.string "length(" *> key0 <* A.char ')')
        <* strip (A.char '>')
        <* strip (A.string "`0`")

    access = do
      x : xs <- A.sepBy1 key (A.char '.')
      pure (x :| xs)

    key = key1 <|> key0
    key1 =
      (Each <$> label <* A.string "[]")
        <|> (Last <$> label <* A.string "[-1]")
    key0 = (Key <$> label)

    label =
      strip $ do
        text <- A.takeWhile1 (A.notInClass "[].()|><= ")
        next <- A.peekChar

        Monad.when (next == Just '(') $
          fail $
            "encountered start of unexpected function application "
              ++ show text
              ++ "(...)"

        pure (mkId text)

    strip p =
      A.skipSpace *> p <* A.skipSpace
