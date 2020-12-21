-- |
-- Module      : Gen.Types.Notation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Notation where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types.Id

data Builtin
  = IsEmpty
  | NotEmpty
  | Each
  | Last
  deriving stock (Eq, Show)

data Notation a
  = Key a
  | App Builtin (Notation a)
  | Dot (Notation a) (Notation a)
  | Alt (Notation a) (Notation a)
  deriving stock (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
  parseJSON = Aeson.withText "notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation text =
  annotate `first` A.parseOnly expr2 text
  where
    annotate err =
      "Failed parsing index notation: "
        ++ Text.unpack text
        ++ ", with "
        ++ err

    expr0 = each <|> last <|> key
    expr1 = isEmpty <|> notEmpty <|> expr0
    expr2 = dot <|> alt <|> expr1

    key = Key <$> label

    alt = Alt <$> (expr1 <* A.string "||") <*> expr2

    dot = List.foldr1 Dot <$> A.sepBy1 expr1 (A.char '.')

    each = App Each <$> key <* A.string "[]"

    last = App Last <$> key <* A.string "[-1]"

    isEmpty =
      A.string "length(" *> fmap (App IsEmpty) expr2 <* A.char ')'
        <* strip (A.string "==")
        <* strip (A.string "`0`")

    notEmpty =
      A.string "length(" *> fmap (App NotEmpty) expr2 <* A.char ')'
        <* strip (A.char '>')
        <* strip (A.string "`0`")

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
