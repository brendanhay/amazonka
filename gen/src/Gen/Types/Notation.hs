module Gen.Types.Notation where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types.Id

data Key a
  = Key {fromKey :: a}
  | Each {fromKey :: a}
  | Last {fromKey :: a}
  deriving stock (Eq, Show, Functor, Foldable)

data Notation a
  = Access (NonEmpty (Key a))
  | IsEmptyList (NonEmpty (Key a))
  | NonEmptyList (NonEmpty (Key a))
  | NonEmptyText (Key a)
  | Choice (Notation a) (Notation a)
  deriving stock (Eq, Show, Functor, Foldable)

instance FromJSON (Notation Id) where
  parseJSON = Aeson.withText "Notation" (either fail pure . parseNotation)

parseNotation :: Text -> Either String (Notation Id)
parseNotation t = mappend msg `first` Atto.parseOnly expr1 t
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
        <* Atto.string "||"
        <*> expr1

    isEmptyList =
      IsEmptyList
        <$> (Atto.string "length(" *> access <* Atto.char ')')
        <* strip (Atto.string "==")
        <* strip (Atto.string "`0`")

    nonEmptyList =
      NonEmptyList
        <$> (Atto.string "length(" *> access <* Atto.char ')')
        <* strip (Atto.char '>')
        <* strip (Atto.string "`0`")

    nonEmptyText =
      NonEmptyText
        <$> (Atto.string "length(" *> key0 <* Atto.char ')')
        <* strip (Atto.char '>')
        <* strip (Atto.string "`0`")

    access = do
      x : xs <- Atto.sepBy1 key (Atto.char '.')
      pure (x :| xs)

    key = key1 <|> key0
    key1 =
      (Each <$> label <* Atto.string "[]")
        <|> (Last <$> label <* Atto.string "[-1]")
    key0 = Key <$> label

    label =
      strip $ do
        text <- Atto.takeWhile1 (Atto.notInClass "[].()|><= ")
        next <- Atto.peekChar

        Monad.when (next == Just '(') $
          fail $
            "encountered start of unexpected function application "
              ++ show text
              ++ "(...)"

        pure (mkId text)

    strip p =
      Atto.skipSpace *> p <* Atto.skipSpace
