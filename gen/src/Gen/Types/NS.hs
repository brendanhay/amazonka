module Gen.Types.NS where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Gen.Prelude

newtype NS = NS [Text]
  deriving (Eq, Ord, Show)

mkNS :: Text -> NS
mkNS = NS . Text.splitOn "."

nsToPath :: NS -> FilePath
nsToPath (NS xs) = Text.unpack (Text.intercalate "/" xs) <.> "hs"

nsHyphenate :: NS -> Text
nsHyphenate (NS xs) = Text.intercalate "-" xs

instance IsString NS where
  fromString "" = mempty
  fromString s = mkNS (fromString s)

instance Semigroup NS where
  (NS xs) <> (NS ys)
    | null xs = NS ys
    | null ys = NS xs
    | otherwise = NS (xs <> ys)

instance Monoid NS where
  mempty = NS []
  mappend = (<>)

instance FromJSON NS where
  parseJSON = Aeson.withText "NS" (pure . mkNS)

instance ToJSON NS where
  toJSON (NS xs) = Aeson.toJSON (Text.intercalate "." xs)
