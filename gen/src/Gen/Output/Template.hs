{-# LANGUAGE OverloadedLabels #-}

module Gen.Output.Template (Template (..), parseWith, render) where

import qualified Data.Aeson as Aeson
import Data.Generics.Labels ()
import Gen.Prelude
import qualified Text.EDE as EDE

-- | A type-safe template that knows the type of its arguments and how
-- to pass them to EDE.
data Template input = Template
  { edeTemplate :: EDE.Template,
    arguments :: input -> HashMap Text Aeson.Value
  }
  deriving (Generic)

instance Contravariant Template where
  contramap f = #arguments %~ (. f)

parseWith ::
  (Monad m) =>
  (input -> HashMap Text Aeson.Value) ->
  EDE.Resolver m ->
  Text ->
  ByteString ->
  m (Either String (Template input))
parseWith arguments resolver name template = do
  eTemplate <- EDE.eitherParseWith EDE.defaultSyntax resolver name template
  pure $ eTemplate <&> \edeTemplate -> Template {..}

render :: Template a -> a -> Either String TextLazy
render Template {edeTemplate, arguments} =
  EDE.eitherRender edeTemplate . arguments
