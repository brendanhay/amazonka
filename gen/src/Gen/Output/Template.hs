{-# LANGUAGE OverloadedLabels #-}

module Gen.Output.Template
  ( Templates (..),
    Template (..),
    parseWith,
    render,
  )
where

import qualified Data.Aeson as Aeson
import Data.Generics.Labels ()
import Gen.Prelude
import qualified Text.EDE as EDE

-- | All the EDE templates that the generator cares about.
data Templates = Templates
  { cabalTemplate :: EDE.Template,
    tocTemplate :: EDE.Template,
    waitersTemplate :: EDE.Template,
    licenseTemplate :: EDE.Template,
    readmeTemplate :: EDE.Template,
    operationTemplate :: EDE.Template,
    typesTemplate :: EDE.Template,
    lensTemplate :: EDE.Template,
    sumTemplate :: EDE.Template,
    productTemplate :: EDE.Template,
    bootProductTemplate :: EDE.Template,
    testMainTemplate :: EDE.Template,
    testNamespaceTemplate :: EDE.Template,
    testInternalTemplate :: EDE.Template,
    fixturesTemplate :: EDE.Template,
    fixtureRequestTemplate :: EDE.Template,
    blankTemplate :: EDE.Template
  }
  deriving (Generic)

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
