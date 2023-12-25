{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Authorizer (Authorizer)
import Botocore.Service.Authorizer qualified as Authorizer
import Botocore.Service.ClientContextParam (ClientContextParam)
import Botocore.Service.ClientContextParam qualified as ClientContextParam
import Botocore.Service.Metadata (Metadata)
import Botocore.Service.Metadata qualified as Metadata
import Botocore.Service.Operation (Operation)
import Botocore.Service.Operation qualified as Operation
import Botocore.Service.Shape (Shape)
import Botocore.Service.Shape qualified as Shape
import Botocore.Service.Types (XmlNamespace, XmlNamespaceB (..))
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    emptyObject,
    field,
    map,
    optional,
    record,
    text,
  )
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

$( passthroughBareB
     [d|
       data Service = Service
         { metadata :: Metadata,
           operations :: Map Text Operation,
           shapes :: Map Text Shape,
           authorizers :: Maybe (Map Text Authorizer),
           clientContextParams :: Maybe (Map Text ClientContextParam),
           documentation :: Maybe Text,
           examples :: Maybe (),
           version :: Maybe Text,
           xmlNamespace :: Maybe XmlNamespace
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Service
parse =
  record
    Service
      { metadata = field "metadata" Metadata.parse,
        operations = field "operations" $ map Operation.parse,
        shapes = field "shapes" $ map Shape.parse,
        authorizers =
          optional . field "authorizers" $ map Authorizer.parse,
        clientContextParams =
          optional . field "clientContextParams" $ map ClientContextParam.parse,
        documentation = optional $ field "documentation" text,
        examples = optional $ field "examples" emptyObject,
        version = optional $ field "version" text,
        xmlNamespace =
          optional . fmap (\uri -> XmlNamespace {uri, prefix = Nothing}) $
            field "xmlNamespace" text
      }
