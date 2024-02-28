{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Operation.Endpoint (Endpoint)
import Botocore.Service.Operation.Endpoint qualified as Endpoint
import Botocore.Service.Operation.EndpointDiscovery (EndpointDiscovery)
import Botocore.Service.Operation.EndpointDiscovery qualified as EndpointDiscovery
import Botocore.Service.Operation.Error (Error)
import Botocore.Service.Operation.Error qualified as Error
import Botocore.Service.Operation.Http (Http)
import Botocore.Service.Operation.Http qualified as Http
import Botocore.Service.Operation.HttpChecksum (HttpChecksum)
import Botocore.Service.Operation.HttpChecksum qualified as HttpChecksum
import Botocore.Service.Operation.Input (Input)
import Botocore.Service.Operation.Input qualified as Input
import Botocore.Service.Operation.Output (Output)
import Botocore.Service.Operation.Output qualified as Output
import Botocore.Service.Operation.StaticContextParams (StaticContextParams)
import Botocore.Service.Operation.StaticContextParams qualified as StaticContextParams
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    enum,
    field,
    list,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthType = None | V4UnsignedBody
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data Operation = Operation
         { name :: Text,
           alias :: Maybe Text,
           http :: Http,
           httpChecksum :: Maybe HttpChecksum,
           httpChecksumRequired :: Maybe Bool,
           input :: Maybe Input,
           output :: Maybe Output,
           errors :: Maybe [Error],
           endpoint :: Maybe Endpoint,
           endpointDiscovery :: Maybe EndpointDiscovery,
           endpointOperation :: Maybe Bool,
           authType :: Maybe AuthType,
           staticContextParams :: Maybe StaticContextParams,
           documentation :: Maybe Text,
           documentationUrl :: Maybe Text,
           idempotent :: Maybe Bool,
           deprecated :: Maybe Bool,
           deprecatedMessage :: Maybe Text
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Operation
parse =
  record
    Operation
      { name = field "name" text,
        alias = optional $ field "alias" text,
        http = field "http" Http.parse,
        httpChecksum = optional . field "httpChecksum" $ HttpChecksum.parse,
        httpChecksumRequired = optional $ field "httpChecksumRequired" bool,
        input = optional $ field "input" Input.parse,
        output = optional $ field "output" Output.parse,
        errors = optional . field "errors" $ list Error.parse,
        endpoint = optional $ field "endpoint" Endpoint.parse,
        endpointDiscovery =
          optional $ field "endpointdiscovery" EndpointDiscovery.parse,
        endpointOperation = optional $ field "endpointoperation" bool,
        authType = optional . field "authtype" . enum $ \case
          None -> "none"
          V4UnsignedBody -> "v4-unsigned-body",
        staticContextParams =
          optional $ field "staticContextParams" StaticContextParams.parse,
        documentation = optional $ field "documentation" text,
        documentationUrl = optional $ field "documentationUrl" text,
        idempotent = optional $ field "idempotent" bool,
        deprecated = optional $ field "deprecated" bool,
        deprecatedMessage = optional $ field "deprecatedMessage" text
      }
