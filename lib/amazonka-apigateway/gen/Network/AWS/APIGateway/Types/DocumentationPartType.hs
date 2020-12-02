{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationPartType where

import Network.AWS.Prelude

data DocumentationPartType
  = DPTAPI
  | DPTAuthorizer
  | DPTMethod
  | DPTModel
  | DPTPathParameter
  | DPTQueryParameter
  | DPTRequestBody
  | DPTRequestHeader
  | DPTResource
  | DPTResponse
  | DPTResponseBody
  | DPTResponseHeader
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DocumentationPartType where
  parser =
    takeLowerText >>= \case
      "api" -> pure DPTAPI
      "authorizer" -> pure DPTAuthorizer
      "method" -> pure DPTMethod
      "model" -> pure DPTModel
      "path_parameter" -> pure DPTPathParameter
      "query_parameter" -> pure DPTQueryParameter
      "request_body" -> pure DPTRequestBody
      "request_header" -> pure DPTRequestHeader
      "resource" -> pure DPTResource
      "response" -> pure DPTResponse
      "response_body" -> pure DPTResponseBody
      "response_header" -> pure DPTResponseHeader
      e ->
        fromTextError $
          "Failure parsing DocumentationPartType from value: '" <> e
            <> "'. Accepted values: api, authorizer, method, model, path_parameter, query_parameter, request_body, request_header, resource, response, response_body, response_header"

instance ToText DocumentationPartType where
  toText = \case
    DPTAPI -> "API"
    DPTAuthorizer -> "AUTHORIZER"
    DPTMethod -> "METHOD"
    DPTModel -> "MODEL"
    DPTPathParameter -> "PATH_PARAMETER"
    DPTQueryParameter -> "QUERY_PARAMETER"
    DPTRequestBody -> "REQUEST_BODY"
    DPTRequestHeader -> "REQUEST_HEADER"
    DPTResource -> "RESOURCE"
    DPTResponse -> "RESPONSE"
    DPTResponseBody -> "RESPONSE_BODY"
    DPTResponseHeader -> "RESPONSE_HEADER"

instance Hashable DocumentationPartType

instance NFData DocumentationPartType

instance ToByteString DocumentationPartType

instance ToQuery DocumentationPartType

instance ToHeader DocumentationPartType

instance ToJSON DocumentationPartType where
  toJSON = toJSONText

instance FromJSON DocumentationPartType where
  parseJSON = parseJSONText "DocumentationPartType"
