{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.DocumentationPartType
  ( DocumentationPartType
    ( DocumentationPartType'
    , DocumentationPartTypeApi
    , DocumentationPartTypeAuthorizer
    , DocumentationPartTypeModel
    , DocumentationPartTypeResource
    , DocumentationPartTypeMethod
    , DocumentationPartTypePathParameter
    , DocumentationPartTypeQueryParameter
    , DocumentationPartTypeRequestHeader
    , DocumentationPartTypeRequestBody
    , DocumentationPartTypeResponse
    , DocumentationPartTypeResponseHeader
    , DocumentationPartTypeResponseBody
    , fromDocumentationPartType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DocumentationPartType = DocumentationPartType'{fromDocumentationPartType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern DocumentationPartTypeApi :: DocumentationPartType
pattern DocumentationPartTypeApi = DocumentationPartType' "API"

pattern DocumentationPartTypeAuthorizer :: DocumentationPartType
pattern DocumentationPartTypeAuthorizer = DocumentationPartType' "AUTHORIZER"

pattern DocumentationPartTypeModel :: DocumentationPartType
pattern DocumentationPartTypeModel = DocumentationPartType' "MODEL"

pattern DocumentationPartTypeResource :: DocumentationPartType
pattern DocumentationPartTypeResource = DocumentationPartType' "RESOURCE"

pattern DocumentationPartTypeMethod :: DocumentationPartType
pattern DocumentationPartTypeMethod = DocumentationPartType' "METHOD"

pattern DocumentationPartTypePathParameter :: DocumentationPartType
pattern DocumentationPartTypePathParameter = DocumentationPartType' "PATH_PARAMETER"

pattern DocumentationPartTypeQueryParameter :: DocumentationPartType
pattern DocumentationPartTypeQueryParameter = DocumentationPartType' "QUERY_PARAMETER"

pattern DocumentationPartTypeRequestHeader :: DocumentationPartType
pattern DocumentationPartTypeRequestHeader = DocumentationPartType' "REQUEST_HEADER"

pattern DocumentationPartTypeRequestBody :: DocumentationPartType
pattern DocumentationPartTypeRequestBody = DocumentationPartType' "REQUEST_BODY"

pattern DocumentationPartTypeResponse :: DocumentationPartType
pattern DocumentationPartTypeResponse = DocumentationPartType' "RESPONSE"

pattern DocumentationPartTypeResponseHeader :: DocumentationPartType
pattern DocumentationPartTypeResponseHeader = DocumentationPartType' "RESPONSE_HEADER"

pattern DocumentationPartTypeResponseBody :: DocumentationPartType
pattern DocumentationPartTypeResponseBody = DocumentationPartType' "RESPONSE_BODY"

{-# COMPLETE 
  DocumentationPartTypeApi,

  DocumentationPartTypeAuthorizer,

  DocumentationPartTypeModel,

  DocumentationPartTypeResource,

  DocumentationPartTypeMethod,

  DocumentationPartTypePathParameter,

  DocumentationPartTypeQueryParameter,

  DocumentationPartTypeRequestHeader,

  DocumentationPartTypeRequestBody,

  DocumentationPartTypeResponse,

  DocumentationPartTypeResponseHeader,

  DocumentationPartTypeResponseBody,
  DocumentationPartType'
  #-}
