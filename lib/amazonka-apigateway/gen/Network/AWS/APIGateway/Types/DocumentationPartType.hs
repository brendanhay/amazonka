-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationPartType
  ( DocumentationPartType
      ( DocumentationPartType',
        DPTAPI,
        DPTAuthorizer,
        DPTMethod,
        DPTModel,
        DPTPathParameter,
        DPTQueryParameter,
        DPTRequestBody,
        DPTRequestHeader,
        DPTResource,
        DPTResponse,
        DPTResponseBody,
        DPTResponseHeader
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentationPartType = DocumentationPartType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DPTAPI :: DocumentationPartType
pattern DPTAPI = DocumentationPartType' "API"

pattern DPTAuthorizer :: DocumentationPartType
pattern DPTAuthorizer = DocumentationPartType' "AUTHORIZER"

pattern DPTMethod :: DocumentationPartType
pattern DPTMethod = DocumentationPartType' "METHOD"

pattern DPTModel :: DocumentationPartType
pattern DPTModel = DocumentationPartType' "MODEL"

pattern DPTPathParameter :: DocumentationPartType
pattern DPTPathParameter = DocumentationPartType' "PATH_PARAMETER"

pattern DPTQueryParameter :: DocumentationPartType
pattern DPTQueryParameter = DocumentationPartType' "QUERY_PARAMETER"

pattern DPTRequestBody :: DocumentationPartType
pattern DPTRequestBody = DocumentationPartType' "REQUEST_BODY"

pattern DPTRequestHeader :: DocumentationPartType
pattern DPTRequestHeader = DocumentationPartType' "REQUEST_HEADER"

pattern DPTResource :: DocumentationPartType
pattern DPTResource = DocumentationPartType' "RESOURCE"

pattern DPTResponse :: DocumentationPartType
pattern DPTResponse = DocumentationPartType' "RESPONSE"

pattern DPTResponseBody :: DocumentationPartType
pattern DPTResponseBody = DocumentationPartType' "RESPONSE_BODY"

pattern DPTResponseHeader :: DocumentationPartType
pattern DPTResponseHeader = DocumentationPartType' "RESPONSE_HEADER"

{-# COMPLETE
  DPTAPI,
  DPTAuthorizer,
  DPTMethod,
  DPTModel,
  DPTPathParameter,
  DPTQueryParameter,
  DPTRequestBody,
  DPTRequestHeader,
  DPTResource,
  DPTResponse,
  DPTResponseBody,
  DPTResponseHeader,
  DocumentationPartType'
  #-}
