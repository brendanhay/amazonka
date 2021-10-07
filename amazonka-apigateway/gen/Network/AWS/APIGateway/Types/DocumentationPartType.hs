{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationPartType
  ( DocumentationPartType
      ( ..,
        DocumentationPartType_API,
        DocumentationPartType_AUTHORIZER,
        DocumentationPartType_METHOD,
        DocumentationPartType_MODEL,
        DocumentationPartType_PATH_PARAMETER,
        DocumentationPartType_QUERY_PARAMETER,
        DocumentationPartType_REQUEST_BODY,
        DocumentationPartType_REQUEST_HEADER,
        DocumentationPartType_RESOURCE,
        DocumentationPartType_RESPONSE,
        DocumentationPartType_RESPONSE_BODY,
        DocumentationPartType_RESPONSE_HEADER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DocumentationPartType = DocumentationPartType'
  { fromDocumentationPartType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DocumentationPartType_API :: DocumentationPartType
pattern DocumentationPartType_API = DocumentationPartType' "API"

pattern DocumentationPartType_AUTHORIZER :: DocumentationPartType
pattern DocumentationPartType_AUTHORIZER = DocumentationPartType' "AUTHORIZER"

pattern DocumentationPartType_METHOD :: DocumentationPartType
pattern DocumentationPartType_METHOD = DocumentationPartType' "METHOD"

pattern DocumentationPartType_MODEL :: DocumentationPartType
pattern DocumentationPartType_MODEL = DocumentationPartType' "MODEL"

pattern DocumentationPartType_PATH_PARAMETER :: DocumentationPartType
pattern DocumentationPartType_PATH_PARAMETER = DocumentationPartType' "PATH_PARAMETER"

pattern DocumentationPartType_QUERY_PARAMETER :: DocumentationPartType
pattern DocumentationPartType_QUERY_PARAMETER = DocumentationPartType' "QUERY_PARAMETER"

pattern DocumentationPartType_REQUEST_BODY :: DocumentationPartType
pattern DocumentationPartType_REQUEST_BODY = DocumentationPartType' "REQUEST_BODY"

pattern DocumentationPartType_REQUEST_HEADER :: DocumentationPartType
pattern DocumentationPartType_REQUEST_HEADER = DocumentationPartType' "REQUEST_HEADER"

pattern DocumentationPartType_RESOURCE :: DocumentationPartType
pattern DocumentationPartType_RESOURCE = DocumentationPartType' "RESOURCE"

pattern DocumentationPartType_RESPONSE :: DocumentationPartType
pattern DocumentationPartType_RESPONSE = DocumentationPartType' "RESPONSE"

pattern DocumentationPartType_RESPONSE_BODY :: DocumentationPartType
pattern DocumentationPartType_RESPONSE_BODY = DocumentationPartType' "RESPONSE_BODY"

pattern DocumentationPartType_RESPONSE_HEADER :: DocumentationPartType
pattern DocumentationPartType_RESPONSE_HEADER = DocumentationPartType' "RESPONSE_HEADER"

{-# COMPLETE
  DocumentationPartType_API,
  DocumentationPartType_AUTHORIZER,
  DocumentationPartType_METHOD,
  DocumentationPartType_MODEL,
  DocumentationPartType_PATH_PARAMETER,
  DocumentationPartType_QUERY_PARAMETER,
  DocumentationPartType_REQUEST_BODY,
  DocumentationPartType_REQUEST_HEADER,
  DocumentationPartType_RESOURCE,
  DocumentationPartType_RESPONSE,
  DocumentationPartType_RESPONSE_BODY,
  DocumentationPartType_RESPONSE_HEADER,
  DocumentationPartType'
  #-}
