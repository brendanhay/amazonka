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
-- Module      : Amazonka.APIGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DocumentationPartType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentationPartType = DocumentationPartType'
  { fromDocumentationPartType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
