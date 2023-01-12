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
-- Module      : Amazonka.APIGateway.Types.GatewayResponseType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.GatewayResponseType
  ( GatewayResponseType
      ( ..,
        GatewayResponseType_ACCESS_DENIED,
        GatewayResponseType_API_CONFIGURATION_ERROR,
        GatewayResponseType_AUTHORIZER_CONFIGURATION_ERROR,
        GatewayResponseType_AUTHORIZER_FAILURE,
        GatewayResponseType_BAD_REQUEST_BODY,
        GatewayResponseType_BAD_REQUEST_PARAMETERS,
        GatewayResponseType_DEFAULT_4XX,
        GatewayResponseType_DEFAULT_5XX,
        GatewayResponseType_EXPIRED_TOKEN,
        GatewayResponseType_INTEGRATION_FAILURE,
        GatewayResponseType_INTEGRATION_TIMEOUT,
        GatewayResponseType_INVALID_API_KEY,
        GatewayResponseType_INVALID_SIGNATURE,
        GatewayResponseType_MISSING_AUTHENTICATION_TOKEN,
        GatewayResponseType_QUOTA_EXCEEDED,
        GatewayResponseType_REQUEST_TOO_LARGE,
        GatewayResponseType_RESOURCE_NOT_FOUND,
        GatewayResponseType_THROTTLED,
        GatewayResponseType_UNAUTHORIZED,
        GatewayResponseType_UNSUPPORTED_MEDIA_TYPE,
        GatewayResponseType_WAF_FILTERED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GatewayResponseType = GatewayResponseType'
  { fromGatewayResponseType ::
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

pattern GatewayResponseType_ACCESS_DENIED :: GatewayResponseType
pattern GatewayResponseType_ACCESS_DENIED = GatewayResponseType' "ACCESS_DENIED"

pattern GatewayResponseType_API_CONFIGURATION_ERROR :: GatewayResponseType
pattern GatewayResponseType_API_CONFIGURATION_ERROR = GatewayResponseType' "API_CONFIGURATION_ERROR"

pattern GatewayResponseType_AUTHORIZER_CONFIGURATION_ERROR :: GatewayResponseType
pattern GatewayResponseType_AUTHORIZER_CONFIGURATION_ERROR = GatewayResponseType' "AUTHORIZER_CONFIGURATION_ERROR"

pattern GatewayResponseType_AUTHORIZER_FAILURE :: GatewayResponseType
pattern GatewayResponseType_AUTHORIZER_FAILURE = GatewayResponseType' "AUTHORIZER_FAILURE"

pattern GatewayResponseType_BAD_REQUEST_BODY :: GatewayResponseType
pattern GatewayResponseType_BAD_REQUEST_BODY = GatewayResponseType' "BAD_REQUEST_BODY"

pattern GatewayResponseType_BAD_REQUEST_PARAMETERS :: GatewayResponseType
pattern GatewayResponseType_BAD_REQUEST_PARAMETERS = GatewayResponseType' "BAD_REQUEST_PARAMETERS"

pattern GatewayResponseType_DEFAULT_4XX :: GatewayResponseType
pattern GatewayResponseType_DEFAULT_4XX = GatewayResponseType' "DEFAULT_4XX"

pattern GatewayResponseType_DEFAULT_5XX :: GatewayResponseType
pattern GatewayResponseType_DEFAULT_5XX = GatewayResponseType' "DEFAULT_5XX"

pattern GatewayResponseType_EXPIRED_TOKEN :: GatewayResponseType
pattern GatewayResponseType_EXPIRED_TOKEN = GatewayResponseType' "EXPIRED_TOKEN"

pattern GatewayResponseType_INTEGRATION_FAILURE :: GatewayResponseType
pattern GatewayResponseType_INTEGRATION_FAILURE = GatewayResponseType' "INTEGRATION_FAILURE"

pattern GatewayResponseType_INTEGRATION_TIMEOUT :: GatewayResponseType
pattern GatewayResponseType_INTEGRATION_TIMEOUT = GatewayResponseType' "INTEGRATION_TIMEOUT"

pattern GatewayResponseType_INVALID_API_KEY :: GatewayResponseType
pattern GatewayResponseType_INVALID_API_KEY = GatewayResponseType' "INVALID_API_KEY"

pattern GatewayResponseType_INVALID_SIGNATURE :: GatewayResponseType
pattern GatewayResponseType_INVALID_SIGNATURE = GatewayResponseType' "INVALID_SIGNATURE"

pattern GatewayResponseType_MISSING_AUTHENTICATION_TOKEN :: GatewayResponseType
pattern GatewayResponseType_MISSING_AUTHENTICATION_TOKEN = GatewayResponseType' "MISSING_AUTHENTICATION_TOKEN"

pattern GatewayResponseType_QUOTA_EXCEEDED :: GatewayResponseType
pattern GatewayResponseType_QUOTA_EXCEEDED = GatewayResponseType' "QUOTA_EXCEEDED"

pattern GatewayResponseType_REQUEST_TOO_LARGE :: GatewayResponseType
pattern GatewayResponseType_REQUEST_TOO_LARGE = GatewayResponseType' "REQUEST_TOO_LARGE"

pattern GatewayResponseType_RESOURCE_NOT_FOUND :: GatewayResponseType
pattern GatewayResponseType_RESOURCE_NOT_FOUND = GatewayResponseType' "RESOURCE_NOT_FOUND"

pattern GatewayResponseType_THROTTLED :: GatewayResponseType
pattern GatewayResponseType_THROTTLED = GatewayResponseType' "THROTTLED"

pattern GatewayResponseType_UNAUTHORIZED :: GatewayResponseType
pattern GatewayResponseType_UNAUTHORIZED = GatewayResponseType' "UNAUTHORIZED"

pattern GatewayResponseType_UNSUPPORTED_MEDIA_TYPE :: GatewayResponseType
pattern GatewayResponseType_UNSUPPORTED_MEDIA_TYPE = GatewayResponseType' "UNSUPPORTED_MEDIA_TYPE"

pattern GatewayResponseType_WAF_FILTERED :: GatewayResponseType
pattern GatewayResponseType_WAF_FILTERED = GatewayResponseType' "WAF_FILTERED"

{-# COMPLETE
  GatewayResponseType_ACCESS_DENIED,
  GatewayResponseType_API_CONFIGURATION_ERROR,
  GatewayResponseType_AUTHORIZER_CONFIGURATION_ERROR,
  GatewayResponseType_AUTHORIZER_FAILURE,
  GatewayResponseType_BAD_REQUEST_BODY,
  GatewayResponseType_BAD_REQUEST_PARAMETERS,
  GatewayResponseType_DEFAULT_4XX,
  GatewayResponseType_DEFAULT_5XX,
  GatewayResponseType_EXPIRED_TOKEN,
  GatewayResponseType_INTEGRATION_FAILURE,
  GatewayResponseType_INTEGRATION_TIMEOUT,
  GatewayResponseType_INVALID_API_KEY,
  GatewayResponseType_INVALID_SIGNATURE,
  GatewayResponseType_MISSING_AUTHENTICATION_TOKEN,
  GatewayResponseType_QUOTA_EXCEEDED,
  GatewayResponseType_REQUEST_TOO_LARGE,
  GatewayResponseType_RESOURCE_NOT_FOUND,
  GatewayResponseType_THROTTLED,
  GatewayResponseType_UNAUTHORIZED,
  GatewayResponseType_UNSUPPORTED_MEDIA_TYPE,
  GatewayResponseType_WAF_FILTERED,
  GatewayResponseType'
  #-}
