{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.GatewayResponseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.GatewayResponseType
  ( GatewayResponseType
    ( GatewayResponseType'
    , GatewayResponseTypeDefault4XX
    , GatewayResponseTypeDefault5XX
    , GatewayResponseTypeResourceNotFound
    , GatewayResponseTypeUnauthorized
    , GatewayResponseTypeInvalidApiKey
    , GatewayResponseTypeAccessDenied
    , GatewayResponseTypeAuthorizerFailure
    , GatewayResponseTypeAuthorizerConfigurationError
    , GatewayResponseTypeInvalidSignature
    , GatewayResponseTypeExpiredToken
    , GatewayResponseTypeMissingAuthenticationToken
    , GatewayResponseTypeIntegrationFailure
    , GatewayResponseTypeIntegrationTimeout
    , GatewayResponseTypeApiConfigurationError
    , GatewayResponseTypeUnsupportedMediaType
    , GatewayResponseTypeBadRequestParameters
    , GatewayResponseTypeBadRequestBody
    , GatewayResponseTypeRequestTooLarge
    , GatewayResponseTypeThrottled
    , GatewayResponseTypeQuotaExceeded
    , fromGatewayResponseType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype GatewayResponseType = GatewayResponseType'{fromGatewayResponseType
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern GatewayResponseTypeDefault4XX :: GatewayResponseType
pattern GatewayResponseTypeDefault4XX = GatewayResponseType' "DEFAULT_4XX"

pattern GatewayResponseTypeDefault5XX :: GatewayResponseType
pattern GatewayResponseTypeDefault5XX = GatewayResponseType' "DEFAULT_5XX"

pattern GatewayResponseTypeResourceNotFound :: GatewayResponseType
pattern GatewayResponseTypeResourceNotFound = GatewayResponseType' "RESOURCE_NOT_FOUND"

pattern GatewayResponseTypeUnauthorized :: GatewayResponseType
pattern GatewayResponseTypeUnauthorized = GatewayResponseType' "UNAUTHORIZED"

pattern GatewayResponseTypeInvalidApiKey :: GatewayResponseType
pattern GatewayResponseTypeInvalidApiKey = GatewayResponseType' "INVALID_API_KEY"

pattern GatewayResponseTypeAccessDenied :: GatewayResponseType
pattern GatewayResponseTypeAccessDenied = GatewayResponseType' "ACCESS_DENIED"

pattern GatewayResponseTypeAuthorizerFailure :: GatewayResponseType
pattern GatewayResponseTypeAuthorizerFailure = GatewayResponseType' "AUTHORIZER_FAILURE"

pattern GatewayResponseTypeAuthorizerConfigurationError :: GatewayResponseType
pattern GatewayResponseTypeAuthorizerConfigurationError = GatewayResponseType' "AUTHORIZER_CONFIGURATION_ERROR"

pattern GatewayResponseTypeInvalidSignature :: GatewayResponseType
pattern GatewayResponseTypeInvalidSignature = GatewayResponseType' "INVALID_SIGNATURE"

pattern GatewayResponseTypeExpiredToken :: GatewayResponseType
pattern GatewayResponseTypeExpiredToken = GatewayResponseType' "EXPIRED_TOKEN"

pattern GatewayResponseTypeMissingAuthenticationToken :: GatewayResponseType
pattern GatewayResponseTypeMissingAuthenticationToken = GatewayResponseType' "MISSING_AUTHENTICATION_TOKEN"

pattern GatewayResponseTypeIntegrationFailure :: GatewayResponseType
pattern GatewayResponseTypeIntegrationFailure = GatewayResponseType' "INTEGRATION_FAILURE"

pattern GatewayResponseTypeIntegrationTimeout :: GatewayResponseType
pattern GatewayResponseTypeIntegrationTimeout = GatewayResponseType' "INTEGRATION_TIMEOUT"

pattern GatewayResponseTypeApiConfigurationError :: GatewayResponseType
pattern GatewayResponseTypeApiConfigurationError = GatewayResponseType' "API_CONFIGURATION_ERROR"

pattern GatewayResponseTypeUnsupportedMediaType :: GatewayResponseType
pattern GatewayResponseTypeUnsupportedMediaType = GatewayResponseType' "UNSUPPORTED_MEDIA_TYPE"

pattern GatewayResponseTypeBadRequestParameters :: GatewayResponseType
pattern GatewayResponseTypeBadRequestParameters = GatewayResponseType' "BAD_REQUEST_PARAMETERS"

pattern GatewayResponseTypeBadRequestBody :: GatewayResponseType
pattern GatewayResponseTypeBadRequestBody = GatewayResponseType' "BAD_REQUEST_BODY"

pattern GatewayResponseTypeRequestTooLarge :: GatewayResponseType
pattern GatewayResponseTypeRequestTooLarge = GatewayResponseType' "REQUEST_TOO_LARGE"

pattern GatewayResponseTypeThrottled :: GatewayResponseType
pattern GatewayResponseTypeThrottled = GatewayResponseType' "THROTTLED"

pattern GatewayResponseTypeQuotaExceeded :: GatewayResponseType
pattern GatewayResponseTypeQuotaExceeded = GatewayResponseType' "QUOTA_EXCEEDED"

{-# COMPLETE 
  GatewayResponseTypeDefault4XX,

  GatewayResponseTypeDefault5XX,

  GatewayResponseTypeResourceNotFound,

  GatewayResponseTypeUnauthorized,

  GatewayResponseTypeInvalidApiKey,

  GatewayResponseTypeAccessDenied,

  GatewayResponseTypeAuthorizerFailure,

  GatewayResponseTypeAuthorizerConfigurationError,

  GatewayResponseTypeInvalidSignature,

  GatewayResponseTypeExpiredToken,

  GatewayResponseTypeMissingAuthenticationToken,

  GatewayResponseTypeIntegrationFailure,

  GatewayResponseTypeIntegrationTimeout,

  GatewayResponseTypeApiConfigurationError,

  GatewayResponseTypeUnsupportedMediaType,

  GatewayResponseTypeBadRequestParameters,

  GatewayResponseTypeBadRequestBody,

  GatewayResponseTypeRequestTooLarge,

  GatewayResponseTypeThrottled,

  GatewayResponseTypeQuotaExceeded,
  GatewayResponseType'
  #-}
