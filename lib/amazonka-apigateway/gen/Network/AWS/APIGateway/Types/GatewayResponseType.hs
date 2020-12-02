{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.GatewayResponseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.GatewayResponseType where

import Network.AWS.Prelude

data GatewayResponseType
  = APIConfigurationError
  | AccessDenied
  | AuthorizerConfigurationError
  | AuthorizerFailure
  | BadRequestBody
  | BadRequestParameters
  | Default4XX
  | Default5XX
  | ExpiredToken
  | IntegrationFailure
  | IntegrationTimeout
  | InvalidAPIKey
  | InvalidSignature
  | MissingAuthenticationToken
  | QuotaExceeded
  | RequestTooLarge
  | ResourceNotFound
  | Throttled
  | Unauthorized
  | UnsupportedMediaType
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

instance FromText GatewayResponseType where
  parser =
    takeLowerText >>= \case
      "api_configuration_error" -> pure APIConfigurationError
      "access_denied" -> pure AccessDenied
      "authorizer_configuration_error" -> pure AuthorizerConfigurationError
      "authorizer_failure" -> pure AuthorizerFailure
      "bad_request_body" -> pure BadRequestBody
      "bad_request_parameters" -> pure BadRequestParameters
      "default_4xx" -> pure Default4XX
      "default_5xx" -> pure Default5XX
      "expired_token" -> pure ExpiredToken
      "integration_failure" -> pure IntegrationFailure
      "integration_timeout" -> pure IntegrationTimeout
      "invalid_api_key" -> pure InvalidAPIKey
      "invalid_signature" -> pure InvalidSignature
      "missing_authentication_token" -> pure MissingAuthenticationToken
      "quota_exceeded" -> pure QuotaExceeded
      "request_too_large" -> pure RequestTooLarge
      "resource_not_found" -> pure ResourceNotFound
      "throttled" -> pure Throttled
      "unauthorized" -> pure Unauthorized
      "unsupported_media_type" -> pure UnsupportedMediaType
      e ->
        fromTextError $
          "Failure parsing GatewayResponseType from value: '" <> e
            <> "'. Accepted values: api_configuration_error, access_denied, authorizer_configuration_error, authorizer_failure, bad_request_body, bad_request_parameters, default_4xx, default_5xx, expired_token, integration_failure, integration_timeout, invalid_api_key, invalid_signature, missing_authentication_token, quota_exceeded, request_too_large, resource_not_found, throttled, unauthorized, unsupported_media_type"

instance ToText GatewayResponseType where
  toText = \case
    APIConfigurationError -> "API_CONFIGURATION_ERROR"
    AccessDenied -> "ACCESS_DENIED"
    AuthorizerConfigurationError -> "AUTHORIZER_CONFIGURATION_ERROR"
    AuthorizerFailure -> "AUTHORIZER_FAILURE"
    BadRequestBody -> "BAD_REQUEST_BODY"
    BadRequestParameters -> "BAD_REQUEST_PARAMETERS"
    Default4XX -> "DEFAULT_4XX"
    Default5XX -> "DEFAULT_5XX"
    ExpiredToken -> "EXPIRED_TOKEN"
    IntegrationFailure -> "INTEGRATION_FAILURE"
    IntegrationTimeout -> "INTEGRATION_TIMEOUT"
    InvalidAPIKey -> "INVALID_API_KEY"
    InvalidSignature -> "INVALID_SIGNATURE"
    MissingAuthenticationToken -> "MISSING_AUTHENTICATION_TOKEN"
    QuotaExceeded -> "QUOTA_EXCEEDED"
    RequestTooLarge -> "REQUEST_TOO_LARGE"
    ResourceNotFound -> "RESOURCE_NOT_FOUND"
    Throttled -> "THROTTLED"
    Unauthorized -> "UNAUTHORIZED"
    UnsupportedMediaType -> "UNSUPPORTED_MEDIA_TYPE"

instance Hashable GatewayResponseType

instance NFData GatewayResponseType

instance ToByteString GatewayResponseType

instance ToQuery GatewayResponseType

instance ToHeader GatewayResponseType

instance ToJSON GatewayResponseType where
  toJSON = toJSONText

instance FromJSON GatewayResponseType where
  parseJSON = parseJSONText "GatewayResponseType"
