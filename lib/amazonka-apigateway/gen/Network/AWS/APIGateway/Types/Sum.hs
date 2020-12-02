{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.Sum where

import Network.AWS.Prelude

data APIKeySourceType
  = Authorizer
  | Header
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText APIKeySourceType where
    parser = takeLowerText >>= \case
        "authorizer" -> pure Authorizer
        "header" -> pure Header
        e -> fromTextError $ "Failure parsing APIKeySourceType from value: '" <> e
           <> "'. Accepted values: authorizer, header"

instance ToText APIKeySourceType where
    toText = \case
        Authorizer -> "AUTHORIZER"
        Header -> "HEADER"

instance Hashable     APIKeySourceType
instance NFData       APIKeySourceType
instance ToByteString APIKeySourceType
instance ToQuery      APIKeySourceType
instance ToHeader     APIKeySourceType

instance ToJSON APIKeySourceType where
    toJSON = toJSONText

instance FromJSON APIKeySourceType where
    parseJSON = parseJSONText "APIKeySourceType"

data APIKeysFormat =
  CSV
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText APIKeysFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        e -> fromTextError $ "Failure parsing APIKeysFormat from value: '" <> e
           <> "'. Accepted values: csv"

instance ToText APIKeysFormat where
    toText = \case
        CSV -> "csv"

instance Hashable     APIKeysFormat
instance NFData       APIKeysFormat
instance ToByteString APIKeysFormat
instance ToQuery      APIKeysFormat
instance ToHeader     APIKeysFormat

instance ToJSON APIKeysFormat where
    toJSON = toJSONText

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
--
data AuthorizerType
  = AuthorizerCognitoUserPools
  | AuthorizerRequest
  | AuthorizerToken
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthorizerType where
    parser = takeLowerText >>= \case
        "cognito_user_pools" -> pure AuthorizerCognitoUserPools
        "request" -> pure AuthorizerRequest
        "token" -> pure AuthorizerToken
        e -> fromTextError $ "Failure parsing AuthorizerType from value: '" <> e
           <> "'. Accepted values: cognito_user_pools, request, token"

instance ToText AuthorizerType where
    toText = \case
        AuthorizerCognitoUserPools -> "COGNITO_USER_POOLS"
        AuthorizerRequest -> "REQUEST"
        AuthorizerToken -> "TOKEN"

instance Hashable     AuthorizerType
instance NFData       AuthorizerType
instance ToByteString AuthorizerType
instance ToQuery      AuthorizerType
instance ToHeader     AuthorizerType

instance ToJSON AuthorizerType where
    toJSON = toJSONText

instance FromJSON AuthorizerType where
    parseJSON = parseJSONText "AuthorizerType"

-- | Returns the size of the __CacheCluster__ .
--
--
data CacheClusterSize
  = D0_5
  | D118
  | D13_5
  | D1_6
  | D237
  | D28_4
  | D58_2
  | D6_1
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CacheClusterSize where
    parser = takeLowerText >>= \case
        "0.5" -> pure D0_5
        "118" -> pure D118
        "13.5" -> pure D13_5
        "1.6" -> pure D1_6
        "237" -> pure D237
        "28.4" -> pure D28_4
        "58.2" -> pure D58_2
        "6.1" -> pure D6_1
        e -> fromTextError $ "Failure parsing CacheClusterSize from value: '" <> e
           <> "'. Accepted values: 0.5, 118, 13.5, 1.6, 237, 28.4, 58.2, 6.1"

instance ToText CacheClusterSize where
    toText = \case
        D0_5 -> "0.5"
        D118 -> "118"
        D13_5 -> "13.5"
        D1_6 -> "1.6"
        D237 -> "237"
        D28_4 -> "28.4"
        D58_2 -> "58.2"
        D6_1 -> "6.1"

instance Hashable     CacheClusterSize
instance NFData       CacheClusterSize
instance ToByteString CacheClusterSize
instance ToQuery      CacheClusterSize
instance ToHeader     CacheClusterSize

instance ToJSON CacheClusterSize where
    toJSON = toJSONText

instance FromJSON CacheClusterSize where
    parseJSON = parseJSONText "CacheClusterSize"

-- | Returns the status of the __CacheCluster__ .
--
--
data CacheClusterStatus
  = Available
  | CreateInProgress
  | DeleteInProgress
  | FlushInProgress
  | NotAvailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CacheClusterStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "create_in_progress" -> pure CreateInProgress
        "delete_in_progress" -> pure DeleteInProgress
        "flush_in_progress" -> pure FlushInProgress
        "not_available" -> pure NotAvailable
        e -> fromTextError $ "Failure parsing CacheClusterStatus from value: '" <> e
           <> "'. Accepted values: available, create_in_progress, delete_in_progress, flush_in_progress, not_available"

instance ToText CacheClusterStatus where
    toText = \case
        Available -> "AVAILABLE"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        DeleteInProgress -> "DELETE_IN_PROGRESS"
        FlushInProgress -> "FLUSH_IN_PROGRESS"
        NotAvailable -> "NOT_AVAILABLE"

instance Hashable     CacheClusterStatus
instance NFData       CacheClusterStatus
instance ToByteString CacheClusterStatus
instance ToQuery      CacheClusterStatus
instance ToHeader     CacheClusterStatus

instance FromJSON CacheClusterStatus where
    parseJSON = parseJSONText "CacheClusterStatus"

data ConnectionType
  = Internet
  | VPCLink
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionType where
    parser = takeLowerText >>= \case
        "internet" -> pure Internet
        "vpc_link" -> pure VPCLink
        e -> fromTextError $ "Failure parsing ConnectionType from value: '" <> e
           <> "'. Accepted values: internet, vpc_link"

instance ToText ConnectionType where
    toText = \case
        Internet -> "INTERNET"
        VPCLink -> "VPC_LINK"

instance Hashable     ConnectionType
instance NFData       ConnectionType
instance ToByteString ConnectionType
instance ToQuery      ConnectionType
instance ToHeader     ConnectionType

instance ToJSON ConnectionType where
    toJSON = toJSONText

instance FromJSON ConnectionType where
    parseJSON = parseJSONText "ConnectionType"

data ContentHandlingStrategy
  = ConvertToBinary
  | ConvertToText
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContentHandlingStrategy where
    parser = takeLowerText >>= \case
        "convert_to_binary" -> pure ConvertToBinary
        "convert_to_text" -> pure ConvertToText
        e -> fromTextError $ "Failure parsing ContentHandlingStrategy from value: '" <> e
           <> "'. Accepted values: convert_to_binary, convert_to_text"

instance ToText ContentHandlingStrategy where
    toText = \case
        ConvertToBinary -> "CONVERT_TO_BINARY"
        ConvertToText -> "CONVERT_TO_TEXT"

instance Hashable     ContentHandlingStrategy
instance NFData       ContentHandlingStrategy
instance ToByteString ContentHandlingStrategy
instance ToQuery      ContentHandlingStrategy
instance ToHeader     ContentHandlingStrategy

instance ToJSON ContentHandlingStrategy where
    toJSON = toJSONText

instance FromJSON ContentHandlingStrategy where
    parseJSON = parseJSONText "ContentHandlingStrategy"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DocumentationPartType where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing DocumentationPartType from value: '" <> e
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

instance Hashable     DocumentationPartType
instance NFData       DocumentationPartType
instance ToByteString DocumentationPartType
instance ToQuery      DocumentationPartType
instance ToHeader     DocumentationPartType

instance ToJSON DocumentationPartType where
    toJSON = toJSONText

instance FromJSON DocumentationPartType where
    parseJSON = parseJSONText "DocumentationPartType"

-- | The endpoint type. The valid value is @EDGE@ for edge-optimized API setup, most suitable for mobile applications, @REGIONAL@ for regional API endpoint setup, most suitable for calling from AWS Region
--
--
data EndpointType
  = Edge
  | Regional
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointType where
    parser = takeLowerText >>= \case
        "edge" -> pure Edge
        "regional" -> pure Regional
        e -> fromTextError $ "Failure parsing EndpointType from value: '" <> e
           <> "'. Accepted values: edge, regional"

instance ToText EndpointType where
    toText = \case
        Edge -> "EDGE"
        Regional -> "REGIONAL"

instance Hashable     EndpointType
instance NFData       EndpointType
instance ToByteString EndpointType
instance ToQuery      EndpointType
instance ToHeader     EndpointType

instance ToJSON EndpointType where
    toJSON = toJSONText

instance FromJSON EndpointType where
    parseJSON = parseJSONText "EndpointType"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GatewayResponseType where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing GatewayResponseType from value: '" <> e
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

instance Hashable     GatewayResponseType
instance NFData       GatewayResponseType
instance ToByteString GatewayResponseType
instance ToQuery      GatewayResponseType
instance ToHeader     GatewayResponseType

instance ToJSON GatewayResponseType where
    toJSON = toJSONText

instance FromJSON GatewayResponseType where
    parseJSON = parseJSONText "GatewayResponseType"

-- | The integration type. The valid value is @HTTP@ for integrating an API method with an HTTP backend; @AWS@ with any AWS service endpoints; @MOCK@ for testing without actually invoking the backend; @HTTP_PROXY@ for integrating with the HTTP proxy integration; @AWS_PROXY@ for integrating with the Lambda proxy integration.
--
--
data IntegrationType
  = AWS
  | AWSProxy
  | HTTP
  | HTTPProxy
  | Mock
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IntegrationType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "aws_proxy" -> pure AWSProxy
        "http" -> pure HTTP
        "http_proxy" -> pure HTTPProxy
        "mock" -> pure Mock
        e -> fromTextError $ "Failure parsing IntegrationType from value: '" <> e
           <> "'. Accepted values: aws, aws_proxy, http, http_proxy, mock"

instance ToText IntegrationType where
    toText = \case
        AWS -> "AWS"
        AWSProxy -> "AWS_PROXY"
        HTTP -> "HTTP"
        HTTPProxy -> "HTTP_PROXY"
        Mock -> "MOCK"

instance Hashable     IntegrationType
instance NFData       IntegrationType
instance ToByteString IntegrationType
instance ToQuery      IntegrationType
instance ToHeader     IntegrationType

instance ToJSON IntegrationType where
    toJSON = toJSONText

instance FromJSON IntegrationType where
    parseJSON = parseJSONText "IntegrationType"

data LocationStatusType
  = Documented
  | Undocumented
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LocationStatusType where
    parser = takeLowerText >>= \case
        "documented" -> pure Documented
        "undocumented" -> pure Undocumented
        e -> fromTextError $ "Failure parsing LocationStatusType from value: '" <> e
           <> "'. Accepted values: documented, undocumented"

instance ToText LocationStatusType where
    toText = \case
        Documented -> "DOCUMENTED"
        Undocumented -> "UNDOCUMENTED"

instance Hashable     LocationStatusType
instance NFData       LocationStatusType
instance ToByteString LocationStatusType
instance ToQuery      LocationStatusType
instance ToHeader     LocationStatusType

instance ToJSON LocationStatusType where
    toJSON = toJSONText

data Op
  = Add
  | Copy
  | Move
  | Remove
  | Replace
  | Test
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Op where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "copy" -> pure Copy
        "move" -> pure Move
        "remove" -> pure Remove
        "replace" -> pure Replace
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing Op from value: '" <> e
           <> "'. Accepted values: add, copy, move, remove, replace, test"

instance ToText Op where
    toText = \case
        Add -> "add"
        Copy -> "copy"
        Move -> "move"
        Remove -> "remove"
        Replace -> "replace"
        Test -> "test"

instance Hashable     Op
instance NFData       Op
instance ToByteString Op
instance ToQuery      Op
instance ToHeader     Op

instance ToJSON Op where
    toJSON = toJSONText

data PutMode
  = Merge
  | Overwrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PutMode where
    parser = takeLowerText >>= \case
        "merge" -> pure Merge
        "overwrite" -> pure Overwrite
        e -> fromTextError $ "Failure parsing PutMode from value: '" <> e
           <> "'. Accepted values: merge, overwrite"

instance ToText PutMode where
    toText = \case
        Merge -> "merge"
        Overwrite -> "overwrite"

instance Hashable     PutMode
instance NFData       PutMode
instance ToByteString PutMode
instance ToQuery      PutMode
instance ToHeader     PutMode

instance ToJSON PutMode where
    toJSON = toJSONText

data QuotaPeriodType
  = Day
  | Month
  | Week
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QuotaPeriodType where
    parser = takeLowerText >>= \case
        "day" -> pure Day
        "month" -> pure Month
        "week" -> pure Week
        e -> fromTextError $ "Failure parsing QuotaPeriodType from value: '" <> e
           <> "'. Accepted values: day, month, week"

instance ToText QuotaPeriodType where
    toText = \case
        Day -> "DAY"
        Month -> "MONTH"
        Week -> "WEEK"

instance Hashable     QuotaPeriodType
instance NFData       QuotaPeriodType
instance ToByteString QuotaPeriodType
instance ToQuery      QuotaPeriodType
instance ToHeader     QuotaPeriodType

instance ToJSON QuotaPeriodType where
    toJSON = toJSONText

instance FromJSON QuotaPeriodType where
    parseJSON = parseJSONText "QuotaPeriodType"

data UnauthorizedCacheControlHeaderStrategy
  = FailWith403
  | SucceedWithResponseHeader
  | SucceedWithoutResponseHeader
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UnauthorizedCacheControlHeaderStrategy where
    parser = takeLowerText >>= \case
        "fail_with_403" -> pure FailWith403
        "succeed_with_response_header" -> pure SucceedWithResponseHeader
        "succeed_without_response_header" -> pure SucceedWithoutResponseHeader
        e -> fromTextError $ "Failure parsing UnauthorizedCacheControlHeaderStrategy from value: '" <> e
           <> "'. Accepted values: fail_with_403, succeed_with_response_header, succeed_without_response_header"

instance ToText UnauthorizedCacheControlHeaderStrategy where
    toText = \case
        FailWith403 -> "FAIL_WITH_403"
        SucceedWithResponseHeader -> "SUCCEED_WITH_RESPONSE_HEADER"
        SucceedWithoutResponseHeader -> "SUCCEED_WITHOUT_RESPONSE_HEADER"

instance Hashable     UnauthorizedCacheControlHeaderStrategy
instance NFData       UnauthorizedCacheControlHeaderStrategy
instance ToByteString UnauthorizedCacheControlHeaderStrategy
instance ToQuery      UnauthorizedCacheControlHeaderStrategy
instance ToHeader     UnauthorizedCacheControlHeaderStrategy

instance FromJSON UnauthorizedCacheControlHeaderStrategy where
    parseJSON = parseJSONText "UnauthorizedCacheControlHeaderStrategy"

data VPCLinkStatus
  = VLSAvailable
  | VLSDeleting
  | VLSFailed
  | VLSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCLinkStatus where
    parser = takeLowerText >>= \case
        "available" -> pure VLSAvailable
        "deleting" -> pure VLSDeleting
        "failed" -> pure VLSFailed
        "pending" -> pure VLSPending
        e -> fromTextError $ "Failure parsing VPCLinkStatus from value: '" <> e
           <> "'. Accepted values: available, deleting, failed, pending"

instance ToText VPCLinkStatus where
    toText = \case
        VLSAvailable -> "AVAILABLE"
        VLSDeleting -> "DELETING"
        VLSFailed -> "FAILED"
        VLSPending -> "PENDING"

instance Hashable     VPCLinkStatus
instance NFData       VPCLinkStatus
instance ToByteString VPCLinkStatus
instance ToQuery      VPCLinkStatus
instance ToHeader     VPCLinkStatus

instance FromJSON VPCLinkStatus where
    parseJSON = parseJSONText "VPCLinkStatus"
