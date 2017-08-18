{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.Sum where

import           Network.AWS.Prelude

data APIKeysFormat =
    CSV
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

-- | The authorizer type. the current value is @TOKEN@ for a Lambda function or @COGNITO_USER_POOLS@ for an Amazon Cognito Your User Pool.
--
--
data AuthorizerType
    = CognitoUserPools
    | Token
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AuthorizerType where
    parser = takeLowerText >>= \case
        "cognito_user_pools" -> pure CognitoUserPools
        "token" -> pure Token
        e -> fromTextError $ "Failure parsing AuthorizerType from value: '" <> e
           <> "'. Accepted values: cognito_user_pools, token"

instance ToText AuthorizerType where
    toText = \case
        CognitoUserPools -> "COGNITO_USER_POOLS"
        Token -> "TOKEN"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data ContentHandlingStrategy
    = ConvertToBinary
    | ConvertToText
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    = API
    | Authorizer
    | Method
    | Model
    | PathParameter
    | QueryParameter
    | RequestBody
    | RequestHeader
    | Resource
    | Response
    | ResponseBody
    | ResponseHeader
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentationPartType where
    parser = takeLowerText >>= \case
        "api" -> pure API
        "authorizer" -> pure Authorizer
        "method" -> pure Method
        "model" -> pure Model
        "path_parameter" -> pure PathParameter
        "query_parameter" -> pure QueryParameter
        "request_body" -> pure RequestBody
        "request_header" -> pure RequestHeader
        "resource" -> pure Resource
        "response" -> pure Response
        "response_body" -> pure ResponseBody
        "response_header" -> pure ResponseHeader
        e -> fromTextError $ "Failure parsing DocumentationPartType from value: '" <> e
           <> "'. Accepted values: api, authorizer, method, model, path_parameter, query_parameter, request_body, request_header, resource, response, response_body, response_header"

instance ToText DocumentationPartType where
    toText = \case
        API -> "API"
        Authorizer -> "AUTHORIZER"
        Method -> "METHOD"
        Model -> "MODEL"
        PathParameter -> "PATH_PARAMETER"
        QueryParameter -> "QUERY_PARAMETER"
        RequestBody -> "REQUEST_BODY"
        RequestHeader -> "REQUEST_HEADER"
        Resource -> "RESOURCE"
        Response -> "RESPONSE"
        ResponseBody -> "RESPONSE_BODY"
        ResponseHeader -> "RESPONSE_HEADER"

instance Hashable     DocumentationPartType
instance NFData       DocumentationPartType
instance ToByteString DocumentationPartType
instance ToQuery      DocumentationPartType
instance ToHeader     DocumentationPartType

instance ToJSON DocumentationPartType where
    toJSON = toJSONText

instance FromJSON DocumentationPartType where
    parseJSON = parseJSONText "DocumentationPartType"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

-- | The integration type. The valid value is @HTTP@ for integrating with an HTTP back end, @AWS@ for any AWS service endpoints, @MOCK@ for testing without actually invoking the back end, @HTTP_PROXY@ for integrating with the HTTP proxy integration, or @AWS_PROXY@ for integrating with the Lambda proxy integration type.
--
--
data IntegrationType
    = AWS
    | AWSProxy
    | HTTP
    | HTTPProxy
    | Mock
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data Op
    = Add
    | Copy
    | Move
    | Remove
    | Replace
    | Test
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
