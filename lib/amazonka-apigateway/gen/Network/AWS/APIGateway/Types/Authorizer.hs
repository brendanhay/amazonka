{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Authorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Authorizer where

import Network.AWS.APIGateway.Types.AuthorizerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.
--
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
--
-- /See:/ 'authorizer' smart constructor.
data Authorizer = Authorizer'
  { _aAuthorizerURI :: !(Maybe Text),
    _aIdentityValidationExpression :: !(Maybe Text),
    _aProviderARNs :: !(Maybe [Text]),
    _aName :: !(Maybe Text),
    _aId :: !(Maybe Text),
    _aAuthorizerResultTtlInSeconds :: !(Maybe Int),
    _aAuthType :: !(Maybe Text),
    _aType :: !(Maybe AuthorizerType),
    _aIdentitySource :: !(Maybe Text),
    _aAuthorizerCredentials :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Authorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAuthorizerURI' - Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
--
-- * 'aIdentityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
--
-- * 'aProviderARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- * 'aName' - [Required] The name of the authorizer.
--
-- * 'aId' - The identifier for the authorizer resource.
--
-- * 'aAuthorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'aAuthType' - Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
--
-- * 'aType' - The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
-- * 'aIdentitySource' - The identity source for which authorization is requested.     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .    * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'aAuthorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
authorizer ::
  Authorizer
authorizer =
  Authorizer'
    { _aAuthorizerURI = Nothing,
      _aIdentityValidationExpression = Nothing,
      _aProviderARNs = Nothing,
      _aName = Nothing,
      _aId = Nothing,
      _aAuthorizerResultTtlInSeconds = Nothing,
      _aAuthType = Nothing,
      _aType = Nothing,
      _aIdentitySource = Nothing,
      _aAuthorizerCredentials = Nothing
    }

-- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
aAuthorizerURI :: Lens' Authorizer (Maybe Text)
aAuthorizerURI = lens _aAuthorizerURI (\s a -> s {_aAuthorizerURI = a})

-- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
aIdentityValidationExpression :: Lens' Authorizer (Maybe Text)
aIdentityValidationExpression = lens _aIdentityValidationExpression (\s a -> s {_aIdentityValidationExpression = a})

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
aProviderARNs :: Lens' Authorizer [Text]
aProviderARNs = lens _aProviderARNs (\s a -> s {_aProviderARNs = a}) . _Default . _Coerce

-- | [Required] The name of the authorizer.
aName :: Lens' Authorizer (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | The identifier for the authorizer resource.
aId :: Lens' Authorizer (Maybe Text)
aId = lens _aId (\s a -> s {_aId = a})

-- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
aAuthorizerResultTtlInSeconds :: Lens' Authorizer (Maybe Int)
aAuthorizerResultTtlInSeconds = lens _aAuthorizerResultTtlInSeconds (\s a -> s {_aAuthorizerResultTtlInSeconds = a})

-- | Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
aAuthType :: Lens' Authorizer (Maybe Text)
aAuthType = lens _aAuthType (\s a -> s {_aAuthType = a})

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
aType :: Lens' Authorizer (Maybe AuthorizerType)
aType = lens _aType (\s a -> s {_aType = a})

-- | The identity source for which authorization is requested.     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .    * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
aIdentitySource :: Lens' Authorizer (Maybe Text)
aIdentitySource = lens _aIdentitySource (\s a -> s {_aIdentitySource = a})

-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
aAuthorizerCredentials :: Lens' Authorizer (Maybe Text)
aAuthorizerCredentials = lens _aAuthorizerCredentials (\s a -> s {_aAuthorizerCredentials = a})

instance FromJSON Authorizer where
  parseJSON =
    withObject
      "Authorizer"
      ( \x ->
          Authorizer'
            <$> (x .:? "authorizerUri")
            <*> (x .:? "identityValidationExpression")
            <*> (x .:? "providerARNs" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "authorizerResultTtlInSeconds")
            <*> (x .:? "authType")
            <*> (x .:? "type")
            <*> (x .:? "identitySource")
            <*> (x .:? "authorizerCredentials")
      )

instance Hashable Authorizer

instance NFData Authorizer
