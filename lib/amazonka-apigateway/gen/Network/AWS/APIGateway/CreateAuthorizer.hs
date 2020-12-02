{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Authorizer' resource to an existing 'RestApi' resource.
--
--
-- <http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html AWS CLI>
module Network.AWS.APIGateway.CreateAuthorizer
    (
    -- * Creating a Request
      createAuthorizer
    , CreateAuthorizer
    -- * Request Lenses
    , caAuthorizerURI
    , caIdentityValidationExpression
    , caProviderARNs
    , caAuthorizerResultTtlInSeconds
    , caAuthType
    , caIdentitySource
    , caAuthorizerCredentials
    , caRestAPIId
    , caName
    , caType

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aProviderARNs
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aAuthType
    , aType
    , aIdentitySource
    , aAuthorizerCredentials
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to add a new 'Authorizer' to an existing 'RestApi' resource.
--
--
--
-- /See:/ 'createAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { _caAuthorizerURI                :: !(Maybe Text)
  , _caIdentityValidationExpression :: !(Maybe Text)
  , _caProviderARNs                 :: !(Maybe [Text])
  , _caAuthorizerResultTtlInSeconds :: !(Maybe Int)
  , _caAuthType                     :: !(Maybe Text)
  , _caIdentitySource               :: !(Maybe Text)
  , _caAuthorizerCredentials        :: !(Maybe Text)
  , _caRestAPIId                    :: !Text
  , _caName                         :: !Text
  , _caType                         :: !AuthorizerType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caAuthorizerURI' - Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
--
-- * 'caIdentityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
--
-- * 'caProviderARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- * 'caAuthorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'caAuthType' - Optional customer-defined field, used in Swagger imports and exports without functional impact.
--
-- * 'caIdentitySource' - The identity source for which authorization is requested.     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .    * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
-- * 'caAuthorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- * 'caRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'caName' - [Required] The name of the authorizer.
--
-- * 'caType' - [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
createAuthorizer
    :: Text -- ^ 'caRestAPIId'
    -> Text -- ^ 'caName'
    -> AuthorizerType -- ^ 'caType'
    -> CreateAuthorizer
createAuthorizer pRestAPIId_ pName_ pType_ =
  CreateAuthorizer'
    { _caAuthorizerURI = Nothing
    , _caIdentityValidationExpression = Nothing
    , _caProviderARNs = Nothing
    , _caAuthorizerResultTtlInSeconds = Nothing
    , _caAuthType = Nothing
    , _caIdentitySource = Nothing
    , _caAuthorizerCredentials = Nothing
    , _caRestAPIId = pRestAPIId_
    , _caName = pName_
    , _caType = pType_
    }


-- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
caAuthorizerURI :: Lens' CreateAuthorizer (Maybe Text)
caAuthorizerURI = lens _caAuthorizerURI (\ s a -> s{_caAuthorizerURI = a})

-- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
caIdentityValidationExpression :: Lens' CreateAuthorizer (Maybe Text)
caIdentityValidationExpression = lens _caIdentityValidationExpression (\ s a -> s{_caIdentityValidationExpression = a})

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
caProviderARNs :: Lens' CreateAuthorizer [Text]
caProviderARNs = lens _caProviderARNs (\ s a -> s{_caProviderARNs = a}) . _Default . _Coerce

-- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
caAuthorizerResultTtlInSeconds :: Lens' CreateAuthorizer (Maybe Int)
caAuthorizerResultTtlInSeconds = lens _caAuthorizerResultTtlInSeconds (\ s a -> s{_caAuthorizerResultTtlInSeconds = a})

-- | Optional customer-defined field, used in Swagger imports and exports without functional impact.
caAuthType :: Lens' CreateAuthorizer (Maybe Text)
caAuthType = lens _caAuthType (\ s a -> s{_caAuthType = a})

-- | The identity source for which authorization is requested.     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .    * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
caIdentitySource :: Lens' CreateAuthorizer (Maybe Text)
caIdentitySource = lens _caIdentitySource (\ s a -> s{_caIdentitySource = a})

-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
caAuthorizerCredentials :: Lens' CreateAuthorizer (Maybe Text)
caAuthorizerCredentials = lens _caAuthorizerCredentials (\ s a -> s{_caAuthorizerCredentials = a})

-- | [Required] The string identifier of the associated 'RestApi' .
caRestAPIId :: Lens' CreateAuthorizer Text
caRestAPIId = lens _caRestAPIId (\ s a -> s{_caRestAPIId = a})

-- | [Required] The name of the authorizer.
caName :: Lens' CreateAuthorizer Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
caType :: Lens' CreateAuthorizer AuthorizerType
caType = lens _caType (\ s a -> s{_caType = a})

instance AWSRequest CreateAuthorizer where
        type Rs CreateAuthorizer = Authorizer
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAuthorizer where

instance NFData CreateAuthorizer where

instance ToHeaders CreateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateAuthorizer where
        toJSON CreateAuthorizer'{..}
          = object
              (catMaybes
                 [("authorizerUri" .=) <$> _caAuthorizerURI,
                  ("identityValidationExpression" .=) <$>
                    _caIdentityValidationExpression,
                  ("providerARNs" .=) <$> _caProviderARNs,
                  ("authorizerResultTtlInSeconds" .=) <$>
                    _caAuthorizerResultTtlInSeconds,
                  ("authType" .=) <$> _caAuthType,
                  ("identitySource" .=) <$> _caIdentitySource,
                  ("authorizerCredentials" .=) <$>
                    _caAuthorizerCredentials,
                  Just ("name" .= _caName), Just ("type" .= _caType)])

instance ToPath CreateAuthorizer where
        toPath CreateAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _caRestAPIId, "/authorizers"]

instance ToQuery CreateAuthorizer where
        toQuery = const mempty
