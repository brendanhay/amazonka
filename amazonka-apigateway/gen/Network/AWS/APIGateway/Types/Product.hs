{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.Product where

import           Network.AWS.APIGateway.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | A resource that can be distributed to callers for executing 'Method' resources that require an API key. API keys can be mapped to any 'Stage' on any 'RestApi' , which indicates that the callers with the API key can make requests to that stage.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'apiKey' smart constructor.
data APIKey = APIKey'
    { _akEnabled         :: !(Maybe Bool)
    , _akValue           :: !(Maybe Text)
    , _akCustomerId      :: !(Maybe Text)
    , _akCreatedDate     :: !(Maybe POSIX)
    , _akName            :: !(Maybe Text)
    , _akId              :: !(Maybe Text)
    , _akStageKeys       :: !(Maybe [Text])
    , _akLastUpdatedDate :: !(Maybe POSIX)
    , _akDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akEnabled' - Specifies whether the API Key can be used by callers.
--
-- * 'akValue' - The value of the API Key.
--
-- * 'akCustomerId' - An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- * 'akCreatedDate' - The timestamp when the API Key was created.
--
-- * 'akName' - The name of the API Key.
--
-- * 'akId' - The identifier of the API Key.
--
-- * 'akStageKeys' - A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- * 'akLastUpdatedDate' - The timestamp when the API Key was last updated.
--
-- * 'akDescription' - The description of the API Key.
apiKey
    :: APIKey
apiKey =
    APIKey'
    { _akEnabled = Nothing
    , _akValue = Nothing
    , _akCustomerId = Nothing
    , _akCreatedDate = Nothing
    , _akName = Nothing
    , _akId = Nothing
    , _akStageKeys = Nothing
    , _akLastUpdatedDate = Nothing
    , _akDescription = Nothing
    }

-- | Specifies whether the API Key can be used by callers.
akEnabled :: Lens' APIKey (Maybe Bool)
akEnabled = lens _akEnabled (\ s a -> s{_akEnabled = a});

-- | The value of the API Key.
akValue :: Lens' APIKey (Maybe Text)
akValue = lens _akValue (\ s a -> s{_akValue = a});

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
akCustomerId :: Lens' APIKey (Maybe Text)
akCustomerId = lens _akCustomerId (\ s a -> s{_akCustomerId = a});

-- | The timestamp when the API Key was created.
akCreatedDate :: Lens' APIKey (Maybe UTCTime)
akCreatedDate = lens _akCreatedDate (\ s a -> s{_akCreatedDate = a}) . mapping _Time;

-- | The name of the API Key.
akName :: Lens' APIKey (Maybe Text)
akName = lens _akName (\ s a -> s{_akName = a});

-- | The identifier of the API Key.
akId :: Lens' APIKey (Maybe Text)
akId = lens _akId (\ s a -> s{_akId = a});

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
akStageKeys :: Lens' APIKey [Text]
akStageKeys = lens _akStageKeys (\ s a -> s{_akStageKeys = a}) . _Default . _Coerce;

-- | The timestamp when the API Key was last updated.
akLastUpdatedDate :: Lens' APIKey (Maybe UTCTime)
akLastUpdatedDate = lens _akLastUpdatedDate (\ s a -> s{_akLastUpdatedDate = a}) . mapping _Time;

-- | The description of the API Key.
akDescription :: Lens' APIKey (Maybe Text)
akDescription = lens _akDescription (\ s a -> s{_akDescription = a});

instance FromJSON APIKey where
        parseJSON
          = withObject "APIKey"
              (\ x ->
                 APIKey' <$>
                   (x .:? "enabled") <*> (x .:? "value") <*>
                     (x .:? "customerId")
                     <*> (x .:? "createdDate")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "stageKeys" .!= mempty)
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable APIKey

instance NFData APIKey

-- | API stage name of the associated API stage in a usage plan.
--
--
--
-- /See:/ 'apiStage' smart constructor.
data APIStage = APIStage'
    { _asStage :: !(Maybe Text)
    , _asApiId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'APIStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asStage' - API stage name of the associated API stage in a usage plan.
--
-- * 'asApiId' - API Id of the associated API stage in a usage plan.
apiStage
    :: APIStage
apiStage =
    APIStage'
    { _asStage = Nothing
    , _asApiId = Nothing
    }

-- | API stage name of the associated API stage in a usage plan.
asStage :: Lens' APIStage (Maybe Text)
asStage = lens _asStage (\ s a -> s{_asStage = a});

-- | API Id of the associated API stage in a usage plan.
asApiId :: Lens' APIStage (Maybe Text)
asApiId = lens _asApiId (\ s a -> s{_asApiId = a});

instance FromJSON APIStage where
        parseJSON
          = withObject "APIStage"
              (\ x ->
                 APIStage' <$> (x .:? "stage") <*> (x .:? "apiId"))

instance Hashable APIStage

instance NFData APIStage

instance ToJSON APIStage where
        toJSON APIStage'{..}
          = object
              (catMaybes
                 [("stage" .=) <$> _asStage,
                  ("apiId" .=) <$> _asApiId])

-- | Represents an AWS account that is associated with Amazon API Gateway.
--
--
-- To view the account info, call @GET@ on this resource.
--
-- __Error Codes__
-- The following exception may be thrown when the request fails.
--
--     * UnauthorizedException    * NotFoundException    * TooManyRequestsException
--
-- For detailed error code information, including the corresponding HTTP Status Codes, see <http://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes API Gateway Error Codes>
--
-- __Example: Get the information about an account.__
-- __Request__
-- @@GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
--
-- @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } } @ @ In addition to making the REST API call directly, you can use the AWS CLI and an AWS SDK to access this resource.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html API Gateway Limits> <http://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html Developer Guide> , <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html AWS CLI>
--
-- /See:/ 'account' smart constructor.
data Account = Account'
    { _aApiKeyVersion     :: !(Maybe Text)
    , _aCloudwatchRoleARN :: !(Maybe Text)
    , _aFeatures          :: !(Maybe [Text])
    , _aThrottleSettings  :: !(Maybe ThrottleSettings)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aApiKeyVersion' - The version of the API keys used for the account.
--
-- * 'aCloudwatchRoleARN' - The ARN of an Amazon CloudWatch role for the current 'Account' .
--
-- * 'aFeatures' - A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
--
-- * 'aThrottleSettings' - Specifies the API request limits configured for the current 'Account' .
account
    :: Account
account =
    Account'
    { _aApiKeyVersion = Nothing
    , _aCloudwatchRoleARN = Nothing
    , _aFeatures = Nothing
    , _aThrottleSettings = Nothing
    }

-- | The version of the API keys used for the account.
aApiKeyVersion :: Lens' Account (Maybe Text)
aApiKeyVersion = lens _aApiKeyVersion (\ s a -> s{_aApiKeyVersion = a});

-- | The ARN of an Amazon CloudWatch role for the current 'Account' .
aCloudwatchRoleARN :: Lens' Account (Maybe Text)
aCloudwatchRoleARN = lens _aCloudwatchRoleARN (\ s a -> s{_aCloudwatchRoleARN = a});

-- | A list of features supported for the account. When usage plans are enabled, the features list will include an entry of @"UsagePlans"@ .
aFeatures :: Lens' Account [Text]
aFeatures = lens _aFeatures (\ s a -> s{_aFeatures = a}) . _Default . _Coerce;

-- | Specifies the API request limits configured for the current 'Account' .
aThrottleSettings :: Lens' Account (Maybe ThrottleSettings)
aThrottleSettings = lens _aThrottleSettings (\ s a -> s{_aThrottleSettings = a});

instance FromJSON Account where
        parseJSON
          = withObject "Account"
              (\ x ->
                 Account' <$>
                   (x .:? "apiKeyVersion") <*>
                     (x .:? "cloudwatchRoleArn")
                     <*> (x .:? "features" .!= mempty)
                     <*> (x .:? "throttleSettings"))

instance Hashable Account

instance NFData Account

-- | Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html Enable custom authorization>
--
-- /See:/ 'authorizer' smart constructor.
data Authorizer = Authorizer'
    { _aAuthorizerURI                :: !(Maybe Text)
    , _aIdentityValidationExpression :: !(Maybe Text)
    , _aProviderARNs                 :: !(Maybe [Text])
    , _aName                         :: !(Maybe Text)
    , _aId                           :: !(Maybe Text)
    , _aAuthorizerResultTtlInSeconds :: !(Maybe Int)
    , _aAuthType                     :: !(Maybe Text)
    , _aType                         :: !(Maybe AuthorizerType)
    , _aIdentitySource               :: !(Maybe Text)
    , _aAuthorizerCredentials        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Authorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAuthorizerURI' - [Required] Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
--
-- * 'aIdentityValidationExpression' - A validation expression for the incoming identity. For @TOKEN@ authorizers, this value should be a regular expression. The incoming token from the client is matched against this expression, and will proceed if the token matches. If the token doesn't match, the client receives a 401 Unauthorized response.
--
-- * 'aProviderARNs' - A list of the provider ARNs of the authorizer. For an @TOKEN@ authorizer, this is not defined. For authorizers of the @COGNITO_USER_POOLS@ type, each element corresponds to a user pool ARN of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ .
--
-- * 'aName' - [Required] The name of the authorizer.
--
-- * 'aId' - The identifier for the authorizer resource.
--
-- * 'aAuthorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- * 'aAuthType' - Optional customer-defined field, used in Swagger imports/exports. Has no functional impact.
--
-- * 'aType' - [Required] The type of the authorizer. Currently, the valid type is @TOKEN@ for a Lambda function or @COGNITO_USER_POOLS@ for an Amazon Cognito user pool.
--
-- * 'aIdentitySource' - [Required] The source of the identity in an incoming request. For a @TOKEN@ authorizer, this value is a mapping expression with the same syntax as integration parameter mappings. The only valid source for tokens is 'header', so the expression should match 'method.request.header.[headerName]'. The value of the header '[headerName]' will be interpreted as the incoming token. For @COGNITO_USER_POOLS@ authorizers, this property is used.
--
-- * 'aAuthorizerCredentials' - Specifies the credentials required for the authorizer, if any. Two options are available. To specify an IAM role for Amazon API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
authorizer
    :: Authorizer
authorizer =
    Authorizer'
    { _aAuthorizerURI = Nothing
    , _aIdentityValidationExpression = Nothing
    , _aProviderARNs = Nothing
    , _aName = Nothing
    , _aId = Nothing
    , _aAuthorizerResultTtlInSeconds = Nothing
    , _aAuthType = Nothing
    , _aType = Nothing
    , _aIdentitySource = Nothing
    , _aAuthorizerCredentials = Nothing
    }

-- | [Required] Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form /2015-03-31/functions/[FunctionARN]/invocations.
aAuthorizerURI :: Lens' Authorizer (Maybe Text)
aAuthorizerURI = lens _aAuthorizerURI (\ s a -> s{_aAuthorizerURI = a});

-- | A validation expression for the incoming identity. For @TOKEN@ authorizers, this value should be a regular expression. The incoming token from the client is matched against this expression, and will proceed if the token matches. If the token doesn't match, the client receives a 401 Unauthorized response.
aIdentityValidationExpression :: Lens' Authorizer (Maybe Text)
aIdentityValidationExpression = lens _aIdentityValidationExpression (\ s a -> s{_aIdentityValidationExpression = a});

-- | A list of the provider ARNs of the authorizer. For an @TOKEN@ authorizer, this is not defined. For authorizers of the @COGNITO_USER_POOLS@ type, each element corresponds to a user pool ARN of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ .
aProviderARNs :: Lens' Authorizer [Text]
aProviderARNs = lens _aProviderARNs (\ s a -> s{_aProviderARNs = a}) . _Default . _Coerce;

-- | [Required] The name of the authorizer.
aName :: Lens' Authorizer (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The identifier for the authorizer resource.
aId :: Lens' Authorizer (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a});

-- | The TTL in seconds of cached authorizer results. If greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
aAuthorizerResultTtlInSeconds :: Lens' Authorizer (Maybe Int)
aAuthorizerResultTtlInSeconds = lens _aAuthorizerResultTtlInSeconds (\ s a -> s{_aAuthorizerResultTtlInSeconds = a});

-- | Optional customer-defined field, used in Swagger imports/exports. Has no functional impact.
aAuthType :: Lens' Authorizer (Maybe Text)
aAuthType = lens _aAuthType (\ s a -> s{_aAuthType = a});

-- | [Required] The type of the authorizer. Currently, the valid type is @TOKEN@ for a Lambda function or @COGNITO_USER_POOLS@ for an Amazon Cognito user pool.
aType :: Lens' Authorizer (Maybe AuthorizerType)
aType = lens _aType (\ s a -> s{_aType = a});

-- | [Required] The source of the identity in an incoming request. For a @TOKEN@ authorizer, this value is a mapping expression with the same syntax as integration parameter mappings. The only valid source for tokens is 'header', so the expression should match 'method.request.header.[headerName]'. The value of the header '[headerName]' will be interpreted as the incoming token. For @COGNITO_USER_POOLS@ authorizers, this property is used.
aIdentitySource :: Lens' Authorizer (Maybe Text)
aIdentitySource = lens _aIdentitySource (\ s a -> s{_aIdentitySource = a});

-- | Specifies the credentials required for the authorizer, if any. Two options are available. To specify an IAM role for Amazon API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
aAuthorizerCredentials :: Lens' Authorizer (Maybe Text)
aAuthorizerCredentials = lens _aAuthorizerCredentials (\ s a -> s{_aAuthorizerCredentials = a});

instance FromJSON Authorizer where
        parseJSON
          = withObject "Authorizer"
              (\ x ->
                 Authorizer' <$>
                   (x .:? "authorizerUri") <*>
                     (x .:? "identityValidationExpression")
                     <*> (x .:? "providerARNs" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "authorizerResultTtlInSeconds")
                     <*> (x .:? "authType")
                     <*> (x .:? "type")
                     <*> (x .:? "identitySource")
                     <*> (x .:? "authorizerCredentials"))

instance Hashable Authorizer

instance NFData Authorizer

-- | Represents the base path that callers of the API must provide as part of the URL after the domain name.
--
--
-- A custom domain name plus a @BasePathMapping@ specification identifies a deployed 'RestApi' in a given stage of the owner 'Account' .<http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'basePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
    { _bpmStage     :: !(Maybe Text)
    , _bpmBasePath  :: !(Maybe Text)
    , _bpmRestAPIId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmStage' - The name of the API's stage.
--
-- * 'bpmBasePath' - The base path name that callers of the API must provide as part of the URL after the domain name.
--
-- * 'bpmRestAPIId' - The name of the API.
basePathMapping
    :: BasePathMapping
basePathMapping =
    BasePathMapping'
    { _bpmStage = Nothing
    , _bpmBasePath = Nothing
    , _bpmRestAPIId = Nothing
    }

-- | The name of the API's stage.
bpmStage :: Lens' BasePathMapping (Maybe Text)
bpmStage = lens _bpmStage (\ s a -> s{_bpmStage = a});

-- | The base path name that callers of the API must provide as part of the URL after the domain name.
bpmBasePath :: Lens' BasePathMapping (Maybe Text)
bpmBasePath = lens _bpmBasePath (\ s a -> s{_bpmBasePath = a});

-- | The name of the API.
bpmRestAPIId :: Lens' BasePathMapping (Maybe Text)
bpmRestAPIId = lens _bpmRestAPIId (\ s a -> s{_bpmRestAPIId = a});

instance FromJSON BasePathMapping where
        parseJSON
          = withObject "BasePathMapping"
              (\ x ->
                 BasePathMapping' <$>
                   (x .:? "stage") <*> (x .:? "basePath") <*>
                     (x .:? "restApiId"))

instance Hashable BasePathMapping

instance NFData BasePathMapping

-- | Represents a client certificate used to configure client-side SSL authentication while sending requests to the integration endpoint.
--
--
-- Client certificates are used authenticate an API by the back-end server. To authenticate an API client (or user), use a custom 'Authorizer' .<http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'clientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
    { _ccPemEncodedCertificate :: !(Maybe Text)
    , _ccClientCertificateId   :: !(Maybe Text)
    , _ccCreatedDate           :: !(Maybe POSIX)
    , _ccExpirationDate        :: !(Maybe POSIX)
    , _ccDescription           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccPemEncodedCertificate' - The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
--
-- * 'ccClientCertificateId' - The identifier of the client certificate.
--
-- * 'ccCreatedDate' - The timestamp when the client certificate was created.
--
-- * 'ccExpirationDate' - The timestamp when the client certificate will expire.
--
-- * 'ccDescription' - The description of the client certificate.
clientCertificate
    :: ClientCertificate
clientCertificate =
    ClientCertificate'
    { _ccPemEncodedCertificate = Nothing
    , _ccClientCertificateId = Nothing
    , _ccCreatedDate = Nothing
    , _ccExpirationDate = Nothing
    , _ccDescription = Nothing
    }

-- | The PEM-encoded public key of the client certificate, which can be used to configure certificate authentication in the integration endpoint .
ccPemEncodedCertificate :: Lens' ClientCertificate (Maybe Text)
ccPemEncodedCertificate = lens _ccPemEncodedCertificate (\ s a -> s{_ccPemEncodedCertificate = a});

-- | The identifier of the client certificate.
ccClientCertificateId :: Lens' ClientCertificate (Maybe Text)
ccClientCertificateId = lens _ccClientCertificateId (\ s a -> s{_ccClientCertificateId = a});

-- | The timestamp when the client certificate was created.
ccCreatedDate :: Lens' ClientCertificate (Maybe UTCTime)
ccCreatedDate = lens _ccCreatedDate (\ s a -> s{_ccCreatedDate = a}) . mapping _Time;

-- | The timestamp when the client certificate will expire.
ccExpirationDate :: Lens' ClientCertificate (Maybe UTCTime)
ccExpirationDate = lens _ccExpirationDate (\ s a -> s{_ccExpirationDate = a}) . mapping _Time;

-- | The description of the client certificate.
ccDescription :: Lens' ClientCertificate (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a});

instance FromJSON ClientCertificate where
        parseJSON
          = withObject "ClientCertificate"
              (\ x ->
                 ClientCertificate' <$>
                   (x .:? "pemEncodedCertificate") <*>
                     (x .:? "clientCertificateId")
                     <*> (x .:? "createdDate")
                     <*> (x .:? "expirationDate")
                     <*> (x .:? "description"))

instance Hashable ClientCertificate

instance NFData ClientCertificate

-- | An immutable representation of a 'RestApi' resource that can be called by users using 'Stages' . A deployment must be associated with a 'Stage' for it to be callable over the Internet.
--
--
-- To create a deployment, call @POST@ on the 'Deployments' resource of a 'RestApi' . To view, update, or delete a deployment, call @GET@ , @PATCH@ , or @DELETE@ on the specified deployment resource (@/restapis/{restapi_id}/deployments/{deployment_id}@ ).'RestApi' , 'Deployments' , 'Stage' , <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
    { _dApiSummary  :: !(Maybe (Map Text (Map Text MethodSnapshot)))
    , _dCreatedDate :: !(Maybe POSIX)
    , _dId          :: !(Maybe Text)
    , _dDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dApiSummary' - A summary of the 'RestApi' at the date and time that the deployment resource was created.
--
-- * 'dCreatedDate' - The date and time that the deployment resource was created.
--
-- * 'dId' - The identifier for the deployment resource.
--
-- * 'dDescription' - The description for the deployment resource.
deployment
    :: Deployment
deployment =
    Deployment'
    { _dApiSummary = Nothing
    , _dCreatedDate = Nothing
    , _dId = Nothing
    , _dDescription = Nothing
    }

-- | A summary of the 'RestApi' at the date and time that the deployment resource was created.
dApiSummary :: Lens' Deployment (HashMap Text (HashMap Text MethodSnapshot))
dApiSummary = lens _dApiSummary (\ s a -> s{_dApiSummary = a}) . _Default . _Map;

-- | The date and time that the deployment resource was created.
dCreatedDate :: Lens' Deployment (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time;

-- | The identifier for the deployment resource.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a});

-- | The description for the deployment resource.
dDescription :: Lens' Deployment (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "apiSummary" .!= mempty) <*>
                     (x .:? "createdDate")
                     <*> (x .:? "id")
                     <*> (x .:? "description"))

instance Hashable Deployment

instance NFData Deployment

-- | A documentation part for a targeted API entity.
--
--
-- A documentation part consists of a content map (@properties@ ) and a target (@location@ ). The target specifies an API entity to which the documentation content applies. The supported API entity types are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Valid @location@ fields depend on the API entity type. All valid fields are not required.
--
-- The content map is a JSON string of API-specific key-value pairs. Although an API can use any shape for the content map, only the Swagger-compliant documentation fields will be injected into the associated API entity definition in the exported Swagger definition file.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationParts'
--
-- /See:/ 'documentationPart' smart constructor.
data DocumentationPart = DocumentationPart'
    { _dpLocation   :: !(Maybe DocumentationPartLocation)
    , _dpId         :: !(Maybe Text)
    , _dpProperties :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpLocation' - The location of the API entity to which the documentation applies. Valid fields depend on the targeted API entity type. All the valid location fields are not required. If not explicitly specified, a valid location field is treated as a wildcard and associated documentation content may be inherited by matching entities, unless overridden.
--
-- * 'dpId' - The 'DocumentationPart' identifier, generated by Amazon API Gateway when the @DocumentationPart@ is created.
--
-- * 'dpProperties' - A content map of API-specific key-value pairs describing the targeted API entity. The map must be encoded as a JSON string, e.g., @"{ \"description\": \"The API does ...\" }"@ . Only Swagger-compliant documentation-related fields from the @properties@ map are exported and, hence, published as part of the API entity definitions, while the original documentation parts are exported in a Swagger extension of @x-amazon-apigateway-documentation@ .
documentationPart
    :: DocumentationPart
documentationPart =
    DocumentationPart'
    { _dpLocation = Nothing
    , _dpId = Nothing
    , _dpProperties = Nothing
    }

-- | The location of the API entity to which the documentation applies. Valid fields depend on the targeted API entity type. All the valid location fields are not required. If not explicitly specified, a valid location field is treated as a wildcard and associated documentation content may be inherited by matching entities, unless overridden.
dpLocation :: Lens' DocumentationPart (Maybe DocumentationPartLocation)
dpLocation = lens _dpLocation (\ s a -> s{_dpLocation = a});

-- | The 'DocumentationPart' identifier, generated by Amazon API Gateway when the @DocumentationPart@ is created.
dpId :: Lens' DocumentationPart (Maybe Text)
dpId = lens _dpId (\ s a -> s{_dpId = a});

-- | A content map of API-specific key-value pairs describing the targeted API entity. The map must be encoded as a JSON string, e.g., @"{ \"description\": \"The API does ...\" }"@ . Only Swagger-compliant documentation-related fields from the @properties@ map are exported and, hence, published as part of the API entity definitions, while the original documentation parts are exported in a Swagger extension of @x-amazon-apigateway-documentation@ .
dpProperties :: Lens' DocumentationPart (Maybe Text)
dpProperties = lens _dpProperties (\ s a -> s{_dpProperties = a});

instance FromJSON DocumentationPart where
        parseJSON
          = withObject "DocumentationPart"
              (\ x ->
                 DocumentationPart' <$>
                   (x .:? "location") <*> (x .:? "id") <*>
                     (x .:? "properties"))

instance Hashable DocumentationPart

instance NFData DocumentationPart

-- | Specifies the target API entity to which the documentation applies.
--
--
--
-- /See:/ 'documentationPartLocation' smart constructor.
data DocumentationPartLocation = DocumentationPartLocation'
    { _dplPath       :: !(Maybe Text)
    , _dplName       :: !(Maybe Text)
    , _dplMethod     :: !(Maybe Text)
    , _dplStatusCode :: !(Maybe Text)
    , _dplType       :: !DocumentationPartType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentationPartLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dplPath' - The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
--
-- * 'dplName' - The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
--
-- * 'dplMethod' - The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
--
-- * 'dplStatusCode' - The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
--
-- * 'dplType' - The type of API entity to which the documentation content applies. It is a valid and required field for API entity types of @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHROZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
documentationPartLocation
    :: DocumentationPartType -- ^ 'dplType'
    -> DocumentationPartLocation
documentationPartLocation pType_ =
    DocumentationPartLocation'
    { _dplPath = Nothing
    , _dplName = Nothing
    , _dplMethod = Nothing
    , _dplStatusCode = Nothing
    , _dplType = pType_
    }

-- | The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
dplPath :: Lens' DocumentationPartLocation (Maybe Text)
dplPath = lens _dplPath (\ s a -> s{_dplPath = a});

-- | The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
dplName :: Lens' DocumentationPartLocation (Maybe Text)
dplName = lens _dplName (\ s a -> s{_dplName = a});

-- | The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
dplMethod :: Lens' DocumentationPartLocation (Maybe Text)
dplMethod = lens _dplMethod (\ s a -> s{_dplMethod = a});

-- | The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
dplStatusCode :: Lens' DocumentationPartLocation (Maybe Text)
dplStatusCode = lens _dplStatusCode (\ s a -> s{_dplStatusCode = a});

-- | The type of API entity to which the documentation content applies. It is a valid and required field for API entity types of @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHROZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
dplType :: Lens' DocumentationPartLocation DocumentationPartType
dplType = lens _dplType (\ s a -> s{_dplType = a});

instance FromJSON DocumentationPartLocation where
        parseJSON
          = withObject "DocumentationPartLocation"
              (\ x ->
                 DocumentationPartLocation' <$>
                   (x .:? "path") <*> (x .:? "name") <*>
                     (x .:? "method")
                     <*> (x .:? "statusCode")
                     <*> (x .: "type"))

instance Hashable DocumentationPartLocation

instance NFData DocumentationPartLocation

instance ToJSON DocumentationPartLocation where
        toJSON DocumentationPartLocation'{..}
          = object
              (catMaybes
                 [("path" .=) <$> _dplPath, ("name" .=) <$> _dplName,
                  ("method" .=) <$> _dplMethod,
                  ("statusCode" .=) <$> _dplStatusCode,
                  Just ("type" .= _dplType)])

-- | A snapshot of the documentation of an API.
--
--
-- Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., Swagger) file.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersions'
--
-- /See:/ 'documentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
    { _dvCreatedDate :: !(Maybe POSIX)
    , _dvVersion     :: !(Maybe Text)
    , _dvDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvCreatedDate' - The date when the API documentation snapshot is created.
--
-- * 'dvVersion' - The version identifier of the API documentation snapshot.
--
-- * 'dvDescription' - The description of the API documentation snapshot.
documentationVersion
    :: DocumentationVersion
documentationVersion =
    DocumentationVersion'
    { _dvCreatedDate = Nothing
    , _dvVersion = Nothing
    , _dvDescription = Nothing
    }

-- | The date when the API documentation snapshot is created.
dvCreatedDate :: Lens' DocumentationVersion (Maybe UTCTime)
dvCreatedDate = lens _dvCreatedDate (\ s a -> s{_dvCreatedDate = a}) . mapping _Time;

-- | The version identifier of the API documentation snapshot.
dvVersion :: Lens' DocumentationVersion (Maybe Text)
dvVersion = lens _dvVersion (\ s a -> s{_dvVersion = a});

-- | The description of the API documentation snapshot.
dvDescription :: Lens' DocumentationVersion (Maybe Text)
dvDescription = lens _dvDescription (\ s a -> s{_dvDescription = a});

instance FromJSON DocumentationVersion where
        parseJSON
          = withObject "DocumentationVersion"
              (\ x ->
                 DocumentationVersion' <$>
                   (x .:? "createdDate") <*> (x .:? "version") <*>
                     (x .:? "description"))

instance Hashable DocumentationVersion

instance NFData DocumentationVersion

-- | Represents a domain name that is contained in a simpler, more intuitive URL that can be called.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Client-Side Certificate>
--
-- /See:/ 'domainName' smart constructor.
data DomainName = DomainName'
    { _dnCertificateName        :: !(Maybe Text)
    , _dnCertificateARN         :: !(Maybe Text)
    , _dnDomainName             :: !(Maybe Text)
    , _dnCertificateUploadDate  :: !(Maybe POSIX)
    , _dnDistributionDomainName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnCertificateName' - The name of the certificate.
--
-- * 'dnCertificateARN' - The reference to an AWS-managed certificate. AWS Certificate Manager is the only supported source.
--
-- * 'dnDomainName' - The name of the 'DomainName' resource.
--
-- * 'dnCertificateUploadDate' - The timestamp when the certificate was uploaded.
--
-- * 'dnDistributionDomainName' - The domain name of the Amazon CloudFront distribution. For more information, see the <http://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
domainName
    :: DomainName
domainName =
    DomainName'
    { _dnCertificateName = Nothing
    , _dnCertificateARN = Nothing
    , _dnDomainName = Nothing
    , _dnCertificateUploadDate = Nothing
    , _dnDistributionDomainName = Nothing
    }

-- | The name of the certificate.
dnCertificateName :: Lens' DomainName (Maybe Text)
dnCertificateName = lens _dnCertificateName (\ s a -> s{_dnCertificateName = a});

-- | The reference to an AWS-managed certificate. AWS Certificate Manager is the only supported source.
dnCertificateARN :: Lens' DomainName (Maybe Text)
dnCertificateARN = lens _dnCertificateARN (\ s a -> s{_dnCertificateARN = a});

-- | The name of the 'DomainName' resource.
dnDomainName :: Lens' DomainName (Maybe Text)
dnDomainName = lens _dnDomainName (\ s a -> s{_dnDomainName = a});

-- | The timestamp when the certificate was uploaded.
dnCertificateUploadDate :: Lens' DomainName (Maybe UTCTime)
dnCertificateUploadDate = lens _dnCertificateUploadDate (\ s a -> s{_dnCertificateUploadDate = a}) . mapping _Time;

-- | The domain name of the Amazon CloudFront distribution. For more information, see the <http://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
dnDistributionDomainName :: Lens' DomainName (Maybe Text)
dnDistributionDomainName = lens _dnDistributionDomainName (\ s a -> s{_dnDistributionDomainName = a});

instance FromJSON DomainName where
        parseJSON
          = withObject "DomainName"
              (\ x ->
                 DomainName' <$>
                   (x .:? "certificateName") <*>
                     (x .:? "certificateArn")
                     <*> (x .:? "domainName")
                     <*> (x .:? "certificateUploadDate")
                     <*> (x .:? "distributionDomainName"))

instance Hashable DomainName

instance NFData DomainName

-- | Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.
--
--
-- In the API Gateway console, the built-in Lambda integration is an AWS integration.<http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'integration' smart constructor.
data Integration = Integration'
    { _iHttpMethod           :: !(Maybe Text)
    , _iRequestTemplates     :: !(Maybe (Map Text Text))
    , _iCredentials          :: !(Maybe Text)
    , _iRequestParameters    :: !(Maybe (Map Text Text))
    , _iContentHandling      :: !(Maybe ContentHandlingStrategy)
    , _iPassthroughBehavior  :: !(Maybe Text)
    , _iUri                  :: !(Maybe Text)
    , _iIntegrationResponses :: !(Maybe (Map Text IntegrationResponse))
    , _iCacheNamespace       :: !(Maybe Text)
    , _iType                 :: !(Maybe IntegrationType)
    , _iCacheKeyParameters   :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Integration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iHttpMethod' - Specifies the integration's HTTP method type.
--
-- * 'iRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'iCredentials' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for Amazon API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
--
-- * 'iRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- * 'iContentHandling' - Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehaviors@ is configured to support payload pass-through.
--
-- * 'iPassthroughBehavior' - Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .      * @WHEN_NO_MATCH@ passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.     * @WHEN_NO_TEMPLATES@ passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.     * @NEVER@ rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
--
-- * 'iUri' - Specifies the integration's Uniform Resource Identifier (URI). For HTTP integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> . For AWS integrations, the URI should be of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:{path|action}/{service_api}@ . @Region@ , @subdomain@ and @service@ are used to determine the right endpoint. For AWS services that use the @Action=@ query string parameter, @service_api@ should be a valid action for the desired service. For RESTful AWS service APIs, @path@ is used to indicate that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ .
--
-- * 'iIntegrationResponses' - Specifies the integration's responses. __Example: Get integration responses of a method__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__  The successful response returns @200 OK@ status and a payload as follows: @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @  <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- * 'iCacheNamespace' - Specifies the integration's cache namespace.
--
-- * 'iType' - Specifies the integration's type. The valid value is @HTTP@ for integrating with an HTTP back end, @AWS@ for any AWS service endpoints, @MOCK@ for testing without actually invoking the back end, @HTTP_PROXY@ for integrating with the HTTP proxy integration, or @AWS_PROXY@ for integrating with the Lambda proxy integration type.
--
-- * 'iCacheKeyParameters' - Specifies the integration's cache key parameters.
integration
    :: Integration
integration =
    Integration'
    { _iHttpMethod = Nothing
    , _iRequestTemplates = Nothing
    , _iCredentials = Nothing
    , _iRequestParameters = Nothing
    , _iContentHandling = Nothing
    , _iPassthroughBehavior = Nothing
    , _iUri = Nothing
    , _iIntegrationResponses = Nothing
    , _iCacheNamespace = Nothing
    , _iType = Nothing
    , _iCacheKeyParameters = Nothing
    }

-- | Specifies the integration's HTTP method type.
iHttpMethod :: Lens' Integration (Maybe Text)
iHttpMethod = lens _iHttpMethod (\ s a -> s{_iHttpMethod = a});

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
iRequestTemplates :: Lens' Integration (HashMap Text Text)
iRequestTemplates = lens _iRequestTemplates (\ s a -> s{_iRequestTemplates = a}) . _Default . _Map;

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for Amazon API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
iCredentials :: Lens' Integration (Maybe Text)
iCredentials = lens _iCredentials (\ s a -> s{_iCredentials = a});

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
iRequestParameters :: Lens' Integration (HashMap Text Text)
iRequestParameters = lens _iRequestParameters (\ s a -> s{_iRequestParameters = a}) . _Default . _Map;

-- | Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehaviors@ is configured to support payload pass-through.
iContentHandling :: Lens' Integration (Maybe ContentHandlingStrategy)
iContentHandling = lens _iContentHandling (\ s a -> s{_iContentHandling = a});

-- | Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .      * @WHEN_NO_MATCH@ passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.     * @WHEN_NO_TEMPLATES@ passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.     * @NEVER@ rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
iPassthroughBehavior :: Lens' Integration (Maybe Text)
iPassthroughBehavior = lens _iPassthroughBehavior (\ s a -> s{_iPassthroughBehavior = a});

-- | Specifies the integration's Uniform Resource Identifier (URI). For HTTP integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> . For AWS integrations, the URI should be of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:{path|action}/{service_api}@ . @Region@ , @subdomain@ and @service@ are used to determine the right endpoint. For AWS services that use the @Action=@ query string parameter, @service_api@ should be a valid action for the desired service. For RESTful AWS service APIs, @path@ is used to indicate that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ .
iUri :: Lens' Integration (Maybe Text)
iUri = lens _iUri (\ s a -> s{_iUri = a});

-- | Specifies the integration's responses. __Example: Get integration responses of a method__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__  The successful response returns @200 OK@ status and a payload as follows: @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @  <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
iIntegrationResponses :: Lens' Integration (HashMap Text IntegrationResponse)
iIntegrationResponses = lens _iIntegrationResponses (\ s a -> s{_iIntegrationResponses = a}) . _Default . _Map;

-- | Specifies the integration's cache namespace.
iCacheNamespace :: Lens' Integration (Maybe Text)
iCacheNamespace = lens _iCacheNamespace (\ s a -> s{_iCacheNamespace = a});

-- | Specifies the integration's type. The valid value is @HTTP@ for integrating with an HTTP back end, @AWS@ for any AWS service endpoints, @MOCK@ for testing without actually invoking the back end, @HTTP_PROXY@ for integrating with the HTTP proxy integration, or @AWS_PROXY@ for integrating with the Lambda proxy integration type.
iType :: Lens' Integration (Maybe IntegrationType)
iType = lens _iType (\ s a -> s{_iType = a});

-- | Specifies the integration's cache key parameters.
iCacheKeyParameters :: Lens' Integration [Text]
iCacheKeyParameters = lens _iCacheKeyParameters (\ s a -> s{_iCacheKeyParameters = a}) . _Default . _Coerce;

instance FromJSON Integration where
        parseJSON
          = withObject "Integration"
              (\ x ->
                 Integration' <$>
                   (x .:? "httpMethod") <*>
                     (x .:? "requestTemplates" .!= mempty)
                     <*> (x .:? "credentials")
                     <*> (x .:? "requestParameters" .!= mempty)
                     <*> (x .:? "contentHandling")
                     <*> (x .:? "passthroughBehavior")
                     <*> (x .:? "uri")
                     <*> (x .:? "integrationResponses" .!= mempty)
                     <*> (x .:? "cacheNamespace")
                     <*> (x .:? "type")
                     <*> (x .:? "cacheKeyParameters" .!= mempty))

instance Hashable Integration

instance NFData Integration

-- | Represents an integration response. The status code must map to an existing 'MethodResponse' , and parameters and templates can be used to transform the back-end response.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'integrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
    { _intContentHandling    :: !(Maybe ContentHandlingStrategy)
    , _intResponseTemplates  :: !(Maybe (Map Text Text))
    , _intSelectionPattern   :: !(Maybe Text)
    , _intStatusCode         :: !(Maybe Text)
    , _intResponseParameters :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'intContentHandling' - Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- * 'intResponseTemplates' - Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- * 'intSelectionPattern' - Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
--
-- * 'intStatusCode' - Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- * 'intResponseParameters' - A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
integrationResponse
    :: IntegrationResponse
integrationResponse =
    IntegrationResponse'
    { _intContentHandling = Nothing
    , _intResponseTemplates = Nothing
    , _intSelectionPattern = Nothing
    , _intStatusCode = Nothing
    , _intResponseParameters = Nothing
    }

-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string. If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
intContentHandling :: Lens' IntegrationResponse (Maybe ContentHandlingStrategy)
intContentHandling = lens _intContentHandling (\ s a -> s{_intContentHandling = a});

-- | Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
intResponseTemplates :: Lens' IntegrationResponse (HashMap Text Text)
intResponseTemplates = lens _intResponseTemplates (\ s a -> s{_intResponseTemplates = a}) . _Default . _Map;

-- | Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
intSelectionPattern :: Lens' IntegrationResponse (Maybe Text)
intSelectionPattern = lens _intSelectionPattern (\ s a -> s{_intSelectionPattern = a});

-- | Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
intStatusCode :: Lens' IntegrationResponse (Maybe Text)
intStatusCode = lens _intStatusCode (\ s a -> s{_intStatusCode = a});

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
intResponseParameters :: Lens' IntegrationResponse (HashMap Text Text)
intResponseParameters = lens _intResponseParameters (\ s a -> s{_intResponseParameters = a}) . _Default . _Map;

instance FromJSON IntegrationResponse where
        parseJSON
          = withObject "IntegrationResponse"
              (\ x ->
                 IntegrationResponse' <$>
                   (x .:? "contentHandling") <*>
                     (x .:? "responseTemplates" .!= mempty)
                     <*> (x .:? "selectionPattern")
                     <*> (x .:? "statusCode")
                     <*> (x .:? "responseParameters" .!= mempty))

instance Hashable IntegrationResponse

instance NFData IntegrationResponse

-- | Represents a client-facing interface by which the client calls the API to access back-end resources. A __Method__ resource is integrated with an 'Integration' resource. Both consist of a request and one or more responses. The method request takes the client input that is passed to the back end through the integration request. A method response returns the output from the back end to the client through an integration response. A method request is embodied in a __Method__ resource, whereas an integration request is embodied in an 'Integration' resource. On the other hand, a method response is represented by a 'MethodResponse' resource, whereas an integration response is represented by an 'IntegrationResponse' resource.
--
--
--
--
-- __Example: Retrive the GET method on a specified resource__
-- __Request__
-- The following example request retrieves the information about the GET method on an API resource (@3kzxbg5sa2@ ) of an API (@fugvjdxtri@ ).
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T210259Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
--
-- @@{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": true, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E\")" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }@ @ In the example above, the response template for the @200 OK@ response maps the JSON output from the @ListStreams@ action in the back end to an XML output. The mapping template is URL-encoded as @%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E@ and the output is decoded using the <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#util-templat-reference > util.urlDecode()> helper function.
--
-- 'MethodResponse' , 'Integration' , 'IntegrationResponse' , 'Resource' , <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-method-settings.html Set up an API's method>
--
-- /See:/ 'method' smart constructor.
data Method = Method'
    { _mMethodResponses   :: !(Maybe (Map Text MethodResponse))
    , _mHttpMethod        :: !(Maybe Text)
    , _mRequestModels     :: !(Maybe (Map Text Text))
    , _mRequestParameters :: !(Maybe (Map Text Bool))
    , _mAuthorizerId      :: !(Maybe Text)
    , _mOperationName     :: !(Maybe Text)
    , _mAuthorizationType :: !(Maybe Text)
    , _mApiKeyRequired    :: !(Maybe Bool)
    , _mMethodIntegration :: !(Maybe Integration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Method' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMethodResponses' - Gets a method response associated with a given HTTP status code.  The collection of method responses are encapsulated in a key-value map, where the key is a response's HTTP status code and the value is a 'MethodResponse' resource that specifies the response returned to the caller from the back end through the integration response. __Example: Get a 200 OK response of a GET method__  __Request__  @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  The successful response returns a @200 OK@ status code and a payload similar to the following: @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }@ @  <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
--
-- * 'mHttpMethod' - The method's HTTP verb.
--
-- * 'mRequestModels' - A key-value map specifying data schemas, represented by 'Model' resources, (as the mapped value) of the request payloads of given content types (as the mapping key).
--
-- * 'mRequestParameters' - A key-value map defining required or optional method request parameters that can be accepted by Amazon API Gateway. A key is a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or templates.
--
-- * 'mAuthorizerId' - The identifier of an 'Authorizer' to use on this method. The @authorizationType@ must be @CUSTOM@ .
--
-- * 'mOperationName' - A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in <http://petstore-demo-endpoint.execute-api.com/petstore/pets PetStore> example.
--
-- * 'mAuthorizationType' - The method's authorization type.
--
-- * 'mApiKeyRequired' - A boolean flag specifying whether a valid 'ApiKey' is required to invoke this method.
--
-- * 'mMethodIntegration' - Gets the method's integration responsible for passing the client-submitted request to the back end and performing necessary transformations to make the request compliant with the back end. __Example: __  __Request__  @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  The successful response returns a @200 OK@ status code and a payload similar to the following: @@{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }@ @  <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
method
    :: Method
method =
    Method'
    { _mMethodResponses = Nothing
    , _mHttpMethod = Nothing
    , _mRequestModels = Nothing
    , _mRequestParameters = Nothing
    , _mAuthorizerId = Nothing
    , _mOperationName = Nothing
    , _mAuthorizationType = Nothing
    , _mApiKeyRequired = Nothing
    , _mMethodIntegration = Nothing
    }

-- | Gets a method response associated with a given HTTP status code.  The collection of method responses are encapsulated in a key-value map, where the key is a response's HTTP status code and the value is a 'MethodResponse' resource that specifies the response returned to the caller from the back end through the integration response. __Example: Get a 200 OK response of a GET method__  __Request__  @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  The successful response returns a @200 OK@ status code and a payload similar to the following: @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }@ @  <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
mMethodResponses :: Lens' Method (HashMap Text MethodResponse)
mMethodResponses = lens _mMethodResponses (\ s a -> s{_mMethodResponses = a}) . _Default . _Map;

-- | The method's HTTP verb.
mHttpMethod :: Lens' Method (Maybe Text)
mHttpMethod = lens _mHttpMethod (\ s a -> s{_mHttpMethod = a});

-- | A key-value map specifying data schemas, represented by 'Model' resources, (as the mapped value) of the request payloads of given content types (as the mapping key).
mRequestModels :: Lens' Method (HashMap Text Text)
mRequestModels = lens _mRequestModels (\ s a -> s{_mRequestModels = a}) . _Default . _Map;

-- | A key-value map defining required or optional method request parameters that can be accepted by Amazon API Gateway. A key is a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or templates.
mRequestParameters :: Lens' Method (HashMap Text Bool)
mRequestParameters = lens _mRequestParameters (\ s a -> s{_mRequestParameters = a}) . _Default . _Map;

-- | The identifier of an 'Authorizer' to use on this method. The @authorizationType@ must be @CUSTOM@ .
mAuthorizerId :: Lens' Method (Maybe Text)
mAuthorizerId = lens _mAuthorizerId (\ s a -> s{_mAuthorizerId = a});

-- | A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in <http://petstore-demo-endpoint.execute-api.com/petstore/pets PetStore> example.
mOperationName :: Lens' Method (Maybe Text)
mOperationName = lens _mOperationName (\ s a -> s{_mOperationName = a});

-- | The method's authorization type.
mAuthorizationType :: Lens' Method (Maybe Text)
mAuthorizationType = lens _mAuthorizationType (\ s a -> s{_mAuthorizationType = a});

-- | A boolean flag specifying whether a valid 'ApiKey' is required to invoke this method.
mApiKeyRequired :: Lens' Method (Maybe Bool)
mApiKeyRequired = lens _mApiKeyRequired (\ s a -> s{_mApiKeyRequired = a});

-- | Gets the method's integration responsible for passing the client-submitted request to the back end and performing necessary transformations to make the request compliant with the back end. __Example: __  __Request__  @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  The successful response returns a @200 OK@ status code and a payload similar to the following: @@{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }@ @  <http://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
mMethodIntegration :: Lens' Method (Maybe Integration)
mMethodIntegration = lens _mMethodIntegration (\ s a -> s{_mMethodIntegration = a});

instance FromJSON Method where
        parseJSON
          = withObject "Method"
              (\ x ->
                 Method' <$>
                   (x .:? "methodResponses" .!= mempty) <*>
                     (x .:? "httpMethod")
                     <*> (x .:? "requestModels" .!= mempty)
                     <*> (x .:? "requestParameters" .!= mempty)
                     <*> (x .:? "authorizerId")
                     <*> (x .:? "operationName")
                     <*> (x .:? "authorizationType")
                     <*> (x .:? "apiKeyRequired")
                     <*> (x .:? "methodIntegration"))

instance Hashable Method

instance NFData Method

-- | Represents a method response of a given HTTP status code returned to the client. The method response is passed from the back end through the associated integration response that can be transformed using a mapping template.
--
--
--
--
-- __Example: A __MethodResponse__ instance of an API__
-- __Request__
-- The example request retrieves a __MethodResponse__ of the 200 status code.
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T222952Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns @200 OK@ status and a payload as follows:
--
-- @@{ "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" }@ @
--
-- 'Method' , 'IntegrationResponse' , 'Integration' <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'methodResponse' smart constructor.
data MethodResponse = MethodResponse'
    { _mResponseModels     :: !(Maybe (Map Text Text))
    , _mStatusCode         :: !(Maybe Text)
    , _mResponseParameters :: !(Maybe (Map Text Bool))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mResponseModels' - Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
--
-- * 'mStatusCode' - The method response's status code.
--
-- * 'mResponseParameters' - A key-value map specifying required or optional response parameters that Amazon API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. Amazon API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
methodResponse
    :: MethodResponse
methodResponse =
    MethodResponse'
    { _mResponseModels = Nothing
    , _mStatusCode = Nothing
    , _mResponseParameters = Nothing
    }

-- | Specifies the 'Model' resources used for the response's content-type. Response models are represented as a key/value map, with a content-type as the key and a 'Model' name as the value.
mResponseModels :: Lens' MethodResponse (HashMap Text Text)
mResponseModels = lens _mResponseModels (\ s a -> s{_mResponseModels = a}) . _Default . _Map;

-- | The method response's status code.
mStatusCode :: Lens' MethodResponse (Maybe Text)
mStatusCode = lens _mStatusCode (\ s a -> s{_mStatusCode = a});

-- | A key-value map specifying required or optional response parameters that Amazon API Gateway can send back to the caller. A key defines a method response header and the value specifies whether the associated method response header is required or not. The expression of the key must match the pattern @method.response.header.{name}@ , where @name@ is a valid and unique header name. Amazon API Gateway passes certain integration response data to the method response headers specified here according to the mapping you prescribe in the API's 'IntegrationResponse' . The integration response data that can be mapped include an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
mResponseParameters :: Lens' MethodResponse (HashMap Text Bool)
mResponseParameters = lens _mResponseParameters (\ s a -> s{_mResponseParameters = a}) . _Default . _Map;

instance FromJSON MethodResponse where
        parseJSON
          = withObject "MethodResponse"
              (\ x ->
                 MethodResponse' <$>
                   (x .:? "responseModels" .!= mempty) <*>
                     (x .:? "statusCode")
                     <*> (x .:? "responseParameters" .!= mempty))

instance Hashable MethodResponse

instance NFData MethodResponse

-- | Specifies the method setting properties.
--
--
--
-- /See:/ 'methodSetting' smart constructor.
data MethodSetting = MethodSetting'
    { _msCacheTtlInSeconds                      :: !(Maybe Int)
    , _msDataTraceEnabled                       :: !(Maybe Bool)
    , _msThrottlingBurstLimit                   :: !(Maybe Int)
    , _msCacheDataEncrypted                     :: !(Maybe Bool)
    , _msLoggingLevel                           :: !(Maybe Text)
    , _msRequireAuthorizationForCacheControl    :: !(Maybe Bool)
    , _msCachingEnabled                         :: !(Maybe Bool)
    , _msMetricsEnabled                         :: !(Maybe Bool)
    , _msThrottlingRateLimit                    :: !(Maybe Double)
    , _msUnauthorizedCacheControlHeaderStrategy :: !(Maybe UnauthorizedCacheControlHeaderStrategy)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MethodSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msCacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
--
-- * 'msDataTraceEnabled' - Specifies whether data trace logging is enabled for this method, which effects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
--
-- * 'msThrottlingBurstLimit' - Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
--
-- * 'msCacheDataEncrypted' - Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
--
-- * 'msLoggingLevel' - Specifies the logging level for this method, which effects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ .
--
-- * 'msRequireAuthorizationForCacheControl' - Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
--
-- * 'msCachingEnabled' - Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
--
-- * 'msMetricsEnabled' - Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
--
-- * 'msThrottlingRateLimit' - Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
--
-- * 'msUnauthorizedCacheControlHeaderStrategy' - Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
methodSetting
    :: MethodSetting
methodSetting =
    MethodSetting'
    { _msCacheTtlInSeconds = Nothing
    , _msDataTraceEnabled = Nothing
    , _msThrottlingBurstLimit = Nothing
    , _msCacheDataEncrypted = Nothing
    , _msLoggingLevel = Nothing
    , _msRequireAuthorizationForCacheControl = Nothing
    , _msCachingEnabled = Nothing
    , _msMetricsEnabled = Nothing
    , _msThrottlingRateLimit = Nothing
    , _msUnauthorizedCacheControlHeaderStrategy = Nothing
    }

-- | Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
msCacheTtlInSeconds :: Lens' MethodSetting (Maybe Int)
msCacheTtlInSeconds = lens _msCacheTtlInSeconds (\ s a -> s{_msCacheTtlInSeconds = a});

-- | Specifies whether data trace logging is enabled for this method, which effects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
msDataTraceEnabled :: Lens' MethodSetting (Maybe Bool)
msDataTraceEnabled = lens _msDataTraceEnabled (\ s a -> s{_msDataTraceEnabled = a});

-- | Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
msThrottlingBurstLimit :: Lens' MethodSetting (Maybe Int)
msThrottlingBurstLimit = lens _msThrottlingBurstLimit (\ s a -> s{_msThrottlingBurstLimit = a});

-- | Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
msCacheDataEncrypted :: Lens' MethodSetting (Maybe Bool)
msCacheDataEncrypted = lens _msCacheDataEncrypted (\ s a -> s{_msCacheDataEncrypted = a});

-- | Specifies the logging level for this method, which effects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ .
msLoggingLevel :: Lens' MethodSetting (Maybe Text)
msLoggingLevel = lens _msLoggingLevel (\ s a -> s{_msLoggingLevel = a});

-- | Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
msRequireAuthorizationForCacheControl :: Lens' MethodSetting (Maybe Bool)
msRequireAuthorizationForCacheControl = lens _msRequireAuthorizationForCacheControl (\ s a -> s{_msRequireAuthorizationForCacheControl = a});

-- | Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
msCachingEnabled :: Lens' MethodSetting (Maybe Bool)
msCachingEnabled = lens _msCachingEnabled (\ s a -> s{_msCachingEnabled = a});

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
msMetricsEnabled :: Lens' MethodSetting (Maybe Bool)
msMetricsEnabled = lens _msMetricsEnabled (\ s a -> s{_msMetricsEnabled = a});

-- | Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
msThrottlingRateLimit :: Lens' MethodSetting (Maybe Double)
msThrottlingRateLimit = lens _msThrottlingRateLimit (\ s a -> s{_msThrottlingRateLimit = a});

-- | Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
msUnauthorizedCacheControlHeaderStrategy :: Lens' MethodSetting (Maybe UnauthorizedCacheControlHeaderStrategy)
msUnauthorizedCacheControlHeaderStrategy = lens _msUnauthorizedCacheControlHeaderStrategy (\ s a -> s{_msUnauthorizedCacheControlHeaderStrategy = a});

instance FromJSON MethodSetting where
        parseJSON
          = withObject "MethodSetting"
              (\ x ->
                 MethodSetting' <$>
                   (x .:? "cacheTtlInSeconds") <*>
                     (x .:? "dataTraceEnabled")
                     <*> (x .:? "throttlingBurstLimit")
                     <*> (x .:? "cacheDataEncrypted")
                     <*> (x .:? "loggingLevel")
                     <*> (x .:? "requireAuthorizationForCacheControl")
                     <*> (x .:? "cachingEnabled")
                     <*> (x .:? "metricsEnabled")
                     <*> (x .:? "throttlingRateLimit")
                     <*> (x .:? "unauthorizedCacheControlHeaderStrategy"))

instance Hashable MethodSetting

instance NFData MethodSetting

-- | Represents a summary of a 'Method' resource, given a particular date and time.
--
--
--
-- /See:/ 'methodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
    { _msAuthorizationType :: !(Maybe Text)
    , _msApiKeyRequired    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MethodSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msAuthorizationType' - Specifies the type of authorization used for the method.
--
-- * 'msApiKeyRequired' - Specifies whether the method requires a valid 'ApiKey' .
methodSnapshot
    :: MethodSnapshot
methodSnapshot =
    MethodSnapshot'
    { _msAuthorizationType = Nothing
    , _msApiKeyRequired = Nothing
    }

-- | Specifies the type of authorization used for the method.
msAuthorizationType :: Lens' MethodSnapshot (Maybe Text)
msAuthorizationType = lens _msAuthorizationType (\ s a -> s{_msAuthorizationType = a});

-- | Specifies whether the method requires a valid 'ApiKey' .
msApiKeyRequired :: Lens' MethodSnapshot (Maybe Bool)
msApiKeyRequired = lens _msApiKeyRequired (\ s a -> s{_msApiKeyRequired = a});

instance FromJSON MethodSnapshot where
        parseJSON
          = withObject "MethodSnapshot"
              (\ x ->
                 MethodSnapshot' <$>
                   (x .:? "authorizationType") <*>
                     (x .:? "apiKeyRequired"))

instance Hashable MethodSnapshot

instance NFData MethodSnapshot

-- | Represents the data structure of a method's request or response payload.
--
--
-- A request model defines the data structure of the client-supplied request payload. A response model defines the data structure of the response payload returned by the back end. Although not required, models are useful for mapping payloads between the front end and back end.
--
-- A model is used for generating an API's SDK, validating the input request body, and creating a skeletal mapping template.
--
-- 'Method' , 'MethodResponse' , <http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Models and Mappings>
--
-- /See:/ 'model' smart constructor.
data Model = Model'
    { _mSchema      :: !(Maybe Text)
    , _mName        :: !(Maybe Text)
    , _mId          :: !(Maybe Text)
    , _mDescription :: !(Maybe Text)
    , _mContentType :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Model' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSchema' - The schema for the model. For @application/json@ models, this should be <http://json-schema.org/documentation.html JSON-schema draft v4> model. Do not include "\*/" characters in the description of any properties because such "\*/" characters may be interpreted as the closing marker for comments in some languages, such as Java or JavaScript, causing the installation of your API's SDK generated by API Gateway to fail.
--
-- * 'mName' - The name of the model.
--
-- * 'mId' - The identifier for the model resource.
--
-- * 'mDescription' - The description of the model.
--
-- * 'mContentType' - The content-type for the model.
model
    :: Model
model =
    Model'
    { _mSchema = Nothing
    , _mName = Nothing
    , _mId = Nothing
    , _mDescription = Nothing
    , _mContentType = Nothing
    }

-- | The schema for the model. For @application/json@ models, this should be <http://json-schema.org/documentation.html JSON-schema draft v4> model. Do not include "\*/" characters in the description of any properties because such "\*/" characters may be interpreted as the closing marker for comments in some languages, such as Java or JavaScript, causing the installation of your API's SDK generated by API Gateway to fail.
mSchema :: Lens' Model (Maybe Text)
mSchema = lens _mSchema (\ s a -> s{_mSchema = a});

-- | The name of the model.
mName :: Lens' Model (Maybe Text)
mName = lens _mName (\ s a -> s{_mName = a});

-- | The identifier for the model resource.
mId :: Lens' Model (Maybe Text)
mId = lens _mId (\ s a -> s{_mId = a});

-- | The description of the model.
mDescription :: Lens' Model (Maybe Text)
mDescription = lens _mDescription (\ s a -> s{_mDescription = a});

-- | The content-type for the model.
mContentType :: Lens' Model (Maybe Text)
mContentType = lens _mContentType (\ s a -> s{_mContentType = a});

instance FromJSON Model where
        parseJSON
          = withObject "Model"
              (\ x ->
                 Model' <$>
                   (x .:? "schema") <*> (x .:? "name") <*> (x .:? "id")
                     <*> (x .:? "description")
                     <*> (x .:? "contentType"))

instance Hashable Model

instance NFData Model

-- | A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.
--
-- /See:/ 'patchOperation' smart constructor.
data PatchOperation = PatchOperation'
    { _poOp    :: !(Maybe Op)
    , _poPath  :: !(Maybe Text)
    , _poValue :: !(Maybe Text)
    , _poFrom  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PatchOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poOp' - An update operation to be performed with this PATCH request. The valid value can be "add", "remove", or "replace". Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
--
-- * 'poPath' - The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
--
-- * 'poValue' - The new target value of the update operation.
--
-- * 'poFrom' - Not supported.
patchOperation
    :: PatchOperation
patchOperation =
    PatchOperation'
    { _poOp = Nothing
    , _poPath = Nothing
    , _poValue = Nothing
    , _poFrom = Nothing
    }

-- | An update operation to be performed with this PATCH request. The valid value can be "add", "remove", or "replace". Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
poOp :: Lens' PatchOperation (Maybe Op)
poOp = lens _poOp (\ s a -> s{_poOp = a});

-- | The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
poPath :: Lens' PatchOperation (Maybe Text)
poPath = lens _poPath (\ s a -> s{_poPath = a});

-- | The new target value of the update operation.
poValue :: Lens' PatchOperation (Maybe Text)
poValue = lens _poValue (\ s a -> s{_poValue = a});

-- | Not supported.
poFrom :: Lens' PatchOperation (Maybe Text)
poFrom = lens _poFrom (\ s a -> s{_poFrom = a});

instance Hashable PatchOperation

instance NFData PatchOperation

instance ToJSON PatchOperation where
        toJSON PatchOperation'{..}
          = object
              (catMaybes
                 [("op" .=) <$> _poOp, ("path" .=) <$> _poPath,
                  ("value" .=) <$> _poValue, ("from" .=) <$> _poFrom])

-- | Quotas configured for a usage plan.
--
--
--
-- /See:/ 'quotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
    { _qsOffset :: !(Maybe Int)
    , _qsPeriod :: !(Maybe QuotaPeriodType)
    , _qsLimit  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsOffset' - The number of requests subtracted from the given limit in the initial time period.
--
-- * 'qsPeriod' - The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
--
-- * 'qsLimit' - The maximum number of requests that can be made in a given time period.
quotaSettings
    :: QuotaSettings
quotaSettings =
    QuotaSettings'
    { _qsOffset = Nothing
    , _qsPeriod = Nothing
    , _qsLimit = Nothing
    }

-- | The number of requests subtracted from the given limit in the initial time period.
qsOffset :: Lens' QuotaSettings (Maybe Int)
qsOffset = lens _qsOffset (\ s a -> s{_qsOffset = a});

-- | The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
qsPeriod :: Lens' QuotaSettings (Maybe QuotaPeriodType)
qsPeriod = lens _qsPeriod (\ s a -> s{_qsPeriod = a});

-- | The maximum number of requests that can be made in a given time period.
qsLimit :: Lens' QuotaSettings (Maybe Int)
qsLimit = lens _qsLimit (\ s a -> s{_qsLimit = a});

instance FromJSON QuotaSettings where
        parseJSON
          = withObject "QuotaSettings"
              (\ x ->
                 QuotaSettings' <$>
                   (x .:? "offset") <*> (x .:? "period") <*>
                     (x .:? "limit"))

instance Hashable QuotaSettings

instance NFData QuotaSettings

instance ToJSON QuotaSettings where
        toJSON QuotaSettings'{..}
          = object
              (catMaybes
                 [("offset" .=) <$> _qsOffset,
                  ("period" .=) <$> _qsPeriod,
                  ("limit" .=) <$> _qsLimit])

-- | Represents an API resource.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
    { _rPathPart        :: !(Maybe Text)
    , _rPath            :: !(Maybe Text)
    , _rId              :: !(Maybe Text)
    , _rResourceMethods :: !(Maybe (Map Text Method))
    , _rParentId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPathPart' - The last path segment for this resource.
--
-- * 'rPath' - The full path for this resource.
--
-- * 'rId' - The resource's identifier.
--
-- * 'rResourceMethods' - Gets an API resource's method of a given HTTP verb. The resource methods are a map of methods indexed by methods' HTTP verbs enabled on the resource. This method map is included in the @200 OK@ response of the @GET /restapis/{restapi_id}/resources/{resource_id}@ or @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ request. __Example: Get the GET method of an API resource__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160608T031827Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160608/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  @@{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": false, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }@ @ If the @OPTIONS@ is enabled on the resource, you can follow the example here to get that method. Just replace the @GET@ of the last path segment in the request URL with @OPTIONS@ .
--
-- * 'rParentId' - The parent resource's identifier.
resource
    :: Resource
resource =
    Resource'
    { _rPathPart = Nothing
    , _rPath = Nothing
    , _rId = Nothing
    , _rResourceMethods = Nothing
    , _rParentId = Nothing
    }

-- | The last path segment for this resource.
rPathPart :: Lens' Resource (Maybe Text)
rPathPart = lens _rPathPart (\ s a -> s{_rPathPart = a});

-- | The full path for this resource.
rPath :: Lens' Resource (Maybe Text)
rPath = lens _rPath (\ s a -> s{_rPath = a});

-- | The resource's identifier.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a});

-- | Gets an API resource's method of a given HTTP verb. The resource methods are a map of methods indexed by methods' HTTP verbs enabled on the resource. This method map is included in the @200 OK@ response of the @GET /restapis/{restapi_id}/resources/{resource_id}@ or @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ request. __Example: Get the GET method of an API resource__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160608T031827Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160608/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__  @@{ "_links": { "curies": [ { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": false, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }@ @ If the @OPTIONS@ is enabled on the resource, you can follow the example here to get that method. Just replace the @GET@ of the last path segment in the request URL with @OPTIONS@ .
rResourceMethods :: Lens' Resource (HashMap Text Method)
rResourceMethods = lens _rResourceMethods (\ s a -> s{_rResourceMethods = a}) . _Default . _Map;

-- | The parent resource's identifier.
rParentId :: Lens' Resource (Maybe Text)
rParentId = lens _rParentId (\ s a -> s{_rParentId = a});

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "pathPart") <*> (x .:? "path") <*>
                     (x .:? "id")
                     <*> (x .:? "resourceMethods" .!= mempty)
                     <*> (x .:? "parentId"))

instance Hashable Resource

instance NFData Resource

-- | Represents a REST API.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'restAPI' smart constructor.
data RestAPI = RestAPI'
    { _raBinaryMediaTypes :: !(Maybe [Text])
    , _raWarnings         :: !(Maybe [Text])
    , _raCreatedDate      :: !(Maybe POSIX)
    , _raName             :: !(Maybe Text)
    , _raVersion          :: !(Maybe Text)
    , _raId               :: !(Maybe Text)
    , _raDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raBinaryMediaTypes' - The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
--
-- * 'raWarnings' - The warning messages reported when @failonwarnings@ is turned on during API import.
--
-- * 'raCreatedDate' - The timestamp when the API was created.
--
-- * 'raName' - The API's name.
--
-- * 'raVersion' - A version identifier for the API.
--
-- * 'raId' - The API's identifier. This identifier is unique across all of your APIs in Amazon API Gateway.
--
-- * 'raDescription' - The API's description.
restAPI
    :: RestAPI
restAPI =
    RestAPI'
    { _raBinaryMediaTypes = Nothing
    , _raWarnings = Nothing
    , _raCreatedDate = Nothing
    , _raName = Nothing
    , _raVersion = Nothing
    , _raId = Nothing
    , _raDescription = Nothing
    }

-- | The list of binary media types supported by the 'RestApi' . By default, the 'RestApi' supports only UTF-8-encoded text payloads.
raBinaryMediaTypes :: Lens' RestAPI [Text]
raBinaryMediaTypes = lens _raBinaryMediaTypes (\ s a -> s{_raBinaryMediaTypes = a}) . _Default . _Coerce;

-- | The warning messages reported when @failonwarnings@ is turned on during API import.
raWarnings :: Lens' RestAPI [Text]
raWarnings = lens _raWarnings (\ s a -> s{_raWarnings = a}) . _Default . _Coerce;

-- | The timestamp when the API was created.
raCreatedDate :: Lens' RestAPI (Maybe UTCTime)
raCreatedDate = lens _raCreatedDate (\ s a -> s{_raCreatedDate = a}) . mapping _Time;

-- | The API's name.
raName :: Lens' RestAPI (Maybe Text)
raName = lens _raName (\ s a -> s{_raName = a});

-- | A version identifier for the API.
raVersion :: Lens' RestAPI (Maybe Text)
raVersion = lens _raVersion (\ s a -> s{_raVersion = a});

-- | The API's identifier. This identifier is unique across all of your APIs in Amazon API Gateway.
raId :: Lens' RestAPI (Maybe Text)
raId = lens _raId (\ s a -> s{_raId = a});

-- | The API's description.
raDescription :: Lens' RestAPI (Maybe Text)
raDescription = lens _raDescription (\ s a -> s{_raDescription = a});

instance FromJSON RestAPI where
        parseJSON
          = withObject "RestAPI"
              (\ x ->
                 RestAPI' <$>
                   (x .:? "binaryMediaTypes" .!= mempty) <*>
                     (x .:? "warnings" .!= mempty)
                     <*> (x .:? "createdDate")
                     <*> (x .:? "name")
                     <*> (x .:? "version")
                     <*> (x .:? "id")
                     <*> (x .:? "description"))

instance Hashable RestAPI

instance NFData RestAPI

-- | A configuration property of an SDK type.
--
--
--
-- /See:/ 'sdkConfigurationProperty' smart constructor.
data SDKConfigurationProperty = SDKConfigurationProperty'
    { _scpFriendlyName :: !(Maybe Text)
    , _scpRequired     :: !(Maybe Bool)
    , _scpName         :: !(Maybe Text)
    , _scpDefaultValue :: !(Maybe Text)
    , _scpDescription  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SDKConfigurationProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scpFriendlyName' - The user-friendly name of an 'SdkType' configuration property.
--
-- * 'scpRequired' - A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
--
-- * 'scpName' - The name of a an 'SdkType' configuration property.
--
-- * 'scpDefaultValue' - The default value of an 'SdkType' configuration property.
--
-- * 'scpDescription' - The description of an 'SdkType' configuration property.
sdkConfigurationProperty
    :: SDKConfigurationProperty
sdkConfigurationProperty =
    SDKConfigurationProperty'
    { _scpFriendlyName = Nothing
    , _scpRequired = Nothing
    , _scpName = Nothing
    , _scpDefaultValue = Nothing
    , _scpDescription = Nothing
    }

-- | The user-friendly name of an 'SdkType' configuration property.
scpFriendlyName :: Lens' SDKConfigurationProperty (Maybe Text)
scpFriendlyName = lens _scpFriendlyName (\ s a -> s{_scpFriendlyName = a});

-- | A boolean flag of an 'SdkType' configuration property to indicate if the associated SDK configuration property is required (@true@ ) or not (@false@ ).
scpRequired :: Lens' SDKConfigurationProperty (Maybe Bool)
scpRequired = lens _scpRequired (\ s a -> s{_scpRequired = a});

-- | The name of a an 'SdkType' configuration property.
scpName :: Lens' SDKConfigurationProperty (Maybe Text)
scpName = lens _scpName (\ s a -> s{_scpName = a});

-- | The default value of an 'SdkType' configuration property.
scpDefaultValue :: Lens' SDKConfigurationProperty (Maybe Text)
scpDefaultValue = lens _scpDefaultValue (\ s a -> s{_scpDefaultValue = a});

-- | The description of an 'SdkType' configuration property.
scpDescription :: Lens' SDKConfigurationProperty (Maybe Text)
scpDescription = lens _scpDescription (\ s a -> s{_scpDescription = a});

instance FromJSON SDKConfigurationProperty where
        parseJSON
          = withObject "SDKConfigurationProperty"
              (\ x ->
                 SDKConfigurationProperty' <$>
                   (x .:? "friendlyName") <*> (x .:? "required") <*>
                     (x .:? "name")
                     <*> (x .:? "defaultValue")
                     <*> (x .:? "description"))

instance Hashable SDKConfigurationProperty

instance NFData SDKConfigurationProperty

-- | A type of SDK that API Gateway can generate.
--
--
--
-- /See:/ 'sdkType' smart constructor.
data SDKType = SDKType'
    { _stFriendlyName            :: !(Maybe Text)
    , _stConfigurationProperties :: !(Maybe [SDKConfigurationProperty])
    , _stId                      :: !(Maybe Text)
    , _stDescription             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SDKType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stFriendlyName' - The user-friendly name of an 'SdkType' instance.
--
-- * 'stConfigurationProperties' - A list of configuration properties of an 'SdkType' .
--
-- * 'stId' - The identifier of an 'SdkType' instance.
--
-- * 'stDescription' - The description of an 'SdkType' .
sdkType
    :: SDKType
sdkType =
    SDKType'
    { _stFriendlyName = Nothing
    , _stConfigurationProperties = Nothing
    , _stId = Nothing
    , _stDescription = Nothing
    }

-- | The user-friendly name of an 'SdkType' instance.
stFriendlyName :: Lens' SDKType (Maybe Text)
stFriendlyName = lens _stFriendlyName (\ s a -> s{_stFriendlyName = a});

-- | A list of configuration properties of an 'SdkType' .
stConfigurationProperties :: Lens' SDKType [SDKConfigurationProperty]
stConfigurationProperties = lens _stConfigurationProperties (\ s a -> s{_stConfigurationProperties = a}) . _Default . _Coerce;

-- | The identifier of an 'SdkType' instance.
stId :: Lens' SDKType (Maybe Text)
stId = lens _stId (\ s a -> s{_stId = a});

-- | The description of an 'SdkType' .
stDescription :: Lens' SDKType (Maybe Text)
stDescription = lens _stDescription (\ s a -> s{_stDescription = a});

instance FromJSON SDKType where
        parseJSON
          = withObject "SDKType"
              (\ x ->
                 SDKType' <$>
                   (x .:? "friendlyName") <*>
                     (x .:? "configurationProperties" .!= mempty)
                     <*> (x .:? "id")
                     <*> (x .:? "description"))

instance Hashable SDKType

instance NFData SDKType

-- | Represents a unique identifier for a version of a deployed 'RestApi' that is callable by users.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploy an API>
--
-- /See:/ 'stage' smart constructor.
data Stage = Stage'
    { _sDeploymentId         :: !(Maybe Text)
    , _sVariables            :: !(Maybe (Map Text Text))
    , _sDocumentationVersion :: !(Maybe Text)
    , _sClientCertificateId  :: !(Maybe Text)
    , _sCreatedDate          :: !(Maybe POSIX)
    , _sCacheClusterStatus   :: !(Maybe CacheClusterStatus)
    , _sMethodSettings       :: !(Maybe (Map Text MethodSetting))
    , _sLastUpdatedDate      :: !(Maybe POSIX)
    , _sCacheClusterSize     :: !(Maybe CacheClusterSize)
    , _sCacheClusterEnabled  :: !(Maybe Bool)
    , _sStageName            :: !(Maybe Text)
    , _sDescription          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Stage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeploymentId' - The identifier of the 'Deployment' that the stage points to.
--
-- * 'sVariables' - A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- * 'sDocumentationVersion' - The version of the associated API documentation.
--
-- * 'sClientCertificateId' - The identifier of a client certificate for an API stage.
--
-- * 'sCreatedDate' - The timestamp when the stage was created.
--
-- * 'sCacheClusterStatus' - The status of the cache cluster for the stage, if enabled.
--
-- * 'sMethodSettings' - A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
--
-- * 'sLastUpdatedDate' - The timestamp when the stage last updated.
--
-- * 'sCacheClusterSize' - The size of the cache cluster for the stage, if enabled.
--
-- * 'sCacheClusterEnabled' - Specifies whether a cache cluster is enabled for the stage.
--
-- * 'sStageName' - The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to Amazon API Gateway.
--
-- * 'sDescription' - The stage's description.
stage
    :: Stage
stage =
    Stage'
    { _sDeploymentId = Nothing
    , _sVariables = Nothing
    , _sDocumentationVersion = Nothing
    , _sClientCertificateId = Nothing
    , _sCreatedDate = Nothing
    , _sCacheClusterStatus = Nothing
    , _sMethodSettings = Nothing
    , _sLastUpdatedDate = Nothing
    , _sCacheClusterSize = Nothing
    , _sCacheClusterEnabled = Nothing
    , _sStageName = Nothing
    , _sDescription = Nothing
    }

-- | The identifier of the 'Deployment' that the stage points to.
sDeploymentId :: Lens' Stage (Maybe Text)
sDeploymentId = lens _sDeploymentId (\ s a -> s{_sDeploymentId = a});

-- | A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
sVariables :: Lens' Stage (HashMap Text Text)
sVariables = lens _sVariables (\ s a -> s{_sVariables = a}) . _Default . _Map;

-- | The version of the associated API documentation.
sDocumentationVersion :: Lens' Stage (Maybe Text)
sDocumentationVersion = lens _sDocumentationVersion (\ s a -> s{_sDocumentationVersion = a});

-- | The identifier of a client certificate for an API stage.
sClientCertificateId :: Lens' Stage (Maybe Text)
sClientCertificateId = lens _sClientCertificateId (\ s a -> s{_sClientCertificateId = a});

-- | The timestamp when the stage was created.
sCreatedDate :: Lens' Stage (Maybe UTCTime)
sCreatedDate = lens _sCreatedDate (\ s a -> s{_sCreatedDate = a}) . mapping _Time;

-- | The status of the cache cluster for the stage, if enabled.
sCacheClusterStatus :: Lens' Stage (Maybe CacheClusterStatus)
sCacheClusterStatus = lens _sCacheClusterStatus (\ s a -> s{_sCacheClusterStatus = a});

-- | A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
sMethodSettings :: Lens' Stage (HashMap Text MethodSetting)
sMethodSettings = lens _sMethodSettings (\ s a -> s{_sMethodSettings = a}) . _Default . _Map;

-- | The timestamp when the stage last updated.
sLastUpdatedDate :: Lens' Stage (Maybe UTCTime)
sLastUpdatedDate = lens _sLastUpdatedDate (\ s a -> s{_sLastUpdatedDate = a}) . mapping _Time;

-- | The size of the cache cluster for the stage, if enabled.
sCacheClusterSize :: Lens' Stage (Maybe CacheClusterSize)
sCacheClusterSize = lens _sCacheClusterSize (\ s a -> s{_sCacheClusterSize = a});

-- | Specifies whether a cache cluster is enabled for the stage.
sCacheClusterEnabled :: Lens' Stage (Maybe Bool)
sCacheClusterEnabled = lens _sCacheClusterEnabled (\ s a -> s{_sCacheClusterEnabled = a});

-- | The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to Amazon API Gateway.
sStageName :: Lens' Stage (Maybe Text)
sStageName = lens _sStageName (\ s a -> s{_sStageName = a});

-- | The stage's description.
sDescription :: Lens' Stage (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a});

instance FromJSON Stage where
        parseJSON
          = withObject "Stage"
              (\ x ->
                 Stage' <$>
                   (x .:? "deploymentId") <*>
                     (x .:? "variables" .!= mempty)
                     <*> (x .:? "documentationVersion")
                     <*> (x .:? "clientCertificateId")
                     <*> (x .:? "createdDate")
                     <*> (x .:? "cacheClusterStatus")
                     <*> (x .:? "methodSettings" .!= mempty)
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "cacheClusterSize")
                     <*> (x .:? "cacheClusterEnabled")
                     <*> (x .:? "stageName")
                     <*> (x .:? "description"))

instance Hashable Stage

instance NFData Stage

-- | A reference to a unique stage identified in the format @{restApiId}/{stage}@ .
--
--
--
-- /See:/ 'stageKey' smart constructor.
data StageKey = StageKey'
    { _skRestAPIId :: !(Maybe Text)
    , _skStageName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StageKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skRestAPIId' - A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- * 'skStageName' - The stage name in the 'RestApi' that the stage key references.
stageKey
    :: StageKey
stageKey =
    StageKey'
    { _skRestAPIId = Nothing
    , _skStageName = Nothing
    }

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
skRestAPIId :: Lens' StageKey (Maybe Text)
skRestAPIId = lens _skRestAPIId (\ s a -> s{_skRestAPIId = a});

-- | The stage name in the 'RestApi' that the stage key references.
skStageName :: Lens' StageKey (Maybe Text)
skStageName = lens _skStageName (\ s a -> s{_skStageName = a});

instance Hashable StageKey

instance NFData StageKey

instance ToJSON StageKey where
        toJSON StageKey'{..}
          = object
              (catMaybes
                 [("restApiId" .=) <$> _skRestAPIId,
                  ("stageName" .=) <$> _skStageName])

-- | The API request rate limits.
--
--
--
-- /See:/ 'throttleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
    { _tsBurstLimit :: !(Maybe Int)
    , _tsRateLimit  :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThrottleSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsBurstLimit' - The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
--
-- * 'tsRateLimit' - The API request steady-state rate limit.
throttleSettings
    :: ThrottleSettings
throttleSettings =
    ThrottleSettings'
    { _tsBurstLimit = Nothing
    , _tsRateLimit = Nothing
    }

-- | The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
tsBurstLimit :: Lens' ThrottleSettings (Maybe Int)
tsBurstLimit = lens _tsBurstLimit (\ s a -> s{_tsBurstLimit = a});

-- | The API request steady-state rate limit.
tsRateLimit :: Lens' ThrottleSettings (Maybe Double)
tsRateLimit = lens _tsRateLimit (\ s a -> s{_tsRateLimit = a});

instance FromJSON ThrottleSettings where
        parseJSON
          = withObject "ThrottleSettings"
              (\ x ->
                 ThrottleSettings' <$>
                   (x .:? "burstLimit") <*> (x .:? "rateLimit"))

instance Hashable ThrottleSettings

instance NFData ThrottleSettings

instance ToJSON ThrottleSettings where
        toJSON ThrottleSettings'{..}
          = object
              (catMaybes
                 [("burstLimit" .=) <$> _tsBurstLimit,
                  ("rateLimit" .=) <$> _tsRateLimit])

-- | Represents the usage data of a usage plan.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> , <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan>
--
-- /See:/ 'usage' smart constructor.
data Usage = Usage'
    { _uUsagePlanId :: !(Maybe Text)
    , _uEndDate     :: !(Maybe Text)
    , _uItems       :: !(Maybe (Map Text [[Integer]]))
    , _uStartDate   :: !(Maybe Text)
    , _uPosition    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Usage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uUsagePlanId' - The plan Id associated with this usage data.
--
-- * 'uEndDate' - The ending date of the usage data.
--
-- * 'uItems' - The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
--
-- * 'uStartDate' - The starting date of the usage data.
--
-- * 'uPosition' - Undocumented member.
usage
    :: Usage
usage =
    Usage'
    { _uUsagePlanId = Nothing
    , _uEndDate = Nothing
    , _uItems = Nothing
    , _uStartDate = Nothing
    , _uPosition = Nothing
    }

-- | The plan Id associated with this usage data.
uUsagePlanId :: Lens' Usage (Maybe Text)
uUsagePlanId = lens _uUsagePlanId (\ s a -> s{_uUsagePlanId = a});

-- | The ending date of the usage data.
uEndDate :: Lens' Usage (Maybe Text)
uEndDate = lens _uEndDate (\ s a -> s{_uEndDate = a});

-- | The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
uItems :: Lens' Usage (HashMap Text [[Integer]])
uItems = lens _uItems (\ s a -> s{_uItems = a}) . _Default . _Map;

-- | The starting date of the usage data.
uStartDate :: Lens' Usage (Maybe Text)
uStartDate = lens _uStartDate (\ s a -> s{_uStartDate = a});

-- | Undocumented member.
uPosition :: Lens' Usage (Maybe Text)
uPosition = lens _uPosition (\ s a -> s{_uPosition = a});

instance FromJSON Usage where
        parseJSON
          = withObject "Usage"
              (\ x ->
                 Usage' <$>
                   (x .:? "usagePlanId") <*> (x .:? "endDate") <*>
                     (x .:? "values" .!= mempty)
                     <*> (x .:? "startDate")
                     <*> (x .:? "position"))

instance Hashable Usage

instance NFData Usage

-- | Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.
--
--
-- In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'usagePlan' smart constructor.
data UsagePlan = UsagePlan'
    { _upApiStages   :: !(Maybe [APIStage])
    , _upName        :: !(Maybe Text)
    , _upId          :: !(Maybe Text)
    , _upThrottle    :: !(Maybe ThrottleSettings)
    , _upQuota       :: !(Maybe QuotaSettings)
    , _upDescription :: !(Maybe Text)
    , _upProductCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upApiStages' - The associated API stages of a usage plan.
--
-- * 'upName' - The name of a usage plan.
--
-- * 'upId' - The identifier of a 'UsagePlan' resource.
--
-- * 'upThrottle' - The request throttle limits of a usage plan.
--
-- * 'upQuota' - The maximum number of permitted requests per a given unit time interval.
--
-- * 'upDescription' - The description of a usage plan.
--
-- * 'upProductCode' - The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
usagePlan
    :: UsagePlan
usagePlan =
    UsagePlan'
    { _upApiStages = Nothing
    , _upName = Nothing
    , _upId = Nothing
    , _upThrottle = Nothing
    , _upQuota = Nothing
    , _upDescription = Nothing
    , _upProductCode = Nothing
    }

-- | The associated API stages of a usage plan.
upApiStages :: Lens' UsagePlan [APIStage]
upApiStages = lens _upApiStages (\ s a -> s{_upApiStages = a}) . _Default . _Coerce;

-- | The name of a usage plan.
upName :: Lens' UsagePlan (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a});

-- | The identifier of a 'UsagePlan' resource.
upId :: Lens' UsagePlan (Maybe Text)
upId = lens _upId (\ s a -> s{_upId = a});

-- | The request throttle limits of a usage plan.
upThrottle :: Lens' UsagePlan (Maybe ThrottleSettings)
upThrottle = lens _upThrottle (\ s a -> s{_upThrottle = a});

-- | The maximum number of permitted requests per a given unit time interval.
upQuota :: Lens' UsagePlan (Maybe QuotaSettings)
upQuota = lens _upQuota (\ s a -> s{_upQuota = a});

-- | The description of a usage plan.
upDescription :: Lens' UsagePlan (Maybe Text)
upDescription = lens _upDescription (\ s a -> s{_upDescription = a});

-- | The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
upProductCode :: Lens' UsagePlan (Maybe Text)
upProductCode = lens _upProductCode (\ s a -> s{_upProductCode = a});

instance FromJSON UsagePlan where
        parseJSON
          = withObject "UsagePlan"
              (\ x ->
                 UsagePlan' <$>
                   (x .:? "apiStages" .!= mempty) <*> (x .:? "name") <*>
                     (x .:? "id")
                     <*> (x .:? "throttle")
                     <*> (x .:? "quota")
                     <*> (x .:? "description")
                     <*> (x .:? "productCode"))

instance Hashable UsagePlan

instance NFData UsagePlan

-- | Represents a usage plan key to identify a plan customer.
--
--
-- To associate an API stage with a selected API key in a usage plan, you must create a UsagePlanKey resource to represent the selected 'ApiKey' .
--
-- " <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'usagePlanKey' smart constructor.
data UsagePlanKey = UsagePlanKey'
    { _upkValue :: !(Maybe Text)
    , _upkName  :: !(Maybe Text)
    , _upkId    :: !(Maybe Text)
    , _upkType  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsagePlanKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkValue' - The value of a usage plan key.
--
-- * 'upkName' - The name of a usage plan key.
--
-- * 'upkId' - The Id of a usage plan key.
--
-- * 'upkType' - The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
usagePlanKey
    :: UsagePlanKey
usagePlanKey =
    UsagePlanKey'
    { _upkValue = Nothing
    , _upkName = Nothing
    , _upkId = Nothing
    , _upkType = Nothing
    }

-- | The value of a usage plan key.
upkValue :: Lens' UsagePlanKey (Maybe Text)
upkValue = lens _upkValue (\ s a -> s{_upkValue = a});

-- | The name of a usage plan key.
upkName :: Lens' UsagePlanKey (Maybe Text)
upkName = lens _upkName (\ s a -> s{_upkName = a});

-- | The Id of a usage plan key.
upkId :: Lens' UsagePlanKey (Maybe Text)
upkId = lens _upkId (\ s a -> s{_upkId = a});

-- | The type of a usage plan key. Currently, the valid key type is @API_KEY@ .
upkType :: Lens' UsagePlanKey (Maybe Text)
upkType = lens _upkType (\ s a -> s{_upkType = a});

instance FromJSON UsagePlanKey where
        parseJSON
          = withObject "UsagePlanKey"
              (\ x ->
                 UsagePlanKey' <$>
                   (x .:? "value") <*> (x .:? "name") <*> (x .:? "id")
                     <*> (x .:? "type"))

instance Hashable UsagePlanKey

instance NFData UsagePlanKey
