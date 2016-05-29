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

-- | A resource that can be distributed to callers for executing < Method>
-- resources that require an API key. API keys can be mapped to any
-- < Stage> on any < RestApi>, which indicates that the callers with the
-- API key can make requests to that stage.
--
-- /See:/ 'apiKey' smart constructor.
data APIKey = APIKey'
    { _akEnabled         :: !(Maybe Bool)
    , _akCreatedDate     :: !(Maybe ISO8601)
    , _akName            :: !(Maybe Text)
    , _akId              :: !(Maybe Text)
    , _akStageKeys       :: !(Maybe [Text])
    , _akLastUpdatedDate :: !(Maybe ISO8601)
    , _akDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akEnabled'
--
-- * 'akCreatedDate'
--
-- * 'akName'
--
-- * 'akId'
--
-- * 'akStageKeys'
--
-- * 'akLastUpdatedDate'
--
-- * 'akDescription'
apiKey
    :: APIKey
apiKey =
    APIKey'
    { _akEnabled = Nothing
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

-- | The date when the API Key was created, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
akCreatedDate :: Lens' APIKey (Maybe UTCTime)
akCreatedDate = lens _akCreatedDate (\ s a -> s{_akCreatedDate = a}) . mapping _Time;

-- | The name of the API Key.
akName :: Lens' APIKey (Maybe Text)
akName = lens _akName (\ s a -> s{_akName = a});

-- | The identifier of the API Key.
akId :: Lens' APIKey (Maybe Text)
akId = lens _akId (\ s a -> s{_akId = a});

-- | A list of < Stage> resources that are associated with the < ApiKey>
-- resource.
akStageKeys :: Lens' APIKey [Text]
akStageKeys = lens _akStageKeys (\ s a -> s{_akStageKeys = a}) . _Default . _Coerce;

-- | When the API Key was last updated, in ISO 8601 format.
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
                   (x .:? "enabled") <*> (x .:? "createdDate") <*>
                     (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "stageKeys" .!= mempty)
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable APIKey

instance NFData APIKey

-- | Represents an AWS account that is associated with Amazon API Gateway.
--
-- /See:/ 'account' smart constructor.
data Account = Account'
    { _aCloudwatchRoleARN :: !(Maybe Text)
    , _aThrottleSettings  :: !(Maybe ThrottleSettings)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCloudwatchRoleARN'
--
-- * 'aThrottleSettings'
account
    :: Account
account =
    Account'
    { _aCloudwatchRoleARN = Nothing
    , _aThrottleSettings = Nothing
    }

-- | Specifies the Amazon resource name (ARN) of an Amazon CloudWatch role
-- for the current < Account> resource.
aCloudwatchRoleARN :: Lens' Account (Maybe Text)
aCloudwatchRoleARN = lens _aCloudwatchRoleARN (\ s a -> s{_aCloudwatchRoleARN = a});

-- | Specifies the application programming interface (API) throttle settings
-- for the current < Account> resource.
aThrottleSettings :: Lens' Account (Maybe ThrottleSettings)
aThrottleSettings = lens _aThrottleSettings (\ s a -> s{_aThrottleSettings = a});

instance FromJSON Account where
        parseJSON
          = withObject "Account"
              (\ x ->
                 Account' <$>
                   (x .:? "cloudwatchRoleArn") <*>
                     (x .:? "throttleSettings"))

instance Hashable Account

instance NFData Account

-- | Represents an authorization layer for methods. If enabled on a method,
-- API Gateway will activate the authorizer when a client calls the method.
--
-- /See:/ 'authorizer' smart constructor.
data Authorizer = Authorizer'
    { _aAuthorizerURI                :: !(Maybe Text)
    , _aIdentityValidationExpression :: !(Maybe Text)
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
-- * 'aAuthorizerURI'
--
-- * 'aIdentityValidationExpression'
--
-- * 'aName'
--
-- * 'aId'
--
-- * 'aAuthorizerResultTtlInSeconds'
--
-- * 'aAuthType'
--
-- * 'aType'
--
-- * 'aIdentitySource'
--
-- * 'aAuthorizerCredentials'
authorizer
    :: Authorizer
authorizer =
    Authorizer'
    { _aAuthorizerURI = Nothing
    , _aIdentityValidationExpression = Nothing
    , _aName = Nothing
    , _aId = Nothing
    , _aAuthorizerResultTtlInSeconds = Nothing
    , _aAuthType = Nothing
    , _aType = Nothing
    , _aIdentitySource = Nothing
    , _aAuthorizerCredentials = Nothing
    }

-- | [Required] Specifies the authorizer\'s Uniform Resource Identifier
-- (URI). For TOKEN authorizers, this must be a well-formed Lambda function
-- URI. The URI should be of the form
-- 'arn:aws:apigateway:{region}:lambda:path\/{service_api}'. 'Region' is
-- used to determine the right endpoint. In this case, 'path' is used to
-- indicate that the remaining substring in the URI should be treated as
-- the path to the resource, including the initial '\/'. For Lambda
-- functions, this is usually of the form
-- \/2015-03-31\/functions\/[FunctionARN]\/invocations
aAuthorizerURI :: Lens' Authorizer (Maybe Text)
aAuthorizerURI = lens _aAuthorizerURI (\ s a -> s{_aAuthorizerURI = a});

-- | A validation expression for the incoming identity. For TOKEN
-- authorizers, this value should be a regular expression. The incoming
-- token from the client is matched against this expression, and will
-- proceed if the token matches. If the token doesn\'t match, the client
-- receives a 401 Unauthorized response.
aIdentityValidationExpression :: Lens' Authorizer (Maybe Text)
aIdentityValidationExpression = lens _aIdentityValidationExpression (\ s a -> s{_aIdentityValidationExpression = a});

-- | [Required] The name of the authorizer.
aName :: Lens' Authorizer (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The identifier for the authorizer resource.
aId :: Lens' Authorizer (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a});

-- | The TTL in seconds of cached authorizer results. If greater than 0, API
-- Gateway will cache authorizer responses. If this field is not set, the
-- default value is 300. The maximum value is 3600, or 1 hour.
aAuthorizerResultTtlInSeconds :: Lens' Authorizer (Maybe Int)
aAuthorizerResultTtlInSeconds = lens _aAuthorizerResultTtlInSeconds (\ s a -> s{_aAuthorizerResultTtlInSeconds = a});

-- | Optional customer-defined field, used in Swagger imports\/exports. Has
-- no functional impact.
aAuthType :: Lens' Authorizer (Maybe Text)
aAuthType = lens _aAuthType (\ s a -> s{_aAuthType = a});

-- | [Required] The type of the authorizer. Currently, the only valid type is
-- TOKEN.
aType :: Lens' Authorizer (Maybe AuthorizerType)
aType = lens _aType (\ s a -> s{_aType = a});

-- | [Required] The source of the identity in an incoming request. For TOKEN
-- authorizers, this value is a mapping expression with the same syntax as
-- integration parameter mappings. The only valid source for tokens is
-- \'header\', so the expression should match
-- \'method.request.header.[headerName]\'. The value of the header
-- \'[headerName]\' will be interpreted as the incoming token.
aIdentitySource :: Lens' Authorizer (Maybe Text)
aIdentitySource = lens _aIdentitySource (\ s a -> s{_aIdentitySource = a});

-- | Specifies the credentials required for the authorizer, if any. Two
-- options are available. To specify an IAM Role for Amazon API Gateway to
-- assume, use the role\'s Amazon Resource Name (ARN). To use
-- resource-based permissions on the Lambda function, specify null.
aAuthorizerCredentials :: Lens' Authorizer (Maybe Text)
aAuthorizerCredentials = lens _aAuthorizerCredentials (\ s a -> s{_aAuthorizerCredentials = a});

instance FromJSON Authorizer where
        parseJSON
          = withObject "Authorizer"
              (\ x ->
                 Authorizer' <$>
                   (x .:? "authorizerUri") <*>
                     (x .:? "identityValidationExpression")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "authorizerResultTtlInSeconds")
                     <*> (x .:? "authType")
                     <*> (x .:? "type")
                     <*> (x .:? "identitySource")
                     <*> (x .:? "authorizerCredentials"))

instance Hashable Authorizer

instance NFData Authorizer

-- | Represents the base path that callers of the API that must provide as
-- part of the URL after the domain name.
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
-- * 'bpmStage'
--
-- * 'bpmBasePath'
--
-- * 'bpmRestAPIId'
basePathMapping
    :: BasePathMapping
basePathMapping =
    BasePathMapping'
    { _bpmStage = Nothing
    , _bpmBasePath = Nothing
    , _bpmRestAPIId = Nothing
    }

-- | The name of the API\'s stage.
bpmStage :: Lens' BasePathMapping (Maybe Text)
bpmStage = lens _bpmStage (\ s a -> s{_bpmStage = a});

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name.
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

-- | Represents a Client Certificate used to configure client-side SSL
-- authentication while sending requests to the integration endpoint.
--
-- /See:/ 'clientCertificate' smart constructor.
data ClientCertificate = ClientCertificate'
    { _ccPemEncodedCertificate :: !(Maybe Text)
    , _ccClientCertificateId   :: !(Maybe Text)
    , _ccCreatedDate           :: !(Maybe ISO8601)
    , _ccExpirationDate        :: !(Maybe ISO8601)
    , _ccDescription           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccPemEncodedCertificate'
--
-- * 'ccClientCertificateId'
--
-- * 'ccCreatedDate'
--
-- * 'ccExpirationDate'
--
-- * 'ccDescription'
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

-- | The PEM-encoded public key of the Client Certificate, that can be used
-- to configure certificate authentication in the integration endpoint .
ccPemEncodedCertificate :: Lens' ClientCertificate (Maybe Text)
ccPemEncodedCertificate = lens _ccPemEncodedCertificate (\ s a -> s{_ccPemEncodedCertificate = a});

-- | The identifier of the Client Certificate.
ccClientCertificateId :: Lens' ClientCertificate (Maybe Text)
ccClientCertificateId = lens _ccClientCertificateId (\ s a -> s{_ccClientCertificateId = a});

-- | The date when the Client Certificate was created, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
ccCreatedDate :: Lens' ClientCertificate (Maybe UTCTime)
ccCreatedDate = lens _ccCreatedDate (\ s a -> s{_ccCreatedDate = a}) . mapping _Time;

-- | The date when the Client Certificate will expire, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
ccExpirationDate :: Lens' ClientCertificate (Maybe UTCTime)
ccExpirationDate = lens _ccExpirationDate (\ s a -> s{_ccExpirationDate = a}) . mapping _Time;

-- | The description of the Client Certificate.
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

-- | An immutable representation of a < RestApi> resource that can be called
-- by users using < Stages>. A deployment must be associated with a
-- < Stage> for it to be callable over the Internet.
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
    { _dApiSummary  :: !(Maybe (Map Text (Map Text MethodSnapshot)))
    , _dCreatedDate :: !(Maybe ISO8601)
    , _dId          :: !(Maybe Text)
    , _dDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dApiSummary'
--
-- * 'dCreatedDate'
--
-- * 'dId'
--
-- * 'dDescription'
deployment
    :: Deployment
deployment =
    Deployment'
    { _dApiSummary = Nothing
    , _dCreatedDate = Nothing
    , _dId = Nothing
    , _dDescription = Nothing
    }

-- | Gets a summary of the < RestApi> at the date and time that the
-- deployment resource was created.
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

-- | Represents a domain name that is contained in a simpler, more intuitive
-- URL that can be called.
--
-- /See:/ 'domainName' smart constructor.
data DomainName = DomainName'
    { _dnCertificateName        :: !(Maybe Text)
    , _dnDomainName             :: !(Maybe Text)
    , _dnCertificateUploadDate  :: !(Maybe ISO8601)
    , _dnDistributionDomainName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnCertificateName'
--
-- * 'dnDomainName'
--
-- * 'dnCertificateUploadDate'
--
-- * 'dnDistributionDomainName'
domainName
    :: DomainName
domainName =
    DomainName'
    { _dnCertificateName = Nothing
    , _dnDomainName = Nothing
    , _dnCertificateUploadDate = Nothing
    , _dnDistributionDomainName = Nothing
    }

-- | The name of the certificate.
dnCertificateName :: Lens' DomainName (Maybe Text)
dnCertificateName = lens _dnCertificateName (\ s a -> s{_dnCertificateName = a});

-- | The name of the < DomainName> resource.
dnDomainName :: Lens' DomainName (Maybe Text)
dnDomainName = lens _dnDomainName (\ s a -> s{_dnDomainName = a});

-- | The date when the certificate was uploaded, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
dnCertificateUploadDate :: Lens' DomainName (Maybe UTCTime)
dnCertificateUploadDate = lens _dnCertificateUploadDate (\ s a -> s{_dnCertificateUploadDate = a}) . mapping _Time;

-- | The domain name of the Amazon CloudFront distribution. For more
-- information, see the
-- <http://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
dnDistributionDomainName :: Lens' DomainName (Maybe Text)
dnDistributionDomainName = lens _dnDistributionDomainName (\ s a -> s{_dnDistributionDomainName = a});

instance FromJSON DomainName where
        parseJSON
          = withObject "DomainName"
              (\ x ->
                 DomainName' <$>
                   (x .:? "certificateName") <*> (x .:? "domainName")
                     <*> (x .:? "certificateUploadDate")
                     <*> (x .:? "distributionDomainName"))

instance Hashable DomainName

instance NFData DomainName

-- | Represents a HTTP, AWS, or Mock integration.
--
-- /See:/ 'integration' smart constructor.
data Integration = Integration'
    { _iHttpMethod           :: !(Maybe Text)
    , _iRequestTemplates     :: !(Maybe (Map Text Text))
    , _iCredentials          :: !(Maybe Text)
    , _iRequestParameters    :: !(Maybe (Map Text Text))
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
-- * 'iHttpMethod'
--
-- * 'iRequestTemplates'
--
-- * 'iCredentials'
--
-- * 'iRequestParameters'
--
-- * 'iUri'
--
-- * 'iIntegrationResponses'
--
-- * 'iCacheNamespace'
--
-- * 'iType'
--
-- * 'iCacheKeyParameters'
integration
    :: Integration
integration =
    Integration'
    { _iHttpMethod = Nothing
    , _iRequestTemplates = Nothing
    , _iCredentials = Nothing
    , _iRequestParameters = Nothing
    , _iUri = Nothing
    , _iIntegrationResponses = Nothing
    , _iCacheNamespace = Nothing
    , _iType = Nothing
    , _iCacheKeyParameters = Nothing
    }

-- | Specifies the integration\'s HTTP method type.
iHttpMethod :: Lens' Integration (Maybe Text)
iHttpMethod = lens _iHttpMethod (\ s a -> s{_iHttpMethod = a});

-- | Specifies the integration\'s request templates.
iRequestTemplates :: Lens' Integration (HashMap Text Text)
iRequestTemplates = lens _iRequestTemplates (\ s a -> s{_iRequestTemplates = a}) . _Default . _Map;

-- | Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- Amazon API Gateway to assume, use the role\'s Amazon Resource Name
-- (ARN). To require that the caller\'s identity be passed through from the
-- request, specify the string 'arn:aws:iam::\\*:user\/\\*'. To use
-- resource-based permissions on supported AWS services, specify null.
iCredentials :: Lens' Integration (Maybe Text)
iCredentials = lens _iCredentials (\ s a -> s{_iCredentials = a});

-- | Represents requests parameters that are sent with the backend request.
-- Request parameters are represented as a key\/value map, with a
-- destination as the key and a source as the value. A source must match an
-- existing method request parameter, or a static value. Static values must
-- be enclosed with single quotes, and be pre-encoded based on their
-- destination in the request. The destination must match the pattern
-- 'integration.request.{location}.{name}', where 'location' is either
-- querystring, path, or header. 'name' must be a valid, unique parameter
-- name.
iRequestParameters :: Lens' Integration (HashMap Text Text)
iRequestParameters = lens _iRequestParameters (\ s a -> s{_iRequestParameters = a}) . _Default . _Map;

-- | Specifies the integration\'s Uniform Resource Identifier (URI). For HTTP
-- integrations, the URI must be a fully formed, encoded HTTP(S) URL
-- according to the
-- <https://www.ietf.org/rfc/rfc3986.txt RFC-3986 specification>. For AWS
-- integrations, the URI should be of the form
-- 'arn:aws:apigateway:{region}:{subdomain.service|service}:{path|action}\/{service_api}'.
-- 'Region', 'subdomain' and 'service' are used to determine the right
-- endpoint. For AWS services that use the 'Action=' query string
-- parameter, 'service_api' should be a valid action for the desired
-- service. For RESTful AWS service APIs, 'path' is used to indicate that
-- the remaining substring in the URI should be treated as the path to the
-- resource, including the initial '\/'.
iUri :: Lens' Integration (Maybe Text)
iUri = lens _iUri (\ s a -> s{_iUri = a});

-- | Specifies the integration\'s responses.
iIntegrationResponses :: Lens' Integration (HashMap Text IntegrationResponse)
iIntegrationResponses = lens _iIntegrationResponses (\ s a -> s{_iIntegrationResponses = a}) . _Default . _Map;

-- | Specifies the integration\'s cache namespace.
iCacheNamespace :: Lens' Integration (Maybe Text)
iCacheNamespace = lens _iCacheNamespace (\ s a -> s{_iCacheNamespace = a});

-- | Specifies the integration\'s type. The valid value is 'HTTP', 'AWS', or
-- 'MOCK'.
iType :: Lens' Integration (Maybe IntegrationType)
iType = lens _iType (\ s a -> s{_iType = a});

-- | Specifies the integration\'s cache key parameters.
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
                     <*> (x .:? "uri")
                     <*> (x .:? "integrationResponses" .!= mempty)
                     <*> (x .:? "cacheNamespace")
                     <*> (x .:? "type")
                     <*> (x .:? "cacheKeyParameters" .!= mempty))

instance Hashable Integration

instance NFData Integration

-- | Represents an integration response. The status code must map to an
-- existing < MethodResponse>, and parameters and templates can be used to
-- transform the backend response.
--
-- /See:/ 'integrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
    { _iResponseTemplates  :: !(Maybe (Map Text Text))
    , _iSelectionPattern   :: !(Maybe Text)
    , _iStatusCode         :: !(Maybe Text)
    , _iResponseParameters :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iResponseTemplates'
--
-- * 'iSelectionPattern'
--
-- * 'iStatusCode'
--
-- * 'iResponseParameters'
integrationResponse
    :: IntegrationResponse
integrationResponse =
    IntegrationResponse'
    { _iResponseTemplates = Nothing
    , _iSelectionPattern = Nothing
    , _iStatusCode = Nothing
    , _iResponseParameters = Nothing
    }

-- | Specifies the templates used to transform the integration response body.
-- Response templates are represented as a key\/value map, with a
-- content-type as the key and a template as the value.
iResponseTemplates :: Lens' IntegrationResponse (HashMap Text Text)
iResponseTemplates = lens _iResponseTemplates (\ s a -> s{_iResponseTemplates = a}) . _Default . _Map;

-- | Specifies the regular expression (regex) pattern used to choose an
-- integration response based on the response from the backend. If the
-- backend is an AWS Lambda function, the AWS Lambda function error header
-- is matched. For all other HTTP and AWS backends, the HTTP status code is
-- matched.
iSelectionPattern :: Lens' IntegrationResponse (Maybe Text)
iSelectionPattern = lens _iSelectionPattern (\ s a -> s{_iSelectionPattern = a});

-- | Specifies the status code that is used to map the integration response
-- to an existing < MethodResponse>.
iStatusCode :: Lens' IntegrationResponse (Maybe Text)
iStatusCode = lens _iStatusCode (\ s a -> s{_iStatusCode = a});

-- | Represents response parameters that can be read from the backend
-- response. Response parameters are represented as a key\/value map, with
-- a destination as the key and a source as the value. A destination must
-- match an existing response parameter in the < MethodResponse>. The
-- source can be a header from the backend response, or a static value.
-- Static values are specified using enclosing single quotes, and backend
-- response headers can be read using the pattern
-- 'integration.response.header.{name}'.
iResponseParameters :: Lens' IntegrationResponse (HashMap Text Text)
iResponseParameters = lens _iResponseParameters (\ s a -> s{_iResponseParameters = a}) . _Default . _Map;

instance FromJSON IntegrationResponse where
        parseJSON
          = withObject "IntegrationResponse"
              (\ x ->
                 IntegrationResponse' <$>
                   (x .:? "responseTemplates" .!= mempty) <*>
                     (x .:? "selectionPattern")
                     <*> (x .:? "statusCode")
                     <*> (x .:? "responseParameters" .!= mempty))

instance Hashable IntegrationResponse

instance NFData IntegrationResponse

-- | Represents a method.
--
-- /See:/ 'method' smart constructor.
data Method = Method'
    { _mMethodResponses   :: !(Maybe (Map Text MethodResponse))
    , _mHttpMethod        :: !(Maybe Text)
    , _mRequestModels     :: !(Maybe (Map Text Text))
    , _mRequestParameters :: !(Maybe (Map Text Bool))
    , _mAuthorizerId      :: !(Maybe Text)
    , _mAuthorizationType :: !(Maybe Text)
    , _mApiKeyRequired    :: !(Maybe Bool)
    , _mMethodIntegration :: !(Maybe Integration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Method' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMethodResponses'
--
-- * 'mHttpMethod'
--
-- * 'mRequestModels'
--
-- * 'mRequestParameters'
--
-- * 'mAuthorizerId'
--
-- * 'mAuthorizationType'
--
-- * 'mApiKeyRequired'
--
-- * 'mMethodIntegration'
method
    :: Method
method =
    Method'
    { _mMethodResponses = Nothing
    , _mHttpMethod = Nothing
    , _mRequestModels = Nothing
    , _mRequestParameters = Nothing
    , _mAuthorizerId = Nothing
    , _mAuthorizationType = Nothing
    , _mApiKeyRequired = Nothing
    , _mMethodIntegration = Nothing
    }

-- | Represents available responses that can be sent to the caller. Method
-- responses are represented as a key\/value map, with an HTTP status code
-- as the key and a < MethodResponse> as the value. The status codes are
-- available for the < Integration> responses to map to.
mMethodResponses :: Lens' Method (HashMap Text MethodResponse)
mMethodResponses = lens _mMethodResponses (\ s a -> s{_mMethodResponses = a}) . _Default . _Map;

-- | The HTTP method.
mHttpMethod :: Lens' Method (Maybe Text)
mHttpMethod = lens _mHttpMethod (\ s a -> s{_mHttpMethod = a});

-- | Specifies the < Model> resources used for the request\'s content type.
-- Request models are represented as a key\/value map, with a content type
-- as the key and a < Model> name as the value.
mRequestModels :: Lens' Method (HashMap Text Text)
mRequestModels = lens _mRequestModels (\ s a -> s{_mRequestModels = a}) . _Default . _Map;

-- | Represents request parameters that can be accepted by Amazon API
-- Gateway. Request parameters are represented as a key\/value map, with a
-- source as the key and a Boolean flag as the value. The Boolean flag is
-- used to specify whether the parameter is required. A source must match
-- the pattern 'method.request.{location}.{name}', where 'location' is
-- either querystring, path, or header. 'name' is a valid, unique parameter
-- name. Sources specified here are available to the integration for
-- mapping to integration request parameters or templates.
mRequestParameters :: Lens' Method (HashMap Text Bool)
mRequestParameters = lens _mRequestParameters (\ s a -> s{_mRequestParameters = a}) . _Default . _Map;

-- | Specifies the identifier of an < Authorizer> to use on this Method. The
-- authorizationType must be CUSTOM.
mAuthorizerId :: Lens' Method (Maybe Text)
mAuthorizerId = lens _mAuthorizerId (\ s a -> s{_mAuthorizerId = a});

-- | The method\'s authorization type.
mAuthorizationType :: Lens' Method (Maybe Text)
mAuthorizationType = lens _mAuthorizationType (\ s a -> s{_mAuthorizationType = a});

-- | Specifies whether the method requires a valid < ApiKey>.
mApiKeyRequired :: Lens' Method (Maybe Bool)
mApiKeyRequired = lens _mApiKeyRequired (\ s a -> s{_mApiKeyRequired = a});

-- | The method\'s integration.
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
                     <*> (x .:? "authorizationType")
                     <*> (x .:? "apiKeyRequired")
                     <*> (x .:? "methodIntegration"))

instance Hashable Method

instance NFData Method

-- | Represents a method response. Amazon API Gateway sends back the status
-- code to the caller as the HTTP status code. Parameters and models can be
-- used to transform the response from the method\'s integration.
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
-- * 'mResponseModels'
--
-- * 'mStatusCode'
--
-- * 'mResponseParameters'
methodResponse
    :: MethodResponse
methodResponse =
    MethodResponse'
    { _mResponseModels = Nothing
    , _mStatusCode = Nothing
    , _mResponseParameters = Nothing
    }

-- | Specifies the < Model> resources used for the response\'s content-type.
-- Response models are represented as a key\/value map, with a content-type
-- as the key and a < Model> name as the value.
mResponseModels :: Lens' MethodResponse (HashMap Text Text)
mResponseModels = lens _mResponseModels (\ s a -> s{_mResponseModels = a}) . _Default . _Map;

-- | The method response\'s status code.
mStatusCode :: Lens' MethodResponse (Maybe Text)
mStatusCode = lens _mStatusCode (\ s a -> s{_mStatusCode = a});

-- | Represents response parameters that can be sent back to the caller by
-- Amazon API Gateway. Response parameters are represented as a key\/value
-- map, with a destination as the key and a boolean flag as the value,
-- which is used to specify whether the parameter is required. A
-- destination must match the pattern 'method.response.header.{name}',
-- where 'name' is a valid, unique header name. Destinations specified here
-- are available to the integration for mapping from integration response
-- parameters.
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
-- * 'msCacheTtlInSeconds'
--
-- * 'msDataTraceEnabled'
--
-- * 'msThrottlingBurstLimit'
--
-- * 'msCacheDataEncrypted'
--
-- * 'msLoggingLevel'
--
-- * 'msRequireAuthorizationForCacheControl'
--
-- * 'msCachingEnabled'
--
-- * 'msMetricsEnabled'
--
-- * 'msThrottlingRateLimit'
--
-- * 'msUnauthorizedCacheControlHeaderStrategy'
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

-- | Specifies the time to live (TTL) in seconds, for cached responses. The
-- higher a the TTL, the longer the response will be cached. The PATCH path
-- for this setting is '\/{method_setting_key}\/caching\/ttlInSeconds', and
-- the value is an integer.
msCacheTtlInSeconds :: Lens' MethodSetting (Maybe Int)
msCacheTtlInSeconds = lens _msCacheTtlInSeconds (\ s a -> s{_msCacheTtlInSeconds = a});

-- | Specifies the whether data trace logging is enabled for this method,
-- which effects the log entries pushed to Amazon CloudWatch Logs. The
-- PATCH path for this setting is
-- '\/{method_setting_key}\/logging\/dataTrace', and the value is a
-- Boolean.
msDataTraceEnabled :: Lens' MethodSetting (Maybe Bool)
msDataTraceEnabled = lens _msDataTraceEnabled (\ s a -> s{_msDataTraceEnabled = a});

-- | Specifies the throttling burst limit. The PATCH path for this setting is
-- '\/{method_setting_key}\/throttling\/burstLimit', and the value is an
-- integer.
msThrottlingBurstLimit :: Lens' MethodSetting (Maybe Int)
msThrottlingBurstLimit = lens _msThrottlingBurstLimit (\ s a -> s{_msThrottlingBurstLimit = a});

-- | Specifies whether the cached responses are encrypted. The PATCH path for
-- this setting is '\/{method_setting_key}\/caching\/dataEncrypted', and
-- the value is a Boolean.
msCacheDataEncrypted :: Lens' MethodSetting (Maybe Bool)
msCacheDataEncrypted = lens _msCacheDataEncrypted (\ s a -> s{_msCacheDataEncrypted = a});

-- | Specifies the logging level for this method, which effects the log
-- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
-- setting is '\/{method_setting_key}\/logging\/loglevel', and the
-- available levels are 'OFF', 'ERROR', and 'INFO'.
msLoggingLevel :: Lens' MethodSetting (Maybe Text)
msLoggingLevel = lens _msLoggingLevel (\ s a -> s{_msLoggingLevel = a});

-- | Specifies whether authorization is required for a cache invalidation
-- request. The PATCH path for this setting is
-- '\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl',
-- and the value is a Boolean.
msRequireAuthorizationForCacheControl :: Lens' MethodSetting (Maybe Bool)
msRequireAuthorizationForCacheControl = lens _msRequireAuthorizationForCacheControl (\ s a -> s{_msRequireAuthorizationForCacheControl = a});

-- | Specifies whether responses should be cached and returned for requests.
-- A cache cluster must be enabled on the stage for responses to be cached.
-- The PATCH path for this setting is
-- '\/{method_setting_key}\/caching\/enabled', and the value is a Boolean.
msCachingEnabled :: Lens' MethodSetting (Maybe Bool)
msCachingEnabled = lens _msCachingEnabled (\ s a -> s{_msCachingEnabled = a});

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method.
-- The PATCH path for this setting is
-- '\/{method_setting_key}\/metrics\/enabled', and the value is a Boolean.
msMetricsEnabled :: Lens' MethodSetting (Maybe Bool)
msMetricsEnabled = lens _msMetricsEnabled (\ s a -> s{_msMetricsEnabled = a});

-- | Specifies the throttling rate limit. The PATCH path for this setting is
-- '\/{method_setting_key}\/throttling\/rateLimit', and the value is a
-- double.
msThrottlingRateLimit :: Lens' MethodSetting (Maybe Double)
msThrottlingRateLimit = lens _msThrottlingRateLimit (\ s a -> s{_msThrottlingRateLimit = a});

-- | Specifies the strategy on how to handle the unauthorized requests for
-- cache invalidation. The PATCH path for this setting is
-- '\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy',
-- and the available values are 'FAIL_WITH_403',
-- 'SUCCEED_WITH_RESPONSE_HEADER', 'SUCCEED_WITHOUT_RESPONSE_HEADER'.
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

-- | Represents a summary of a < Method> resource, given a particular date
-- and time.
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
-- * 'msAuthorizationType'
--
-- * 'msApiKeyRequired'
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

-- | Specifies whether the method requires a valid < ApiKey>.
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

-- | Represents the structure of a request or response payload for a method.
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
-- * 'mSchema'
--
-- * 'mName'
--
-- * 'mId'
--
-- * 'mDescription'
--
-- * 'mContentType'
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

-- | The schema for the model. For 'application\/json' models, this should be
-- <http://json-schema.org/documentation.html JSON-schema draft v4> model.
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

-- | A single patch operation to apply to the specified resource. Please
-- refer to http:\/\/tools.ietf.org\/html\/rfc6902#section-4 for an
-- explanation of how each operation is used.
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
-- * 'poOp'
--
-- * 'poPath'
--
-- * 'poValue'
--
-- * 'poFrom'
patchOperation
    :: PatchOperation
patchOperation =
    PatchOperation'
    { _poOp = Nothing
    , _poPath = Nothing
    , _poValue = Nothing
    , _poFrom = Nothing
    }

-- | A patch operation whose value indicates the operation to perform. Its
-- value MUST be one of \"add\", \"remove\", \"replace\", \"move\",
-- \"copy\", or \"test\"; other values are errors.
poOp :: Lens' PatchOperation (Maybe Op)
poOp = lens _poOp (\ s a -> s{_poOp = a});

-- | Operation objects MUST have exactly one \"path\" member. That member\'s
-- value is a string containing a \`JSON-Pointer\` value that references a
-- location within the target document (the \"target location\") where the
-- operation is performed.
poPath :: Lens' PatchOperation (Maybe Text)
poPath = lens _poPath (\ s a -> s{_poPath = a});

-- | The actual value content.
poValue :: Lens' PatchOperation (Maybe Text)
poValue = lens _poValue (\ s a -> s{_poValue = a});

-- | The \"move\" and \"copy\" operation object MUST contain a \"from\"
-- member, which is a string containing a 'JSON Pointer' value that
-- references the location in the target document to move the value from.
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

-- | Represents a resource.
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
-- * 'rPathPart'
--
-- * 'rPath'
--
-- * 'rId'
--
-- * 'rResourceMethods'
--
-- * 'rParentId'
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

-- | The resource\'s identifier.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a});

-- | Map of methods for this resource, which is included only if the request
-- uses the __embed__ query option.
rResourceMethods :: Lens' Resource (HashMap Text Method)
rResourceMethods = lens _rResourceMethods (\ s a -> s{_rResourceMethods = a}) . _Default . _Map;

-- | The parent resource\'s identifier.
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
-- /See:/ 'restAPI' smart constructor.
data RestAPI = RestAPI'
    { _raWarnings    :: !(Maybe [Text])
    , _raCreatedDate :: !(Maybe ISO8601)
    , _raName        :: !(Maybe Text)
    , _raId          :: !(Maybe Text)
    , _raDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raWarnings'
--
-- * 'raCreatedDate'
--
-- * 'raName'
--
-- * 'raId'
--
-- * 'raDescription'
restAPI
    :: RestAPI
restAPI =
    RestAPI'
    { _raWarnings = Nothing
    , _raCreatedDate = Nothing
    , _raName = Nothing
    , _raId = Nothing
    , _raDescription = Nothing
    }

-- | Undocumented member.
raWarnings :: Lens' RestAPI [Text]
raWarnings = lens _raWarnings (\ s a -> s{_raWarnings = a}) . _Default . _Coerce;

-- | The date when the API was created, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
raCreatedDate :: Lens' RestAPI (Maybe UTCTime)
raCreatedDate = lens _raCreatedDate (\ s a -> s{_raCreatedDate = a}) . mapping _Time;

-- | The API\'s name.
raName :: Lens' RestAPI (Maybe Text)
raName = lens _raName (\ s a -> s{_raName = a});

-- | The API\'s identifier. This identifier is unique across all of your APIs
-- in Amazon API Gateway.
raId :: Lens' RestAPI (Maybe Text)
raId = lens _raId (\ s a -> s{_raId = a});

-- | The API\'s description.
raDescription :: Lens' RestAPI (Maybe Text)
raDescription = lens _raDescription (\ s a -> s{_raDescription = a});

instance FromJSON RestAPI where
        parseJSON
          = withObject "RestAPI"
              (\ x ->
                 RestAPI' <$>
                   (x .:? "warnings" .!= mempty) <*>
                     (x .:? "createdDate")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "description"))

instance Hashable RestAPI

instance NFData RestAPI

-- | Represents a unique identifier for a version of a deployed < RestApi>
-- that is callable by users.
--
-- /See:/ 'stage' smart constructor.
data Stage = Stage'
    { _sDeploymentId        :: !(Maybe Text)
    , _sVariables           :: !(Maybe (Map Text Text))
    , _sClientCertificateId :: !(Maybe Text)
    , _sCreatedDate         :: !(Maybe ISO8601)
    , _sCacheClusterStatus  :: !(Maybe CacheClusterStatus)
    , _sMethodSettings      :: !(Maybe (Map Text MethodSetting))
    , _sLastUpdatedDate     :: !(Maybe ISO8601)
    , _sCacheClusterSize    :: !(Maybe CacheClusterSize)
    , _sCacheClusterEnabled :: !(Maybe Bool)
    , _sStageName           :: !(Maybe Text)
    , _sDescription         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Stage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeploymentId'
--
-- * 'sVariables'
--
-- * 'sClientCertificateId'
--
-- * 'sCreatedDate'
--
-- * 'sCacheClusterStatus'
--
-- * 'sMethodSettings'
--
-- * 'sLastUpdatedDate'
--
-- * 'sCacheClusterSize'
--
-- * 'sCacheClusterEnabled'
--
-- * 'sStageName'
--
-- * 'sDescription'
stage
    :: Stage
stage =
    Stage'
    { _sDeploymentId = Nothing
    , _sVariables = Nothing
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

-- | The identifier of the < Deployment> that the stage points to.
sDeploymentId :: Lens' Stage (Maybe Text)
sDeploymentId = lens _sDeploymentId (\ s a -> s{_sDeploymentId = a});

-- | A map that defines the stage variables for a < Stage> resource. Variable
-- names can have alphanumeric characters, and the values must match
-- '[A-Za-z0-9-._~:\/?#&=,]+'.
sVariables :: Lens' Stage (HashMap Text Text)
sVariables = lens _sVariables (\ s a -> s{_sVariables = a}) . _Default . _Map;

-- | Undocumented member.
sClientCertificateId :: Lens' Stage (Maybe Text)
sClientCertificateId = lens _sClientCertificateId (\ s a -> s{_sClientCertificateId = a});

-- | The date and time that the stage was created, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
sCreatedDate :: Lens' Stage (Maybe UTCTime)
sCreatedDate = lens _sCreatedDate (\ s a -> s{_sCreatedDate = a}) . mapping _Time;

-- | The status of the cache cluster for the stage, if enabled.
sCacheClusterStatus :: Lens' Stage (Maybe CacheClusterStatus)
sCacheClusterStatus = lens _sCacheClusterStatus (\ s a -> s{_sCacheClusterStatus = a});

-- | A map that defines the method settings for a < Stage> resource. Keys are
-- defined as '{resource_path}\/{http_method}' for an individual method
-- override, or '\\*\/\\*' for the settings applied to all methods in the
-- stage.
sMethodSettings :: Lens' Stage (HashMap Text MethodSetting)
sMethodSettings = lens _sMethodSettings (\ s a -> s{_sMethodSettings = a}) . _Default . _Map;

-- | The date and time that information about the stage was last updated, in
-- <http://www.iso.org/iso/home/standards/iso8601.htm ISO 8601 format>.
sLastUpdatedDate :: Lens' Stage (Maybe UTCTime)
sLastUpdatedDate = lens _sLastUpdatedDate (\ s a -> s{_sLastUpdatedDate = a}) . mapping _Time;

-- | The size of the cache cluster for the stage, if enabled.
sCacheClusterSize :: Lens' Stage (Maybe CacheClusterSize)
sCacheClusterSize = lens _sCacheClusterSize (\ s a -> s{_sCacheClusterSize = a});

-- | Specifies whether a cache cluster is enabled for the stage.
sCacheClusterEnabled :: Lens' Stage (Maybe Bool)
sCacheClusterEnabled = lens _sCacheClusterEnabled (\ s a -> s{_sCacheClusterEnabled = a});

-- | The name of the stage is the first path segment in the Uniform Resource
-- Identifier (URI) of a call to Amazon API Gateway.
sStageName :: Lens' Stage (Maybe Text)
sStageName = lens _sStageName (\ s a -> s{_sStageName = a});

-- | The stage\'s description.
sDescription :: Lens' Stage (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a});

instance FromJSON Stage where
        parseJSON
          = withObject "Stage"
              (\ x ->
                 Stage' <$>
                   (x .:? "deploymentId") <*>
                     (x .:? "variables" .!= mempty)
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

-- | A reference to a unique stage identified in the format
-- '{restApiId}\/{stage}'.
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
-- * 'skRestAPIId'
--
-- * 'skStageName'
stageKey
    :: StageKey
stageKey =
    StageKey'
    { _skRestAPIId = Nothing
    , _skStageName = Nothing
    }

-- | A list of < Stage> resources that are associated with the < ApiKey>
-- resource.
skRestAPIId :: Lens' StageKey (Maybe Text)
skRestAPIId = lens _skRestAPIId (\ s a -> s{_skRestAPIId = a});

-- | The stage name in the < RestApi> that the stage key references.
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

-- | Returns the throttle settings.
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
-- * 'tsBurstLimit'
--
-- * 'tsRateLimit'
throttleSettings
    :: ThrottleSettings
throttleSettings =
    ThrottleSettings'
    { _tsBurstLimit = Nothing
    , _tsRateLimit = Nothing
    }

-- | Returns the burstLimit when __ThrottleSettings__ is called.
tsBurstLimit :: Lens' ThrottleSettings (Maybe Int)
tsBurstLimit = lens _tsBurstLimit (\ s a -> s{_tsBurstLimit = a});

-- | Returns the rateLimit when __ThrottleSettings__ is called.
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
