{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.Product where

import Network.AWS.AppSync.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an API key.
--
--
-- Customers invoke AWS AppSync GraphQL APIs with API keys as an identity mechanism. There are two key versions:
--
-- __da1__ : This version was introduced at launch in November 2017. These keys always expire after 7 days. Key expiration is managed by DynamoDB TTL. The keys will cease to be valid after Feb 21, 2018 and should not be used after that date.
--
--     * @ListApiKeys@ returns the expiration time in milliseconds.
--
--     * @CreateApiKey@ returns the expiration time in milliseconds.
--
--     * @UpdateApiKey@ is not available for this key version.
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--     * Expiration is stored in DynamoDB as milliseconds. This results in a bug where keys are not automatically deleted because DynamoDB expects the TTL to be stored in seconds. As a one-time action, we will delete these keys from the table after Feb 21, 2018.
--
--
--
-- __da2__ : This version was introduced in February 2018 when AppSync added support to extend key expiration.
--
--     * @ListApiKeys@ returns the expiration time in seconds.
--
--     * @CreateApiKey@ returns the expiration time in seconds and accepts a user-provided expiration time in seconds.
--
--     * @UpdateApiKey@ returns the expiration time in seconds and accepts a user-provided expiration time in seconds. Key expiration can only be updated while the key has not expired.
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--     * Expiration is stored in DynamoDB as seconds.
--
--
--
--
-- /See:/ 'apiKey' smart constructor.
data APIKey = APIKey'
  { _akExpires     :: !(Maybe Integer)
  , _akId          :: !(Maybe Text)
  , _akDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akExpires' - The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- * 'akId' - The API key ID.
--
-- * 'akDescription' - A description of the purpose of the API key.
apiKey
    :: APIKey
apiKey =
  APIKey' {_akExpires = Nothing, _akId = Nothing, _akDescription = Nothing}


-- | The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
akExpires :: Lens' APIKey (Maybe Integer)
akExpires = lens _akExpires (\ s a -> s{_akExpires = a})

-- | The API key ID.
akId :: Lens' APIKey (Maybe Text)
akId = lens _akId (\ s a -> s{_akId = a})

-- | A description of the purpose of the API key.
akDescription :: Lens' APIKey (Maybe Text)
akDescription = lens _akDescription (\ s a -> s{_akDescription = a})

instance FromJSON APIKey where
        parseJSON
          = withObject "APIKey"
              (\ x ->
                 APIKey' <$>
                   (x .:? "expires") <*> (x .:? "id") <*>
                     (x .:? "description"))

instance Hashable APIKey where

instance NFData APIKey where

-- | Describes a data source.
--
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsServiceRoleARN      :: !(Maybe Text)
  , _dsDataSourceARN       :: !(Maybe Text)
  , _dsDynamodbConfig      :: !(Maybe DynamodbDataSourceConfig)
  , _dsName                :: !(Maybe Text)
  , _dsLambdaConfig        :: !(Maybe LambdaDataSourceConfig)
  , _dsType                :: !(Maybe DataSourceType)
  , _dsDescription         :: !(Maybe Text)
  , _dsElasticsearchConfig :: !(Maybe ElasticsearchDataSourceConfig)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsServiceRoleARN' - The IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- * 'dsDataSourceARN' - The data source ARN.
--
-- * 'dsDynamodbConfig' - DynamoDB settings.
--
-- * 'dsName' - The name of the data source.
--
-- * 'dsLambdaConfig' - Lambda settings.
--
-- * 'dsType' - The type of the data source.     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.     * __NONE__ : There is no data source. This type is used when when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.
--
-- * 'dsDescription' - The description of the data source.
--
-- * 'dsElasticsearchConfig' - Amazon Elasticsearch settings.
dataSource
    :: DataSource
dataSource =
  DataSource'
    { _dsServiceRoleARN = Nothing
    , _dsDataSourceARN = Nothing
    , _dsDynamodbConfig = Nothing
    , _dsName = Nothing
    , _dsLambdaConfig = Nothing
    , _dsType = Nothing
    , _dsDescription = Nothing
    , _dsElasticsearchConfig = Nothing
    }


-- | The IAM service role ARN for the data source. The system assumes this role when accessing the data source.
dsServiceRoleARN :: Lens' DataSource (Maybe Text)
dsServiceRoleARN = lens _dsServiceRoleARN (\ s a -> s{_dsServiceRoleARN = a})

-- | The data source ARN.
dsDataSourceARN :: Lens' DataSource (Maybe Text)
dsDataSourceARN = lens _dsDataSourceARN (\ s a -> s{_dsDataSourceARN = a})

-- | DynamoDB settings.
dsDynamodbConfig :: Lens' DataSource (Maybe DynamodbDataSourceConfig)
dsDynamodbConfig = lens _dsDynamodbConfig (\ s a -> s{_dsDynamodbConfig = a})

-- | The name of the data source.
dsName :: Lens' DataSource (Maybe Text)
dsName = lens _dsName (\ s a -> s{_dsName = a})

-- | Lambda settings.
dsLambdaConfig :: Lens' DataSource (Maybe LambdaDataSourceConfig)
dsLambdaConfig = lens _dsLambdaConfig (\ s a -> s{_dsLambdaConfig = a})

-- | The type of the data source.     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.     * __NONE__ : There is no data source. This type is used when when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.
dsType :: Lens' DataSource (Maybe DataSourceType)
dsType = lens _dsType (\ s a -> s{_dsType = a})

-- | The description of the data source.
dsDescription :: Lens' DataSource (Maybe Text)
dsDescription = lens _dsDescription (\ s a -> s{_dsDescription = a})

-- | Amazon Elasticsearch settings.
dsElasticsearchConfig :: Lens' DataSource (Maybe ElasticsearchDataSourceConfig)
dsElasticsearchConfig = lens _dsElasticsearchConfig (\ s a -> s{_dsElasticsearchConfig = a})

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x ->
                 DataSource' <$>
                   (x .:? "serviceRoleArn") <*> (x .:? "dataSourceArn")
                     <*> (x .:? "dynamodbConfig")
                     <*> (x .:? "name")
                     <*> (x .:? "lambdaConfig")
                     <*> (x .:? "type")
                     <*> (x .:? "description")
                     <*> (x .:? "elasticsearchConfig"))

instance Hashable DataSource where

instance NFData DataSource where

-- | Describes a DynamoDB data source configuration.
--
--
--
-- /See:/ 'dynamodbDataSourceConfig' smart constructor.
data DynamodbDataSourceConfig = DynamodbDataSourceConfig'
  { _ddscUseCallerCredentials :: !(Maybe Bool)
  , _ddscTableName            :: !Text
  , _ddscAwsRegion            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DynamodbDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddscUseCallerCredentials' - Set to TRUE to use Amazon Cognito credentials with this data source.
--
-- * 'ddscTableName' - The table name.
--
-- * 'ddscAwsRegion' - The AWS region.
dynamodbDataSourceConfig
    :: Text -- ^ 'ddscTableName'
    -> Text -- ^ 'ddscAwsRegion'
    -> DynamodbDataSourceConfig
dynamodbDataSourceConfig pTableName_ pAwsRegion_ =
  DynamodbDataSourceConfig'
    { _ddscUseCallerCredentials = Nothing
    , _ddscTableName = pTableName_
    , _ddscAwsRegion = pAwsRegion_
    }


-- | Set to TRUE to use Amazon Cognito credentials with this data source.
ddscUseCallerCredentials :: Lens' DynamodbDataSourceConfig (Maybe Bool)
ddscUseCallerCredentials = lens _ddscUseCallerCredentials (\ s a -> s{_ddscUseCallerCredentials = a})

-- | The table name.
ddscTableName :: Lens' DynamodbDataSourceConfig Text
ddscTableName = lens _ddscTableName (\ s a -> s{_ddscTableName = a})

-- | The AWS region.
ddscAwsRegion :: Lens' DynamodbDataSourceConfig Text
ddscAwsRegion = lens _ddscAwsRegion (\ s a -> s{_ddscAwsRegion = a})

instance FromJSON DynamodbDataSourceConfig where
        parseJSON
          = withObject "DynamodbDataSourceConfig"
              (\ x ->
                 DynamodbDataSourceConfig' <$>
                   (x .:? "useCallerCredentials") <*> (x .: "tableName")
                     <*> (x .: "awsRegion"))

instance Hashable DynamodbDataSourceConfig where

instance NFData DynamodbDataSourceConfig where

instance ToJSON DynamodbDataSourceConfig where
        toJSON DynamodbDataSourceConfig'{..}
          = object
              (catMaybes
                 [("useCallerCredentials" .=) <$>
                    _ddscUseCallerCredentials,
                  Just ("tableName" .= _ddscTableName),
                  Just ("awsRegion" .= _ddscAwsRegion)])

-- | Describes an Elasticsearch data source configuration.
--
--
--
-- /See:/ 'elasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { _edscEndpoint  :: !Text
  , _edscAwsRegion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edscEndpoint' - The endpoint.
--
-- * 'edscAwsRegion' - The AWS region.
elasticsearchDataSourceConfig
    :: Text -- ^ 'edscEndpoint'
    -> Text -- ^ 'edscAwsRegion'
    -> ElasticsearchDataSourceConfig
elasticsearchDataSourceConfig pEndpoint_ pAwsRegion_ =
  ElasticsearchDataSourceConfig'
    {_edscEndpoint = pEndpoint_, _edscAwsRegion = pAwsRegion_}


-- | The endpoint.
edscEndpoint :: Lens' ElasticsearchDataSourceConfig Text
edscEndpoint = lens _edscEndpoint (\ s a -> s{_edscEndpoint = a})

-- | The AWS region.
edscAwsRegion :: Lens' ElasticsearchDataSourceConfig Text
edscAwsRegion = lens _edscAwsRegion (\ s a -> s{_edscAwsRegion = a})

instance FromJSON ElasticsearchDataSourceConfig where
        parseJSON
          = withObject "ElasticsearchDataSourceConfig"
              (\ x ->
                 ElasticsearchDataSourceConfig' <$>
                   (x .: "endpoint") <*> (x .: "awsRegion"))

instance Hashable ElasticsearchDataSourceConfig where

instance NFData ElasticsearchDataSourceConfig where

instance ToJSON ElasticsearchDataSourceConfig where
        toJSON ElasticsearchDataSourceConfig'{..}
          = object
              (catMaybes
                 [Just ("endpoint" .= _edscEndpoint),
                  Just ("awsRegion" .= _edscAwsRegion)])

-- | Describes a GraphQL API.
--
--
--
-- /See:/ 'graphqlAPI' smart constructor.
data GraphqlAPI = GraphqlAPI'
  { _gaArn                 :: !(Maybe Text)
  , _gaApiId               :: !(Maybe Text)
  , _gaUris                :: !(Maybe (Map Text Text))
  , _gaOpenIdConnectConfig :: !(Maybe OpenIdConnectConfig)
  , _gaName                :: !(Maybe Text)
  , _gaUserPoolConfig      :: !(Maybe UserPoolConfig)
  , _gaAuthenticationType  :: !(Maybe AuthenticationType)
  , _gaLogConfig           :: !(Maybe LogConfig)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GraphqlAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaArn' - The ARN.
--
-- * 'gaApiId' - The API ID.
--
-- * 'gaUris' - The URIs.
--
-- * 'gaOpenIdConnectConfig' - The Open Id Connect configuration.
--
-- * 'gaName' - The API name.
--
-- * 'gaUserPoolConfig' - The Amazon Cognito User Pool configuration.
--
-- * 'gaAuthenticationType' - The authentication type.
--
-- * 'gaLogConfig' - The Amazon CloudWatch Logs configuration.
graphqlAPI
    :: GraphqlAPI
graphqlAPI =
  GraphqlAPI'
    { _gaArn = Nothing
    , _gaApiId = Nothing
    , _gaUris = Nothing
    , _gaOpenIdConnectConfig = Nothing
    , _gaName = Nothing
    , _gaUserPoolConfig = Nothing
    , _gaAuthenticationType = Nothing
    , _gaLogConfig = Nothing
    }


-- | The ARN.
gaArn :: Lens' GraphqlAPI (Maybe Text)
gaArn = lens _gaArn (\ s a -> s{_gaArn = a})

-- | The API ID.
gaApiId :: Lens' GraphqlAPI (Maybe Text)
gaApiId = lens _gaApiId (\ s a -> s{_gaApiId = a})

-- | The URIs.
gaUris :: Lens' GraphqlAPI (HashMap Text Text)
gaUris = lens _gaUris (\ s a -> s{_gaUris = a}) . _Default . _Map

-- | The Open Id Connect configuration.
gaOpenIdConnectConfig :: Lens' GraphqlAPI (Maybe OpenIdConnectConfig)
gaOpenIdConnectConfig = lens _gaOpenIdConnectConfig (\ s a -> s{_gaOpenIdConnectConfig = a})

-- | The API name.
gaName :: Lens' GraphqlAPI (Maybe Text)
gaName = lens _gaName (\ s a -> s{_gaName = a})

-- | The Amazon Cognito User Pool configuration.
gaUserPoolConfig :: Lens' GraphqlAPI (Maybe UserPoolConfig)
gaUserPoolConfig = lens _gaUserPoolConfig (\ s a -> s{_gaUserPoolConfig = a})

-- | The authentication type.
gaAuthenticationType :: Lens' GraphqlAPI (Maybe AuthenticationType)
gaAuthenticationType = lens _gaAuthenticationType (\ s a -> s{_gaAuthenticationType = a})

-- | The Amazon CloudWatch Logs configuration.
gaLogConfig :: Lens' GraphqlAPI (Maybe LogConfig)
gaLogConfig = lens _gaLogConfig (\ s a -> s{_gaLogConfig = a})

instance FromJSON GraphqlAPI where
        parseJSON
          = withObject "GraphqlAPI"
              (\ x ->
                 GraphqlAPI' <$>
                   (x .:? "arn") <*> (x .:? "apiId") <*>
                     (x .:? "uris" .!= mempty)
                     <*> (x .:? "openIDConnectConfig")
                     <*> (x .:? "name")
                     <*> (x .:? "userPoolConfig")
                     <*> (x .:? "authenticationType")
                     <*> (x .:? "logConfig"))

instance Hashable GraphqlAPI where

instance NFData GraphqlAPI where

-- | Describes a Lambda data source configuration.
--
--
--
-- /See:/ 'lambdaDataSourceConfig' smart constructor.
newtype LambdaDataSourceConfig = LambdaDataSourceConfig'
  { _ldscLambdaFunctionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldscLambdaFunctionARN' - The ARN for the Lambda function.
lambdaDataSourceConfig
    :: Text -- ^ 'ldscLambdaFunctionARN'
    -> LambdaDataSourceConfig
lambdaDataSourceConfig pLambdaFunctionARN_ =
  LambdaDataSourceConfig' {_ldscLambdaFunctionARN = pLambdaFunctionARN_}


-- | The ARN for the Lambda function.
ldscLambdaFunctionARN :: Lens' LambdaDataSourceConfig Text
ldscLambdaFunctionARN = lens _ldscLambdaFunctionARN (\ s a -> s{_ldscLambdaFunctionARN = a})

instance FromJSON LambdaDataSourceConfig where
        parseJSON
          = withObject "LambdaDataSourceConfig"
              (\ x ->
                 LambdaDataSourceConfig' <$>
                   (x .: "lambdaFunctionArn"))

instance Hashable LambdaDataSourceConfig where

instance NFData LambdaDataSourceConfig where

instance ToJSON LambdaDataSourceConfig where
        toJSON LambdaDataSourceConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("lambdaFunctionArn" .= _ldscLambdaFunctionARN)])

-- | The CloudWatch Logs configuration.
--
--
--
-- /See:/ 'logConfig' smart constructor.
data LogConfig = LogConfig'
  { _lcFieldLogLevel         :: !FieldLogLevel
  , _lcCloudWatchLogsRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcFieldLogLevel' - The field logging level. Values can be NONE, ERROR, ALL.      * __NONE__ : No field-level logs are captured.     * __ERROR__ : Logs the following information only for the fields that are in error:     * The error section in the server response.     * Field-level errors.     * The generated request/response functions that got resolved for error fields.     * __ALL__ : The following information is logged for all fields in the query:     * Field-level tracing information.     * The generated request/response functions that got resolved for each field.
--
-- * 'lcCloudWatchLogsRoleARN' - The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
logConfig
    :: FieldLogLevel -- ^ 'lcFieldLogLevel'
    -> Text -- ^ 'lcCloudWatchLogsRoleARN'
    -> LogConfig
logConfig pFieldLogLevel_ pCloudWatchLogsRoleARN_ =
  LogConfig'
    { _lcFieldLogLevel = pFieldLogLevel_
    , _lcCloudWatchLogsRoleARN = pCloudWatchLogsRoleARN_
    }


-- | The field logging level. Values can be NONE, ERROR, ALL.      * __NONE__ : No field-level logs are captured.     * __ERROR__ : Logs the following information only for the fields that are in error:     * The error section in the server response.     * Field-level errors.     * The generated request/response functions that got resolved for error fields.     * __ALL__ : The following information is logged for all fields in the query:     * Field-level tracing information.     * The generated request/response functions that got resolved for each field.
lcFieldLogLevel :: Lens' LogConfig FieldLogLevel
lcFieldLogLevel = lens _lcFieldLogLevel (\ s a -> s{_lcFieldLogLevel = a})

-- | The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
lcCloudWatchLogsRoleARN :: Lens' LogConfig Text
lcCloudWatchLogsRoleARN = lens _lcCloudWatchLogsRoleARN (\ s a -> s{_lcCloudWatchLogsRoleARN = a})

instance FromJSON LogConfig where
        parseJSON
          = withObject "LogConfig"
              (\ x ->
                 LogConfig' <$>
                   (x .: "fieldLogLevel") <*>
                     (x .: "cloudWatchLogsRoleArn"))

instance Hashable LogConfig where

instance NFData LogConfig where

instance ToJSON LogConfig where
        toJSON LogConfig'{..}
          = object
              (catMaybes
                 [Just ("fieldLogLevel" .= _lcFieldLogLevel),
                  Just
                    ("cloudWatchLogsRoleArn" .=
                       _lcCloudWatchLogsRoleARN)])

-- | Describes an Open Id Connect configuration.
--
--
--
-- /See:/ 'openIdConnectConfig' smart constructor.
data OpenIdConnectConfig = OpenIdConnectConfig'
  { _oiccAuthTTL  :: !(Maybe Integer)
  , _oiccClientId :: !(Maybe Text)
  , _oiccIatTTL   :: !(Maybe Integer)
  , _oiccIssuer   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OpenIdConnectConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiccAuthTTL' - The number of milliseconds a token is valid after being authenticated.
--
-- * 'oiccClientId' - The client identifier of the Relying party at the OpenID Provider. This identifier is typically obtained when the Relying party is registered with the OpenID Provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time
--
-- * 'oiccIatTTL' - The number of milliseconds a token is valid after being issued to a user.
--
-- * 'oiccIssuer' - The issuer for the open id connect configuration. The issuer returned by discovery MUST exactly match the value of iss in the ID Token.
openIdConnectConfig
    :: Text -- ^ 'oiccIssuer'
    -> OpenIdConnectConfig
openIdConnectConfig pIssuer_ =
  OpenIdConnectConfig'
    { _oiccAuthTTL = Nothing
    , _oiccClientId = Nothing
    , _oiccIatTTL = Nothing
    , _oiccIssuer = pIssuer_
    }


-- | The number of milliseconds a token is valid after being authenticated.
oiccAuthTTL :: Lens' OpenIdConnectConfig (Maybe Integer)
oiccAuthTTL = lens _oiccAuthTTL (\ s a -> s{_oiccAuthTTL = a})

-- | The client identifier of the Relying party at the OpenID Provider. This identifier is typically obtained when the Relying party is registered with the OpenID Provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time
oiccClientId :: Lens' OpenIdConnectConfig (Maybe Text)
oiccClientId = lens _oiccClientId (\ s a -> s{_oiccClientId = a})

-- | The number of milliseconds a token is valid after being issued to a user.
oiccIatTTL :: Lens' OpenIdConnectConfig (Maybe Integer)
oiccIatTTL = lens _oiccIatTTL (\ s a -> s{_oiccIatTTL = a})

-- | The issuer for the open id connect configuration. The issuer returned by discovery MUST exactly match the value of iss in the ID Token.
oiccIssuer :: Lens' OpenIdConnectConfig Text
oiccIssuer = lens _oiccIssuer (\ s a -> s{_oiccIssuer = a})

instance FromJSON OpenIdConnectConfig where
        parseJSON
          = withObject "OpenIdConnectConfig"
              (\ x ->
                 OpenIdConnectConfig' <$>
                   (x .:? "authTTL") <*> (x .:? "clientId") <*>
                     (x .:? "iatTTL")
                     <*> (x .: "issuer"))

instance Hashable OpenIdConnectConfig where

instance NFData OpenIdConnectConfig where

instance ToJSON OpenIdConnectConfig where
        toJSON OpenIdConnectConfig'{..}
          = object
              (catMaybes
                 [("authTTL" .=) <$> _oiccAuthTTL,
                  ("clientId" .=) <$> _oiccClientId,
                  ("iatTTL" .=) <$> _oiccIatTTL,
                  Just ("issuer" .= _oiccIssuer)])

-- | Describes a resolver.
--
--
--
-- /See:/ 'resolver' smart constructor.
data Resolver = Resolver'
  { _rTypeName                :: !(Maybe Text)
  , _rDataSourceName          :: !(Maybe Text)
  , _rRequestMappingTemplate  :: !(Maybe Text)
  , _rResolverARN             :: !(Maybe Text)
  , _rResponseMappingTemplate :: !(Maybe Text)
  , _rFieldName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rTypeName' - The resolver type name.
--
-- * 'rDataSourceName' - The resolver data source name.
--
-- * 'rRequestMappingTemplate' - The request mapping template.
--
-- * 'rResolverARN' - The resolver ARN.
--
-- * 'rResponseMappingTemplate' - The response mapping template.
--
-- * 'rFieldName' - The resolver field name.
resolver
    :: Resolver
resolver =
  Resolver'
    { _rTypeName = Nothing
    , _rDataSourceName = Nothing
    , _rRequestMappingTemplate = Nothing
    , _rResolverARN = Nothing
    , _rResponseMappingTemplate = Nothing
    , _rFieldName = Nothing
    }


-- | The resolver type name.
rTypeName :: Lens' Resolver (Maybe Text)
rTypeName = lens _rTypeName (\ s a -> s{_rTypeName = a})

-- | The resolver data source name.
rDataSourceName :: Lens' Resolver (Maybe Text)
rDataSourceName = lens _rDataSourceName (\ s a -> s{_rDataSourceName = a})

-- | The request mapping template.
rRequestMappingTemplate :: Lens' Resolver (Maybe Text)
rRequestMappingTemplate = lens _rRequestMappingTemplate (\ s a -> s{_rRequestMappingTemplate = a})

-- | The resolver ARN.
rResolverARN :: Lens' Resolver (Maybe Text)
rResolverARN = lens _rResolverARN (\ s a -> s{_rResolverARN = a})

-- | The response mapping template.
rResponseMappingTemplate :: Lens' Resolver (Maybe Text)
rResponseMappingTemplate = lens _rResponseMappingTemplate (\ s a -> s{_rResponseMappingTemplate = a})

-- | The resolver field name.
rFieldName :: Lens' Resolver (Maybe Text)
rFieldName = lens _rFieldName (\ s a -> s{_rFieldName = a})

instance FromJSON Resolver where
        parseJSON
          = withObject "Resolver"
              (\ x ->
                 Resolver' <$>
                   (x .:? "typeName") <*> (x .:? "dataSourceName") <*>
                     (x .:? "requestMappingTemplate")
                     <*> (x .:? "resolverArn")
                     <*> (x .:? "responseMappingTemplate")
                     <*> (x .:? "fieldName"))

instance Hashable Resolver where

instance NFData Resolver where

-- | Describes a type.
--
--
--
-- /See:/ 'type'' smart constructor.
data Type = Type'
  { _tArn         :: !(Maybe Text)
  , _tDefinition  :: !(Maybe Text)
  , _tFormat      :: !(Maybe TypeDefinitionFormat)
  , _tName        :: !(Maybe Text)
  , _tDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Type' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tArn' - The type ARN.
--
-- * 'tDefinition' - The type definition.
--
-- * 'tFormat' - The type format: SDL or JSON.
--
-- * 'tName' - The type name.
--
-- * 'tDescription' - The type description.
type'
    :: Type
type' =
  Type'
    { _tArn = Nothing
    , _tDefinition = Nothing
    , _tFormat = Nothing
    , _tName = Nothing
    , _tDescription = Nothing
    }


-- | The type ARN.
tArn :: Lens' Type (Maybe Text)
tArn = lens _tArn (\ s a -> s{_tArn = a})

-- | The type definition.
tDefinition :: Lens' Type (Maybe Text)
tDefinition = lens _tDefinition (\ s a -> s{_tDefinition = a})

-- | The type format: SDL or JSON.
tFormat :: Lens' Type (Maybe TypeDefinitionFormat)
tFormat = lens _tFormat (\ s a -> s{_tFormat = a})

-- | The type name.
tName :: Lens' Type (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

-- | The type description.
tDescription :: Lens' Type (Maybe Text)
tDescription = lens _tDescription (\ s a -> s{_tDescription = a})

instance FromJSON Type where
        parseJSON
          = withObject "Type"
              (\ x ->
                 Type' <$>
                   (x .:? "arn") <*> (x .:? "definition") <*>
                     (x .:? "format")
                     <*> (x .:? "name")
                     <*> (x .:? "description"))

instance Hashable Type where

instance NFData Type where

-- | Describes an Amazon Cognito User Pool configuration.
--
--
--
-- /See:/ 'userPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { _upcAppIdClientRegex :: !(Maybe Text)
  , _upcUserPoolId       :: !Text
  , _upcAwsRegion        :: !Text
  , _upcDefaultAction    :: !DefaultAction
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserPoolConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcAppIdClientRegex' - A regular expression for validating the incoming Amazon Cognito User Pool app client ID.
--
-- * 'upcUserPoolId' - The user pool ID.
--
-- * 'upcAwsRegion' - The AWS region in which the user pool was created.
--
-- * 'upcDefaultAction' - The action that you want your GraphQL API to take when a request that uses Amazon Cognito User Pool authentication doesn't match the Amazon Cognito User Pool configuration.
userPoolConfig
    :: Text -- ^ 'upcUserPoolId'
    -> Text -- ^ 'upcAwsRegion'
    -> DefaultAction -- ^ 'upcDefaultAction'
    -> UserPoolConfig
userPoolConfig pUserPoolId_ pAwsRegion_ pDefaultAction_ =
  UserPoolConfig'
    { _upcAppIdClientRegex = Nothing
    , _upcUserPoolId = pUserPoolId_
    , _upcAwsRegion = pAwsRegion_
    , _upcDefaultAction = pDefaultAction_
    }


-- | A regular expression for validating the incoming Amazon Cognito User Pool app client ID.
upcAppIdClientRegex :: Lens' UserPoolConfig (Maybe Text)
upcAppIdClientRegex = lens _upcAppIdClientRegex (\ s a -> s{_upcAppIdClientRegex = a})

-- | The user pool ID.
upcUserPoolId :: Lens' UserPoolConfig Text
upcUserPoolId = lens _upcUserPoolId (\ s a -> s{_upcUserPoolId = a})

-- | The AWS region in which the user pool was created.
upcAwsRegion :: Lens' UserPoolConfig Text
upcAwsRegion = lens _upcAwsRegion (\ s a -> s{_upcAwsRegion = a})

-- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito User Pool authentication doesn't match the Amazon Cognito User Pool configuration.
upcDefaultAction :: Lens' UserPoolConfig DefaultAction
upcDefaultAction = lens _upcDefaultAction (\ s a -> s{_upcDefaultAction = a})

instance FromJSON UserPoolConfig where
        parseJSON
          = withObject "UserPoolConfig"
              (\ x ->
                 UserPoolConfig' <$>
                   (x .:? "appIdClientRegex") <*> (x .: "userPoolId")
                     <*> (x .: "awsRegion")
                     <*> (x .: "defaultAction"))

instance Hashable UserPoolConfig where

instance NFData UserPoolConfig where

instance ToJSON UserPoolConfig where
        toJSON UserPoolConfig'{..}
          = object
              (catMaybes
                 [("appIdClientRegex" .=) <$> _upcAppIdClientRegex,
                  Just ("userPoolId" .= _upcUserPoolId),
                  Just ("awsRegion" .= _upcAwsRegion),
                  Just ("defaultAction" .= _upcDefaultAction)])
