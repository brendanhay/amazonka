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
-- Module      : Network.AWS.DMS.CreateEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the provided settings.
--
--
module Network.AWS.DMS.CreateEndpoint
    (
    -- * Creating a Request
      createEndpoint
    , CreateEndpoint
    -- * Request Lenses
    , ceServerName
    , ceCertificateARN
    , ceServiceAccessRoleARN
    , ceExtraConnectionAttributes
    , ceUsername
    , ceExternalTableDefinition
    , ceKMSKeyId
    , ceMongoDBSettings
    , ceSSLMode
    , cePassword
    , ceDatabaseName
    , ceS3Settings
    , ceDynamoDBSettings
    , ceTags
    , cePort
    , ceEndpointIdentifier
    , ceEndpointType
    , ceEngineName

    -- * Destructuring the Response
    , createEndpointResponse
    , CreateEndpointResponse
    -- * Response Lenses
    , cersEndpoint
    , cersResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { _ceServerName                :: !(Maybe Text)
  , _ceCertificateARN            :: !(Maybe Text)
  , _ceServiceAccessRoleARN      :: !(Maybe Text)
  , _ceExtraConnectionAttributes :: !(Maybe Text)
  , _ceUsername                  :: !(Maybe Text)
  , _ceExternalTableDefinition   :: !(Maybe Text)
  , _ceKMSKeyId                  :: !(Maybe Text)
  , _ceMongoDBSettings           :: !(Maybe MongoDBSettings)
  , _ceSSLMode                   :: !(Maybe DmsSSLModeValue)
  , _cePassword                  :: !(Maybe (Sensitive Text))
  , _ceDatabaseName              :: !(Maybe Text)
  , _ceS3Settings                :: !(Maybe S3Settings)
  , _ceDynamoDBSettings          :: !(Maybe DynamoDBSettings)
  , _ceTags                      :: !(Maybe [Tag])
  , _cePort                      :: !(Maybe Int)
  , _ceEndpointIdentifier        :: !Text
  , _ceEndpointType              :: !ReplicationEndpointTypeValue
  , _ceEngineName                :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceServerName' - The name of the server where the endpoint database resides.
--
-- * 'ceCertificateARN' - The Amazon Resource Name (ARN) for the certificate.
--
-- * 'ceServiceAccessRoleARN' - The Amazon Resource Name (ARN) for the service access role you want to use to create the endpoint.
--
-- * 'ceExtraConnectionAttributes' - Additional attributes associated with the connection.
--
-- * 'ceUsername' - The user name to be used to login to the endpoint database.
--
-- * 'ceExternalTableDefinition' - The external table definition.
--
-- * 'ceKMSKeyId' - The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'ceMongoDBSettings' - Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the __Configuration Properties When Using MongoDB as a Source for AWS Database Migration Service__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using Amazon S3 as a Target for AWS Database Migration Service> .
--
-- * 'ceSSLMode' - The SSL mode to use for the SSL connection. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
--
-- * 'cePassword' - The password to be used to login to the endpoint database.
--
-- * 'ceDatabaseName' - The name of the endpoint database.
--
-- * 'ceS3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see the __Extra Connection Attributes__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html Using Amazon S3 as a Target for AWS Database Migration Service> .
--
-- * 'ceDynamoDBSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For more information about the available settings, see the __Using Object Mapping to Migrate Data to DynamoDB__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using an Amazon DynamoDB Database as a Target for AWS Database Migration Service> .
--
-- * 'ceTags' - Tags to be added to the endpoint.
--
-- * 'cePort' - The port used by the endpoint database.
--
-- * 'ceEndpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
--
-- * 'ceEndpointType' - The type of endpoint.
--
-- * 'ceEngineName' - The type of engine for the endpoint. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, dynamodb, mongodb, and sqlserver.
createEndpoint
    :: Text -- ^ 'ceEndpointIdentifier'
    -> ReplicationEndpointTypeValue -- ^ 'ceEndpointType'
    -> Text -- ^ 'ceEngineName'
    -> CreateEndpoint
createEndpoint pEndpointIdentifier_ pEndpointType_ pEngineName_ =
  CreateEndpoint'
    { _ceServerName = Nothing
    , _ceCertificateARN = Nothing
    , _ceServiceAccessRoleARN = Nothing
    , _ceExtraConnectionAttributes = Nothing
    , _ceUsername = Nothing
    , _ceExternalTableDefinition = Nothing
    , _ceKMSKeyId = Nothing
    , _ceMongoDBSettings = Nothing
    , _ceSSLMode = Nothing
    , _cePassword = Nothing
    , _ceDatabaseName = Nothing
    , _ceS3Settings = Nothing
    , _ceDynamoDBSettings = Nothing
    , _ceTags = Nothing
    , _cePort = Nothing
    , _ceEndpointIdentifier = pEndpointIdentifier_
    , _ceEndpointType = pEndpointType_
    , _ceEngineName = pEngineName_
    }


-- | The name of the server where the endpoint database resides.
ceServerName :: Lens' CreateEndpoint (Maybe Text)
ceServerName = lens _ceServerName (\ s a -> s{_ceServerName = a})

-- | The Amazon Resource Name (ARN) for the certificate.
ceCertificateARN :: Lens' CreateEndpoint (Maybe Text)
ceCertificateARN = lens _ceCertificateARN (\ s a -> s{_ceCertificateARN = a})

-- | The Amazon Resource Name (ARN) for the service access role you want to use to create the endpoint.
ceServiceAccessRoleARN :: Lens' CreateEndpoint (Maybe Text)
ceServiceAccessRoleARN = lens _ceServiceAccessRoleARN (\ s a -> s{_ceServiceAccessRoleARN = a})

-- | Additional attributes associated with the connection.
ceExtraConnectionAttributes :: Lens' CreateEndpoint (Maybe Text)
ceExtraConnectionAttributes = lens _ceExtraConnectionAttributes (\ s a -> s{_ceExtraConnectionAttributes = a})

-- | The user name to be used to login to the endpoint database.
ceUsername :: Lens' CreateEndpoint (Maybe Text)
ceUsername = lens _ceUsername (\ s a -> s{_ceUsername = a})

-- | The external table definition.
ceExternalTableDefinition :: Lens' CreateEndpoint (Maybe Text)
ceExternalTableDefinition = lens _ceExternalTableDefinition (\ s a -> s{_ceExternalTableDefinition = a})

-- | The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
ceKMSKeyId :: Lens' CreateEndpoint (Maybe Text)
ceKMSKeyId = lens _ceKMSKeyId (\ s a -> s{_ceKMSKeyId = a})

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the __Configuration Properties When Using MongoDB as a Source for AWS Database Migration Service__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using Amazon S3 as a Target for AWS Database Migration Service> .
ceMongoDBSettings :: Lens' CreateEndpoint (Maybe MongoDBSettings)
ceMongoDBSettings = lens _ceMongoDBSettings (\ s a -> s{_ceMongoDBSettings = a})

-- | The SSL mode to use for the SSL connection. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
ceSSLMode :: Lens' CreateEndpoint (Maybe DmsSSLModeValue)
ceSSLMode = lens _ceSSLMode (\ s a -> s{_ceSSLMode = a})

-- | The password to be used to login to the endpoint database.
cePassword :: Lens' CreateEndpoint (Maybe Text)
cePassword = lens _cePassword (\ s a -> s{_cePassword = a}) . mapping _Sensitive

-- | The name of the endpoint database.
ceDatabaseName :: Lens' CreateEndpoint (Maybe Text)
ceDatabaseName = lens _ceDatabaseName (\ s a -> s{_ceDatabaseName = a})

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see the __Extra Connection Attributes__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html Using Amazon S3 as a Target for AWS Database Migration Service> .
ceS3Settings :: Lens' CreateEndpoint (Maybe S3Settings)
ceS3Settings = lens _ceS3Settings (\ s a -> s{_ceS3Settings = a})

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For more information about the available settings, see the __Using Object Mapping to Migrate Data to DynamoDB__ section at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using an Amazon DynamoDB Database as a Target for AWS Database Migration Service> .
ceDynamoDBSettings :: Lens' CreateEndpoint (Maybe DynamoDBSettings)
ceDynamoDBSettings = lens _ceDynamoDBSettings (\ s a -> s{_ceDynamoDBSettings = a})

-- | Tags to be added to the endpoint.
ceTags :: Lens' CreateEndpoint [Tag]
ceTags = lens _ceTags (\ s a -> s{_ceTags = a}) . _Default . _Coerce

-- | The port used by the endpoint database.
cePort :: Lens' CreateEndpoint (Maybe Int)
cePort = lens _cePort (\ s a -> s{_cePort = a})

-- | The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
ceEndpointIdentifier :: Lens' CreateEndpoint Text
ceEndpointIdentifier = lens _ceEndpointIdentifier (\ s a -> s{_ceEndpointIdentifier = a})

-- | The type of endpoint.
ceEndpointType :: Lens' CreateEndpoint ReplicationEndpointTypeValue
ceEndpointType = lens _ceEndpointType (\ s a -> s{_ceEndpointType = a})

-- | The type of engine for the endpoint. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, dynamodb, mongodb, and sqlserver.
ceEngineName :: Lens' CreateEndpoint Text
ceEngineName = lens _ceEngineName (\ s a -> s{_ceEngineName = a})

instance AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' <$>
                   (x .?> "Endpoint") <*> (pure (fromEnum s)))

instance Hashable CreateEndpoint where

instance NFData CreateEndpoint where

instance ToHeaders CreateEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEndpoint where
        toJSON CreateEndpoint'{..}
          = object
              (catMaybes
                 [("ServerName" .=) <$> _ceServerName,
                  ("CertificateArn" .=) <$> _ceCertificateARN,
                  ("ServiceAccessRoleArn" .=) <$>
                    _ceServiceAccessRoleARN,
                  ("ExtraConnectionAttributes" .=) <$>
                    _ceExtraConnectionAttributes,
                  ("Username" .=) <$> _ceUsername,
                  ("ExternalTableDefinition" .=) <$>
                    _ceExternalTableDefinition,
                  ("KmsKeyId" .=) <$> _ceKMSKeyId,
                  ("MongoDbSettings" .=) <$> _ceMongoDBSettings,
                  ("SslMode" .=) <$> _ceSSLMode,
                  ("Password" .=) <$> _cePassword,
                  ("DatabaseName" .=) <$> _ceDatabaseName,
                  ("S3Settings" .=) <$> _ceS3Settings,
                  ("DynamoDbSettings" .=) <$> _ceDynamoDBSettings,
                  ("Tags" .=) <$> _ceTags, ("Port" .=) <$> _cePort,
                  Just ("EndpointIdentifier" .= _ceEndpointIdentifier),
                  Just ("EndpointType" .= _ceEndpointType),
                  Just ("EngineName" .= _ceEngineName)])

instance ToPath CreateEndpoint where
        toPath = const "/"

instance ToQuery CreateEndpoint where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'createEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { _cersEndpoint       :: !(Maybe Endpoint)
  , _cersResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersEndpoint' - The endpoint that was created.
--
-- * 'cersResponseStatus' - -- | The response status code.
createEndpointResponse
    :: Int -- ^ 'cersResponseStatus'
    -> CreateEndpointResponse
createEndpointResponse pResponseStatus_ =
  CreateEndpointResponse'
    {_cersEndpoint = Nothing, _cersResponseStatus = pResponseStatus_}


-- | The endpoint that was created.
cersEndpoint :: Lens' CreateEndpointResponse (Maybe Endpoint)
cersEndpoint = lens _cersEndpoint (\ s a -> s{_cersEndpoint = a})

-- | -- | The response status code.
cersResponseStatus :: Lens' CreateEndpointResponse Int
cersResponseStatus = lens _cersResponseStatus (\ s a -> s{_cersResponseStatus = a})

instance NFData CreateEndpointResponse where
