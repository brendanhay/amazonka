{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Product where

import Network.AWS.DMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a quota for an AWS account, for example, the number of replication instances allowed.
--
--
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { _aqMax              :: !(Maybe Integer)
  , _aqUsed             :: !(Maybe Integer)
  , _aqAccountQuotaName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqMax' - The maximum allowed value for the quota.
--
-- * 'aqUsed' - The amount currently used toward the quota maximum.
--
-- * 'aqAccountQuotaName' - The name of the AWS DMS quota for this AWS account.
accountQuota
    :: AccountQuota
accountQuota =
  AccountQuota'
    {_aqMax = Nothing, _aqUsed = Nothing, _aqAccountQuotaName = Nothing}


-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\ s a -> s{_aqMax = a})

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\ s a -> s{_aqUsed = a})

-- | The name of the AWS DMS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\ s a -> s{_aqAccountQuotaName = a})

instance FromJSON AccountQuota where
        parseJSON
          = withObject "AccountQuota"
              (\ x ->
                 AccountQuota' <$>
                   (x .:? "Max") <*> (x .:? "Used") <*>
                     (x .:? "AccountQuotaName"))

instance Hashable AccountQuota where

instance NFData AccountQuota where

-- |
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { _azName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the availability zone.
availabilityZone
    :: AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}


-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a})

instance FromJSON AvailabilityZone where
        parseJSON
          = withObject "AvailabilityZone"
              (\ x -> AvailabilityZone' <$> (x .:? "Name"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateOwner        :: !(Maybe Text)
  , _cSigningAlgorithm        :: !(Maybe Text)
  , _cValidFromDate           :: !(Maybe POSIX)
  , _cCertificatePem          :: !(Maybe Text)
  , _cCertificateARN          :: !(Maybe Text)
  , _cCertificateCreationDate :: !(Maybe POSIX)
  , _cCertificateIdentifier   :: !(Maybe Text)
  , _cCertificateWallet       :: !(Maybe Base64)
  , _cKeyLength               :: !(Maybe Int)
  , _cValidToDate             :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateOwner' - The owner of the certificate.
--
-- * 'cSigningAlgorithm' - The signing algorithm for the certificate.
--
-- * 'cValidFromDate' - The beginning date that the certificate is valid.
--
-- * 'cCertificatePem' - The contents of the .pem X.509 certificate file for the certificate.
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) for the certificate.
--
-- * 'cCertificateCreationDate' - The date that the certificate was created.
--
-- * 'cCertificateIdentifier' - The customer-assigned name of the certificate. Valid characters are A-z and 0-9.
--
-- * 'cCertificateWallet' - The location of the imported Oracle Wallet certificate for use with SSL.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'cKeyLength' - The key length of the cryptographic algorithm being used.
--
-- * 'cValidToDate' - The final date that the certificate is valid.
certificate
    :: Certificate
certificate =
  Certificate'
    { _cCertificateOwner = Nothing
    , _cSigningAlgorithm = Nothing
    , _cValidFromDate = Nothing
    , _cCertificatePem = Nothing
    , _cCertificateARN = Nothing
    , _cCertificateCreationDate = Nothing
    , _cCertificateIdentifier = Nothing
    , _cCertificateWallet = Nothing
    , _cKeyLength = Nothing
    , _cValidToDate = Nothing
    }


-- | The owner of the certificate.
cCertificateOwner :: Lens' Certificate (Maybe Text)
cCertificateOwner = lens _cCertificateOwner (\ s a -> s{_cCertificateOwner = a})

-- | The signing algorithm for the certificate.
cSigningAlgorithm :: Lens' Certificate (Maybe Text)
cSigningAlgorithm = lens _cSigningAlgorithm (\ s a -> s{_cSigningAlgorithm = a})

-- | The beginning date that the certificate is valid.
cValidFromDate :: Lens' Certificate (Maybe UTCTime)
cValidFromDate = lens _cValidFromDate (\ s a -> s{_cValidFromDate = a}) . mapping _Time

-- | The contents of the .pem X.509 certificate file for the certificate.
cCertificatePem :: Lens' Certificate (Maybe Text)
cCertificatePem = lens _cCertificatePem (\ s a -> s{_cCertificatePem = a})

-- | The Amazon Resource Name (ARN) for the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | The date that the certificate was created.
cCertificateCreationDate :: Lens' Certificate (Maybe UTCTime)
cCertificateCreationDate = lens _cCertificateCreationDate (\ s a -> s{_cCertificateCreationDate = a}) . mapping _Time

-- | The customer-assigned name of the certificate. Valid characters are A-z and 0-9.
cCertificateIdentifier :: Lens' Certificate (Maybe Text)
cCertificateIdentifier = lens _cCertificateIdentifier (\ s a -> s{_cCertificateIdentifier = a})

-- | The location of the imported Oracle Wallet certificate for use with SSL.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
cCertificateWallet :: Lens' Certificate (Maybe ByteString)
cCertificateWallet = lens _cCertificateWallet (\ s a -> s{_cCertificateWallet = a}) . mapping _Base64

-- | The key length of the cryptographic algorithm being used.
cKeyLength :: Lens' Certificate (Maybe Int)
cKeyLength = lens _cKeyLength (\ s a -> s{_cKeyLength = a})

-- | The final date that the certificate is valid.
cValidToDate :: Lens' Certificate (Maybe UTCTime)
cValidToDate = lens _cValidToDate (\ s a -> s{_cValidToDate = a}) . mapping _Time

instance FromJSON Certificate where
        parseJSON
          = withObject "Certificate"
              (\ x ->
                 Certificate' <$>
                   (x .:? "CertificateOwner") <*>
                     (x .:? "SigningAlgorithm")
                     <*> (x .:? "ValidFromDate")
                     <*> (x .:? "CertificatePem")
                     <*> (x .:? "CertificateArn")
                     <*> (x .:? "CertificateCreationDate")
                     <*> (x .:? "CertificateIdentifier")
                     <*> (x .:? "CertificateWallet")
                     <*> (x .:? "KeyLength")
                     <*> (x .:? "ValidToDate"))

instance Hashable Certificate where

instance NFData Certificate where

-- |
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _cStatus                        :: !(Maybe Text)
  , _cReplicationInstanceARN        :: !(Maybe Text)
  , _cEndpointIdentifier            :: !(Maybe Text)
  , _cReplicationInstanceIdentifier :: !(Maybe Text)
  , _cEndpointARN                   :: !(Maybe Text)
  , _cLastFailureMessage            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The connection status.
--
-- * 'cReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'cEndpointIdentifier' - The identifier of the endpoint. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
--
-- * 'cReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
--
-- * 'cEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'cLastFailureMessage' - The error message when the connection last failed.
connection
    :: Connection
connection =
  Connection'
    { _cStatus = Nothing
    , _cReplicationInstanceARN = Nothing
    , _cEndpointIdentifier = Nothing
    , _cReplicationInstanceIdentifier = Nothing
    , _cEndpointARN = Nothing
    , _cLastFailureMessage = Nothing
    }


-- | The connection status.
cStatus :: Lens' Connection (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
cReplicationInstanceARN :: Lens' Connection (Maybe Text)
cReplicationInstanceARN = lens _cReplicationInstanceARN (\ s a -> s{_cReplicationInstanceARN = a})

-- | The identifier of the endpoint. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
cEndpointIdentifier :: Lens' Connection (Maybe Text)
cEndpointIdentifier = lens _cEndpointIdentifier (\ s a -> s{_cEndpointIdentifier = a})

-- | The replication instance identifier. This parameter is stored as a lowercase string.
cReplicationInstanceIdentifier :: Lens' Connection (Maybe Text)
cReplicationInstanceIdentifier = lens _cReplicationInstanceIdentifier (\ s a -> s{_cReplicationInstanceIdentifier = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
cEndpointARN :: Lens' Connection (Maybe Text)
cEndpointARN = lens _cEndpointARN (\ s a -> s{_cEndpointARN = a})

-- | The error message when the connection last failed.
cLastFailureMessage :: Lens' Connection (Maybe Text)
cLastFailureMessage = lens _cLastFailureMessage (\ s a -> s{_cLastFailureMessage = a})

instance FromJSON Connection where
        parseJSON
          = withObject "Connection"
              (\ x ->
                 Connection' <$>
                   (x .:? "Status") <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "EndpointIdentifier")
                     <*> (x .:? "ReplicationInstanceIdentifier")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable Connection where

instance NFData Connection where

-- |
--
--
--
-- /See:/ 'dynamoDBSettings' smart constructor.
newtype DynamoDBSettings = DynamoDBSettings'
  { _ddsServiceAccessRoleARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DynamoDBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
dynamoDBSettings
    :: Text -- ^ 'ddsServiceAccessRoleARN'
    -> DynamoDBSettings
dynamoDBSettings pServiceAccessRoleARN_ =
  DynamoDBSettings' {_ddsServiceAccessRoleARN = pServiceAccessRoleARN_}


-- | The Amazon Resource Name (ARN) used by the service access IAM role.
ddsServiceAccessRoleARN :: Lens' DynamoDBSettings Text
ddsServiceAccessRoleARN = lens _ddsServiceAccessRoleARN (\ s a -> s{_ddsServiceAccessRoleARN = a})

instance FromJSON DynamoDBSettings where
        parseJSON
          = withObject "DynamoDBSettings"
              (\ x ->
                 DynamoDBSettings' <$> (x .: "ServiceAccessRoleArn"))

instance Hashable DynamoDBSettings where

instance NFData DynamoDBSettings where

instance ToJSON DynamoDBSettings where
        toJSON DynamoDBSettings'{..}
          = object
              (catMaybes
                 [Just
                    ("ServiceAccessRoleArn" .=
                       _ddsServiceAccessRoleARN)])

-- |
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eStatus                    :: !(Maybe Text)
  , _eServerName                :: !(Maybe Text)
  , _eCertificateARN            :: !(Maybe Text)
  , _eServiceAccessRoleARN      :: !(Maybe Text)
  , _eEngineDisplayName         :: !(Maybe Text)
  , _eExtraConnectionAttributes :: !(Maybe Text)
  , _eEndpointType              :: !(Maybe ReplicationEndpointTypeValue)
  , _eUsername                  :: !(Maybe Text)
  , _eExternalTableDefinition   :: !(Maybe Text)
  , _eEngineName                :: !(Maybe Text)
  , _eKMSKeyId                  :: !(Maybe Text)
  , _eMongoDBSettings           :: !(Maybe MongoDBSettings)
  , _eSSLMode                   :: !(Maybe DmsSSLModeValue)
  , _eDatabaseName              :: !(Maybe Text)
  , _eS3Settings                :: !(Maybe S3Settings)
  , _eEndpointIdentifier        :: !(Maybe Text)
  , _eExternalId                :: !(Maybe Text)
  , _eDynamoDBSettings          :: !(Maybe DynamoDBSettings)
  , _eEndpointARN               :: !(Maybe Text)
  , _ePort                      :: !(Maybe Int)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus' - The status of the endpoint.
--
-- * 'eServerName' - The name of the server at the endpoint.
--
-- * 'eCertificateARN' - The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- * 'eServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- * 'eEngineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- * 'eExtraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
--
-- * 'eEndpointType' - The type of endpoint.
--
-- * 'eUsername' - The user name used to connect to the endpoint.
--
-- * 'eExternalTableDefinition' - The external table definition.
--
-- * 'eEngineName' - The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
--
-- * 'eKMSKeyId' - The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'eMongoDBSettings' - The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
--
-- * 'eSSLMode' - The SSL mode used to connect to the endpoint. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
--
-- * 'eDatabaseName' - The name of the database at the endpoint.
--
-- * 'eS3Settings' - The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
--
-- * 'eEndpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
--
-- * 'eExternalId' - Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account.
--
-- * 'eDynamoDBSettings' - The settings for the target DynamoDB database. For more information, see the @DynamoDBSettings@ structure.
--
-- * 'eEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'ePort' - The port value used to access the endpoint.
endpoint
    :: Endpoint
endpoint =
  Endpoint'
    { _eStatus = Nothing
    , _eServerName = Nothing
    , _eCertificateARN = Nothing
    , _eServiceAccessRoleARN = Nothing
    , _eEngineDisplayName = Nothing
    , _eExtraConnectionAttributes = Nothing
    , _eEndpointType = Nothing
    , _eUsername = Nothing
    , _eExternalTableDefinition = Nothing
    , _eEngineName = Nothing
    , _eKMSKeyId = Nothing
    , _eMongoDBSettings = Nothing
    , _eSSLMode = Nothing
    , _eDatabaseName = Nothing
    , _eS3Settings = Nothing
    , _eEndpointIdentifier = Nothing
    , _eExternalId = Nothing
    , _eDynamoDBSettings = Nothing
    , _eEndpointARN = Nothing
    , _ePort = Nothing
    }


-- | The status of the endpoint.
eStatus :: Lens' Endpoint (Maybe Text)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a})

-- | The name of the server at the endpoint.
eServerName :: Lens' Endpoint (Maybe Text)
eServerName = lens _eServerName (\ s a -> s{_eServerName = a})

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
eCertificateARN :: Lens' Endpoint (Maybe Text)
eCertificateARN = lens _eCertificateARN (\ s a -> s{_eCertificateARN = a})

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
eServiceAccessRoleARN :: Lens' Endpoint (Maybe Text)
eServiceAccessRoleARN = lens _eServiceAccessRoleARN (\ s a -> s{_eServiceAccessRoleARN = a})

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
eEngineDisplayName :: Lens' Endpoint (Maybe Text)
eEngineDisplayName = lens _eEngineDisplayName (\ s a -> s{_eEngineDisplayName = a})

-- | Additional connection attributes used to connect to the endpoint.
eExtraConnectionAttributes :: Lens' Endpoint (Maybe Text)
eExtraConnectionAttributes = lens _eExtraConnectionAttributes (\ s a -> s{_eExtraConnectionAttributes = a})

-- | The type of endpoint.
eEndpointType :: Lens' Endpoint (Maybe ReplicationEndpointTypeValue)
eEndpointType = lens _eEndpointType (\ s a -> s{_eEndpointType = a})

-- | The user name used to connect to the endpoint.
eUsername :: Lens' Endpoint (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a})

-- | The external table definition.
eExternalTableDefinition :: Lens' Endpoint (Maybe Text)
eExternalTableDefinition = lens _eExternalTableDefinition (\ s a -> s{_eExternalTableDefinition = a})

-- | The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
eEngineName :: Lens' Endpoint (Maybe Text)
eEngineName = lens _eEngineName (\ s a -> s{_eEngineName = a})

-- | The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
eKMSKeyId :: Lens' Endpoint (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a})

-- | The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
eMongoDBSettings :: Lens' Endpoint (Maybe MongoDBSettings)
eMongoDBSettings = lens _eMongoDBSettings (\ s a -> s{_eMongoDBSettings = a})

-- | The SSL mode used to connect to the endpoint. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
eSSLMode :: Lens' Endpoint (Maybe DmsSSLModeValue)
eSSLMode = lens _eSSLMode (\ s a -> s{_eSSLMode = a})

-- | The name of the database at the endpoint.
eDatabaseName :: Lens' Endpoint (Maybe Text)
eDatabaseName = lens _eDatabaseName (\ s a -> s{_eDatabaseName = a})

-- | The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
eS3Settings :: Lens' Endpoint (Maybe S3Settings)
eS3Settings = lens _eS3Settings (\ s a -> s{_eS3Settings = a})

-- | The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
eEndpointIdentifier :: Lens' Endpoint (Maybe Text)
eEndpointIdentifier = lens _eEndpointIdentifier (\ s a -> s{_eEndpointIdentifier = a})

-- | Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account.
eExternalId :: Lens' Endpoint (Maybe Text)
eExternalId = lens _eExternalId (\ s a -> s{_eExternalId = a})

-- | The settings for the target DynamoDB database. For more information, see the @DynamoDBSettings@ structure.
eDynamoDBSettings :: Lens' Endpoint (Maybe DynamoDBSettings)
eDynamoDBSettings = lens _eDynamoDBSettings (\ s a -> s{_eDynamoDBSettings = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\ s a -> s{_eEndpointARN = a})

-- | The port value used to access the endpoint.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x ->
                 Endpoint' <$>
                   (x .:? "Status") <*> (x .:? "ServerName") <*>
                     (x .:? "CertificateArn")
                     <*> (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "EngineDisplayName")
                     <*> (x .:? "ExtraConnectionAttributes")
                     <*> (x .:? "EndpointType")
                     <*> (x .:? "Username")
                     <*> (x .:? "ExternalTableDefinition")
                     <*> (x .:? "EngineName")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "MongoDbSettings")
                     <*> (x .:? "SslMode")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "S3Settings")
                     <*> (x .:? "EndpointIdentifier")
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "DynamoDbSettings")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "Port"))

instance Hashable Endpoint where

instance NFData Endpoint where

-- |
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType       :: !(Maybe SourceType)
  , _eSourceIdentifier :: !(Maybe Text)
  , _eDate             :: !(Maybe POSIX)
  , _eEventCategories  :: !(Maybe [Text])
  , _eMessage          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - The type of AWS DMS resource that generates events.  Valid values: replication-instance | endpoint | migration-task
--
-- * 'eSourceIdentifier' - The identifier of the event source. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it cannot end with a hyphen or contain two consecutive hyphens.  Constraints:replication instance, endpoint, migration task
--
-- * 'eDate' - The date of the event.
--
-- * 'eEventCategories' - The event categories available for the specified source type.
--
-- * 'eMessage' - The event message.
event
    :: Event
event =
  Event'
    { _eSourceType = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eEventCategories = Nothing
    , _eMessage = Nothing
    }


-- | The type of AWS DMS resource that generates events.  Valid values: replication-instance | endpoint | migration-task
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The identifier of the event source. An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it cannot end with a hyphen or contain two consecutive hyphens.  Constraints:replication instance, endpoint, migration task
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a})

-- | The date of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | The event categories available for the specified source type.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\ s a -> s{_eEventCategories = a}) . _Default . _Coerce

-- | The event message.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "SourceType") <*> (x .:? "SourceIdentifier")
                     <*> (x .:? "Date")
                     <*> (x .:? "EventCategories" .!= mempty)
                     <*> (x .:? "Message"))

instance Hashable Event where

instance NFData Event where

-- |
--
--
--
-- /See:/ 'eventCategoryGroup' smart constructor.
data EventCategoryGroup = EventCategoryGroup'
  { _ecgSourceType      :: !(Maybe Text)
  , _ecgEventCategories :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventCategoryGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecgSourceType' - The type of AWS DMS resource that generates events.  Valid values: replication-instance | replication-server | security-group | migration-task
--
-- * 'ecgEventCategories' - A list of event categories for a @SourceType@ that you want to subscribe to.
eventCategoryGroup
    :: EventCategoryGroup
eventCategoryGroup =
  EventCategoryGroup' {_ecgSourceType = Nothing, _ecgEventCategories = Nothing}


-- | The type of AWS DMS resource that generates events.  Valid values: replication-instance | replication-server | security-group | migration-task
ecgSourceType :: Lens' EventCategoryGroup (Maybe Text)
ecgSourceType = lens _ecgSourceType (\ s a -> s{_ecgSourceType = a})

-- | A list of event categories for a @SourceType@ that you want to subscribe to.
ecgEventCategories :: Lens' EventCategoryGroup [Text]
ecgEventCategories = lens _ecgEventCategories (\ s a -> s{_ecgEventCategories = a}) . _Default . _Coerce

instance FromJSON EventCategoryGroup where
        parseJSON
          = withObject "EventCategoryGroup"
              (\ x ->
                 EventCategoryGroup' <$>
                   (x .:? "SourceType") <*>
                     (x .:? "EventCategories" .!= mempty))

instance Hashable EventCategoryGroup where

instance NFData EventCategoryGroup where

-- |
--
--
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { _esStatus                   :: !(Maybe Text)
  , _esCustomerAWSId            :: !(Maybe Text)
  , _esCustSubscriptionId       :: !(Maybe Text)
  , _esSNSTopicARN              :: !(Maybe Text)
  , _esEnabled                  :: !(Maybe Bool)
  , _esSourceType               :: !(Maybe Text)
  , _esSubscriptionCreationTime :: !(Maybe Text)
  , _esEventCategoriesList      :: !(Maybe [Text])
  , _esSourceIdsList            :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStatus' - The status of the AWS DMS event notification subscription. Constraints: Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- * 'esCustomerAWSId' - The AWS customer account associated with the AWS DMS event notification subscription.
--
-- * 'esCustSubscriptionId' - The AWS DMS event notification subscription Id.
--
-- * 'esSNSTopicARN' - The topic ARN of the AWS DMS event notification subscription.
--
-- * 'esEnabled' - Boolean value that indicates if the event subscription is enabled.
--
-- * 'esSourceType' - The type of AWS DMS resource that generates events.  Valid values: replication-instance | replication-server | security-group | migration-task
--
-- * 'esSubscriptionCreationTime' - The time the RDS event notification subscription was created.
--
-- * 'esEventCategoriesList' - A lists of event categories.
--
-- * 'esSourceIdsList' - A list of source Ids for the event subscription.
eventSubscription
    :: EventSubscription
eventSubscription =
  EventSubscription'
    { _esStatus = Nothing
    , _esCustomerAWSId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSNSTopicARN = Nothing
    , _esEnabled = Nothing
    , _esSourceType = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esEventCategoriesList = Nothing
    , _esSourceIdsList = Nothing
    }


-- | The status of the AWS DMS event notification subscription. Constraints: Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist The status "no-permission" indicates that AWS DMS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a})

-- | The AWS customer account associated with the AWS DMS event notification subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a})

-- | The AWS DMS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a})

-- | The topic ARN of the AWS DMS event notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a})

-- | Boolean value that indicates if the event subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a})

-- | The type of AWS DMS resource that generates events.  Valid values: replication-instance | replication-server | security-group | migration-task
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a})

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a})

-- | A lists of event categories.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default . _Coerce

-- | A list of source Ids for the event subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default . _Coerce

instance FromJSON EventSubscription where
        parseJSON
          = withObject "EventSubscription"
              (\ x ->
                 EventSubscription' <$>
                   (x .:? "Status") <*> (x .:? "CustomerAwsId") <*>
                     (x .:? "CustSubscriptionId")
                     <*> (x .:? "SnsTopicArn")
                     <*> (x .:? "Enabled")
                     <*> (x .:? "SourceType")
                     <*> (x .:? "SubscriptionCreationTime")
                     <*> (x .:? "EventCategoriesList" .!= mempty)
                     <*> (x .:? "SourceIdsList" .!= mempty))

instance Hashable EventSubscription where

instance NFData EventSubscription where

-- |
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fName   :: !Text
  , _fValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter.
--
-- * 'fValues' - The filter value.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ = Filter' {_fName = pName_, _fValues = mempty}


-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | The filter value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _fName),
                  Just ("Values" .= _fValues)])

-- |
--
--
--
-- /See:/ 'mongoDBSettings' smart constructor.
data MongoDBSettings = MongoDBSettings'
  { _mdsServerName        :: !(Maybe Text)
  , _mdsAuthMechanism     :: !(Maybe AuthMechanismValue)
  , _mdsUsername          :: !(Maybe Text)
  , _mdsKMSKeyId          :: !(Maybe Text)
  , _mdsPassword          :: !(Maybe (Sensitive Text))
  , _mdsNestingLevel      :: !(Maybe NestingLevelValue)
  , _mdsDatabaseName      :: !(Maybe Text)
  , _mdsDocsToInvestigate :: !(Maybe Text)
  , _mdsAuthSource        :: !(Maybe Text)
  , _mdsExtractDocId      :: !(Maybe Text)
  , _mdsAuthType          :: !(Maybe AuthTypeValue)
  , _mdsPort              :: !(Maybe Int)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MongoDBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsServerName' - The name of the server on the MongoDB source endpoint.
--
-- * 'mdsAuthMechanism' - The authentication mechanism you use to access the MongoDB source endpoint. Valid values: DEFAULT, MONGODB_CR, SCRAM_SHA_1  DEFAULT – For MongoDB version 2.x, use MONGODB_CR. For MongoDB version 3.x, use SCRAM_SHA_1. This attribute is not used when authType=No.
--
-- * 'mdsUsername' - The user name you use to access the MongoDB source endpoint.
--
-- * 'mdsKMSKeyId' - The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'mdsPassword' - The password for the user account you use to access the MongoDB source endpoint.
--
-- * 'mdsNestingLevel' - Specifies either document or table mode.  Valid values: NONE, ONE Default value is NONE. Specify NONE to use document mode. Specify ONE to use table mode.
--
-- * 'mdsDatabaseName' - The database name on the MongoDB source endpoint.
--
-- * 'mdsDocsToInvestigate' - Indicates the number of documents to preview to determine the document organization. Use this attribute when @NestingLevel@ is set to ONE.  Must be a positive value greater than 0. Default value is 1000.
--
-- * 'mdsAuthSource' - The MongoDB database name. This attribute is not used when @authType=NO@ .  The default is admin.
--
-- * 'mdsExtractDocId' - Specifies the document ID. Use this attribute when @NestingLevel@ is set to NONE.  Default value is false.
--
-- * 'mdsAuthType' - The authentication type you use to access the MongoDB source endpoint. Valid values: NO, PASSWORD  When NO is selected, user name and password parameters are not used and can be empty.
--
-- * 'mdsPort' - The port value for the MongoDB source endpoint.
mongoDBSettings
    :: MongoDBSettings
mongoDBSettings =
  MongoDBSettings'
    { _mdsServerName = Nothing
    , _mdsAuthMechanism = Nothing
    , _mdsUsername = Nothing
    , _mdsKMSKeyId = Nothing
    , _mdsPassword = Nothing
    , _mdsNestingLevel = Nothing
    , _mdsDatabaseName = Nothing
    , _mdsDocsToInvestigate = Nothing
    , _mdsAuthSource = Nothing
    , _mdsExtractDocId = Nothing
    , _mdsAuthType = Nothing
    , _mdsPort = Nothing
    }


-- | The name of the server on the MongoDB source endpoint.
mdsServerName :: Lens' MongoDBSettings (Maybe Text)
mdsServerName = lens _mdsServerName (\ s a -> s{_mdsServerName = a})

-- | The authentication mechanism you use to access the MongoDB source endpoint. Valid values: DEFAULT, MONGODB_CR, SCRAM_SHA_1  DEFAULT – For MongoDB version 2.x, use MONGODB_CR. For MongoDB version 3.x, use SCRAM_SHA_1. This attribute is not used when authType=No.
mdsAuthMechanism :: Lens' MongoDBSettings (Maybe AuthMechanismValue)
mdsAuthMechanism = lens _mdsAuthMechanism (\ s a -> s{_mdsAuthMechanism = a})

-- | The user name you use to access the MongoDB source endpoint.
mdsUsername :: Lens' MongoDBSettings (Maybe Text)
mdsUsername = lens _mdsUsername (\ s a -> s{_mdsUsername = a})

-- | The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
mdsKMSKeyId :: Lens' MongoDBSettings (Maybe Text)
mdsKMSKeyId = lens _mdsKMSKeyId (\ s a -> s{_mdsKMSKeyId = a})

-- | The password for the user account you use to access the MongoDB source endpoint.
mdsPassword :: Lens' MongoDBSettings (Maybe Text)
mdsPassword = lens _mdsPassword (\ s a -> s{_mdsPassword = a}) . mapping _Sensitive

-- | Specifies either document or table mode.  Valid values: NONE, ONE Default value is NONE. Specify NONE to use document mode. Specify ONE to use table mode.
mdsNestingLevel :: Lens' MongoDBSettings (Maybe NestingLevelValue)
mdsNestingLevel = lens _mdsNestingLevel (\ s a -> s{_mdsNestingLevel = a})

-- | The database name on the MongoDB source endpoint.
mdsDatabaseName :: Lens' MongoDBSettings (Maybe Text)
mdsDatabaseName = lens _mdsDatabaseName (\ s a -> s{_mdsDatabaseName = a})

-- | Indicates the number of documents to preview to determine the document organization. Use this attribute when @NestingLevel@ is set to ONE.  Must be a positive value greater than 0. Default value is 1000.
mdsDocsToInvestigate :: Lens' MongoDBSettings (Maybe Text)
mdsDocsToInvestigate = lens _mdsDocsToInvestigate (\ s a -> s{_mdsDocsToInvestigate = a})

-- | The MongoDB database name. This attribute is not used when @authType=NO@ .  The default is admin.
mdsAuthSource :: Lens' MongoDBSettings (Maybe Text)
mdsAuthSource = lens _mdsAuthSource (\ s a -> s{_mdsAuthSource = a})

-- | Specifies the document ID. Use this attribute when @NestingLevel@ is set to NONE.  Default value is false.
mdsExtractDocId :: Lens' MongoDBSettings (Maybe Text)
mdsExtractDocId = lens _mdsExtractDocId (\ s a -> s{_mdsExtractDocId = a})

-- | The authentication type you use to access the MongoDB source endpoint. Valid values: NO, PASSWORD  When NO is selected, user name and password parameters are not used and can be empty.
mdsAuthType :: Lens' MongoDBSettings (Maybe AuthTypeValue)
mdsAuthType = lens _mdsAuthType (\ s a -> s{_mdsAuthType = a})

-- | The port value for the MongoDB source endpoint.
mdsPort :: Lens' MongoDBSettings (Maybe Int)
mdsPort = lens _mdsPort (\ s a -> s{_mdsPort = a})

instance FromJSON MongoDBSettings where
        parseJSON
          = withObject "MongoDBSettings"
              (\ x ->
                 MongoDBSettings' <$>
                   (x .:? "ServerName") <*> (x .:? "AuthMechanism") <*>
                     (x .:? "Username")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "Password")
                     <*> (x .:? "NestingLevel")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "DocsToInvestigate")
                     <*> (x .:? "AuthSource")
                     <*> (x .:? "ExtractDocId")
                     <*> (x .:? "AuthType")
                     <*> (x .:? "Port"))

instance Hashable MongoDBSettings where

instance NFData MongoDBSettings where

instance ToJSON MongoDBSettings where
        toJSON MongoDBSettings'{..}
          = object
              (catMaybes
                 [("ServerName" .=) <$> _mdsServerName,
                  ("AuthMechanism" .=) <$> _mdsAuthMechanism,
                  ("Username" .=) <$> _mdsUsername,
                  ("KmsKeyId" .=) <$> _mdsKMSKeyId,
                  ("Password" .=) <$> _mdsPassword,
                  ("NestingLevel" .=) <$> _mdsNestingLevel,
                  ("DatabaseName" .=) <$> _mdsDatabaseName,
                  ("DocsToInvestigate" .=) <$> _mdsDocsToInvestigate,
                  ("AuthSource" .=) <$> _mdsAuthSource,
                  ("ExtractDocId" .=) <$> _mdsExtractDocId,
                  ("AuthType" .=) <$> _mdsAuthType,
                  ("Port" .=) <$> _mdsPort])

-- |
--
--
--
-- /See:/ 'orderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
  { _oriEngineVersion            :: !(Maybe Text)
  , _oriMinAllocatedStorage      :: !(Maybe Int)
  , _oriIncludedAllocatedStorage :: !(Maybe Int)
  , _oriMaxAllocatedStorage      :: !(Maybe Int)
  , _oriReplicationInstanceClass :: !(Maybe Text)
  , _oriDefaultAllocatedStorage  :: !(Maybe Int)
  , _oriStorageType              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrderableReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oriEngineVersion' - The version of the replication engine.
--
-- * 'oriMinAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- * 'oriIncludedAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'oriMaxAllocatedStorage' - The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- * 'oriReplicationInstanceClass' - The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
--
-- * 'oriDefaultAllocatedStorage' - The default amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'oriStorageType' - The type of storage used by the replication instance.
orderableReplicationInstance
    :: OrderableReplicationInstance
orderableReplicationInstance =
  OrderableReplicationInstance'
    { _oriEngineVersion = Nothing
    , _oriMinAllocatedStorage = Nothing
    , _oriIncludedAllocatedStorage = Nothing
    , _oriMaxAllocatedStorage = Nothing
    , _oriReplicationInstanceClass = Nothing
    , _oriDefaultAllocatedStorage = Nothing
    , _oriStorageType = Nothing
    }


-- | The version of the replication engine.
oriEngineVersion :: Lens' OrderableReplicationInstance (Maybe Text)
oriEngineVersion = lens _oriEngineVersion (\ s a -> s{_oriEngineVersion = a})

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMinAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMinAllocatedStorage = lens _oriMinAllocatedStorage (\ s a -> s{_oriMinAllocatedStorage = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
oriIncludedAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriIncludedAllocatedStorage = lens _oriIncludedAllocatedStorage (\ s a -> s{_oriIncludedAllocatedStorage = a})

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMaxAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMaxAllocatedStorage = lens _oriMaxAllocatedStorage (\ s a -> s{_oriMaxAllocatedStorage = a})

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
oriReplicationInstanceClass :: Lens' OrderableReplicationInstance (Maybe Text)
oriReplicationInstanceClass = lens _oriReplicationInstanceClass (\ s a -> s{_oriReplicationInstanceClass = a})

-- | The default amount of storage (in gigabytes) that is allocated for the replication instance.
oriDefaultAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriDefaultAllocatedStorage = lens _oriDefaultAllocatedStorage (\ s a -> s{_oriDefaultAllocatedStorage = a})

-- | The type of storage used by the replication instance.
oriStorageType :: Lens' OrderableReplicationInstance (Maybe Text)
oriStorageType = lens _oriStorageType (\ s a -> s{_oriStorageType = a})

instance FromJSON OrderableReplicationInstance where
        parseJSON
          = withObject "OrderableReplicationInstance"
              (\ x ->
                 OrderableReplicationInstance' <$>
                   (x .:? "EngineVersion") <*>
                     (x .:? "MinAllocatedStorage")
                     <*> (x .:? "IncludedAllocatedStorage")
                     <*> (x .:? "MaxAllocatedStorage")
                     <*> (x .:? "ReplicationInstanceClass")
                     <*> (x .:? "DefaultAllocatedStorage")
                     <*> (x .:? "StorageType"))

instance Hashable OrderableReplicationInstance where

instance NFData OrderableReplicationInstance where

-- |
--
--
--
-- /See:/ 'refreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { _rssStatus                 :: !(Maybe RefreshSchemasStatusTypeValue)
  , _rssLastRefreshDate        :: !(Maybe POSIX)
  , _rssReplicationInstanceARN :: !(Maybe Text)
  , _rssEndpointARN            :: !(Maybe Text)
  , _rssLastFailureMessage     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RefreshSchemasStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rssStatus' - The status of the schema.
--
-- * 'rssLastRefreshDate' - The date the schema was last refreshed.
--
-- * 'rssReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rssEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rssLastFailureMessage' - The last failure message for the schema.
refreshSchemasStatus
    :: RefreshSchemasStatus
refreshSchemasStatus =
  RefreshSchemasStatus'
    { _rssStatus = Nothing
    , _rssLastRefreshDate = Nothing
    , _rssReplicationInstanceARN = Nothing
    , _rssEndpointARN = Nothing
    , _rssLastFailureMessage = Nothing
    }


-- | The status of the schema.
rssStatus :: Lens' RefreshSchemasStatus (Maybe RefreshSchemasStatusTypeValue)
rssStatus = lens _rssStatus (\ s a -> s{_rssStatus = a})

-- | The date the schema was last refreshed.
rssLastRefreshDate :: Lens' RefreshSchemasStatus (Maybe UTCTime)
rssLastRefreshDate = lens _rssLastRefreshDate (\ s a -> s{_rssLastRefreshDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication instance.
rssReplicationInstanceARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssReplicationInstanceARN = lens _rssReplicationInstanceARN (\ s a -> s{_rssReplicationInstanceARN = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rssEndpointARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssEndpointARN = lens _rssEndpointARN (\ s a -> s{_rssEndpointARN = a})

-- | The last failure message for the schema.
rssLastFailureMessage :: Lens' RefreshSchemasStatus (Maybe Text)
rssLastFailureMessage = lens _rssLastFailureMessage (\ s a -> s{_rssLastFailureMessage = a})

instance FromJSON RefreshSchemasStatus where
        parseJSON
          = withObject "RefreshSchemasStatus"
              (\ x ->
                 RefreshSchemasStatus' <$>
                   (x .:? "Status") <*> (x .:? "LastRefreshDate") <*>
                     (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable RefreshSchemasStatus where

instance NFData RefreshSchemasStatus where

-- |
--
--
--
-- /See:/ 'replicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { _riEngineVersion :: !(Maybe Text)
  , _riPubliclyAccessible :: !(Maybe Bool)
  , _riAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _riReplicationInstancePublicIPAddresses :: !(Maybe [Text])
  , _riReplicationSubnetGroup :: !(Maybe ReplicationSubnetGroup)
  , _riInstanceCreateTime :: !(Maybe POSIX)
  , _riFreeUntil :: !(Maybe POSIX)
  , _riReplicationInstanceStatus :: !(Maybe Text)
  , _riReplicationInstancePrivateIPAddresses :: !(Maybe [Text])
  , _riPreferredMaintenanceWindow :: !(Maybe Text)
  , _riReplicationInstancePrivateIPAddress :: !(Maybe Text)
  , _riKMSKeyId :: !(Maybe Text)
  , _riAvailabilityZone :: !(Maybe Text)
  , _riVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership])
  , _riMultiAZ :: !(Maybe Bool)
  , _riSecondaryAvailabilityZone :: !(Maybe Text)
  , _riReplicationInstanceARN :: !(Maybe Text)
  , _riAllocatedStorage :: !(Maybe Int)
  , _riReplicationInstancePublicIPAddress :: !(Maybe Text)
  , _riReplicationInstanceClass :: !(Maybe Text)
  , _riReplicationInstanceIdentifier :: !(Maybe Text)
  , _riPendingModifiedValues :: !(Maybe ReplicationPendingModifiedValues)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riEngineVersion' - The engine version number of the replication instance.
--
-- * 'riPubliclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- * 'riAutoMinorVersionUpgrade' - Boolean value indicating if minor version upgrades will be automatically applied to the instance.
--
-- * 'riReplicationInstancePublicIPAddresses' - The public IP address of the replication instance.
--
-- * 'riReplicationSubnetGroup' - The subnet group for the replication instance.
--
-- * 'riInstanceCreateTime' - The time the replication instance was created.
--
-- * 'riFreeUntil' - The expiration date of the free replication instance that is part of the Free DMS program.
--
-- * 'riReplicationInstanceStatus' - The status of the replication instance.
--
-- * 'riReplicationInstancePrivateIPAddresses' - The private IP address of the replication instance.
--
-- * 'riPreferredMaintenanceWindow' - The maintenance window times for the replication instance.
--
-- * 'riReplicationInstancePrivateIPAddress' - The private IP address of the replication instance.
--
-- * 'riKMSKeyId' - The KMS key identifier that is used to encrypt the content on the replication instance. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'riAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'riVPCSecurityGroups' - The VPC security group for the instance.
--
-- * 'riMultiAZ' - Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'riSecondaryAvailabilityZone' - The availability zone of the standby replication instance in a Multi-AZ deployment.
--
-- * 'riReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'riAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'riReplicationInstancePublicIPAddress' - The public IP address of the replication instance.
--
-- * 'riReplicationInstanceClass' - The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
--
-- * 'riReplicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
--
-- * 'riPendingModifiedValues' - The pending modification values.
replicationInstance
    :: ReplicationInstance
replicationInstance =
  ReplicationInstance'
    { _riEngineVersion = Nothing
    , _riPubliclyAccessible = Nothing
    , _riAutoMinorVersionUpgrade = Nothing
    , _riReplicationInstancePublicIPAddresses = Nothing
    , _riReplicationSubnetGroup = Nothing
    , _riInstanceCreateTime = Nothing
    , _riFreeUntil = Nothing
    , _riReplicationInstanceStatus = Nothing
    , _riReplicationInstancePrivateIPAddresses = Nothing
    , _riPreferredMaintenanceWindow = Nothing
    , _riReplicationInstancePrivateIPAddress = Nothing
    , _riKMSKeyId = Nothing
    , _riAvailabilityZone = Nothing
    , _riVPCSecurityGroups = Nothing
    , _riMultiAZ = Nothing
    , _riSecondaryAvailabilityZone = Nothing
    , _riReplicationInstanceARN = Nothing
    , _riAllocatedStorage = Nothing
    , _riReplicationInstancePublicIPAddress = Nothing
    , _riReplicationInstanceClass = Nothing
    , _riReplicationInstanceIdentifier = Nothing
    , _riPendingModifiedValues = Nothing
    }


-- | The engine version number of the replication instance.
riEngineVersion :: Lens' ReplicationInstance (Maybe Text)
riEngineVersion = lens _riEngineVersion (\ s a -> s{_riEngineVersion = a})

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
riPubliclyAccessible :: Lens' ReplicationInstance (Maybe Bool)
riPubliclyAccessible = lens _riPubliclyAccessible (\ s a -> s{_riPubliclyAccessible = a})

-- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
riAutoMinorVersionUpgrade :: Lens' ReplicationInstance (Maybe Bool)
riAutoMinorVersionUpgrade = lens _riAutoMinorVersionUpgrade (\ s a -> s{_riAutoMinorVersionUpgrade = a})

-- | The public IP address of the replication instance.
riReplicationInstancePublicIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePublicIPAddresses = lens _riReplicationInstancePublicIPAddresses (\ s a -> s{_riReplicationInstancePublicIPAddresses = a}) . _Default . _Coerce

-- | The subnet group for the replication instance.
riReplicationSubnetGroup :: Lens' ReplicationInstance (Maybe ReplicationSubnetGroup)
riReplicationSubnetGroup = lens _riReplicationSubnetGroup (\ s a -> s{_riReplicationSubnetGroup = a})

-- | The time the replication instance was created.
riInstanceCreateTime :: Lens' ReplicationInstance (Maybe UTCTime)
riInstanceCreateTime = lens _riInstanceCreateTime (\ s a -> s{_riInstanceCreateTime = a}) . mapping _Time

-- | The expiration date of the free replication instance that is part of the Free DMS program.
riFreeUntil :: Lens' ReplicationInstance (Maybe UTCTime)
riFreeUntil = lens _riFreeUntil (\ s a -> s{_riFreeUntil = a}) . mapping _Time

-- | The status of the replication instance.
riReplicationInstanceStatus :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceStatus = lens _riReplicationInstanceStatus (\ s a -> s{_riReplicationInstanceStatus = a})

-- | The private IP address of the replication instance.
riReplicationInstancePrivateIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePrivateIPAddresses = lens _riReplicationInstancePrivateIPAddresses (\ s a -> s{_riReplicationInstancePrivateIPAddresses = a}) . _Default . _Coerce

-- | The maintenance window times for the replication instance.
riPreferredMaintenanceWindow :: Lens' ReplicationInstance (Maybe Text)
riPreferredMaintenanceWindow = lens _riPreferredMaintenanceWindow (\ s a -> s{_riPreferredMaintenanceWindow = a})

-- | The private IP address of the replication instance.
riReplicationInstancePrivateIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePrivateIPAddress = lens _riReplicationInstancePrivateIPAddress (\ s a -> s{_riReplicationInstancePrivateIPAddress = a})

-- | The KMS key identifier that is used to encrypt the content on the replication instance. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
riKMSKeyId :: Lens' ReplicationInstance (Maybe Text)
riKMSKeyId = lens _riKMSKeyId (\ s a -> s{_riKMSKeyId = a})

-- | The Availability Zone for the instance.
riAvailabilityZone :: Lens' ReplicationInstance (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\ s a -> s{_riAvailabilityZone = a})

-- | The VPC security group for the instance.
riVPCSecurityGroups :: Lens' ReplicationInstance [VPCSecurityGroupMembership]
riVPCSecurityGroups = lens _riVPCSecurityGroups (\ s a -> s{_riVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
riMultiAZ :: Lens' ReplicationInstance (Maybe Bool)
riMultiAZ = lens _riMultiAZ (\ s a -> s{_riMultiAZ = a})

-- | The availability zone of the standby replication instance in a Multi-AZ deployment.
riSecondaryAvailabilityZone :: Lens' ReplicationInstance (Maybe Text)
riSecondaryAvailabilityZone = lens _riSecondaryAvailabilityZone (\ s a -> s{_riSecondaryAvailabilityZone = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
riReplicationInstanceARN :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceARN = lens _riReplicationInstanceARN (\ s a -> s{_riReplicationInstanceARN = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
riAllocatedStorage :: Lens' ReplicationInstance (Maybe Int)
riAllocatedStorage = lens _riAllocatedStorage (\ s a -> s{_riAllocatedStorage = a})

-- | The public IP address of the replication instance.
riReplicationInstancePublicIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePublicIPAddress = lens _riReplicationInstancePublicIPAddress (\ s a -> s{_riReplicationInstancePublicIPAddress = a})

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
riReplicationInstanceClass :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceClass = lens _riReplicationInstanceClass (\ s a -> s{_riReplicationInstanceClass = a})

-- | The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
riReplicationInstanceIdentifier :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceIdentifier = lens _riReplicationInstanceIdentifier (\ s a -> s{_riReplicationInstanceIdentifier = a})

-- | The pending modification values.
riPendingModifiedValues :: Lens' ReplicationInstance (Maybe ReplicationPendingModifiedValues)
riPendingModifiedValues = lens _riPendingModifiedValues (\ s a -> s{_riPendingModifiedValues = a})

instance FromJSON ReplicationInstance where
        parseJSON
          = withObject "ReplicationInstance"
              (\ x ->
                 ReplicationInstance' <$>
                   (x .:? "EngineVersion") <*>
                     (x .:? "PubliclyAccessible")
                     <*> (x .:? "AutoMinorVersionUpgrade")
                     <*>
                     (x .:? "ReplicationInstancePublicIpAddresses" .!=
                        mempty)
                     <*> (x .:? "ReplicationSubnetGroup")
                     <*> (x .:? "InstanceCreateTime")
                     <*> (x .:? "FreeUntil")
                     <*> (x .:? "ReplicationInstanceStatus")
                     <*>
                     (x .:? "ReplicationInstancePrivateIpAddresses" .!=
                        mempty)
                     <*> (x .:? "PreferredMaintenanceWindow")
                     <*> (x .:? "ReplicationInstancePrivateIpAddress")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "VpcSecurityGroups" .!= mempty)
                     <*> (x .:? "MultiAZ")
                     <*> (x .:? "SecondaryAvailabilityZone")
                     <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "AllocatedStorage")
                     <*> (x .:? "ReplicationInstancePublicIpAddress")
                     <*> (x .:? "ReplicationInstanceClass")
                     <*> (x .:? "ReplicationInstanceIdentifier")
                     <*> (x .:? "PendingModifiedValues"))

instance Hashable ReplicationInstance where

instance NFData ReplicationInstance where

-- | Contains metadata for a replication instance task log.
--
--
--
-- /See:/ 'replicationInstanceTaskLog' smart constructor.
data ReplicationInstanceTaskLog = ReplicationInstanceTaskLog'
  { _ritlReplicationTaskName            :: !(Maybe Text)
  , _ritlReplicationTaskARN             :: !(Maybe Text)
  , _ritlReplicationInstanceTaskLogSize :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationInstanceTaskLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ritlReplicationTaskName' - The name of the replication task.
--
-- * 'ritlReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'ritlReplicationInstanceTaskLogSize' - The size, in bytes, of the replication task log.
replicationInstanceTaskLog
    :: ReplicationInstanceTaskLog
replicationInstanceTaskLog =
  ReplicationInstanceTaskLog'
    { _ritlReplicationTaskName = Nothing
    , _ritlReplicationTaskARN = Nothing
    , _ritlReplicationInstanceTaskLogSize = Nothing
    }


-- | The name of the replication task.
ritlReplicationTaskName :: Lens' ReplicationInstanceTaskLog (Maybe Text)
ritlReplicationTaskName = lens _ritlReplicationTaskName (\ s a -> s{_ritlReplicationTaskName = a})

-- | The Amazon Resource Name (ARN) of the replication task.
ritlReplicationTaskARN :: Lens' ReplicationInstanceTaskLog (Maybe Text)
ritlReplicationTaskARN = lens _ritlReplicationTaskARN (\ s a -> s{_ritlReplicationTaskARN = a})

-- | The size, in bytes, of the replication task log.
ritlReplicationInstanceTaskLogSize :: Lens' ReplicationInstanceTaskLog (Maybe Integer)
ritlReplicationInstanceTaskLogSize = lens _ritlReplicationInstanceTaskLogSize (\ s a -> s{_ritlReplicationInstanceTaskLogSize = a})

instance FromJSON ReplicationInstanceTaskLog where
        parseJSON
          = withObject "ReplicationInstanceTaskLog"
              (\ x ->
                 ReplicationInstanceTaskLog' <$>
                   (x .:? "ReplicationTaskName") <*>
                     (x .:? "ReplicationTaskArn")
                     <*> (x .:? "ReplicationInstanceTaskLogSize"))

instance Hashable ReplicationInstanceTaskLog where

instance NFData ReplicationInstanceTaskLog where

-- |
--
--
--
-- /See:/ 'replicationPendingModifiedValues' smart constructor.
data ReplicationPendingModifiedValues = ReplicationPendingModifiedValues'
  { _rpmvEngineVersion            :: !(Maybe Text)
  , _rpmvMultiAZ                  :: !(Maybe Bool)
  , _rpmvAllocatedStorage         :: !(Maybe Int)
  , _rpmvReplicationInstanceClass :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationPendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmvEngineVersion' - The engine version number of the replication instance.
--
-- * 'rpmvMultiAZ' - Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'rpmvAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'rpmvReplicationInstanceClass' - The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
replicationPendingModifiedValues
    :: ReplicationPendingModifiedValues
replicationPendingModifiedValues =
  ReplicationPendingModifiedValues'
    { _rpmvEngineVersion = Nothing
    , _rpmvMultiAZ = Nothing
    , _rpmvAllocatedStorage = Nothing
    , _rpmvReplicationInstanceClass = Nothing
    }


-- | The engine version number of the replication instance.
rpmvEngineVersion :: Lens' ReplicationPendingModifiedValues (Maybe Text)
rpmvEngineVersion = lens _rpmvEngineVersion (\ s a -> s{_rpmvEngineVersion = a})

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
rpmvMultiAZ :: Lens' ReplicationPendingModifiedValues (Maybe Bool)
rpmvMultiAZ = lens _rpmvMultiAZ (\ s a -> s{_rpmvMultiAZ = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
rpmvAllocatedStorage :: Lens' ReplicationPendingModifiedValues (Maybe Int)
rpmvAllocatedStorage = lens _rpmvAllocatedStorage (\ s a -> s{_rpmvAllocatedStorage = a})

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
rpmvReplicationInstanceClass :: Lens' ReplicationPendingModifiedValues (Maybe Text)
rpmvReplicationInstanceClass = lens _rpmvReplicationInstanceClass (\ s a -> s{_rpmvReplicationInstanceClass = a})

instance FromJSON ReplicationPendingModifiedValues
         where
        parseJSON
          = withObject "ReplicationPendingModifiedValues"
              (\ x ->
                 ReplicationPendingModifiedValues' <$>
                   (x .:? "EngineVersion") <*> (x .:? "MultiAZ") <*>
                     (x .:? "AllocatedStorage")
                     <*> (x .:? "ReplicationInstanceClass"))

instance Hashable ReplicationPendingModifiedValues
         where

instance NFData ReplicationPendingModifiedValues
         where

-- |
--
--
--
-- /See:/ 'replicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { _rsgVPCId                             :: !(Maybe Text)
  , _rsgSubnets                           :: !(Maybe [Subnet])
  , _rsgReplicationSubnetGroupIdentifier  :: !(Maybe Text)
  , _rsgSubnetGroupStatus                 :: !(Maybe Text)
  , _rsgReplicationSubnetGroupDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgVPCId' - The ID of the VPC.
--
-- * 'rsgSubnets' - The subnets that are in the subnet group.
--
-- * 'rsgReplicationSubnetGroupIdentifier' - The identifier of the replication instance subnet group.
--
-- * 'rsgSubnetGroupStatus' - The status of the subnet group.
--
-- * 'rsgReplicationSubnetGroupDescription' - The description of the replication subnet group.
replicationSubnetGroup
    :: ReplicationSubnetGroup
replicationSubnetGroup =
  ReplicationSubnetGroup'
    { _rsgVPCId = Nothing
    , _rsgSubnets = Nothing
    , _rsgReplicationSubnetGroupIdentifier = Nothing
    , _rsgSubnetGroupStatus = Nothing
    , _rsgReplicationSubnetGroupDescription = Nothing
    }


-- | The ID of the VPC.
rsgVPCId :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgVPCId = lens _rsgVPCId (\ s a -> s{_rsgVPCId = a})

-- | The subnets that are in the subnet group.
rsgSubnets :: Lens' ReplicationSubnetGroup [Subnet]
rsgSubnets = lens _rsgSubnets (\ s a -> s{_rsgSubnets = a}) . _Default . _Coerce

-- | The identifier of the replication instance subnet group.
rsgReplicationSubnetGroupIdentifier :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupIdentifier = lens _rsgReplicationSubnetGroupIdentifier (\ s a -> s{_rsgReplicationSubnetGroupIdentifier = a})

-- | The status of the subnet group.
rsgSubnetGroupStatus :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgSubnetGroupStatus = lens _rsgSubnetGroupStatus (\ s a -> s{_rsgSubnetGroupStatus = a})

-- | The description of the replication subnet group.
rsgReplicationSubnetGroupDescription :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupDescription = lens _rsgReplicationSubnetGroupDescription (\ s a -> s{_rsgReplicationSubnetGroupDescription = a})

instance FromJSON ReplicationSubnetGroup where
        parseJSON
          = withObject "ReplicationSubnetGroup"
              (\ x ->
                 ReplicationSubnetGroup' <$>
                   (x .:? "VpcId") <*> (x .:? "Subnets" .!= mempty) <*>
                     (x .:? "ReplicationSubnetGroupIdentifier")
                     <*> (x .:? "SubnetGroupStatus")
                     <*> (x .:? "ReplicationSubnetGroupDescription"))

instance Hashable ReplicationSubnetGroup where

instance NFData ReplicationSubnetGroup where

-- |
--
--
--
-- /See:/ 'replicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
  { _rReplicationTaskSettings     :: !(Maybe Text)
  , _rStatus                      :: !(Maybe Text)
  , _rStopReason                  :: !(Maybe Text)
  , _rTargetEndpointARN           :: !(Maybe Text)
  , _rReplicationTaskIdentifier   :: !(Maybe Text)
  , _rCdcStartPosition            :: !(Maybe Text)
  , _rReplicationTaskStartDate    :: !(Maybe POSIX)
  , _rSourceEndpointARN           :: !(Maybe Text)
  , _rRecoveryCheckpoint          :: !(Maybe Text)
  , _rTableMappings               :: !(Maybe Text)
  , _rReplicationTaskCreationDate :: !(Maybe POSIX)
  , _rMigrationType               :: !(Maybe MigrationTypeValue)
  , _rReplicationTaskARN          :: !(Maybe Text)
  , _rCdcStopPosition             :: !(Maybe Text)
  , _rReplicationTaskStats        :: !(Maybe ReplicationTaskStats)
  , _rReplicationInstanceARN      :: !(Maybe Text)
  , _rLastFailureMessage          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rReplicationTaskSettings' - The settings for the replication task.
--
-- * 'rStatus' - The status of the replication task.
--
-- * 'rStopReason' - The reason the replication task was stopped.
--
-- * 'rTargetEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'rCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'rReplicationTaskStartDate' - The date the replication task is scheduled to start.
--
-- * 'rSourceEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rRecoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- * 'rTableMappings' - Table mappings specified in the task.
--
-- * 'rReplicationTaskCreationDate' - The date the replication task was created.
--
-- * 'rMigrationType' - The type of migration.
--
-- * 'rReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'rCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
--
-- * 'rReplicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- * 'rReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rLastFailureMessage' - The last error (failure) message generated for the replication instance.
replicationTask
    :: ReplicationTask
replicationTask =
  ReplicationTask'
    { _rReplicationTaskSettings = Nothing
    , _rStatus = Nothing
    , _rStopReason = Nothing
    , _rTargetEndpointARN = Nothing
    , _rReplicationTaskIdentifier = Nothing
    , _rCdcStartPosition = Nothing
    , _rReplicationTaskStartDate = Nothing
    , _rSourceEndpointARN = Nothing
    , _rRecoveryCheckpoint = Nothing
    , _rTableMappings = Nothing
    , _rReplicationTaskCreationDate = Nothing
    , _rMigrationType = Nothing
    , _rReplicationTaskARN = Nothing
    , _rCdcStopPosition = Nothing
    , _rReplicationTaskStats = Nothing
    , _rReplicationInstanceARN = Nothing
    , _rLastFailureMessage = Nothing
    }


-- | The settings for the replication task.
rReplicationTaskSettings :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskSettings = lens _rReplicationTaskSettings (\ s a -> s{_rReplicationTaskSettings = a})

-- | The status of the replication task.
rStatus :: Lens' ReplicationTask (Maybe Text)
rStatus = lens _rStatus (\ s a -> s{_rStatus = a})

-- | The reason the replication task was stopped.
rStopReason :: Lens' ReplicationTask (Maybe Text)
rStopReason = lens _rStopReason (\ s a -> s{_rStopReason = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rTargetEndpointARN :: Lens' ReplicationTask (Maybe Text)
rTargetEndpointARN = lens _rTargetEndpointARN (\ s a -> s{_rTargetEndpointARN = a})

-- | The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
rReplicationTaskIdentifier :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskIdentifier = lens _rReplicationTaskIdentifier (\ s a -> s{_rReplicationTaskIdentifier = a})

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
rCdcStartPosition :: Lens' ReplicationTask (Maybe Text)
rCdcStartPosition = lens _rCdcStartPosition (\ s a -> s{_rCdcStartPosition = a})

-- | The date the replication task is scheduled to start.
rReplicationTaskStartDate :: Lens' ReplicationTask (Maybe UTCTime)
rReplicationTaskStartDate = lens _rReplicationTaskStartDate (\ s a -> s{_rReplicationTaskStartDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rSourceEndpointARN :: Lens' ReplicationTask (Maybe Text)
rSourceEndpointARN = lens _rSourceEndpointARN (\ s a -> s{_rSourceEndpointARN = a})

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
rRecoveryCheckpoint :: Lens' ReplicationTask (Maybe Text)
rRecoveryCheckpoint = lens _rRecoveryCheckpoint (\ s a -> s{_rRecoveryCheckpoint = a})

-- | Table mappings specified in the task.
rTableMappings :: Lens' ReplicationTask (Maybe Text)
rTableMappings = lens _rTableMappings (\ s a -> s{_rTableMappings = a})

-- | The date the replication task was created.
rReplicationTaskCreationDate :: Lens' ReplicationTask (Maybe UTCTime)
rReplicationTaskCreationDate = lens _rReplicationTaskCreationDate (\ s a -> s{_rReplicationTaskCreationDate = a}) . mapping _Time

-- | The type of migration.
rMigrationType :: Lens' ReplicationTask (Maybe MigrationTypeValue)
rMigrationType = lens _rMigrationType (\ s a -> s{_rMigrationType = a})

-- | The Amazon Resource Name (ARN) of the replication task.
rReplicationTaskARN :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskARN = lens _rReplicationTaskARN (\ s a -> s{_rReplicationTaskARN = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
rCdcStopPosition :: Lens' ReplicationTask (Maybe Text)
rCdcStopPosition = lens _rCdcStopPosition (\ s a -> s{_rCdcStopPosition = a})

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
rReplicationTaskStats :: Lens' ReplicationTask (Maybe ReplicationTaskStats)
rReplicationTaskStats = lens _rReplicationTaskStats (\ s a -> s{_rReplicationTaskStats = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
rReplicationInstanceARN :: Lens' ReplicationTask (Maybe Text)
rReplicationInstanceARN = lens _rReplicationInstanceARN (\ s a -> s{_rReplicationInstanceARN = a})

-- | The last error (failure) message generated for the replication instance.
rLastFailureMessage :: Lens' ReplicationTask (Maybe Text)
rLastFailureMessage = lens _rLastFailureMessage (\ s a -> s{_rLastFailureMessage = a})

instance FromJSON ReplicationTask where
        parseJSON
          = withObject "ReplicationTask"
              (\ x ->
                 ReplicationTask' <$>
                   (x .:? "ReplicationTaskSettings") <*>
                     (x .:? "Status")
                     <*> (x .:? "StopReason")
                     <*> (x .:? "TargetEndpointArn")
                     <*> (x .:? "ReplicationTaskIdentifier")
                     <*> (x .:? "CdcStartPosition")
                     <*> (x .:? "ReplicationTaskStartDate")
                     <*> (x .:? "SourceEndpointArn")
                     <*> (x .:? "RecoveryCheckpoint")
                     <*> (x .:? "TableMappings")
                     <*> (x .:? "ReplicationTaskCreationDate")
                     <*> (x .:? "MigrationType")
                     <*> (x .:? "ReplicationTaskArn")
                     <*> (x .:? "CdcStopPosition")
                     <*> (x .:? "ReplicationTaskStats")
                     <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable ReplicationTask where

instance NFData ReplicationTask where

-- | The task assessment report in JSON format.
--
--
--
-- /See:/ 'replicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { _rtarAssessmentResults                 :: !(Maybe Text)
  , _rtarAssessmentResultsFile             :: !(Maybe Text)
  , _rtarReplicationTaskIdentifier         :: !(Maybe Text)
  , _rtarAssessmentStatus                  :: !(Maybe Text)
  , _rtarS3ObjectURL                       :: !(Maybe Text)
  , _rtarReplicationTaskLastAssessmentDate :: !(Maybe POSIX)
  , _rtarReplicationTaskARN                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationTaskAssessmentResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtarAssessmentResults' - The task assessment results in JSON format.
--
-- * 'rtarAssessmentResultsFile' - The file containing the results of the task assessment.
--
-- * 'rtarReplicationTaskIdentifier' - The replication task identifier of the task on which the task assessment was run.
--
-- * 'rtarAssessmentStatus' - The status of the task assessment.
--
-- * 'rtarS3ObjectURL' - The URL of the S3 object containing the task assessment results.
--
-- * 'rtarReplicationTaskLastAssessmentDate' - The date the task assessment was completed.
--
-- * 'rtarReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
replicationTaskAssessmentResult
    :: ReplicationTaskAssessmentResult
replicationTaskAssessmentResult =
  ReplicationTaskAssessmentResult'
    { _rtarAssessmentResults = Nothing
    , _rtarAssessmentResultsFile = Nothing
    , _rtarReplicationTaskIdentifier = Nothing
    , _rtarAssessmentStatus = Nothing
    , _rtarS3ObjectURL = Nothing
    , _rtarReplicationTaskLastAssessmentDate = Nothing
    , _rtarReplicationTaskARN = Nothing
    }


-- | The task assessment results in JSON format.
rtarAssessmentResults :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentResults = lens _rtarAssessmentResults (\ s a -> s{_rtarAssessmentResults = a})

-- | The file containing the results of the task assessment.
rtarAssessmentResultsFile :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentResultsFile = lens _rtarAssessmentResultsFile (\ s a -> s{_rtarAssessmentResultsFile = a})

-- | The replication task identifier of the task on which the task assessment was run.
rtarReplicationTaskIdentifier :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarReplicationTaskIdentifier = lens _rtarReplicationTaskIdentifier (\ s a -> s{_rtarReplicationTaskIdentifier = a})

-- | The status of the task assessment.
rtarAssessmentStatus :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentStatus = lens _rtarAssessmentStatus (\ s a -> s{_rtarAssessmentStatus = a})

-- | The URL of the S3 object containing the task assessment results.
rtarS3ObjectURL :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarS3ObjectURL = lens _rtarS3ObjectURL (\ s a -> s{_rtarS3ObjectURL = a})

-- | The date the task assessment was completed.
rtarReplicationTaskLastAssessmentDate :: Lens' ReplicationTaskAssessmentResult (Maybe UTCTime)
rtarReplicationTaskLastAssessmentDate = lens _rtarReplicationTaskLastAssessmentDate (\ s a -> s{_rtarReplicationTaskLastAssessmentDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task.
rtarReplicationTaskARN :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarReplicationTaskARN = lens _rtarReplicationTaskARN (\ s a -> s{_rtarReplicationTaskARN = a})

instance FromJSON ReplicationTaskAssessmentResult
         where
        parseJSON
          = withObject "ReplicationTaskAssessmentResult"
              (\ x ->
                 ReplicationTaskAssessmentResult' <$>
                   (x .:? "AssessmentResults") <*>
                     (x .:? "AssessmentResultsFile")
                     <*> (x .:? "ReplicationTaskIdentifier")
                     <*> (x .:? "AssessmentStatus")
                     <*> (x .:? "S3ObjectUrl")
                     <*> (x .:? "ReplicationTaskLastAssessmentDate")
                     <*> (x .:? "ReplicationTaskArn"))

instance Hashable ReplicationTaskAssessmentResult
         where

instance NFData ReplicationTaskAssessmentResult where

-- |
--
--
--
-- /See:/ 'replicationTaskStats' smart constructor.
data ReplicationTaskStats = ReplicationTaskStats'
  { _rtsFullLoadProgressPercent :: !(Maybe Int)
  , _rtsElapsedTimeMillis       :: !(Maybe Integer)
  , _rtsTablesErrored           :: !(Maybe Int)
  , _rtsTablesLoaded            :: !(Maybe Int)
  , _rtsTablesQueued            :: !(Maybe Int)
  , _rtsTablesLoading           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationTaskStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtsFullLoadProgressPercent' - The percent complete for the full load migration task.
--
-- * 'rtsElapsedTimeMillis' - The elapsed time of the task, in milliseconds.
--
-- * 'rtsTablesErrored' - The number of errors that have occurred during this task.
--
-- * 'rtsTablesLoaded' - The number of tables loaded for this task.
--
-- * 'rtsTablesQueued' - The number of tables queued for this task.
--
-- * 'rtsTablesLoading' - The number of tables currently loading for this task.
replicationTaskStats
    :: ReplicationTaskStats
replicationTaskStats =
  ReplicationTaskStats'
    { _rtsFullLoadProgressPercent = Nothing
    , _rtsElapsedTimeMillis = Nothing
    , _rtsTablesErrored = Nothing
    , _rtsTablesLoaded = Nothing
    , _rtsTablesQueued = Nothing
    , _rtsTablesLoading = Nothing
    }


-- | The percent complete for the full load migration task.
rtsFullLoadProgressPercent :: Lens' ReplicationTaskStats (Maybe Int)
rtsFullLoadProgressPercent = lens _rtsFullLoadProgressPercent (\ s a -> s{_rtsFullLoadProgressPercent = a})

-- | The elapsed time of the task, in milliseconds.
rtsElapsedTimeMillis :: Lens' ReplicationTaskStats (Maybe Integer)
rtsElapsedTimeMillis = lens _rtsElapsedTimeMillis (\ s a -> s{_rtsElapsedTimeMillis = a})

-- | The number of errors that have occurred during this task.
rtsTablesErrored :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesErrored = lens _rtsTablesErrored (\ s a -> s{_rtsTablesErrored = a})

-- | The number of tables loaded for this task.
rtsTablesLoaded :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoaded = lens _rtsTablesLoaded (\ s a -> s{_rtsTablesLoaded = a})

-- | The number of tables queued for this task.
rtsTablesQueued :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesQueued = lens _rtsTablesQueued (\ s a -> s{_rtsTablesQueued = a})

-- | The number of tables currently loading for this task.
rtsTablesLoading :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoading = lens _rtsTablesLoading (\ s a -> s{_rtsTablesLoading = a})

instance FromJSON ReplicationTaskStats where
        parseJSON
          = withObject "ReplicationTaskStats"
              (\ x ->
                 ReplicationTaskStats' <$>
                   (x .:? "FullLoadProgressPercent") <*>
                     (x .:? "ElapsedTimeMillis")
                     <*> (x .:? "TablesErrored")
                     <*> (x .:? "TablesLoaded")
                     <*> (x .:? "TablesQueued")
                     <*> (x .:? "TablesLoading"))

instance Hashable ReplicationTaskStats where

instance NFData ReplicationTaskStats where

-- |
--
--
--
-- /See:/ 's3Settings' smart constructor.
data S3Settings = S3Settings'
  { _ssCSVDelimiter            :: !(Maybe Text)
  , _ssServiceAccessRoleARN    :: !(Maybe Text)
  , _ssBucketFolder            :: !(Maybe Text)
  , _ssExternalTableDefinition :: !(Maybe Text)
  , _ssBucketName              :: !(Maybe Text)
  , _ssCSVRowDelimiter         :: !(Maybe Text)
  , _ssCompressionType         :: !(Maybe CompressionTypeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCSVDelimiter' - The delimiter used to separate columns in the source files. The default is a comma.
--
-- * 'ssServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- * 'ssBucketFolder' - An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path <bucketFolder>/<schema_name>/<table_name>/. If this parameter is not specified, then the path used is <schema_name>/<table_name>/.
--
-- * 'ssExternalTableDefinition' - The external table definition.
--
-- * 'ssBucketName' - The name of the S3 bucket.
--
-- * 'ssCSVRowDelimiter' - The delimiter used to separate rows in the source files. The default is a carriage return (\n).
--
-- * 'ssCompressionType' - An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Set to NONE (the default) or do not use to leave the files uncompressed.
s3Settings
    :: S3Settings
s3Settings =
  S3Settings'
    { _ssCSVDelimiter = Nothing
    , _ssServiceAccessRoleARN = Nothing
    , _ssBucketFolder = Nothing
    , _ssExternalTableDefinition = Nothing
    , _ssBucketName = Nothing
    , _ssCSVRowDelimiter = Nothing
    , _ssCompressionType = Nothing
    }


-- | The delimiter used to separate columns in the source files. The default is a comma.
ssCSVDelimiter :: Lens' S3Settings (Maybe Text)
ssCSVDelimiter = lens _ssCSVDelimiter (\ s a -> s{_ssCSVDelimiter = a})

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
ssServiceAccessRoleARN :: Lens' S3Settings (Maybe Text)
ssServiceAccessRoleARN = lens _ssServiceAccessRoleARN (\ s a -> s{_ssServiceAccessRoleARN = a})

-- | An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path <bucketFolder>/<schema_name>/<table_name>/. If this parameter is not specified, then the path used is <schema_name>/<table_name>/.
ssBucketFolder :: Lens' S3Settings (Maybe Text)
ssBucketFolder = lens _ssBucketFolder (\ s a -> s{_ssBucketFolder = a})

-- | The external table definition.
ssExternalTableDefinition :: Lens' S3Settings (Maybe Text)
ssExternalTableDefinition = lens _ssExternalTableDefinition (\ s a -> s{_ssExternalTableDefinition = a})

-- | The name of the S3 bucket.
ssBucketName :: Lens' S3Settings (Maybe Text)
ssBucketName = lens _ssBucketName (\ s a -> s{_ssBucketName = a})

-- | The delimiter used to separate rows in the source files. The default is a carriage return (\n).
ssCSVRowDelimiter :: Lens' S3Settings (Maybe Text)
ssCSVRowDelimiter = lens _ssCSVRowDelimiter (\ s a -> s{_ssCSVRowDelimiter = a})

-- | An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Set to NONE (the default) or do not use to leave the files uncompressed.
ssCompressionType :: Lens' S3Settings (Maybe CompressionTypeValue)
ssCompressionType = lens _ssCompressionType (\ s a -> s{_ssCompressionType = a})

instance FromJSON S3Settings where
        parseJSON
          = withObject "S3Settings"
              (\ x ->
                 S3Settings' <$>
                   (x .:? "CsvDelimiter") <*>
                     (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "BucketFolder")
                     <*> (x .:? "ExternalTableDefinition")
                     <*> (x .:? "BucketName")
                     <*> (x .:? "CsvRowDelimiter")
                     <*> (x .:? "CompressionType"))

instance Hashable S3Settings where

instance NFData S3Settings where

instance ToJSON S3Settings where
        toJSON S3Settings'{..}
          = object
              (catMaybes
                 [("CsvDelimiter" .=) <$> _ssCSVDelimiter,
                  ("ServiceAccessRoleArn" .=) <$>
                    _ssServiceAccessRoleARN,
                  ("BucketFolder" .=) <$> _ssBucketFolder,
                  ("ExternalTableDefinition" .=) <$>
                    _ssExternalTableDefinition,
                  ("BucketName" .=) <$> _ssBucketName,
                  ("CsvRowDelimiter" .=) <$> _ssCSVRowDelimiter,
                  ("CompressionType" .=) <$> _ssCompressionType])

-- |
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus           :: !(Maybe Text)
  , _sSubnetIdentifier       :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - The status of the subnet.
--
-- * 'sSubnetIdentifier' - The subnet identifier.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone of the subnet.
subnet
    :: Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing
    , _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }


-- | The status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a})

-- | The subnet identifier.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | The Availability Zone of the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromJSON Subnet where
        parseJSON
          = withObject "Subnet"
              (\ x ->
                 Subnet' <$>
                   (x .:? "SubnetStatus") <*> (x .:? "SubnetIdentifier")
                     <*> (x .:? "SubnetAvailabilityZone"))

instance Hashable Subnet where

instance NFData Subnet where

-- |
--
--
--
-- /See:/ 'supportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { _setEngineDisplayName :: !(Maybe Text)
  , _setEndpointType      :: !(Maybe ReplicationEndpointTypeValue)
  , _setEngineName        :: !(Maybe Text)
  , _setSupportsCDC       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SupportedEndpointType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setEngineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- * 'setEndpointType' - The type of endpoint.
--
-- * 'setEngineName' - The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
--
-- * 'setSupportsCDC' - Indicates if Change Data Capture (CDC) is supported.
supportedEndpointType
    :: SupportedEndpointType
supportedEndpointType =
  SupportedEndpointType'
    { _setEngineDisplayName = Nothing
    , _setEndpointType = Nothing
    , _setEngineName = Nothing
    , _setSupportsCDC = Nothing
    }


-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
setEngineDisplayName :: Lens' SupportedEndpointType (Maybe Text)
setEngineDisplayName = lens _setEngineDisplayName (\ s a -> s{_setEngineDisplayName = a})

-- | The type of endpoint.
setEndpointType :: Lens' SupportedEndpointType (Maybe ReplicationEndpointTypeValue)
setEndpointType = lens _setEndpointType (\ s a -> s{_setEndpointType = a})

-- | The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
setEngineName :: Lens' SupportedEndpointType (Maybe Text)
setEngineName = lens _setEngineName (\ s a -> s{_setEngineName = a})

-- | Indicates if Change Data Capture (CDC) is supported.
setSupportsCDC :: Lens' SupportedEndpointType (Maybe Bool)
setSupportsCDC = lens _setSupportsCDC (\ s a -> s{_setSupportsCDC = a})

instance FromJSON SupportedEndpointType where
        parseJSON
          = withObject "SupportedEndpointType"
              (\ x ->
                 SupportedEndpointType' <$>
                   (x .:? "EngineDisplayName") <*>
                     (x .:? "EndpointType")
                     <*> (x .:? "EngineName")
                     <*> (x .:? "SupportsCDC"))

instance Hashable SupportedEndpointType where

instance NFData SupportedEndpointType where

-- |
--
--
--
-- /See:/ 'tableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { _tsValidationState              :: !(Maybe Text)
  , _tsFullLoadRows                 :: !(Maybe Integer)
  , _tsInserts                      :: !(Maybe Integer)
  , _tsFullLoadCondtnlChkFailedRows :: !(Maybe Integer)
  , _tsValidationFailedRecords      :: !(Maybe Integer)
  , _tsValidationSuspendedRecords   :: !(Maybe Integer)
  , _tsSchemaName                   :: !(Maybe Text)
  , _tsTableState                   :: !(Maybe Text)
  , _tsFullLoadErrorRows            :: !(Maybe Integer)
  , _tsDdls                         :: !(Maybe Integer)
  , _tsDeletes                      :: !(Maybe Integer)
  , _tsUpdates                      :: !(Maybe Integer)
  , _tsValidationPendingRecords     :: !(Maybe Integer)
  , _tsLastUpdateTime               :: !(Maybe POSIX)
  , _tsTableName                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsValidationState' - The validation state of the table. The parameter can have the following values     * Not enabled—Validation is not enabled for the table in the migration task.     * Pending records—Some records in the table are waiting for validation.     * Mismatched records—Some records in the table do not match between the source and target.     * Suspended records—Some records in the table could not be validated.     * No primary key—The table could not be validated because it had no primary key.     * Table error—The table was not validated because it was in an error state and some data was not migrated.     * Validated—All rows in the table were validated. If the table is updated, the status can change from Validated.     * Error—The table could not be validated because of an unexpected error.
--
-- * 'tsFullLoadRows' - The number of rows added during the Full Load operation.
--
-- * 'tsInserts' - The number of insert actions performed on a table.
--
-- * 'tsFullLoadCondtnlChkFailedRows' - The number of rows that failed conditional checks during the Full Load operation (valid only for DynamoDB as a target migrations).
--
-- * 'tsValidationFailedRecords' - The number of records that failed validation.
--
-- * 'tsValidationSuspendedRecords' - The number of records that could not be validated.
--
-- * 'tsSchemaName' - The schema name.
--
-- * 'tsTableState' - The state of the tables described. Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
--
-- * 'tsFullLoadErrorRows' - The number of rows that failed to load during the Full Load operation (valid only for DynamoDB as a target migrations).
--
-- * 'tsDdls' - The Data Definition Language (DDL) used to build and modify the structure of your tables.
--
-- * 'tsDeletes' - The number of delete actions performed on a table.
--
-- * 'tsUpdates' - The number of update actions performed on a table.
--
-- * 'tsValidationPendingRecords' - The number of records that have yet to be validated.
--
-- * 'tsLastUpdateTime' - The last time the table was updated.
--
-- * 'tsTableName' - The name of the table.
tableStatistics
    :: TableStatistics
tableStatistics =
  TableStatistics'
    { _tsValidationState = Nothing
    , _tsFullLoadRows = Nothing
    , _tsInserts = Nothing
    , _tsFullLoadCondtnlChkFailedRows = Nothing
    , _tsValidationFailedRecords = Nothing
    , _tsValidationSuspendedRecords = Nothing
    , _tsSchemaName = Nothing
    , _tsTableState = Nothing
    , _tsFullLoadErrorRows = Nothing
    , _tsDdls = Nothing
    , _tsDeletes = Nothing
    , _tsUpdates = Nothing
    , _tsValidationPendingRecords = Nothing
    , _tsLastUpdateTime = Nothing
    , _tsTableName = Nothing
    }


-- | The validation state of the table. The parameter can have the following values     * Not enabled—Validation is not enabled for the table in the migration task.     * Pending records—Some records in the table are waiting for validation.     * Mismatched records—Some records in the table do not match between the source and target.     * Suspended records—Some records in the table could not be validated.     * No primary key—The table could not be validated because it had no primary key.     * Table error—The table was not validated because it was in an error state and some data was not migrated.     * Validated—All rows in the table were validated. If the table is updated, the status can change from Validated.     * Error—The table could not be validated because of an unexpected error.
tsValidationState :: Lens' TableStatistics (Maybe Text)
tsValidationState = lens _tsValidationState (\ s a -> s{_tsValidationState = a})

-- | The number of rows added during the Full Load operation.
tsFullLoadRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadRows = lens _tsFullLoadRows (\ s a -> s{_tsFullLoadRows = a})

-- | The number of insert actions performed on a table.
tsInserts :: Lens' TableStatistics (Maybe Integer)
tsInserts = lens _tsInserts (\ s a -> s{_tsInserts = a})

-- | The number of rows that failed conditional checks during the Full Load operation (valid only for DynamoDB as a target migrations).
tsFullLoadCondtnlChkFailedRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadCondtnlChkFailedRows = lens _tsFullLoadCondtnlChkFailedRows (\ s a -> s{_tsFullLoadCondtnlChkFailedRows = a})

-- | The number of records that failed validation.
tsValidationFailedRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationFailedRecords = lens _tsValidationFailedRecords (\ s a -> s{_tsValidationFailedRecords = a})

-- | The number of records that could not be validated.
tsValidationSuspendedRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationSuspendedRecords = lens _tsValidationSuspendedRecords (\ s a -> s{_tsValidationSuspendedRecords = a})

-- | The schema name.
tsSchemaName :: Lens' TableStatistics (Maybe Text)
tsSchemaName = lens _tsSchemaName (\ s a -> s{_tsSchemaName = a})

-- | The state of the tables described. Valid states: Table does not exist | Before load | Full load | Table completed | Table cancelled | Table error | Table all | Table updates | Table is being reloaded
tsTableState :: Lens' TableStatistics (Maybe Text)
tsTableState = lens _tsTableState (\ s a -> s{_tsTableState = a})

-- | The number of rows that failed to load during the Full Load operation (valid only for DynamoDB as a target migrations).
tsFullLoadErrorRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadErrorRows = lens _tsFullLoadErrorRows (\ s a -> s{_tsFullLoadErrorRows = a})

-- | The Data Definition Language (DDL) used to build and modify the structure of your tables.
tsDdls :: Lens' TableStatistics (Maybe Integer)
tsDdls = lens _tsDdls (\ s a -> s{_tsDdls = a})

-- | The number of delete actions performed on a table.
tsDeletes :: Lens' TableStatistics (Maybe Integer)
tsDeletes = lens _tsDeletes (\ s a -> s{_tsDeletes = a})

-- | The number of update actions performed on a table.
tsUpdates :: Lens' TableStatistics (Maybe Integer)
tsUpdates = lens _tsUpdates (\ s a -> s{_tsUpdates = a})

-- | The number of records that have yet to be validated.
tsValidationPendingRecords :: Lens' TableStatistics (Maybe Integer)
tsValidationPendingRecords = lens _tsValidationPendingRecords (\ s a -> s{_tsValidationPendingRecords = a})

-- | The last time the table was updated.
tsLastUpdateTime :: Lens' TableStatistics (Maybe UTCTime)
tsLastUpdateTime = lens _tsLastUpdateTime (\ s a -> s{_tsLastUpdateTime = a}) . mapping _Time

-- | The name of the table.
tsTableName :: Lens' TableStatistics (Maybe Text)
tsTableName = lens _tsTableName (\ s a -> s{_tsTableName = a})

instance FromJSON TableStatistics where
        parseJSON
          = withObject "TableStatistics"
              (\ x ->
                 TableStatistics' <$>
                   (x .:? "ValidationState") <*> (x .:? "FullLoadRows")
                     <*> (x .:? "Inserts")
                     <*> (x .:? "FullLoadCondtnlChkFailedRows")
                     <*> (x .:? "ValidationFailedRecords")
                     <*> (x .:? "ValidationSuspendedRecords")
                     <*> (x .:? "SchemaName")
                     <*> (x .:? "TableState")
                     <*> (x .:? "FullLoadErrorRows")
                     <*> (x .:? "Ddls")
                     <*> (x .:? "Deletes")
                     <*> (x .:? "Updates")
                     <*> (x .:? "ValidationPendingRecords")
                     <*> (x .:? "LastUpdateTime")
                     <*> (x .:? "TableName"))

instance Hashable TableStatistics where

instance NFData TableStatistics where

-- |
--
--
--
-- /See:/ 'tableToReload' smart constructor.
data TableToReload = TableToReload'
  { _ttrSchemaName :: !(Maybe Text)
  , _ttrTableName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableToReload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrSchemaName' - The schema name of the table to be reloaded.
--
-- * 'ttrTableName' - The table name of the table to be reloaded.
tableToReload
    :: TableToReload
tableToReload =
  TableToReload' {_ttrSchemaName = Nothing, _ttrTableName = Nothing}


-- | The schema name of the table to be reloaded.
ttrSchemaName :: Lens' TableToReload (Maybe Text)
ttrSchemaName = lens _ttrSchemaName (\ s a -> s{_ttrSchemaName = a})

-- | The table name of the table to be reloaded.
ttrTableName :: Lens' TableToReload (Maybe Text)
ttrTableName = lens _ttrTableName (\ s a -> s{_ttrTableName = a})

instance Hashable TableToReload where

instance NFData TableToReload where

instance ToJSON TableToReload where
        toJSON TableToReload'{..}
          = object
              (catMaybes
                 [("SchemaName" .=) <$> _ttrSchemaName,
                  ("TableName" .=) <$> _ttrTableName])

-- |
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
--
-- * 'tagKey' - A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- |
--
--
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
  { _vsgmStatus             :: !(Maybe Text)
  , _vsgmVPCSecurityGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus' - The status of the VPC security group.
--
-- * 'vsgmVPCSecurityGroupId' - The VPC security group Id.
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
  VPCSecurityGroupMembership'
    {_vsgmStatus = Nothing, _vsgmVPCSecurityGroupId = Nothing}


-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a})

-- | The VPC security group Id.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a})

instance FromJSON VPCSecurityGroupMembership where
        parseJSON
          = withObject "VPCSecurityGroupMembership"
              (\ x ->
                 VPCSecurityGroupMembership' <$>
                   (x .:? "Status") <*> (x .:? "VpcSecurityGroupId"))

instance Hashable VPCSecurityGroupMembership where

instance NFData VPCSecurityGroupMembership where
