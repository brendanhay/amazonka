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

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
--
--
-- /See:/ 'dmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { _dtsServiceAccessRoleARN :: !(Maybe Text)
  , _dtsBucketName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DmsTransferSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsServiceAccessRoleARN' - The IAM role that has permission to access the Amazon S3 bucket.
--
-- * 'dtsBucketName' - The name of the S3 bucket to use.
dmsTransferSettings
    :: DmsTransferSettings
dmsTransferSettings =
  DmsTransferSettings'
    {_dtsServiceAccessRoleARN = Nothing, _dtsBucketName = Nothing}


-- | The IAM role that has permission to access the Amazon S3 bucket.
dtsServiceAccessRoleARN :: Lens' DmsTransferSettings (Maybe Text)
dtsServiceAccessRoleARN = lens _dtsServiceAccessRoleARN (\ s a -> s{_dtsServiceAccessRoleARN = a})

-- | The name of the S3 bucket to use.
dtsBucketName :: Lens' DmsTransferSettings (Maybe Text)
dtsBucketName = lens _dtsBucketName (\ s a -> s{_dtsBucketName = a})

instance FromJSON DmsTransferSettings where
        parseJSON
          = withObject "DmsTransferSettings"
              (\ x ->
                 DmsTransferSettings' <$>
                   (x .:? "ServiceAccessRoleArn") <*>
                     (x .:? "BucketName"))

instance Hashable DmsTransferSettings where

instance NFData DmsTransferSettings where

instance ToJSON DmsTransferSettings where
        toJSON DmsTransferSettings'{..}
          = object
              (catMaybes
                 [("ServiceAccessRoleArn" .=) <$>
                    _dtsServiceAccessRoleARN,
                  ("BucketName" .=) <$> _dtsBucketName])

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
-- /See:/ 'elasticsearchSettings' smart constructor.
data ElasticsearchSettings = ElasticsearchSettings'
  { _esFullLoadErrorPercentage :: !(Maybe Int)
  , _esErrorRetryDuration      :: !(Maybe Int)
  , _esServiceAccessRoleARN    :: !Text
  , _esEndpointURI             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esFullLoadErrorPercentage' - The maximum percentage of records that can fail to be written before a full load operation stops.
--
-- * 'esErrorRetryDuration' - The maximum number of seconds that DMS retries failed API requests to the Elasticsearch cluster.
--
-- * 'esServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by service to access the IAM role.
--
-- * 'esEndpointURI' - The endpoint for the ElasticSearch cluster.
elasticsearchSettings
    :: Text -- ^ 'esServiceAccessRoleARN'
    -> Text -- ^ 'esEndpointURI'
    -> ElasticsearchSettings
elasticsearchSettings pServiceAccessRoleARN_ pEndpointURI_ =
  ElasticsearchSettings'
    { _esFullLoadErrorPercentage = Nothing
    , _esErrorRetryDuration = Nothing
    , _esServiceAccessRoleARN = pServiceAccessRoleARN_
    , _esEndpointURI = pEndpointURI_
    }


-- | The maximum percentage of records that can fail to be written before a full load operation stops.
esFullLoadErrorPercentage :: Lens' ElasticsearchSettings (Maybe Int)
esFullLoadErrorPercentage = lens _esFullLoadErrorPercentage (\ s a -> s{_esFullLoadErrorPercentage = a})

-- | The maximum number of seconds that DMS retries failed API requests to the Elasticsearch cluster.
esErrorRetryDuration :: Lens' ElasticsearchSettings (Maybe Int)
esErrorRetryDuration = lens _esErrorRetryDuration (\ s a -> s{_esErrorRetryDuration = a})

-- | The Amazon Resource Name (ARN) used by service to access the IAM role.
esServiceAccessRoleARN :: Lens' ElasticsearchSettings Text
esServiceAccessRoleARN = lens _esServiceAccessRoleARN (\ s a -> s{_esServiceAccessRoleARN = a})

-- | The endpoint for the ElasticSearch cluster.
esEndpointURI :: Lens' ElasticsearchSettings Text
esEndpointURI = lens _esEndpointURI (\ s a -> s{_esEndpointURI = a})

instance FromJSON ElasticsearchSettings where
        parseJSON
          = withObject "ElasticsearchSettings"
              (\ x ->
                 ElasticsearchSettings' <$>
                   (x .:? "FullLoadErrorPercentage") <*>
                     (x .:? "ErrorRetryDuration")
                     <*> (x .: "ServiceAccessRoleArn")
                     <*> (x .: "EndpointUri"))

instance Hashable ElasticsearchSettings where

instance NFData ElasticsearchSettings where

instance ToJSON ElasticsearchSettings where
        toJSON ElasticsearchSettings'{..}
          = object
              (catMaybes
                 [("FullLoadErrorPercentage" .=) <$>
                    _esFullLoadErrorPercentage,
                  ("ErrorRetryDuration" .=) <$> _esErrorRetryDuration,
                  Just
                    ("ServiceAccessRoleArn" .= _esServiceAccessRoleARN),
                  Just ("EndpointUri" .= _esEndpointURI)])

-- |
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eStatus                    :: !(Maybe Text)
  , _eDmsTransferSettings       :: !(Maybe DmsTransferSettings)
  , _eServerName                :: !(Maybe Text)
  , _eCertificateARN            :: !(Maybe Text)
  , _eServiceAccessRoleARN      :: !(Maybe Text)
  , _eEngineDisplayName         :: !(Maybe Text)
  , _eExtraConnectionAttributes :: !(Maybe Text)
  , _eEndpointType              :: !(Maybe ReplicationEndpointTypeValue)
  , _eRedshiftSettings          :: !(Maybe RedshiftSettings)
  , _eElasticsearchSettings     :: !(Maybe ElasticsearchSettings)
  , _eUsername                  :: !(Maybe Text)
  , _eExternalTableDefinition   :: !(Maybe Text)
  , _eEngineName                :: !(Maybe Text)
  , _eKMSKeyId                  :: !(Maybe Text)
  , _eMongoDBSettings           :: !(Maybe MongoDBSettings)
  , _eSSLMode                   :: !(Maybe DmsSSLModeValue)
  , _eDatabaseName              :: !(Maybe Text)
  , _eS3Settings                :: !(Maybe S3Settings)
  , _eKinesisSettings           :: !(Maybe KinesisSettings)
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
-- * 'eDmsTransferSettings' - The settings in JSON format for the DMS transfer type of source endpoint.  Possible attributes include the following:     * @serviceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @bucketName@ - The name of the S3 bucket to use.     * @compressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these attributes is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these attributes is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
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
-- * 'eRedshiftSettings' - Settings for the Amazon Redshift endpoint
--
-- * 'eElasticsearchSettings' - The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
--
-- * 'eUsername' - The user name used to connect to the endpoint.
--
-- * 'eExternalTableDefinition' - The external table definition.
--
-- * 'eEngineName' - The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
--
-- * 'eKMSKeyId' - The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'eMongoDBSettings' - The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
--
-- * 'eSSLMode' - The SSL mode used to connect to the endpoint. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
--
-- * 'eDatabaseName' - The name of the database at the endpoint.
--
-- * 'eS3Settings' - The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
--
-- * 'eKinesisSettings' - The settings for the Amazon Kinesis source endpoint. For more information, see the @KinesisSettings@ structure.
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
    , _eDmsTransferSettings = Nothing
    , _eServerName = Nothing
    , _eCertificateARN = Nothing
    , _eServiceAccessRoleARN = Nothing
    , _eEngineDisplayName = Nothing
    , _eExtraConnectionAttributes = Nothing
    , _eEndpointType = Nothing
    , _eRedshiftSettings = Nothing
    , _eElasticsearchSettings = Nothing
    , _eUsername = Nothing
    , _eExternalTableDefinition = Nothing
    , _eEngineName = Nothing
    , _eKMSKeyId = Nothing
    , _eMongoDBSettings = Nothing
    , _eSSLMode = Nothing
    , _eDatabaseName = Nothing
    , _eS3Settings = Nothing
    , _eKinesisSettings = Nothing
    , _eEndpointIdentifier = Nothing
    , _eExternalId = Nothing
    , _eDynamoDBSettings = Nothing
    , _eEndpointARN = Nothing
    , _ePort = Nothing
    }


-- | The status of the endpoint.
eStatus :: Lens' Endpoint (Maybe Text)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a})

-- | The settings in JSON format for the DMS transfer type of source endpoint.  Possible attributes include the following:     * @serviceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @bucketName@ - The name of the S3 bucket to use.     * @compressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these attributes is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these attributes is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
eDmsTransferSettings :: Lens' Endpoint (Maybe DmsTransferSettings)
eDmsTransferSettings = lens _eDmsTransferSettings (\ s a -> s{_eDmsTransferSettings = a})

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

-- | Settings for the Amazon Redshift endpoint
eRedshiftSettings :: Lens' Endpoint (Maybe RedshiftSettings)
eRedshiftSettings = lens _eRedshiftSettings (\ s a -> s{_eRedshiftSettings = a})

-- | The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
eElasticsearchSettings :: Lens' Endpoint (Maybe ElasticsearchSettings)
eElasticsearchSettings = lens _eElasticsearchSettings (\ s a -> s{_eElasticsearchSettings = a})

-- | The user name used to connect to the endpoint.
eUsername :: Lens' Endpoint (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a})

-- | The external table definition.
eExternalTableDefinition :: Lens' Endpoint (Maybe Text)
eExternalTableDefinition = lens _eExternalTableDefinition (\ s a -> s{_eExternalTableDefinition = a})

-- | The database engine name. Valid values, depending on the EndPointType, include mysql, oracle, postgres, mariadb, aurora, aurora-postgresql, redshift, s3, db2, azuredb, sybase, sybase, dynamodb, mongodb, and sqlserver.
eEngineName :: Lens' Endpoint (Maybe Text)
eEngineName = lens _eEngineName (\ s a -> s{_eEngineName = a})

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
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

-- | The settings for the Amazon Kinesis source endpoint. For more information, see the @KinesisSettings@ structure.
eKinesisSettings :: Lens' Endpoint (Maybe KinesisSettings)
eKinesisSettings = lens _eKinesisSettings (\ s a -> s{_eKinesisSettings = a})

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
                   (x .:? "Status") <*> (x .:? "DmsTransferSettings")
                     <*> (x .:? "ServerName")
                     <*> (x .:? "CertificateArn")
                     <*> (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "EngineDisplayName")
                     <*> (x .:? "ExtraConnectionAttributes")
                     <*> (x .:? "EndpointType")
                     <*> (x .:? "RedshiftSettings")
                     <*> (x .:? "ElasticsearchSettings")
                     <*> (x .:? "Username")
                     <*> (x .:? "ExternalTableDefinition")
                     <*> (x .:? "EngineName")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "MongoDbSettings")
                     <*> (x .:? "SslMode")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "S3Settings")
                     <*> (x .:? "KinesisSettings")
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
-- /See:/ 'kinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { _ksServiceAccessRoleARN :: !(Maybe Text)
  , _ksStreamARN            :: !(Maybe Text)
  , _ksMessageFormat        :: !(Maybe MessageFormatValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksServiceAccessRoleARN' - The Amazon Resource Name (ARN) for the IAM role that DMS uses to write to the Amazon Kinesis data stream.
--
-- * 'ksStreamARN' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
--
-- * 'ksMessageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ .
kinesisSettings
    :: KinesisSettings
kinesisSettings =
  KinesisSettings'
    { _ksServiceAccessRoleARN = Nothing
    , _ksStreamARN = Nothing
    , _ksMessageFormat = Nothing
    }


-- | The Amazon Resource Name (ARN) for the IAM role that DMS uses to write to the Amazon Kinesis data stream.
ksServiceAccessRoleARN :: Lens' KinesisSettings (Maybe Text)
ksServiceAccessRoleARN = lens _ksServiceAccessRoleARN (\ s a -> s{_ksServiceAccessRoleARN = a})

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
ksStreamARN :: Lens' KinesisSettings (Maybe Text)
ksStreamARN = lens _ksStreamARN (\ s a -> s{_ksStreamARN = a})

-- | The output format for the records created on the endpoint. The message format is @JSON@ .
ksMessageFormat :: Lens' KinesisSettings (Maybe MessageFormatValue)
ksMessageFormat = lens _ksMessageFormat (\ s a -> s{_ksMessageFormat = a})

instance FromJSON KinesisSettings where
        parseJSON
          = withObject "KinesisSettings"
              (\ x ->
                 KinesisSettings' <$>
                   (x .:? "ServiceAccessRoleArn") <*>
                     (x .:? "StreamArn")
                     <*> (x .:? "MessageFormat"))

instance Hashable KinesisSettings where

instance NFData KinesisSettings where

instance ToJSON KinesisSettings where
        toJSON KinesisSettings'{..}
          = object
              (catMaybes
                 [("ServiceAccessRoleArn" .=) <$>
                    _ksServiceAccessRoleARN,
                  ("StreamArn" .=) <$> _ksStreamARN,
                  ("MessageFormat" .=) <$> _ksMessageFormat])

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
-- * 'mdsAuthMechanism' - The authentication mechanism you use to access the MongoDB source endpoint. Valid values: DEFAULT, MONGODB_CR, SCRAM_SHA_1  DEFAULT
