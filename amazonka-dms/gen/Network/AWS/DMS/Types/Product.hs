{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Product where

import           Network.AWS.DMS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Describes a quota for an AWS account, for example, the number of replication instances allowed.
--
--
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
    { _aqMax              :: !(Maybe Integer)
    , _aqUsed             :: !(Maybe Integer)
    , _aqAccountQuotaName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _aqMax = Nothing
    , _aqUsed = Nothing
    , _aqAccountQuotaName = Nothing
    }

-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\ s a -> s{_aqMax = a});

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\ s a -> s{_aqUsed = a});

-- | The name of the AWS DMS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\ s a -> s{_aqAccountQuotaName = a});

instance FromJSON AccountQuota where
        parseJSON
          = withObject "AccountQuota"
              (\ x ->
                 AccountQuota' <$>
                   (x .:? "Max") <*> (x .:? "Used") <*>
                     (x .:? "AccountQuotaName"))

instance Hashable AccountQuota

instance NFData AccountQuota

-- |
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
    { _azName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the availability zone.
availabilityZone
    :: AvailabilityZone
availabilityZone =
    AvailabilityZone'
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a});

instance FromJSON AvailabilityZone where
        parseJSON
          = withObject "AvailabilityZone"
              (\ x -> AvailabilityZone' <$> (x .:? "Name"))

instance Hashable AvailabilityZone

instance NFData AvailabilityZone

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
    , _cKeyLength               :: !(Maybe Int)
    , _cValidToDate             :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateOwner' - The owner of the certificate.
--
-- * 'cSigningAlgorithm' - The signing algorithm for the certificate.
--
-- * 'cValidFromDate' - The beginning date the certificate is valid.
--
-- * 'cCertificatePem' - The contents of the .pem X.509 certificate file.
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) for the certificate.
--
-- * 'cCertificateCreationDate' - the date the certificate was created.
--
-- * 'cCertificateIdentifier' - The customer-assigned name of the certificate. Valid characters are [A-z_0-9].
--
-- * 'cKeyLength' - The key length of the cryptographic algorithm being used.
--
-- * 'cValidToDate' - the final date the certificate is valid.
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
    , _cKeyLength = Nothing
    , _cValidToDate = Nothing
    }

-- | The owner of the certificate.
cCertificateOwner :: Lens' Certificate (Maybe Text)
cCertificateOwner = lens _cCertificateOwner (\ s a -> s{_cCertificateOwner = a});

-- | The signing algorithm for the certificate.
cSigningAlgorithm :: Lens' Certificate (Maybe Text)
cSigningAlgorithm = lens _cSigningAlgorithm (\ s a -> s{_cSigningAlgorithm = a});

-- | The beginning date the certificate is valid.
cValidFromDate :: Lens' Certificate (Maybe UTCTime)
cValidFromDate = lens _cValidFromDate (\ s a -> s{_cValidFromDate = a}) . mapping _Time;

-- | The contents of the .pem X.509 certificate file.
cCertificatePem :: Lens' Certificate (Maybe Text)
cCertificatePem = lens _cCertificatePem (\ s a -> s{_cCertificatePem = a});

-- | The Amazon Resource Name (ARN) for the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a});

-- | the date the certificate was created.
cCertificateCreationDate :: Lens' Certificate (Maybe UTCTime)
cCertificateCreationDate = lens _cCertificateCreationDate (\ s a -> s{_cCertificateCreationDate = a}) . mapping _Time;

-- | The customer-assigned name of the certificate. Valid characters are [A-z_0-9].
cCertificateIdentifier :: Lens' Certificate (Maybe Text)
cCertificateIdentifier = lens _cCertificateIdentifier (\ s a -> s{_cCertificateIdentifier = a});

-- | The key length of the cryptographic algorithm being used.
cKeyLength :: Lens' Certificate (Maybe Int)
cKeyLength = lens _cKeyLength (\ s a -> s{_cKeyLength = a});

-- | the final date the certificate is valid.
cValidToDate :: Lens' Certificate (Maybe UTCTime)
cValidToDate = lens _cValidToDate (\ s a -> s{_cValidToDate = a}) . mapping _Time;

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
                     <*> (x .:? "KeyLength")
                     <*> (x .:? "ValidToDate"))

instance Hashable Certificate

instance NFData Certificate

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The Amazon Resource Name (ARN) of the replication instance.
cReplicationInstanceARN :: Lens' Connection (Maybe Text)
cReplicationInstanceARN = lens _cReplicationInstanceARN (\ s a -> s{_cReplicationInstanceARN = a});

-- | The identifier of the endpoint. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
cEndpointIdentifier :: Lens' Connection (Maybe Text)
cEndpointIdentifier = lens _cEndpointIdentifier (\ s a -> s{_cEndpointIdentifier = a});

-- | The replication instance identifier. This parameter is stored as a lowercase string.
cReplicationInstanceIdentifier :: Lens' Connection (Maybe Text)
cReplicationInstanceIdentifier = lens _cReplicationInstanceIdentifier (\ s a -> s{_cReplicationInstanceIdentifier = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
cEndpointARN :: Lens' Connection (Maybe Text)
cEndpointARN = lens _cEndpointARN (\ s a -> s{_cEndpointARN = a});

-- | The error message when the connection last failed.
cLastFailureMessage :: Lens' Connection (Maybe Text)
cLastFailureMessage = lens _cLastFailureMessage (\ s a -> s{_cLastFailureMessage = a});

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

instance Hashable Connection

instance NFData Connection

-- |
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
    { _eStatus                    :: !(Maybe Text)
    , _eServerName                :: !(Maybe Text)
    , _eCertificateARN            :: !(Maybe Text)
    , _eExtraConnectionAttributes :: !(Maybe Text)
    , _eEndpointType              :: !(Maybe ReplicationEndpointTypeValue)
    , _eUsername                  :: !(Maybe Text)
    , _eEngineName                :: !(Maybe Text)
    , _eKMSKeyId                  :: !(Maybe Text)
    , _eSSLMode                   :: !(Maybe DmsSSLModeValue)
    , _eDatabaseName              :: !(Maybe Text)
    , _eEndpointIdentifier        :: !(Maybe Text)
    , _eEndpointARN               :: !(Maybe Text)
    , _ePort                      :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- * 'eExtraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
--
-- * 'eEndpointType' - The type of endpoint.
--
-- * 'eUsername' - The user name used to connect to the endpoint.
--
-- * 'eEngineName' - The database engine name.
--
-- * 'eKMSKeyId' - The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
--
-- * 'eSSLMode' - The SSL mode used to connect to the endpoint. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
--
-- * 'eDatabaseName' - The name of the database at the endpoint.
--
-- * 'eEndpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
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
    , _eExtraConnectionAttributes = Nothing
    , _eEndpointType = Nothing
    , _eUsername = Nothing
    , _eEngineName = Nothing
    , _eKMSKeyId = Nothing
    , _eSSLMode = Nothing
    , _eDatabaseName = Nothing
    , _eEndpointIdentifier = Nothing
    , _eEndpointARN = Nothing
    , _ePort = Nothing
    }

-- | The status of the endpoint.
eStatus :: Lens' Endpoint (Maybe Text)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a});

-- | The name of the server at the endpoint.
eServerName :: Lens' Endpoint (Maybe Text)
eServerName = lens _eServerName (\ s a -> s{_eServerName = a});

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
eCertificateARN :: Lens' Endpoint (Maybe Text)
eCertificateARN = lens _eCertificateARN (\ s a -> s{_eCertificateARN = a});

-- | Additional connection attributes used to connect to the endpoint.
eExtraConnectionAttributes :: Lens' Endpoint (Maybe Text)
eExtraConnectionAttributes = lens _eExtraConnectionAttributes (\ s a -> s{_eExtraConnectionAttributes = a});

-- | The type of endpoint.
eEndpointType :: Lens' Endpoint (Maybe ReplicationEndpointTypeValue)
eEndpointType = lens _eEndpointType (\ s a -> s{_eEndpointType = a});

-- | The user name used to connect to the endpoint.
eUsername :: Lens' Endpoint (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a});

-- | The database engine name.
eEngineName :: Lens' Endpoint (Maybe Text)
eEngineName = lens _eEngineName (\ s a -> s{_eEngineName = a});

-- | The KMS key identifier that will be used to encrypt the connection parameters. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
eKMSKeyId :: Lens' Endpoint (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a});

-- | The SSL mode used to connect to the endpoint. SSL mode can be one of four values: none, require, verify-ca, verify-full.  The default value is none.
eSSLMode :: Lens' Endpoint (Maybe DmsSSLModeValue)
eSSLMode = lens _eSSLMode (\ s a -> s{_eSSLMode = a});

-- | The name of the database at the endpoint.
eDatabaseName :: Lens' Endpoint (Maybe Text)
eDatabaseName = lens _eDatabaseName (\ s a -> s{_eDatabaseName = a});

-- | The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
eEndpointIdentifier :: Lens' Endpoint (Maybe Text)
eEndpointIdentifier = lens _eEndpointIdentifier (\ s a -> s{_eEndpointIdentifier = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\ s a -> s{_eEndpointARN = a});

-- | The port value used to access the endpoint.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a});

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x ->
                 Endpoint' <$>
                   (x .:? "Status") <*> (x .:? "ServerName") <*>
                     (x .:? "CertificateArn")
                     <*> (x .:? "ExtraConnectionAttributes")
                     <*> (x .:? "EndpointType")
                     <*> (x .:? "Username")
                     <*> (x .:? "EngineName")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "SslMode")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "EndpointIdentifier")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "Port"))

instance Hashable Endpoint

instance NFData Endpoint

-- |
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
    { _fName   :: !Text
    , _fValues :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
filter' pName_ =
    Filter'
    { _fName = pName_
    , _fValues = mempty
    }

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a});

-- | The filter value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce;

instance Hashable Filter

instance NFData Filter

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
-- /See:/ 'orderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
    { _oriEngineVersion            :: !(Maybe Text)
    , _oriMinAllocatedStorage      :: !(Maybe Int)
    , _oriIncludedAllocatedStorage :: !(Maybe Int)
    , _oriMaxAllocatedStorage      :: !(Maybe Int)
    , _oriReplicationInstanceClass :: !(Maybe Text)
    , _oriDefaultAllocatedStorage  :: !(Maybe Int)
    , _oriStorageType              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
oriEngineVersion = lens _oriEngineVersion (\ s a -> s{_oriEngineVersion = a});

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMinAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMinAllocatedStorage = lens _oriMinAllocatedStorage (\ s a -> s{_oriMinAllocatedStorage = a});

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
oriIncludedAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriIncludedAllocatedStorage = lens _oriIncludedAllocatedStorage (\ s a -> s{_oriIncludedAllocatedStorage = a});

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
oriMaxAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriMaxAllocatedStorage = lens _oriMaxAllocatedStorage (\ s a -> s{_oriMaxAllocatedStorage = a});

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
oriReplicationInstanceClass :: Lens' OrderableReplicationInstance (Maybe Text)
oriReplicationInstanceClass = lens _oriReplicationInstanceClass (\ s a -> s{_oriReplicationInstanceClass = a});

-- | The default amount of storage (in gigabytes) that is allocated for the replication instance.
oriDefaultAllocatedStorage :: Lens' OrderableReplicationInstance (Maybe Int)
oriDefaultAllocatedStorage = lens _oriDefaultAllocatedStorage (\ s a -> s{_oriDefaultAllocatedStorage = a});

-- | The type of storage used by the replication instance.
oriStorageType :: Lens' OrderableReplicationInstance (Maybe Text)
oriStorageType = lens _oriStorageType (\ s a -> s{_oriStorageType = a});

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

instance Hashable OrderableReplicationInstance

instance NFData OrderableReplicationInstance

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
rssStatus = lens _rssStatus (\ s a -> s{_rssStatus = a});

-- | The date the schema was last refreshed.
rssLastRefreshDate :: Lens' RefreshSchemasStatus (Maybe UTCTime)
rssLastRefreshDate = lens _rssLastRefreshDate (\ s a -> s{_rssLastRefreshDate = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the replication instance.
rssReplicationInstanceARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssReplicationInstanceARN = lens _rssReplicationInstanceARN (\ s a -> s{_rssReplicationInstanceARN = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rssEndpointARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssEndpointARN = lens _rssEndpointARN (\ s a -> s{_rssEndpointARN = a});

-- | The last failure message for the schema.
rssLastFailureMessage :: Lens' RefreshSchemasStatus (Maybe Text)
rssLastFailureMessage = lens _rssLastFailureMessage (\ s a -> s{_rssLastFailureMessage = a});

instance FromJSON RefreshSchemasStatus where
        parseJSON
          = withObject "RefreshSchemasStatus"
              (\ x ->
                 RefreshSchemasStatus' <$>
                   (x .:? "Status") <*> (x .:? "LastRefreshDate") <*>
                     (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "EndpointArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable RefreshSchemasStatus

instance NFData RefreshSchemasStatus

-- |
--
--
--
-- /See:/ 'replicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
    { _riEngineVersion                         :: !(Maybe Text)
    , _riPubliclyAccessible                    :: !(Maybe Bool)
    , _riAutoMinorVersionUpgrade               :: !(Maybe Bool)
    , _riReplicationInstancePublicIPAddresses  :: !(Maybe [Text])
    , _riReplicationSubnetGroup                :: !(Maybe ReplicationSubnetGroup)
    , _riInstanceCreateTime                    :: !(Maybe POSIX)
    , _riReplicationInstanceStatus             :: !(Maybe Text)
    , _riReplicationInstancePrivateIPAddresses :: !(Maybe [Text])
    , _riPreferredMaintenanceWindow            :: !(Maybe Text)
    , _riReplicationInstancePrivateIPAddress   :: !(Maybe Text)
    , _riKMSKeyId                              :: !(Maybe Text)
    , _riAvailabilityZone                      :: !(Maybe Text)
    , _riVPCSecurityGroups                     :: !(Maybe [VPCSecurityGroupMembership])
    , _riMultiAZ                               :: !(Maybe Bool)
    , _riReplicationInstanceARN                :: !(Maybe Text)
    , _riAllocatedStorage                      :: !(Maybe Int)
    , _riReplicationInstancePublicIPAddress    :: !(Maybe Text)
    , _riReplicationInstanceClass              :: !(Maybe Text)
    , _riReplicationInstanceIdentifier         :: !(Maybe Text)
    , _riPendingModifiedValues                 :: !(Maybe ReplicationPendingModifiedValues)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    , _riReplicationInstanceStatus = Nothing
    , _riReplicationInstancePrivateIPAddresses = Nothing
    , _riPreferredMaintenanceWindow = Nothing
    , _riReplicationInstancePrivateIPAddress = Nothing
    , _riKMSKeyId = Nothing
    , _riAvailabilityZone = Nothing
    , _riVPCSecurityGroups = Nothing
    , _riMultiAZ = Nothing
    , _riReplicationInstanceARN = Nothing
    , _riAllocatedStorage = Nothing
    , _riReplicationInstancePublicIPAddress = Nothing
    , _riReplicationInstanceClass = Nothing
    , _riReplicationInstanceIdentifier = Nothing
    , _riPendingModifiedValues = Nothing
    }

-- | The engine version number of the replication instance.
riEngineVersion :: Lens' ReplicationInstance (Maybe Text)
riEngineVersion = lens _riEngineVersion (\ s a -> s{_riEngineVersion = a});

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
riPubliclyAccessible :: Lens' ReplicationInstance (Maybe Bool)
riPubliclyAccessible = lens _riPubliclyAccessible (\ s a -> s{_riPubliclyAccessible = a});

-- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
riAutoMinorVersionUpgrade :: Lens' ReplicationInstance (Maybe Bool)
riAutoMinorVersionUpgrade = lens _riAutoMinorVersionUpgrade (\ s a -> s{_riAutoMinorVersionUpgrade = a});

-- | The public IP address of the replication instance.
riReplicationInstancePublicIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePublicIPAddresses = lens _riReplicationInstancePublicIPAddresses (\ s a -> s{_riReplicationInstancePublicIPAddresses = a}) . _Default . _Coerce;

-- | The subnet group for the replication instance.
riReplicationSubnetGroup :: Lens' ReplicationInstance (Maybe ReplicationSubnetGroup)
riReplicationSubnetGroup = lens _riReplicationSubnetGroup (\ s a -> s{_riReplicationSubnetGroup = a});

-- | The time the replication instance was created.
riInstanceCreateTime :: Lens' ReplicationInstance (Maybe UTCTime)
riInstanceCreateTime = lens _riInstanceCreateTime (\ s a -> s{_riInstanceCreateTime = a}) . mapping _Time;

-- | The status of the replication instance.
riReplicationInstanceStatus :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceStatus = lens _riReplicationInstanceStatus (\ s a -> s{_riReplicationInstanceStatus = a});

-- | The private IP address of the replication instance.
riReplicationInstancePrivateIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePrivateIPAddresses = lens _riReplicationInstancePrivateIPAddresses (\ s a -> s{_riReplicationInstancePrivateIPAddresses = a}) . _Default . _Coerce;

-- | The maintenance window times for the replication instance.
riPreferredMaintenanceWindow :: Lens' ReplicationInstance (Maybe Text)
riPreferredMaintenanceWindow = lens _riPreferredMaintenanceWindow (\ s a -> s{_riPreferredMaintenanceWindow = a});

-- | The private IP address of the replication instance.
riReplicationInstancePrivateIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePrivateIPAddress = lens _riReplicationInstancePrivateIPAddress (\ s a -> s{_riReplicationInstancePrivateIPAddress = a});

-- | The KMS key identifier that is used to encrypt the content on the replication instance. If you do not specify a value for the KmsKeyId parameter, then AWS DMS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS region.
riKMSKeyId :: Lens' ReplicationInstance (Maybe Text)
riKMSKeyId = lens _riKMSKeyId (\ s a -> s{_riKMSKeyId = a});

-- | The Availability Zone for the instance.
riAvailabilityZone :: Lens' ReplicationInstance (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\ s a -> s{_riAvailabilityZone = a});

-- | The VPC security group for the instance.
riVPCSecurityGroups :: Lens' ReplicationInstance [VPCSecurityGroupMembership]
riVPCSecurityGroups = lens _riVPCSecurityGroups (\ s a -> s{_riVPCSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
riMultiAZ :: Lens' ReplicationInstance (Maybe Bool)
riMultiAZ = lens _riMultiAZ (\ s a -> s{_riMultiAZ = a});

-- | The Amazon Resource Name (ARN) of the replication instance.
riReplicationInstanceARN :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceARN = lens _riReplicationInstanceARN (\ s a -> s{_riReplicationInstanceARN = a});

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
riAllocatedStorage :: Lens' ReplicationInstance (Maybe Int)
riAllocatedStorage = lens _riAllocatedStorage (\ s a -> s{_riAllocatedStorage = a});

-- | The public IP address of the replication instance.
riReplicationInstancePublicIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePublicIPAddress = lens _riReplicationInstancePublicIPAddress (\ s a -> s{_riReplicationInstancePublicIPAddress = a});

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
riReplicationInstanceClass :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceClass = lens _riReplicationInstanceClass (\ s a -> s{_riReplicationInstanceClass = a});

-- | The replication instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
riReplicationInstanceIdentifier :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceIdentifier = lens _riReplicationInstanceIdentifier (\ s a -> s{_riReplicationInstanceIdentifier = a});

-- | The pending modification values.
riPendingModifiedValues :: Lens' ReplicationInstance (Maybe ReplicationPendingModifiedValues)
riPendingModifiedValues = lens _riPendingModifiedValues (\ s a -> s{_riPendingModifiedValues = a});

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
                     <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "AllocatedStorage")
                     <*> (x .:? "ReplicationInstancePublicIpAddress")
                     <*> (x .:? "ReplicationInstanceClass")
                     <*> (x .:? "ReplicationInstanceIdentifier")
                     <*> (x .:? "PendingModifiedValues"))

instance Hashable ReplicationInstance

instance NFData ReplicationInstance

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
rpmvEngineVersion = lens _rpmvEngineVersion (\ s a -> s{_rpmvEngineVersion = a});

-- | Specifies if the replication instance is a Multi-AZ deployment. You cannot set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
rpmvMultiAZ :: Lens' ReplicationPendingModifiedValues (Maybe Bool)
rpmvMultiAZ = lens _rpmvMultiAZ (\ s a -> s{_rpmvMultiAZ = a});

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
rpmvAllocatedStorage :: Lens' ReplicationPendingModifiedValues (Maybe Int)
rpmvAllocatedStorage = lens _rpmvAllocatedStorage (\ s a -> s{_rpmvAllocatedStorage = a});

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @
rpmvReplicationInstanceClass :: Lens' ReplicationPendingModifiedValues (Maybe Text)
rpmvReplicationInstanceClass = lens _rpmvReplicationInstanceClass (\ s a -> s{_rpmvReplicationInstanceClass = a});

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

instance NFData ReplicationPendingModifiedValues

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
rsgVPCId = lens _rsgVPCId (\ s a -> s{_rsgVPCId = a});

-- | The subnets that are in the subnet group.
rsgSubnets :: Lens' ReplicationSubnetGroup [Subnet]
rsgSubnets = lens _rsgSubnets (\ s a -> s{_rsgSubnets = a}) . _Default . _Coerce;

-- | The identifier of the replication instance subnet group.
rsgReplicationSubnetGroupIdentifier :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupIdentifier = lens _rsgReplicationSubnetGroupIdentifier (\ s a -> s{_rsgReplicationSubnetGroupIdentifier = a});

-- | The status of the subnet group.
rsgSubnetGroupStatus :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgSubnetGroupStatus = lens _rsgSubnetGroupStatus (\ s a -> s{_rsgSubnetGroupStatus = a});

-- | The description of the replication subnet group.
rsgReplicationSubnetGroupDescription :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupDescription = lens _rsgReplicationSubnetGroupDescription (\ s a -> s{_rsgReplicationSubnetGroupDescription = a});

instance FromJSON ReplicationSubnetGroup where
        parseJSON
          = withObject "ReplicationSubnetGroup"
              (\ x ->
                 ReplicationSubnetGroup' <$>
                   (x .:? "VpcId") <*> (x .:? "Subnets" .!= mempty) <*>
                     (x .:? "ReplicationSubnetGroupIdentifier")
                     <*> (x .:? "SubnetGroupStatus")
                     <*> (x .:? "ReplicationSubnetGroupDescription"))

instance Hashable ReplicationSubnetGroup

instance NFData ReplicationSubnetGroup

-- |
--
--
--
-- /See:/ 'replicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
    { _rtReplicationTaskSettings     :: !(Maybe Text)
    , _rtStatus                      :: !(Maybe Text)
    , _rtTargetEndpointARN           :: !(Maybe Text)
    , _rtReplicationTaskIdentifier   :: !(Maybe Text)
    , _rtReplicationTaskStartDate    :: !(Maybe POSIX)
    , _rtSourceEndpointARN           :: !(Maybe Text)
    , _rtTableMappings               :: !(Maybe Text)
    , _rtReplicationTaskCreationDate :: !(Maybe POSIX)
    , _rtMigrationType               :: !(Maybe MigrationTypeValue)
    , _rtReplicationTaskARN          :: !(Maybe Text)
    , _rtReplicationTaskStats        :: !(Maybe ReplicationTaskStats)
    , _rtReplicationInstanceARN      :: !(Maybe Text)
    , _rtLastFailureMessage          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtReplicationTaskSettings' - The settings for the replication task.
--
-- * 'rtStatus' - The status of the replication task.
--
-- * 'rtTargetEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rtReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'rtReplicationTaskStartDate' - The date the replication task is scheduled to start.
--
-- * 'rtSourceEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rtTableMappings' - Table mappings specified in the task.
--
-- * 'rtReplicationTaskCreationDate' - The date the replication task was created.
--
-- * 'rtMigrationType' - The type of migration.
--
-- * 'rtReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'rtReplicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- * 'rtReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rtLastFailureMessage' - The last error (failure) message generated for the replication instance.
replicationTask
    :: ReplicationTask
replicationTask =
    ReplicationTask'
    { _rtReplicationTaskSettings = Nothing
    , _rtStatus = Nothing
    , _rtTargetEndpointARN = Nothing
    , _rtReplicationTaskIdentifier = Nothing
    , _rtReplicationTaskStartDate = Nothing
    , _rtSourceEndpointARN = Nothing
    , _rtTableMappings = Nothing
    , _rtReplicationTaskCreationDate = Nothing
    , _rtMigrationType = Nothing
    , _rtReplicationTaskARN = Nothing
    , _rtReplicationTaskStats = Nothing
    , _rtReplicationInstanceARN = Nothing
    , _rtLastFailureMessage = Nothing
    }

-- | The settings for the replication task.
rtReplicationTaskSettings :: Lens' ReplicationTask (Maybe Text)
rtReplicationTaskSettings = lens _rtReplicationTaskSettings (\ s a -> s{_rtReplicationTaskSettings = a});

-- | The status of the replication task.
rtStatus :: Lens' ReplicationTask (Maybe Text)
rtStatus = lens _rtStatus (\ s a -> s{_rtStatus = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rtTargetEndpointARN :: Lens' ReplicationTask (Maybe Text)
rtTargetEndpointARN = lens _rtTargetEndpointARN (\ s a -> s{_rtTargetEndpointARN = a});

-- | The replication task identifier. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
rtReplicationTaskIdentifier :: Lens' ReplicationTask (Maybe Text)
rtReplicationTaskIdentifier = lens _rtReplicationTaskIdentifier (\ s a -> s{_rtReplicationTaskIdentifier = a});

-- | The date the replication task is scheduled to start.
rtReplicationTaskStartDate :: Lens' ReplicationTask (Maybe UTCTime)
rtReplicationTaskStartDate = lens _rtReplicationTaskStartDate (\ s a -> s{_rtReplicationTaskStartDate = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rtSourceEndpointARN :: Lens' ReplicationTask (Maybe Text)
rtSourceEndpointARN = lens _rtSourceEndpointARN (\ s a -> s{_rtSourceEndpointARN = a});

-- | Table mappings specified in the task.
rtTableMappings :: Lens' ReplicationTask (Maybe Text)
rtTableMappings = lens _rtTableMappings (\ s a -> s{_rtTableMappings = a});

-- | The date the replication task was created.
rtReplicationTaskCreationDate :: Lens' ReplicationTask (Maybe UTCTime)
rtReplicationTaskCreationDate = lens _rtReplicationTaskCreationDate (\ s a -> s{_rtReplicationTaskCreationDate = a}) . mapping _Time;

-- | The type of migration.
rtMigrationType :: Lens' ReplicationTask (Maybe MigrationTypeValue)
rtMigrationType = lens _rtMigrationType (\ s a -> s{_rtMigrationType = a});

-- | The Amazon Resource Name (ARN) of the replication task.
rtReplicationTaskARN :: Lens' ReplicationTask (Maybe Text)
rtReplicationTaskARN = lens _rtReplicationTaskARN (\ s a -> s{_rtReplicationTaskARN = a});

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
rtReplicationTaskStats :: Lens' ReplicationTask (Maybe ReplicationTaskStats)
rtReplicationTaskStats = lens _rtReplicationTaskStats (\ s a -> s{_rtReplicationTaskStats = a});

-- | The Amazon Resource Name (ARN) of the replication instance.
rtReplicationInstanceARN :: Lens' ReplicationTask (Maybe Text)
rtReplicationInstanceARN = lens _rtReplicationInstanceARN (\ s a -> s{_rtReplicationInstanceARN = a});

-- | The last error (failure) message generated for the replication instance.
rtLastFailureMessage :: Lens' ReplicationTask (Maybe Text)
rtLastFailureMessage = lens _rtLastFailureMessage (\ s a -> s{_rtLastFailureMessage = a});

instance FromJSON ReplicationTask where
        parseJSON
          = withObject "ReplicationTask"
              (\ x ->
                 ReplicationTask' <$>
                   (x .:? "ReplicationTaskSettings") <*>
                     (x .:? "Status")
                     <*> (x .:? "TargetEndpointArn")
                     <*> (x .:? "ReplicationTaskIdentifier")
                     <*> (x .:? "ReplicationTaskStartDate")
                     <*> (x .:? "SourceEndpointArn")
                     <*> (x .:? "TableMappings")
                     <*> (x .:? "ReplicationTaskCreationDate")
                     <*> (x .:? "MigrationType")
                     <*> (x .:? "ReplicationTaskArn")
                     <*> (x .:? "ReplicationTaskStats")
                     <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable ReplicationTask

instance NFData ReplicationTask

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
rtsFullLoadProgressPercent = lens _rtsFullLoadProgressPercent (\ s a -> s{_rtsFullLoadProgressPercent = a});

-- | The elapsed time of the task, in milliseconds.
rtsElapsedTimeMillis :: Lens' ReplicationTaskStats (Maybe Integer)
rtsElapsedTimeMillis = lens _rtsElapsedTimeMillis (\ s a -> s{_rtsElapsedTimeMillis = a});

-- | The number of errors that have occurred during this task.
rtsTablesErrored :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesErrored = lens _rtsTablesErrored (\ s a -> s{_rtsTablesErrored = a});

-- | The number of tables loaded for this task.
rtsTablesLoaded :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoaded = lens _rtsTablesLoaded (\ s a -> s{_rtsTablesLoaded = a});

-- | The number of tables queued for this task.
rtsTablesQueued :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesQueued = lens _rtsTablesQueued (\ s a -> s{_rtsTablesQueued = a});

-- | The number of tables currently loading for this task.
rtsTablesLoading :: Lens' ReplicationTaskStats (Maybe Int)
rtsTablesLoading = lens _rtsTablesLoading (\ s a -> s{_rtsTablesLoading = a});

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

instance Hashable ReplicationTaskStats

instance NFData ReplicationTaskStats

-- |
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
    { _sSubnetStatus           :: !(Maybe Text)
    , _sSubnetIdentifier       :: !(Maybe Text)
    , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a});

-- | The subnet identifier.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a});

-- | The Availability Zone of the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a});

instance FromJSON Subnet where
        parseJSON
          = withObject "Subnet"
              (\ x ->
                 Subnet' <$>
                   (x .:? "SubnetStatus") <*> (x .:? "SubnetIdentifier")
                     <*> (x .:? "SubnetAvailabilityZone"))

instance Hashable Subnet

instance NFData Subnet

-- |
--
--
--
-- /See:/ 'supportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
    { _setEndpointType :: !(Maybe ReplicationEndpointTypeValue)
    , _setEngineName   :: !(Maybe Text)
    , _setSupportsCDC  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SupportedEndpointType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setEndpointType' - The type of endpoint.
--
-- * 'setEngineName' - The database engine name.
--
-- * 'setSupportsCDC' - Indicates if Change Data Capture (CDC) is supported.
supportedEndpointType
    :: SupportedEndpointType
supportedEndpointType =
    SupportedEndpointType'
    { _setEndpointType = Nothing
    , _setEngineName = Nothing
    , _setSupportsCDC = Nothing
    }

-- | The type of endpoint.
setEndpointType :: Lens' SupportedEndpointType (Maybe ReplicationEndpointTypeValue)
setEndpointType = lens _setEndpointType (\ s a -> s{_setEndpointType = a});

-- | The database engine name.
setEngineName :: Lens' SupportedEndpointType (Maybe Text)
setEngineName = lens _setEngineName (\ s a -> s{_setEngineName = a});

-- | Indicates if Change Data Capture (CDC) is supported.
setSupportsCDC :: Lens' SupportedEndpointType (Maybe Bool)
setSupportsCDC = lens _setSupportsCDC (\ s a -> s{_setSupportsCDC = a});

instance FromJSON SupportedEndpointType where
        parseJSON
          = withObject "SupportedEndpointType"
              (\ x ->
                 SupportedEndpointType' <$>
                   (x .:? "EndpointType") <*> (x .:? "EngineName") <*>
                     (x .:? "SupportsCDC"))

instance Hashable SupportedEndpointType

instance NFData SupportedEndpointType

-- |
--
--
--
-- /See:/ 'tableStatistics' smart constructor.
data TableStatistics = TableStatistics'
    { _tsFullLoadRows   :: !(Maybe Integer)
    , _tsInserts        :: !(Maybe Integer)
    , _tsSchemaName     :: !(Maybe Text)
    , _tsTableState     :: !(Maybe Text)
    , _tsDdls           :: !(Maybe Integer)
    , _tsDeletes        :: !(Maybe Integer)
    , _tsUpdates        :: !(Maybe Integer)
    , _tsLastUpdateTime :: !(Maybe POSIX)
    , _tsTableName      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TableStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsFullLoadRows' - The number of rows added during the Full Load operation.
--
-- * 'tsInserts' - The number of insert actions performed on a table.
--
-- * 'tsSchemaName' - The schema name.
--
-- * 'tsTableState' - The state of the table.
--
-- * 'tsDdls' - The Data Definition Language (DDL) used to build and modify the structure of your tables.
--
-- * 'tsDeletes' - The number of delete actions performed on a table.
--
-- * 'tsUpdates' - The number of update actions performed on a table.
--
-- * 'tsLastUpdateTime' - The last time the table was updated.
--
-- * 'tsTableName' - The name of the table.
tableStatistics
    :: TableStatistics
tableStatistics =
    TableStatistics'
    { _tsFullLoadRows = Nothing
    , _tsInserts = Nothing
    , _tsSchemaName = Nothing
    , _tsTableState = Nothing
    , _tsDdls = Nothing
    , _tsDeletes = Nothing
    , _tsUpdates = Nothing
    , _tsLastUpdateTime = Nothing
    , _tsTableName = Nothing
    }

-- | The number of rows added during the Full Load operation.
tsFullLoadRows :: Lens' TableStatistics (Maybe Integer)
tsFullLoadRows = lens _tsFullLoadRows (\ s a -> s{_tsFullLoadRows = a});

-- | The number of insert actions performed on a table.
tsInserts :: Lens' TableStatistics (Maybe Integer)
tsInserts = lens _tsInserts (\ s a -> s{_tsInserts = a});

-- | The schema name.
tsSchemaName :: Lens' TableStatistics (Maybe Text)
tsSchemaName = lens _tsSchemaName (\ s a -> s{_tsSchemaName = a});

-- | The state of the table.
tsTableState :: Lens' TableStatistics (Maybe Text)
tsTableState = lens _tsTableState (\ s a -> s{_tsTableState = a});

-- | The Data Definition Language (DDL) used to build and modify the structure of your tables.
tsDdls :: Lens' TableStatistics (Maybe Integer)
tsDdls = lens _tsDdls (\ s a -> s{_tsDdls = a});

-- | The number of delete actions performed on a table.
tsDeletes :: Lens' TableStatistics (Maybe Integer)
tsDeletes = lens _tsDeletes (\ s a -> s{_tsDeletes = a});

-- | The number of update actions performed on a table.
tsUpdates :: Lens' TableStatistics (Maybe Integer)
tsUpdates = lens _tsUpdates (\ s a -> s{_tsUpdates = a});

-- | The last time the table was updated.
tsLastUpdateTime :: Lens' TableStatistics (Maybe UTCTime)
tsLastUpdateTime = lens _tsLastUpdateTime (\ s a -> s{_tsLastUpdateTime = a}) . mapping _Time;

-- | The name of the table.
tsTableName :: Lens' TableStatistics (Maybe Text)
tsTableName = lens _tsTableName (\ s a -> s{_tsTableName = a});

instance FromJSON TableStatistics where
        parseJSON
          = withObject "TableStatistics"
              (\ x ->
                 TableStatistics' <$>
                   (x .:? "FullLoadRows") <*> (x .:? "Inserts") <*>
                     (x .:? "SchemaName")
                     <*> (x .:? "TableState")
                     <*> (x .:? "Ddls")
                     <*> (x .:? "Deletes")
                     <*> (x .:? "Updates")
                     <*> (x .:? "LastUpdateTime")
                     <*> (x .:? "TableName"))

instance Hashable TableStatistics

instance NFData TableStatistics

-- |
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
--
-- * 'tagKey' - A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and cannot be prefixed with "aws:" or "dms:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag

instance NFData Tag

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _vsgmStatus = Nothing
    , _vsgmVPCSecurityGroupId = Nothing
    }

-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a});

-- | The VPC security group Id.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a});

instance FromJSON VPCSecurityGroupMembership where
        parseJSON
          = withObject "VPCSecurityGroupMembership"
              (\ x ->
                 VPCSecurityGroupMembership' <$>
                   (x .:? "Status") <*> (x .:? "VpcSecurityGroupId"))

instance Hashable VPCSecurityGroupMembership

instance NFData VPCSecurityGroupMembership
