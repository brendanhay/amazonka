{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.Product where

import Network.AWS.CloudHSMv2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a backup of an AWS CloudHSM cluster.
--
--
--
-- /See:/ 'backup' smart constructor.
data Backup = Backup'
  { _bClusterId       :: !(Maybe Text)
  , _bCreateTimestamp :: !(Maybe POSIX)
  , _bBackupState     :: !(Maybe BackupState)
  , _bBackupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bClusterId' - The identifier (ID) of the cluster that was backed up.
--
-- * 'bCreateTimestamp' - The date and time when the backup was created.
--
-- * 'bBackupState' - The state of the backup.
--
-- * 'bBackupId' - The identifier (ID) of the backup.
backup
    :: Text -- ^ 'bBackupId'
    -> Backup
backup pBackupId_ =
  Backup'
    { _bClusterId = Nothing
    , _bCreateTimestamp = Nothing
    , _bBackupState = Nothing
    , _bBackupId = pBackupId_
    }


-- | The identifier (ID) of the cluster that was backed up.
bClusterId :: Lens' Backup (Maybe Text)
bClusterId = lens _bClusterId (\ s a -> s{_bClusterId = a})

-- | The date and time when the backup was created.
bCreateTimestamp :: Lens' Backup (Maybe UTCTime)
bCreateTimestamp = lens _bCreateTimestamp (\ s a -> s{_bCreateTimestamp = a}) . mapping _Time

-- | The state of the backup.
bBackupState :: Lens' Backup (Maybe BackupState)
bBackupState = lens _bBackupState (\ s a -> s{_bBackupState = a})

-- | The identifier (ID) of the backup.
bBackupId :: Lens' Backup Text
bBackupId = lens _bBackupId (\ s a -> s{_bBackupId = a})

instance FromJSON Backup where
        parseJSON
          = withObject "Backup"
              (\ x ->
                 Backup' <$>
                   (x .:? "ClusterId") <*> (x .:? "CreateTimestamp") <*>
                     (x .:? "BackupState")
                     <*> (x .: "BackupId"))

instance Hashable Backup where

instance NFData Backup where

-- | Contains one or more certificates or a certificate signing request (CSR).
--
--
--
-- /See:/ 'certificates' smart constructor.
data Certificates = Certificates'
  { _cManufacturerHardwareCertificate :: !(Maybe Text)
  , _cClusterCSR                      :: !(Maybe Text)
  , _cHSMCertificate                  :: !(Maybe Text)
  , _cClusterCertificate              :: !(Maybe Text)
  , _cAWSHardwareCertificate          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cManufacturerHardwareCertificate' - The HSM hardware certificate issued (signed) by the hardware manufacturer.
--
-- * 'cClusterCSR' - The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
--
-- * 'cHSMCertificate' - The HSM certificate issued (signed) by the HSM hardware.
--
-- * 'cClusterCertificate' - The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
--
-- * 'cAWSHardwareCertificate' - The HSM hardware certificate issued (signed) by AWS CloudHSM.
certificates
    :: Certificates
certificates =
  Certificates'
    { _cManufacturerHardwareCertificate = Nothing
    , _cClusterCSR = Nothing
    , _cHSMCertificate = Nothing
    , _cClusterCertificate = Nothing
    , _cAWSHardwareCertificate = Nothing
    }


-- | The HSM hardware certificate issued (signed) by the hardware manufacturer.
cManufacturerHardwareCertificate :: Lens' Certificates (Maybe Text)
cManufacturerHardwareCertificate = lens _cManufacturerHardwareCertificate (\ s a -> s{_cManufacturerHardwareCertificate = a})

-- | The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
cClusterCSR :: Lens' Certificates (Maybe Text)
cClusterCSR = lens _cClusterCSR (\ s a -> s{_cClusterCSR = a})

-- | The HSM certificate issued (signed) by the HSM hardware.
cHSMCertificate :: Lens' Certificates (Maybe Text)
cHSMCertificate = lens _cHSMCertificate (\ s a -> s{_cHSMCertificate = a})

-- | The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
cClusterCertificate :: Lens' Certificates (Maybe Text)
cClusterCertificate = lens _cClusterCertificate (\ s a -> s{_cClusterCertificate = a})

-- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
cAWSHardwareCertificate :: Lens' Certificates (Maybe Text)
cAWSHardwareCertificate = lens _cAWSHardwareCertificate (\ s a -> s{_cAWSHardwareCertificate = a})

instance FromJSON Certificates where
        parseJSON
          = withObject "Certificates"
              (\ x ->
                 Certificates' <$>
                   (x .:? "ManufacturerHardwareCertificate") <*>
                     (x .:? "ClusterCsr")
                     <*> (x .:? "HsmCertificate")
                     <*> (x .:? "ClusterCertificate")
                     <*> (x .:? "AwsHardwareCertificate"))

instance Hashable Certificates where

instance NFData Certificates where

-- | Contains information about an AWS CloudHSM cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cPreCoPassword   :: !(Maybe Text)
  , _cStateMessage    :: !(Maybe Text)
  , _cState           :: !(Maybe ClusterState)
  , _cSubnetMapping   :: !(Maybe (Map Text Text))
  , _cHSMs            :: !(Maybe [HSM])
  , _cVPCId           :: !(Maybe Text)
  , _cSourceBackupId  :: !(Maybe Text)
  , _cCertificates    :: !(Maybe Certificates)
  , _cSecurityGroup   :: !(Maybe Text)
  , _cClusterId       :: !(Maybe Text)
  , _cCreateTimestamp :: !(Maybe POSIX)
  , _cBackupPolicy    :: !(Maybe BackupPolicy)
  , _cHSMType         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPreCoPassword' - The default password for the cluster's Pre-Crypto Officer (PRECO) user.
--
-- * 'cStateMessage' - A description of the cluster's state.
--
-- * 'cState' - The cluster's state.
--
-- * 'cSubnetMapping' - A map of the cluster's subnets and their corresponding Availability Zones.
--
-- * 'cHSMs' - Contains information about the HSMs in the cluster.
--
-- * 'cVPCId' - The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
--
-- * 'cSourceBackupId' - The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
--
-- * 'cCertificates' - Contains one or more certificates or a certificate signing request (CSR).
--
-- * 'cSecurityGroup' - The identifier (ID) of the cluster's security group.
--
-- * 'cClusterId' - The cluster's identifier (ID).
--
-- * 'cCreateTimestamp' - The date and time when the cluster was created.
--
-- * 'cBackupPolicy' - The cluster's backup policy.
--
-- * 'cHSMType' - The type of HSM that the cluster contains.
cluster
    :: Cluster
cluster =
  Cluster'
    { _cPreCoPassword = Nothing
    , _cStateMessage = Nothing
    , _cState = Nothing
    , _cSubnetMapping = Nothing
    , _cHSMs = Nothing
    , _cVPCId = Nothing
    , _cSourceBackupId = Nothing
    , _cCertificates = Nothing
    , _cSecurityGroup = Nothing
    , _cClusterId = Nothing
    , _cCreateTimestamp = Nothing
    , _cBackupPolicy = Nothing
    , _cHSMType = Nothing
    }


-- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
cPreCoPassword :: Lens' Cluster (Maybe Text)
cPreCoPassword = lens _cPreCoPassword (\ s a -> s{_cPreCoPassword = a})

-- | A description of the cluster's state.
cStateMessage :: Lens' Cluster (Maybe Text)
cStateMessage = lens _cStateMessage (\ s a -> s{_cStateMessage = a})

-- | The cluster's state.
cState :: Lens' Cluster (Maybe ClusterState)
cState = lens _cState (\ s a -> s{_cState = a})

-- | A map of the cluster's subnets and their corresponding Availability Zones.
cSubnetMapping :: Lens' Cluster (HashMap Text Text)
cSubnetMapping = lens _cSubnetMapping (\ s a -> s{_cSubnetMapping = a}) . _Default . _Map

-- | Contains information about the HSMs in the cluster.
cHSMs :: Lens' Cluster [HSM]
cHSMs = lens _cHSMs (\ s a -> s{_cHSMs = a}) . _Default . _Coerce

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
cVPCId :: Lens' Cluster (Maybe Text)
cVPCId = lens _cVPCId (\ s a -> s{_cVPCId = a})

-- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
cSourceBackupId :: Lens' Cluster (Maybe Text)
cSourceBackupId = lens _cSourceBackupId (\ s a -> s{_cSourceBackupId = a})

-- | Contains one or more certificates or a certificate signing request (CSR).
cCertificates :: Lens' Cluster (Maybe Certificates)
cCertificates = lens _cCertificates (\ s a -> s{_cCertificates = a})

-- | The identifier (ID) of the cluster's security group.
cSecurityGroup :: Lens' Cluster (Maybe Text)
cSecurityGroup = lens _cSecurityGroup (\ s a -> s{_cSecurityGroup = a})

-- | The cluster's identifier (ID).
cClusterId :: Lens' Cluster (Maybe Text)
cClusterId = lens _cClusterId (\ s a -> s{_cClusterId = a})

-- | The date and time when the cluster was created.
cCreateTimestamp :: Lens' Cluster (Maybe UTCTime)
cCreateTimestamp = lens _cCreateTimestamp (\ s a -> s{_cCreateTimestamp = a}) . mapping _Time

-- | The cluster's backup policy.
cBackupPolicy :: Lens' Cluster (Maybe BackupPolicy)
cBackupPolicy = lens _cBackupPolicy (\ s a -> s{_cBackupPolicy = a})

-- | The type of HSM that the cluster contains.
cHSMType :: Lens' Cluster (Maybe Text)
cHSMType = lens _cHSMType (\ s a -> s{_cHSMType = a})

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "PreCoPassword") <*> (x .:? "StateMessage")
                     <*> (x .:? "State")
                     <*> (x .:? "SubnetMapping" .!= mempty)
                     <*> (x .:? "Hsms" .!= mempty)
                     <*> (x .:? "VpcId")
                     <*> (x .:? "SourceBackupId")
                     <*> (x .:? "Certificates")
                     <*> (x .:? "SecurityGroup")
                     <*> (x .:? "ClusterId")
                     <*> (x .:? "CreateTimestamp")
                     <*> (x .:? "BackupPolicy")
                     <*> (x .:? "HsmType"))

instance Hashable Cluster where

instance NFData Cluster where

-- | Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.
--
--
--
-- /See:/ 'hsm' smart constructor.
data HSM = HSM'
  { _hsmStateMessage     :: !(Maybe Text)
  , _hsmState            :: !(Maybe HSMState)
  , _hsmEniId            :: !(Maybe Text)
  , _hsmSubnetId         :: !(Maybe Text)
  , _hsmAvailabilityZone :: !(Maybe Text)
  , _hsmClusterId        :: !(Maybe Text)
  , _hsmEniIP            :: !(Maybe Text)
  , _hsmHSMId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsmStateMessage' - A description of the HSM's state.
--
-- * 'hsmState' - The HSM's state.
--
-- * 'hsmEniId' - The identifier (ID) of the HSM's elastic network interface (ENI).
--
-- * 'hsmSubnetId' - The subnet that contains the HSM's elastic network interface (ENI).
--
-- * 'hsmAvailabilityZone' - The Availability Zone that contains the HSM.
--
-- * 'hsmClusterId' - The identifier (ID) of the cluster that contains the HSM.
--
-- * 'hsmEniIP' - The IP address of the HSM's elastic network interface (ENI).
--
-- * 'hsmHSMId' - The HSM's identifier (ID).
hsm
    :: Text -- ^ 'hsmHSMId'
    -> HSM
hsm pHSMId_ =
  HSM'
    { _hsmStateMessage = Nothing
    , _hsmState = Nothing
    , _hsmEniId = Nothing
    , _hsmSubnetId = Nothing
    , _hsmAvailabilityZone = Nothing
    , _hsmClusterId = Nothing
    , _hsmEniIP = Nothing
    , _hsmHSMId = pHSMId_
    }


-- | A description of the HSM's state.
hsmStateMessage :: Lens' HSM (Maybe Text)
hsmStateMessage = lens _hsmStateMessage (\ s a -> s{_hsmStateMessage = a})

-- | The HSM's state.
hsmState :: Lens' HSM (Maybe HSMState)
hsmState = lens _hsmState (\ s a -> s{_hsmState = a})

-- | The identifier (ID) of the HSM's elastic network interface (ENI).
hsmEniId :: Lens' HSM (Maybe Text)
hsmEniId = lens _hsmEniId (\ s a -> s{_hsmEniId = a})

-- | The subnet that contains the HSM's elastic network interface (ENI).
hsmSubnetId :: Lens' HSM (Maybe Text)
hsmSubnetId = lens _hsmSubnetId (\ s a -> s{_hsmSubnetId = a})

-- | The Availability Zone that contains the HSM.
hsmAvailabilityZone :: Lens' HSM (Maybe Text)
hsmAvailabilityZone = lens _hsmAvailabilityZone (\ s a -> s{_hsmAvailabilityZone = a})

-- | The identifier (ID) of the cluster that contains the HSM.
hsmClusterId :: Lens' HSM (Maybe Text)
hsmClusterId = lens _hsmClusterId (\ s a -> s{_hsmClusterId = a})

-- | The IP address of the HSM's elastic network interface (ENI).
hsmEniIP :: Lens' HSM (Maybe Text)
hsmEniIP = lens _hsmEniIP (\ s a -> s{_hsmEniIP = a})

-- | The HSM's identifier (ID).
hsmHSMId :: Lens' HSM Text
hsmHSMId = lens _hsmHSMId (\ s a -> s{_hsmHSMId = a})

instance FromJSON HSM where
        parseJSON
          = withObject "HSM"
              (\ x ->
                 HSM' <$>
                   (x .:? "StateMessage") <*> (x .:? "State") <*>
                     (x .:? "EniId")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "ClusterId")
                     <*> (x .:? "EniIp")
                     <*> (x .: "HsmId"))

instance Hashable HSM where

instance NFData HSM where

-- | Contains a tag. A tag is a key-value pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key of the tag.
--
-- * 'tagValue' - The value of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])
