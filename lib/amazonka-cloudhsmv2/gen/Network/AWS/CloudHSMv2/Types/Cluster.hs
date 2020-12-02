{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Cluster where

import Network.AWS.CloudHSMv2.Types.BackupPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.ClusterState
import Network.AWS.CloudHSMv2.Types.HSM
import Network.AWS.CloudHSMv2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an AWS CloudHSM cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cPreCoPassword :: !(Maybe Text),
    _cStateMessage :: !(Maybe Text),
    _cState :: !(Maybe ClusterState),
    _cSubnetMapping :: !(Maybe (Map Text (Text))),
    _cBackupRetentionPolicy :: !(Maybe BackupRetentionPolicy),
    _cHSMs :: !(Maybe [HSM]),
    _cVPCId :: !(Maybe Text),
    _cTagList :: !(Maybe [Tag]),
    _cSourceBackupId :: !(Maybe Text),
    _cCertificates :: !(Maybe Certificates),
    _cSecurityGroup :: !(Maybe Text),
    _cClusterId :: !(Maybe Text),
    _cCreateTimestamp :: !(Maybe POSIX),
    _cBackupPolicy :: !(Maybe BackupPolicy),
    _cHSMType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'cSubnetMapping' - A map from availability zone to the cluster’s subnet in that availability zone.
--
-- * 'cBackupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- * 'cHSMs' - Contains information about the HSMs in the cluster.
--
-- * 'cVPCId' - The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
--
-- * 'cTagList' - The list of tags for the cluster.
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
cluster ::
  Cluster
cluster =
  Cluster'
    { _cPreCoPassword = Nothing,
      _cStateMessage = Nothing,
      _cState = Nothing,
      _cSubnetMapping = Nothing,
      _cBackupRetentionPolicy = Nothing,
      _cHSMs = Nothing,
      _cVPCId = Nothing,
      _cTagList = Nothing,
      _cSourceBackupId = Nothing,
      _cCertificates = Nothing,
      _cSecurityGroup = Nothing,
      _cClusterId = Nothing,
      _cCreateTimestamp = Nothing,
      _cBackupPolicy = Nothing,
      _cHSMType = Nothing
    }

-- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
cPreCoPassword :: Lens' Cluster (Maybe Text)
cPreCoPassword = lens _cPreCoPassword (\s a -> s {_cPreCoPassword = a})

-- | A description of the cluster's state.
cStateMessage :: Lens' Cluster (Maybe Text)
cStateMessage = lens _cStateMessage (\s a -> s {_cStateMessage = a})

-- | The cluster's state.
cState :: Lens' Cluster (Maybe ClusterState)
cState = lens _cState (\s a -> s {_cState = a})

-- | A map from availability zone to the cluster’s subnet in that availability zone.
cSubnetMapping :: Lens' Cluster (HashMap Text (Text))
cSubnetMapping = lens _cSubnetMapping (\s a -> s {_cSubnetMapping = a}) . _Default . _Map

-- | A policy that defines how the service retains backups.
cBackupRetentionPolicy :: Lens' Cluster (Maybe BackupRetentionPolicy)
cBackupRetentionPolicy = lens _cBackupRetentionPolicy (\s a -> s {_cBackupRetentionPolicy = a})

-- | Contains information about the HSMs in the cluster.
cHSMs :: Lens' Cluster [HSM]
cHSMs = lens _cHSMs (\s a -> s {_cHSMs = a}) . _Default . _Coerce

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
cVPCId :: Lens' Cluster (Maybe Text)
cVPCId = lens _cVPCId (\s a -> s {_cVPCId = a})

-- | The list of tags for the cluster.
cTagList :: Lens' Cluster [Tag]
cTagList = lens _cTagList (\s a -> s {_cTagList = a}) . _Default . _Coerce

-- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
cSourceBackupId :: Lens' Cluster (Maybe Text)
cSourceBackupId = lens _cSourceBackupId (\s a -> s {_cSourceBackupId = a})

-- | Contains one or more certificates or a certificate signing request (CSR).
cCertificates :: Lens' Cluster (Maybe Certificates)
cCertificates = lens _cCertificates (\s a -> s {_cCertificates = a})

-- | The identifier (ID) of the cluster's security group.
cSecurityGroup :: Lens' Cluster (Maybe Text)
cSecurityGroup = lens _cSecurityGroup (\s a -> s {_cSecurityGroup = a})

-- | The cluster's identifier (ID).
cClusterId :: Lens' Cluster (Maybe Text)
cClusterId = lens _cClusterId (\s a -> s {_cClusterId = a})

-- | The date and time when the cluster was created.
cCreateTimestamp :: Lens' Cluster (Maybe UTCTime)
cCreateTimestamp = lens _cCreateTimestamp (\s a -> s {_cCreateTimestamp = a}) . mapping _Time

-- | The cluster's backup policy.
cBackupPolicy :: Lens' Cluster (Maybe BackupPolicy)
cBackupPolicy = lens _cBackupPolicy (\s a -> s {_cBackupPolicy = a})

-- | The type of HSM that the cluster contains.
cHSMType :: Lens' Cluster (Maybe Text)
cHSMType = lens _cHSMType (\s a -> s {_cHSMType = a})

instance FromJSON Cluster where
  parseJSON =
    withObject
      "Cluster"
      ( \x ->
          Cluster'
            <$> (x .:? "PreCoPassword")
            <*> (x .:? "StateMessage")
            <*> (x .:? "State")
            <*> (x .:? "SubnetMapping" .!= mempty)
            <*> (x .:? "BackupRetentionPolicy")
            <*> (x .:? "Hsms" .!= mempty)
            <*> (x .:? "VpcId")
            <*> (x .:? "TagList" .!= mempty)
            <*> (x .:? "SourceBackupId")
            <*> (x .:? "Certificates")
            <*> (x .:? "SecurityGroup")
            <*> (x .:? "ClusterId")
            <*> (x .:? "CreateTimestamp")
            <*> (x .:? "BackupPolicy")
            <*> (x .:? "HsmType")
      )

instance Hashable Cluster

instance NFData Cluster
