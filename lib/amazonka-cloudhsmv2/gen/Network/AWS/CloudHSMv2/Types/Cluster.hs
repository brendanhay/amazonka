{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Cluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Cluster
  ( Cluster (..),

    -- * Smart constructor
    mkCluster,

    -- * Lenses
    cPreCoPassword,
    cStateMessage,
    cState,
    cSubnetMapping,
    cBackupRetentionPolicy,
    cHSMs,
    cVPCId,
    cTagList,
    cSourceBackupId,
    cCertificates,
    cSecurityGroup,
    cClusterId,
    cCreateTimestamp,
    cBackupPolicy,
    cHSMType,
  )
where

import Network.AWS.CloudHSMv2.Types.BackupPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.ClusterState
import Network.AWS.CloudHSMv2.Types.HSM
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an AWS CloudHSM cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
    preCoPassword :: Lude.Maybe Lude.Text,
    -- | A description of the cluster's state.
    stateMessage :: Lude.Maybe Lude.Text,
    -- | The cluster's state.
    state :: Lude.Maybe ClusterState,
    -- | A map from availability zone to the cluster’s subnet in that availability zone.
    subnetMapping :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Lude.Maybe BackupRetentionPolicy,
    -- | Contains information about the HSMs in the cluster.
    hsms :: Lude.Maybe [HSM],
    -- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The list of tags for the cluster.
    tagList :: Lude.Maybe [Tag],
    -- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
    sourceBackupId :: Lude.Maybe Lude.Text,
    -- | Contains one or more certificates or a certificate signing request (CSR).
    certificates :: Lude.Maybe Certificates,
    -- | The identifier (ID) of the cluster's security group.
    securityGroup :: Lude.Maybe Lude.Text,
    -- | The cluster's identifier (ID).
    clusterId :: Lude.Maybe Lude.Text,
    -- | The date and time when the cluster was created.
    createTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The cluster's backup policy.
    backupPolicy :: Lude.Maybe BackupPolicy,
    -- | The type of HSM that the cluster contains.
    hsmType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- * 'preCoPassword' - The default password for the cluster's Pre-Crypto Officer (PRECO) user.
-- * 'stateMessage' - A description of the cluster's state.
-- * 'state' - The cluster's state.
-- * 'subnetMapping' - A map from availability zone to the cluster’s subnet in that availability zone.
-- * 'backupRetentionPolicy' - A policy that defines how the service retains backups.
-- * 'hsms' - Contains information about the HSMs in the cluster.
-- * 'vpcId' - The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
-- * 'tagList' - The list of tags for the cluster.
-- * 'sourceBackupId' - The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
-- * 'certificates' - Contains one or more certificates or a certificate signing request (CSR).
-- * 'securityGroup' - The identifier (ID) of the cluster's security group.
-- * 'clusterId' - The cluster's identifier (ID).
-- * 'createTimestamp' - The date and time when the cluster was created.
-- * 'backupPolicy' - The cluster's backup policy.
-- * 'hsmType' - The type of HSM that the cluster contains.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { preCoPassword = Lude.Nothing,
      stateMessage = Lude.Nothing,
      state = Lude.Nothing,
      subnetMapping = Lude.Nothing,
      backupRetentionPolicy = Lude.Nothing,
      hsms = Lude.Nothing,
      vpcId = Lude.Nothing,
      tagList = Lude.Nothing,
      sourceBackupId = Lude.Nothing,
      certificates = Lude.Nothing,
      securityGroup = Lude.Nothing,
      clusterId = Lude.Nothing,
      createTimestamp = Lude.Nothing,
      backupPolicy = Lude.Nothing,
      hsmType = Lude.Nothing
    }

-- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
--
-- /Note:/ Consider using 'preCoPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreCoPassword :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cPreCoPassword = Lens.lens (preCoPassword :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {preCoPassword = a} :: Cluster)
{-# DEPRECATED cPreCoPassword "Use generic-lens or generic-optics with 'preCoPassword' instead." #-}

-- | A description of the cluster's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStateMessage :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cStateMessage = Lens.lens (stateMessage :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: Cluster)
{-# DEPRECATED cStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The cluster's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Cluster (Lude.Maybe ClusterState)
cState = Lens.lens (state :: Cluster -> Lude.Maybe ClusterState) (\s a -> s {state = a} :: Cluster)
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A map from availability zone to the cluster’s subnet in that availability zone.
--
-- /Note:/ Consider using 'subnetMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnetMapping :: Lens.Lens' Cluster (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cSubnetMapping = Lens.lens (subnetMapping :: Cluster -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {subnetMapping = a} :: Cluster)
{-# DEPRECATED cSubnetMapping "Use generic-lens or generic-optics with 'subnetMapping' instead." #-}

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBackupRetentionPolicy :: Lens.Lens' Cluster (Lude.Maybe BackupRetentionPolicy)
cBackupRetentionPolicy = Lens.lens (backupRetentionPolicy :: Cluster -> Lude.Maybe BackupRetentionPolicy) (\s a -> s {backupRetentionPolicy = a} :: Cluster)
{-# DEPRECATED cBackupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead." #-}

-- | Contains information about the HSMs in the cluster.
--
-- /Note:/ Consider using 'hsms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHSMs :: Lens.Lens' Cluster (Lude.Maybe [HSM])
cHSMs = Lens.lens (hsms :: Cluster -> Lude.Maybe [HSM]) (\s a -> s {hsms = a} :: Cluster)
{-# DEPRECATED cHSMs "Use generic-lens or generic-optics with 'hsms' instead." #-}

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVPCId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cVPCId = Lens.lens (vpcId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Cluster)
{-# DEPRECATED cVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The list of tags for the cluster.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTagList :: Lens.Lens' Cluster (Lude.Maybe [Tag])
cTagList = Lens.lens (tagList :: Cluster -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: Cluster)
{-# DEPRECATED cTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
--
-- /Note:/ Consider using 'sourceBackupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceBackupId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cSourceBackupId = Lens.lens (sourceBackupId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {sourceBackupId = a} :: Cluster)
{-# DEPRECATED cSourceBackupId "Use generic-lens or generic-optics with 'sourceBackupId' instead." #-}

-- | Contains one or more certificates or a certificate signing request (CSR).
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificates :: Lens.Lens' Cluster (Lude.Maybe Certificates)
cCertificates = Lens.lens (certificates :: Cluster -> Lude.Maybe Certificates) (\s a -> s {certificates = a} :: Cluster)
{-# DEPRECATED cCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The identifier (ID) of the cluster's security group.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecurityGroup :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cSecurityGroup = Lens.lens (securityGroup :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {securityGroup = a} :: Cluster)
{-# DEPRECATED cSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The cluster's identifier (ID).
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterId :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cClusterId = Lens.lens (clusterId :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: Cluster)
{-# DEPRECATED cClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The date and time when the cluster was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreateTimestamp :: Lens.Lens' Cluster (Lude.Maybe Lude.Timestamp)
cCreateTimestamp = Lens.lens (createTimestamp :: Cluster -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTimestamp = a} :: Cluster)
{-# DEPRECATED cCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

-- | The cluster's backup policy.
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBackupPolicy :: Lens.Lens' Cluster (Lude.Maybe BackupPolicy)
cBackupPolicy = Lens.lens (backupPolicy :: Cluster -> Lude.Maybe BackupPolicy) (\s a -> s {backupPolicy = a} :: Cluster)
{-# DEPRECATED cBackupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead." #-}

-- | The type of HSM that the cluster contains.
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHSMType :: Lens.Lens' Cluster (Lude.Maybe Lude.Text)
cHSMType = Lens.lens (hsmType :: Cluster -> Lude.Maybe Lude.Text) (\s a -> s {hsmType = a} :: Cluster)
{-# DEPRECATED cHSMType "Use generic-lens or generic-optics with 'hsmType' instead." #-}

instance Lude.FromJSON Cluster where
  parseJSON =
    Lude.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Lude.<$> (x Lude..:? "PreCoPassword")
            Lude.<*> (x Lude..:? "StateMessage")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "SubnetMapping" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "BackupRetentionPolicy")
            Lude.<*> (x Lude..:? "Hsms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "TagList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SourceBackupId")
            Lude.<*> (x Lude..:? "Certificates")
            Lude.<*> (x Lude..:? "SecurityGroup")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "CreateTimestamp")
            Lude.<*> (x Lude..:? "BackupPolicy")
            Lude.<*> (x Lude..:? "HsmType")
      )
