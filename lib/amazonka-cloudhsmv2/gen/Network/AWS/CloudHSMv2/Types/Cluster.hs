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
    cBackupPolicy,
    cBackupRetentionPolicy,
    cCertificates,
    cClusterId,
    cCreateTimestamp,
    cHsmType,
    cHsms,
    cPreCoPassword,
    cSecurityGroup,
    cSourceBackupId,
    cState,
    cStateMessage,
    cSubnetMapping,
    cTagList,
    cVpcId,
  )
where

import qualified Network.AWS.CloudHSMv2.Types.BackupPolicy as Types
import qualified Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy as Types
import qualified Network.AWS.CloudHSMv2.Types.Certificates as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterId as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterState as Types
import qualified Network.AWS.CloudHSMv2.Types.ExternalAz as Types
import qualified Network.AWS.CloudHSMv2.Types.Hsm as Types
import qualified Network.AWS.CloudHSMv2.Types.HsmType as Types
import qualified Network.AWS.CloudHSMv2.Types.PreCoPassword as Types
import qualified Network.AWS.CloudHSMv2.Types.SecurityGroup as Types
import qualified Network.AWS.CloudHSMv2.Types.SourceBackupId as Types
import qualified Network.AWS.CloudHSMv2.Types.StateMessage as Types
import qualified Network.AWS.CloudHSMv2.Types.SubnetId as Types
import qualified Network.AWS.CloudHSMv2.Types.Tag as Types
import qualified Network.AWS.CloudHSMv2.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AWS CloudHSM cluster.
--
-- /See:/ 'mkCluster' smart constructor.
data Cluster = Cluster'
  { -- | The cluster's backup policy.
    backupPolicy :: Core.Maybe Types.BackupPolicy,
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Core.Maybe Types.BackupRetentionPolicy,
    -- | Contains one or more certificates or a certificate signing request (CSR).
    certificates :: Core.Maybe Types.Certificates,
    -- | The cluster's identifier (ID).
    clusterId :: Core.Maybe Types.ClusterId,
    -- | The date and time when the cluster was created.
    createTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The type of HSM that the cluster contains.
    hsmType :: Core.Maybe Types.HsmType,
    -- | Contains information about the HSMs in the cluster.
    hsms :: Core.Maybe [Types.Hsm],
    -- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
    preCoPassword :: Core.Maybe Types.PreCoPassword,
    -- | The identifier (ID) of the cluster's security group.
    securityGroup :: Core.Maybe Types.SecurityGroup,
    -- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
    sourceBackupId :: Core.Maybe Types.SourceBackupId,
    -- | The cluster's state.
    state :: Core.Maybe Types.ClusterState,
    -- | A description of the cluster's state.
    stateMessage :: Core.Maybe Types.StateMessage,
    -- | A map from availability zone to the cluster’s subnet in that availability zone.
    subnetMapping :: Core.Maybe (Core.HashMap Types.ExternalAz Types.SubnetId),
    -- | The list of tags for the cluster.
    tagList :: Core.Maybe [Types.Tag],
    -- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Cluster' value with any optional fields omitted.
mkCluster ::
  Cluster
mkCluster =
  Cluster'
    { backupPolicy = Core.Nothing,
      backupRetentionPolicy = Core.Nothing,
      certificates = Core.Nothing,
      clusterId = Core.Nothing,
      createTimestamp = Core.Nothing,
      hsmType = Core.Nothing,
      hsms = Core.Nothing,
      preCoPassword = Core.Nothing,
      securityGroup = Core.Nothing,
      sourceBackupId = Core.Nothing,
      state = Core.Nothing,
      stateMessage = Core.Nothing,
      subnetMapping = Core.Nothing,
      tagList = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The cluster's backup policy.
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBackupPolicy :: Lens.Lens' Cluster (Core.Maybe Types.BackupPolicy)
cBackupPolicy = Lens.field @"backupPolicy"
{-# DEPRECATED cBackupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead." #-}

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBackupRetentionPolicy :: Lens.Lens' Cluster (Core.Maybe Types.BackupRetentionPolicy)
cBackupRetentionPolicy = Lens.field @"backupRetentionPolicy"
{-# DEPRECATED cBackupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead." #-}

-- | Contains one or more certificates or a certificate signing request (CSR).
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificates :: Lens.Lens' Cluster (Core.Maybe Types.Certificates)
cCertificates = Lens.field @"certificates"
{-# DEPRECATED cCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | The cluster's identifier (ID).
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterId :: Lens.Lens' Cluster (Core.Maybe Types.ClusterId)
cClusterId = Lens.field @"clusterId"
{-# DEPRECATED cClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The date and time when the cluster was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreateTimestamp :: Lens.Lens' Cluster (Core.Maybe Core.NominalDiffTime)
cCreateTimestamp = Lens.field @"createTimestamp"
{-# DEPRECATED cCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

-- | The type of HSM that the cluster contains.
--
-- /Note:/ Consider using 'hsmType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHsmType :: Lens.Lens' Cluster (Core.Maybe Types.HsmType)
cHsmType = Lens.field @"hsmType"
{-# DEPRECATED cHsmType "Use generic-lens or generic-optics with 'hsmType' instead." #-}

-- | Contains information about the HSMs in the cluster.
--
-- /Note:/ Consider using 'hsms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHsms :: Lens.Lens' Cluster (Core.Maybe [Types.Hsm])
cHsms = Lens.field @"hsms"
{-# DEPRECATED cHsms "Use generic-lens or generic-optics with 'hsms' instead." #-}

-- | The default password for the cluster's Pre-Crypto Officer (PRECO) user.
--
-- /Note:/ Consider using 'preCoPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPreCoPassword :: Lens.Lens' Cluster (Core.Maybe Types.PreCoPassword)
cPreCoPassword = Lens.field @"preCoPassword"
{-# DEPRECATED cPreCoPassword "Use generic-lens or generic-optics with 'preCoPassword' instead." #-}

-- | The identifier (ID) of the cluster's security group.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecurityGroup :: Lens.Lens' Cluster (Core.Maybe Types.SecurityGroup)
cSecurityGroup = Lens.field @"securityGroup"
{-# DEPRECATED cSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The identifier (ID) of the backup used to create the cluster. This value exists only when the cluster was created from a backup.
--
-- /Note:/ Consider using 'sourceBackupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceBackupId :: Lens.Lens' Cluster (Core.Maybe Types.SourceBackupId)
cSourceBackupId = Lens.field @"sourceBackupId"
{-# DEPRECATED cSourceBackupId "Use generic-lens or generic-optics with 'sourceBackupId' instead." #-}

-- | The cluster's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Cluster (Core.Maybe Types.ClusterState)
cState = Lens.field @"state"
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A description of the cluster's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStateMessage :: Lens.Lens' Cluster (Core.Maybe Types.StateMessage)
cStateMessage = Lens.field @"stateMessage"
{-# DEPRECATED cStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | A map from availability zone to the cluster’s subnet in that availability zone.
--
-- /Note:/ Consider using 'subnetMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnetMapping :: Lens.Lens' Cluster (Core.Maybe (Core.HashMap Types.ExternalAz Types.SubnetId))
cSubnetMapping = Lens.field @"subnetMapping"
{-# DEPRECATED cSubnetMapping "Use generic-lens or generic-optics with 'subnetMapping' instead." #-}

-- | The list of tags for the cluster.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTagList :: Lens.Lens' Cluster (Core.Maybe [Types.Tag])
cTagList = Lens.field @"tagList"
{-# DEPRECATED cTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the cluster.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcId :: Lens.Lens' Cluster (Core.Maybe Types.VpcId)
cVpcId = Lens.field @"vpcId"
{-# DEPRECATED cVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject "Cluster" Core.$
      \x ->
        Cluster'
          Core.<$> (x Core..:? "BackupPolicy")
          Core.<*> (x Core..:? "BackupRetentionPolicy")
          Core.<*> (x Core..:? "Certificates")
          Core.<*> (x Core..:? "ClusterId")
          Core.<*> (x Core..:? "CreateTimestamp")
          Core.<*> (x Core..:? "HsmType")
          Core.<*> (x Core..:? "Hsms")
          Core.<*> (x Core..:? "PreCoPassword")
          Core.<*> (x Core..:? "SecurityGroup")
          Core.<*> (x Core..:? "SourceBackupId")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "StateMessage")
          Core.<*> (x Core..:? "SubnetMapping")
          Core.<*> (x Core..:? "TagList")
          Core.<*> (x Core..:? "VpcId")
