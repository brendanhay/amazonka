{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaDescription
  ( ReplicaDescription (..),

    -- * Smart constructor
    mkReplicaDescription,

    -- * Lenses
    rdReplicaStatus,
    rdRegionName,
    rdReplicaStatusPercentProgress,
    rdReplicaStatusDescription,
    rdReplicaInaccessibleDateTime,
    rdKMSMasterKeyId,
    rdProvisionedThroughputOverride,
    rdGlobalSecondaryIndexes,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of the replica.
--
-- /See:/ 'mkReplicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { -- | The current state of the replica:
    --
    --
    --     * @CREATING@ - The replica is being created.
    --
    --
    --     * @UPDATING@ - The replica is being updated.
    --
    --
    --     * @DELETING@ - The replica is being deleted.
    --
    --
    --     * @ACTIVE@ - The replica is ready for use.
    --
    --
    --     * @REGION_DISABLED@ - The replica is inaccessible because the AWS Region has been disabled.
    --
    --
    --     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to encrypt the table is inaccessible.
    replicaStatus :: Lude.Maybe ReplicaStatus,
    -- | The name of the Region.
    regionName :: Lude.Maybe Lude.Text,
    -- | Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
    replicaStatusPercentProgress :: Lude.Maybe Lude.Text,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Lude.Maybe Lude.Text,
    -- | The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
    replicaInaccessibleDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
    kmsMasterKeyId :: Lude.Maybe Lude.Text,
    -- | Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
    provisionedThroughputOverride :: Lude.Maybe ProvisionedThroughputOverride,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Lude.Maybe [ReplicaGlobalSecondaryIndexDescription]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaDescription' with the minimum fields required to make a request.
--
-- * 'replicaStatus' - The current state of the replica:
--
--
--     * @CREATING@ - The replica is being created.
--
--
--     * @UPDATING@ - The replica is being updated.
--
--
--     * @DELETING@ - The replica is being deleted.
--
--
--     * @ACTIVE@ - The replica is ready for use.
--
--
--     * @REGION_DISABLED@ - The replica is inaccessible because the AWS Region has been disabled.
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to encrypt the table is inaccessible.
--
--
-- * 'regionName' - The name of the Region.
-- * 'replicaStatusPercentProgress' - Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
-- * 'replicaStatusDescription' - Detailed information about the replica status.
-- * 'replicaInaccessibleDateTime' - The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
-- * 'kmsMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
-- * 'provisionedThroughputOverride' - Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
-- * 'globalSecondaryIndexes' - Replica-specific global secondary index settings.
mkReplicaDescription ::
  ReplicaDescription
mkReplicaDescription =
  ReplicaDescription'
    { replicaStatus = Lude.Nothing,
      regionName = Lude.Nothing,
      replicaStatusPercentProgress = Lude.Nothing,
      replicaStatusDescription = Lude.Nothing,
      replicaInaccessibleDateTime = Lude.Nothing,
      kmsMasterKeyId = Lude.Nothing,
      provisionedThroughputOverride = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing
    }

-- | The current state of the replica:
--
--
--     * @CREATING@ - The replica is being created.
--
--
--     * @UPDATING@ - The replica is being updated.
--
--
--     * @DELETING@ - The replica is being deleted.
--
--
--     * @ACTIVE@ - The replica is ready for use.
--
--
--     * @REGION_DISABLED@ - The replica is inaccessible because the AWS Region has been disabled.
--
--
--     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to encrypt the table is inaccessible.
--
--
--
-- /Note:/ Consider using 'replicaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaStatus :: Lens.Lens' ReplicaDescription (Lude.Maybe ReplicaStatus)
rdReplicaStatus = Lens.lens (replicaStatus :: ReplicaDescription -> Lude.Maybe ReplicaStatus) (\s a -> s {replicaStatus = a} :: ReplicaDescription)
{-# DEPRECATED rdReplicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead." #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionName :: Lens.Lens' ReplicaDescription (Lude.Maybe Lude.Text)
rdRegionName = Lens.lens (regionName :: ReplicaDescription -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: ReplicaDescription)
{-# DEPRECATED rdRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
--
-- /Note:/ Consider using 'replicaStatusPercentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaStatusPercentProgress :: Lens.Lens' ReplicaDescription (Lude.Maybe Lude.Text)
rdReplicaStatusPercentProgress = Lens.lens (replicaStatusPercentProgress :: ReplicaDescription -> Lude.Maybe Lude.Text) (\s a -> s {replicaStatusPercentProgress = a} :: ReplicaDescription)
{-# DEPRECATED rdReplicaStatusPercentProgress "Use generic-lens or generic-optics with 'replicaStatusPercentProgress' instead." #-}

-- | Detailed information about the replica status.
--
-- /Note:/ Consider using 'replicaStatusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaStatusDescription :: Lens.Lens' ReplicaDescription (Lude.Maybe Lude.Text)
rdReplicaStatusDescription = Lens.lens (replicaStatusDescription :: ReplicaDescription -> Lude.Maybe Lude.Text) (\s a -> s {replicaStatusDescription = a} :: ReplicaDescription)
{-# DEPRECATED rdReplicaStatusDescription "Use generic-lens or generic-optics with 'replicaStatusDescription' instead." #-}

-- | The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
--
-- /Note:/ Consider using 'replicaInaccessibleDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaInaccessibleDateTime :: Lens.Lens' ReplicaDescription (Lude.Maybe Lude.Timestamp)
rdReplicaInaccessibleDateTime = Lens.lens (replicaInaccessibleDateTime :: ReplicaDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicaInaccessibleDateTime = a} :: ReplicaDescription)
{-# DEPRECATED rdReplicaInaccessibleDateTime "Use generic-lens or generic-optics with 'replicaInaccessibleDateTime' instead." #-}

-- | The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
--
-- /Note:/ Consider using 'kmsMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdKMSMasterKeyId :: Lens.Lens' ReplicaDescription (Lude.Maybe Lude.Text)
rdKMSMasterKeyId = Lens.lens (kmsMasterKeyId :: ReplicaDescription -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyId = a} :: ReplicaDescription)
{-# DEPRECATED rdKMSMasterKeyId "Use generic-lens or generic-optics with 'kmsMasterKeyId' instead." #-}

-- | Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedThroughputOverride :: Lens.Lens' ReplicaDescription (Lude.Maybe ProvisionedThroughputOverride)
rdProvisionedThroughputOverride = Lens.lens (provisionedThroughputOverride :: ReplicaDescription -> Lude.Maybe ProvisionedThroughputOverride) (\s a -> s {provisionedThroughputOverride = a} :: ReplicaDescription)
{-# DEPRECATED rdProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdGlobalSecondaryIndexes :: Lens.Lens' ReplicaDescription (Lude.Maybe [ReplicaGlobalSecondaryIndexDescription])
rdGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: ReplicaDescription -> Lude.Maybe [ReplicaGlobalSecondaryIndexDescription]) (\s a -> s {globalSecondaryIndexes = a} :: ReplicaDescription)
{-# DEPRECATED rdGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

instance Lude.FromJSON ReplicaDescription where
  parseJSON =
    Lude.withObject
      "ReplicaDescription"
      ( \x ->
          ReplicaDescription'
            Lude.<$> (x Lude..:? "ReplicaStatus")
            Lude.<*> (x Lude..:? "RegionName")
            Lude.<*> (x Lude..:? "ReplicaStatusPercentProgress")
            Lude.<*> (x Lude..:? "ReplicaStatusDescription")
            Lude.<*> (x Lude..:? "ReplicaInaccessibleDateTime")
            Lude.<*> (x Lude..:? "KMSMasterKeyId")
            Lude.<*> (x Lude..:? "ProvisionedThroughputOverride")
            Lude.<*> (x Lude..:? "GlobalSecondaryIndexes" Lude..!= Lude.mempty)
      )
