{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaDescription
  ( ReplicaDescription (..)
  -- * Smart constructor
  , mkReplicaDescription
  -- * Lenses
  , rdGlobalSecondaryIndexes
  , rdKMSMasterKeyId
  , rdProvisionedThroughputOverride
  , rdRegionName
  , rdReplicaInaccessibleDateTime
  , rdReplicaStatus
  , rdReplicaStatusDescription
  , rdReplicaStatusPercentProgress
  ) where

import qualified Network.AWS.DynamoDB.Types.KMSMasterKeyId as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaStatus as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaStatusDescription as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaStatusPercentProgress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of the replica.
--
-- /See:/ 'mkReplicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { globalSecondaryIndexes :: Core.Maybe [Types.ReplicaGlobalSecondaryIndexDescription]
    -- ^ Replica-specific global secondary index settings.
  , kMSMasterKeyId :: Core.Maybe Types.KMSMasterKeyId
    -- ^ The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
  , provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughputOverride
    -- ^ Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ The name of the Region.
  , replicaInaccessibleDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
  , replicaStatus :: Core.Maybe Types.ReplicaStatus
    -- ^ The current state of the replica:
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
  , replicaStatusDescription :: Core.Maybe Types.ReplicaStatusDescription
    -- ^ Detailed information about the replica status.
  , replicaStatusPercentProgress :: Core.Maybe Types.ReplicaStatusPercentProgress
    -- ^ Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicaDescription' value with any optional fields omitted.
mkReplicaDescription
    :: ReplicaDescription
mkReplicaDescription
  = ReplicaDescription'{globalSecondaryIndexes = Core.Nothing,
                        kMSMasterKeyId = Core.Nothing,
                        provisionedThroughputOverride = Core.Nothing,
                        regionName = Core.Nothing,
                        replicaInaccessibleDateTime = Core.Nothing,
                        replicaStatus = Core.Nothing,
                        replicaStatusDescription = Core.Nothing,
                        replicaStatusPercentProgress = Core.Nothing}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdGlobalSecondaryIndexes :: Lens.Lens' ReplicaDescription (Core.Maybe [Types.ReplicaGlobalSecondaryIndexDescription])
rdGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE rdGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | The AWS KMS customer master key (CMK) of the replica that will be used for AWS KMS encryption.
--
-- /Note:/ Consider using 'kMSMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdKMSMasterKeyId :: Lens.Lens' ReplicaDescription (Core.Maybe Types.KMSMasterKeyId)
rdKMSMasterKeyId = Lens.field @"kMSMasterKeyId"
{-# INLINEABLE rdKMSMasterKeyId #-}
{-# DEPRECATED kMSMasterKeyId "Use generic-lens or generic-optics with 'kMSMasterKeyId' instead"  #-}

-- | Replica-specific provisioned throughput. If not described, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedThroughputOverride :: Lens.Lens' ReplicaDescription (Core.Maybe Types.ProvisionedThroughputOverride)
rdProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# INLINEABLE rdProvisionedThroughputOverride #-}
{-# DEPRECATED provisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead"  #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRegionName :: Lens.Lens' ReplicaDescription (Core.Maybe Types.RegionName)
rdRegionName = Lens.field @"regionName"
{-# INLINEABLE rdRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | The time at which the replica was first detected as inaccessible. To determine cause of inaccessibility check the @ReplicaStatus@ property.
--
-- /Note:/ Consider using 'replicaInaccessibleDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaInaccessibleDateTime :: Lens.Lens' ReplicaDescription (Core.Maybe Core.NominalDiffTime)
rdReplicaInaccessibleDateTime = Lens.field @"replicaInaccessibleDateTime"
{-# INLINEABLE rdReplicaInaccessibleDateTime #-}
{-# DEPRECATED replicaInaccessibleDateTime "Use generic-lens or generic-optics with 'replicaInaccessibleDateTime' instead"  #-}

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
rdReplicaStatus :: Lens.Lens' ReplicaDescription (Core.Maybe Types.ReplicaStatus)
rdReplicaStatus = Lens.field @"replicaStatus"
{-# INLINEABLE rdReplicaStatus #-}
{-# DEPRECATED replicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead"  #-}

-- | Detailed information about the replica status.
--
-- /Note:/ Consider using 'replicaStatusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaStatusDescription :: Lens.Lens' ReplicaDescription (Core.Maybe Types.ReplicaStatusDescription)
rdReplicaStatusDescription = Lens.field @"replicaStatusDescription"
{-# INLINEABLE rdReplicaStatusDescription #-}
{-# DEPRECATED replicaStatusDescription "Use generic-lens or generic-optics with 'replicaStatusDescription' instead"  #-}

-- | Specifies the progress of a Create, Update, or Delete action on the replica as a percentage.
--
-- /Note:/ Consider using 'replicaStatusPercentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdReplicaStatusPercentProgress :: Lens.Lens' ReplicaDescription (Core.Maybe Types.ReplicaStatusPercentProgress)
rdReplicaStatusPercentProgress = Lens.field @"replicaStatusPercentProgress"
{-# INLINEABLE rdReplicaStatusPercentProgress #-}
{-# DEPRECATED replicaStatusPercentProgress "Use generic-lens or generic-optics with 'replicaStatusPercentProgress' instead"  #-}

instance Core.FromJSON ReplicaDescription where
        parseJSON
          = Core.withObject "ReplicaDescription" Core.$
              \ x ->
                ReplicaDescription' Core.<$>
                  (x Core..:? "GlobalSecondaryIndexes") Core.<*>
                    x Core..:? "KMSMasterKeyId"
                    Core.<*> x Core..:? "ProvisionedThroughputOverride"
                    Core.<*> x Core..:? "RegionName"
                    Core.<*> x Core..:? "ReplicaInaccessibleDateTime"
                    Core.<*> x Core..:? "ReplicaStatus"
                    Core.<*> x Core..:? "ReplicaStatusDescription"
                    Core.<*> x Core..:? "ReplicaStatusPercentProgress"
