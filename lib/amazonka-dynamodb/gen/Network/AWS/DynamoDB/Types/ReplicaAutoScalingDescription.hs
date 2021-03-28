{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
  ( ReplicaAutoScalingDescription (..)
  -- * Smart constructor
  , mkReplicaAutoScalingDescription
  -- * Lenses
  , rasdGlobalSecondaryIndexes
  , rasdRegionName
  , rasdReplicaProvisionedReadCapacityAutoScalingSettings
  , rasdReplicaProvisionedWriteCapacityAutoScalingSettings
  , rasdReplicaStatus
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings of the replica.
--
-- /See:/ 'mkReplicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { globalSecondaryIndexes :: Core.Maybe [Types.ReplicaGlobalSecondaryIndexAutoScalingDescription]
    -- ^ Replica-specific global secondary index auto scaling settings.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ The Region where the replica exists.
  , replicaProvisionedReadCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
  , replicaProvisionedWriteCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaAutoScalingDescription' value with any optional fields omitted.
mkReplicaAutoScalingDescription
    :: ReplicaAutoScalingDescription
mkReplicaAutoScalingDescription
  = ReplicaAutoScalingDescription'{globalSecondaryIndexes =
                                     Core.Nothing,
                                   regionName = Core.Nothing,
                                   replicaProvisionedReadCapacityAutoScalingSettings = Core.Nothing,
                                   replicaProvisionedWriteCapacityAutoScalingSettings =
                                     Core.Nothing,
                                   replicaStatus = Core.Nothing}

-- | Replica-specific global secondary index auto scaling settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdGlobalSecondaryIndexes :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe [Types.ReplicaGlobalSecondaryIndexAutoScalingDescription])
rasdGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE rasdGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdRegionName :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe Types.RegionName)
rasdRegionName = Lens.field @"regionName"
{-# INLINEABLE rasdRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rasdReplicaProvisionedReadCapacityAutoScalingSettings = Lens.field @"replicaProvisionedReadCapacityAutoScalingSettings"
{-# INLINEABLE rasdReplicaProvisionedReadCapacityAutoScalingSettings #-}
{-# DEPRECATED replicaProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rasdReplicaProvisionedWriteCapacityAutoScalingSettings = Lens.field @"replicaProvisionedWriteCapacityAutoScalingSettings"
{-# INLINEABLE rasdReplicaProvisionedWriteCapacityAutoScalingSettings #-}
{-# DEPRECATED replicaProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityAutoScalingSettings' instead"  #-}

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
--
-- /Note:/ Consider using 'replicaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaStatus :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe Types.ReplicaStatus)
rasdReplicaStatus = Lens.field @"replicaStatus"
{-# INLINEABLE rasdReplicaStatus #-}
{-# DEPRECATED replicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead"  #-}

instance Core.FromJSON ReplicaAutoScalingDescription where
        parseJSON
          = Core.withObject "ReplicaAutoScalingDescription" Core.$
              \ x ->
                ReplicaAutoScalingDescription' Core.<$>
                  (x Core..:? "GlobalSecondaryIndexes") Core.<*>
                    x Core..:? "RegionName"
                    Core.<*>
                    x Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                    Core.<*>
                    x Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                    Core.<*> x Core..:? "ReplicaStatus"
