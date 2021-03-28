{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
  ( ReplicaAutoScalingUpdate (..)
  -- * Smart constructor
  , mkReplicaAutoScalingUpdate
  -- * Lenses
  , rasuRegionName
  , rasuReplicaGlobalSecondaryIndexUpdates
  , rasuReplicaProvisionedReadCapacityAutoScalingUpdate
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings of a replica that will be modified.
--
-- /See:/ 'mkReplicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { regionName :: Types.RegionName
    -- ^ The Region where the replica exists.
  , replicaGlobalSecondaryIndexUpdates :: Core.Maybe [Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate]
    -- ^ Represents the auto scaling settings of global secondary indexes that will be modified.
  , replicaProvisionedReadCapacityAutoScalingUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaAutoScalingUpdate' value with any optional fields omitted.
mkReplicaAutoScalingUpdate
    :: Types.RegionName -- ^ 'regionName'
    -> ReplicaAutoScalingUpdate
mkReplicaAutoScalingUpdate regionName
  = ReplicaAutoScalingUpdate'{regionName,
                              replicaGlobalSecondaryIndexUpdates = Core.Nothing,
                              replicaProvisionedReadCapacityAutoScalingUpdate = Core.Nothing}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuRegionName :: Lens.Lens' ReplicaAutoScalingUpdate Types.RegionName
rasuRegionName = Lens.field @"regionName"
{-# INLINEABLE rasuRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | Represents the auto scaling settings of global secondary indexes that will be modified.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuReplicaGlobalSecondaryIndexUpdates :: Lens.Lens' ReplicaAutoScalingUpdate (Core.Maybe [Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate])
rasuReplicaGlobalSecondaryIndexUpdates = Lens.field @"replicaGlobalSecondaryIndexUpdates"
{-# INLINEABLE rasuReplicaGlobalSecondaryIndexUpdates #-}
{-# DEPRECATED replicaGlobalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexUpdates' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuReplicaProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaAutoScalingUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
rasuReplicaProvisionedReadCapacityAutoScalingUpdate = Lens.field @"replicaProvisionedReadCapacityAutoScalingUpdate"
{-# INLINEABLE rasuReplicaProvisionedReadCapacityAutoScalingUpdate #-}
{-# DEPRECATED replicaProvisionedReadCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingUpdate' instead"  #-}

instance Core.FromJSON ReplicaAutoScalingUpdate where
        toJSON ReplicaAutoScalingUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegionName" Core..= regionName),
                  ("ReplicaGlobalSecondaryIndexUpdates" Core..=) Core.<$>
                    replicaGlobalSecondaryIndexUpdates,
                  ("ReplicaProvisionedReadCapacityAutoScalingUpdate" Core..=)
                    Core.<$> replicaProvisionedReadCapacityAutoScalingUpdate])
