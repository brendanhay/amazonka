{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
  ( ReplicaSettingsUpdate (..),

    -- * Smart constructor
    mkReplicaSettingsUpdate,

    -- * Lenses
    rsuRegionName,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings for a global table in a Region that will be modified.
--
-- /See:/ 'mkReplicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { -- | The Region of the replica to be added.
    regionName :: Types.RegionName,
    -- | Represents the settings of a global secondary index for a global table that will be modified.
    replicaGlobalSecondaryIndexSettingsUpdate :: Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndexSettingsUpdate),
    -- | Auto scaling settings for managing a global table replica's read capacity units.
    replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate,
    -- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
    replicaProvisionedReadCapacityUnits :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaSettingsUpdate' value with any optional fields omitted.
mkReplicaSettingsUpdate ::
  -- | 'regionName'
  Types.RegionName ->
  ReplicaSettingsUpdate
mkReplicaSettingsUpdate regionName =
  ReplicaSettingsUpdate'
    { regionName,
      replicaGlobalSecondaryIndexSettingsUpdate = Core.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Core.Nothing,
      replicaProvisionedReadCapacityUnits = Core.Nothing
    }

-- | The Region of the replica to be added.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuRegionName :: Lens.Lens' ReplicaSettingsUpdate Types.RegionName
rsuRegionName = Lens.field @"regionName"
{-# DEPRECATED rsuRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndexSettingsUpdate))
rsuReplicaGlobalSecondaryIndexSettingsUpdate = Lens.field @"replicaGlobalSecondaryIndexSettingsUpdate"
{-# DEPRECATED rsuReplicaGlobalSecondaryIndexSettingsUpdate "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexSettingsUpdate' instead." #-}

-- | Auto scaling settings for managing a global table replica's read capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.field @"replicaProvisionedReadCapacityAutoScalingSettingsUpdate"
{-# DEPRECATED rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe Core.Natural)
rsuReplicaProvisionedReadCapacityUnits = Lens.field @"replicaProvisionedReadCapacityUnits"
{-# DEPRECATED rsuReplicaProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityUnits' instead." #-}

instance Core.FromJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegionName" Core..= regionName),
            ("ReplicaGlobalSecondaryIndexSettingsUpdate" Core..=)
              Core.<$> replicaGlobalSecondaryIndexSettingsUpdate,
            ("ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate" Core..=)
              Core.<$> replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" Core..=)
              Core.<$> replicaProvisionedReadCapacityUnits
          ]
      )
