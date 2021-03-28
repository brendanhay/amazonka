{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
  ( ReplicaGlobalSecondaryIndexSettingsUpdate (..)
  -- * Smart constructor
  , mkReplicaGlobalSecondaryIndexSettingsUpdate
  -- * Lenses
  , rgsisuIndexName
  , rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate
  , rgsisuProvisionedReadCapacityUnits
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexSettingsUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsUpdate = ReplicaGlobalSecondaryIndexSettingsUpdate'
  { indexName :: Types.IndexName
    -- ^ The name of the global secondary index. The name must be unique among all other indexes on this table.
  , provisionedReadCapacityAutoScalingSettingsUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate
    -- ^ Auto scaling settings for managing a global secondary index replica's read capacity units.
  , provisionedReadCapacityUnits :: Core.Maybe Core.Natural
    -- ^ The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndexSettingsUpdate' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndexSettingsUpdate
    :: Types.IndexName -- ^ 'indexName'
    -> ReplicaGlobalSecondaryIndexSettingsUpdate
mkReplicaGlobalSecondaryIndexSettingsUpdate indexName
  = ReplicaGlobalSecondaryIndexSettingsUpdate'{indexName,
                                               provisionedReadCapacityAutoScalingSettingsUpdate =
                                                 Core.Nothing,
                                               provisionedReadCapacityUnits = Core.Nothing}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate Types.IndexName
rgsisuIndexName = Lens.field @"indexName"
{-# INLINEABLE rgsisuIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | Auto scaling settings for managing a global secondary index replica's read capacity units.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.field @"provisionedReadCapacityAutoScalingSettingsUpdate"
{-# INLINEABLE rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate #-}
{-# DEPRECATED provisionedReadCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettingsUpdate' instead"  #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisuProvisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Core.Maybe Core.Natural)
rgsisuProvisionedReadCapacityUnits = Lens.field @"provisionedReadCapacityUnits"
{-# INLINEABLE rgsisuProvisionedReadCapacityUnits #-}
{-# DEPRECATED provisionedReadCapacityUnits "Use generic-lens or generic-optics with 'provisionedReadCapacityUnits' instead"  #-}

instance Core.FromJSON ReplicaGlobalSecondaryIndexSettingsUpdate
         where
        toJSON ReplicaGlobalSecondaryIndexSettingsUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexName" Core..= indexName),
                  ("ProvisionedReadCapacityAutoScalingSettingsUpdate" Core..=)
                    Core.<$> provisionedReadCapacityAutoScalingSettingsUpdate,
                  ("ProvisionedReadCapacityUnits" Core..=) Core.<$>
                    provisionedReadCapacityUnits])
