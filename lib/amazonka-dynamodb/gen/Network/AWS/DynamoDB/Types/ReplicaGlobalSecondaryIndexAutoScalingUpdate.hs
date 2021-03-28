{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
  ( ReplicaGlobalSecondaryIndexAutoScalingUpdate (..)
  -- * Smart constructor
  , mkReplicaGlobalSecondaryIndexAutoScalingUpdate
  -- * Lenses
  , rgsiasuIndexName
  , rgsiasuProvisionedReadCapacityAutoScalingUpdate
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings of a global secondary index for a replica that will be modified.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingUpdate = ReplicaGlobalSecondaryIndexAutoScalingUpdate'
  { indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the global secondary index.
  , provisionedReadCapacityAutoScalingUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndexAutoScalingUpdate' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndexAutoScalingUpdate
    :: ReplicaGlobalSecondaryIndexAutoScalingUpdate
mkReplicaGlobalSecondaryIndexAutoScalingUpdate
  = ReplicaGlobalSecondaryIndexAutoScalingUpdate'{indexName =
                                                    Core.Nothing,
                                                  provisionedReadCapacityAutoScalingUpdate =
                                                    Core.Nothing}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasuIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Types.IndexName)
rgsiasuIndexName = Lens.field @"indexName"
{-# INLINEABLE rgsiasuIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasuProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
rgsiasuProvisionedReadCapacityAutoScalingUpdate = Lens.field @"provisionedReadCapacityAutoScalingUpdate"
{-# INLINEABLE rgsiasuProvisionedReadCapacityAutoScalingUpdate #-}
{-# DEPRECATED provisionedReadCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingUpdate' instead"  #-}

instance Core.FromJSON ReplicaGlobalSecondaryIndexAutoScalingUpdate
         where
        toJSON ReplicaGlobalSecondaryIndexAutoScalingUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("IndexName" Core..=) Core.<$> indexName,
                  ("ProvisionedReadCapacityAutoScalingUpdate" Core..=) Core.<$>
                    provisionedReadCapacityAutoScalingUpdate])
