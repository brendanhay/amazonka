{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
  ( GlobalTableGlobalSecondaryIndexSettingsUpdate (..),

    -- * Smart constructor
    mkGlobalTableGlobalSecondaryIndexSettingsUpdate,

    -- * Lenses
    gtgsisuIndexName,
    gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
    gtgsisuProvisionedWriteCapacityUnits,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkGlobalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { -- | The name of the global secondary index. The name must be unique among all other indexes on this table.
    indexName :: Types.IndexName,
    -- | Auto scaling settings for managing a global secondary index's write capacity units.
    provisionedWriteCapacityAutoScalingSettingsUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate,
    -- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
    provisionedWriteCapacityUnits :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalTableGlobalSecondaryIndexSettingsUpdate' value with any optional fields omitted.
mkGlobalTableGlobalSecondaryIndexSettingsUpdate ::
  -- | 'indexName'
  Types.IndexName ->
  GlobalTableGlobalSecondaryIndexSettingsUpdate
mkGlobalTableGlobalSecondaryIndexSettingsUpdate indexName =
  GlobalTableGlobalSecondaryIndexSettingsUpdate'
    { indexName,
      provisionedWriteCapacityAutoScalingSettingsUpdate =
        Core.Nothing,
      provisionedWriteCapacityUnits = Core.Nothing
    }

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuIndexName :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Types.IndexName
gtgsisuIndexName = Lens.field @"indexName"
{-# DEPRECATED gtgsisuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Auto scaling settings for managing a global secondary index's write capacity units.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.field @"provisionedWriteCapacityAutoScalingSettingsUpdate"
{-# DEPRECATED gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- /Note:/ Consider using 'provisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsisuProvisionedWriteCapacityUnits :: Lens.Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Core.Maybe Core.Natural)
gtgsisuProvisionedWriteCapacityUnits = Lens.field @"provisionedWriteCapacityUnits"
{-# DEPRECATED gtgsisuProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'provisionedWriteCapacityUnits' instead." #-}

instance
  Core.FromJSON
    GlobalTableGlobalSecondaryIndexSettingsUpdate
  where
  toJSON GlobalTableGlobalSecondaryIndexSettingsUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexName" Core..= indexName),
            ("ProvisionedWriteCapacityAutoScalingSettingsUpdate" Core..=)
              Core.<$> provisionedWriteCapacityAutoScalingSettingsUpdate,
            ("ProvisionedWriteCapacityUnits" Core..=)
              Core.<$> provisionedWriteCapacityUnits
          ]
      )
