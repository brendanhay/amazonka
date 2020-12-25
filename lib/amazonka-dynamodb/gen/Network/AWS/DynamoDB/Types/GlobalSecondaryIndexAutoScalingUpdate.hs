{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
  ( GlobalSecondaryIndexAutoScalingUpdate (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexAutoScalingUpdate,

    -- * Lenses
    gsiasuIndexName,
    gsiasuProvisionedWriteCapacityAutoScalingUpdate,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings of a global secondary index for a global table that will be modified.
--
-- /See:/ 'mkGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data GlobalSecondaryIndexAutoScalingUpdate = GlobalSecondaryIndexAutoScalingUpdate'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Types.IndexName,
    provisionedWriteCapacityAutoScalingUpdate :: Core.Maybe Types.AutoScalingSettingsUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalSecondaryIndexAutoScalingUpdate' value with any optional fields omitted.
mkGlobalSecondaryIndexAutoScalingUpdate ::
  GlobalSecondaryIndexAutoScalingUpdate
mkGlobalSecondaryIndexAutoScalingUpdate =
  GlobalSecondaryIndexAutoScalingUpdate'
    { indexName = Core.Nothing,
      provisionedWriteCapacityAutoScalingUpdate = Core.Nothing
    }

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiasuIndexName :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Types.IndexName)
gsiasuIndexName = Lens.field @"indexName"
{-# DEPRECATED gsiasuIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiasuProvisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Core.Maybe Types.AutoScalingSettingsUpdate)
gsiasuProvisionedWriteCapacityAutoScalingUpdate = Lens.field @"provisionedWriteCapacityAutoScalingUpdate"
{-# DEPRECATED gsiasuProvisionedWriteCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingUpdate' instead." #-}

instance Core.FromJSON GlobalSecondaryIndexAutoScalingUpdate where
  toJSON GlobalSecondaryIndexAutoScalingUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("IndexName" Core..=) Core.<$> indexName,
            ("ProvisionedWriteCapacityAutoScalingUpdate" Core..=)
              Core.<$> provisionedWriteCapacityAutoScalingUpdate
          ]
      )
