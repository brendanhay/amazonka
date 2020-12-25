{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackSummary
  ( StackSummary (..),

    -- * Smart constructor
    mkStackSummary,

    -- * Lenses
    ssAppsCount,
    ssArn,
    ssInstancesCount,
    ssLayersCount,
    ssName,
    ssStackId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.Arn as Types
import qualified Network.AWS.OpsWorks.Types.InstancesCount as Types
import qualified Network.AWS.OpsWorks.Types.Name as Types
import qualified Network.AWS.OpsWorks.Types.StackId as Types
import qualified Network.AWS.Prelude as Core

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'mkStackSummary' smart constructor.
data StackSummary = StackSummary'
  { -- | The number of apps.
    appsCount :: Core.Maybe Core.Int,
    -- | The stack's ARN.
    arn :: Core.Maybe Types.Arn,
    -- | An @InstancesCount@ object with the number of instances in each status.
    instancesCount :: Core.Maybe Types.InstancesCount,
    -- | The number of layers.
    layersCount :: Core.Maybe Core.Int,
    -- | The stack name.
    name :: Core.Maybe Types.Name,
    -- | The stack ID.
    stackId :: Core.Maybe Types.StackId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackSummary' value with any optional fields omitted.
mkStackSummary ::
  StackSummary
mkStackSummary =
  StackSummary'
    { appsCount = Core.Nothing,
      arn = Core.Nothing,
      instancesCount = Core.Nothing,
      layersCount = Core.Nothing,
      name = Core.Nothing,
      stackId = Core.Nothing
    }

-- | The number of apps.
--
-- /Note:/ Consider using 'appsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAppsCount :: Lens.Lens' StackSummary (Core.Maybe Core.Int)
ssAppsCount = Lens.field @"appsCount"
{-# DEPRECATED ssAppsCount "Use generic-lens or generic-optics with 'appsCount' instead." #-}

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssArn :: Lens.Lens' StackSummary (Core.Maybe Types.Arn)
ssArn = Lens.field @"arn"
{-# DEPRECATED ssArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An @InstancesCount@ object with the number of instances in each status.
--
-- /Note:/ Consider using 'instancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInstancesCount :: Lens.Lens' StackSummary (Core.Maybe Types.InstancesCount)
ssInstancesCount = Lens.field @"instancesCount"
{-# DEPRECATED ssInstancesCount "Use generic-lens or generic-optics with 'instancesCount' instead." #-}

-- | The number of layers.
--
-- /Note:/ Consider using 'layersCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLayersCount :: Lens.Lens' StackSummary (Core.Maybe Core.Int)
ssLayersCount = Lens.field @"layersCount"
{-# DEPRECATED ssLayersCount "Use generic-lens or generic-optics with 'layersCount' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' StackSummary (Core.Maybe Types.Name)
ssName = Lens.field @"name"
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackId :: Lens.Lens' StackSummary (Core.Maybe Types.StackId)
ssStackId = Lens.field @"stackId"
{-# DEPRECATED ssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON StackSummary where
  parseJSON =
    Core.withObject "StackSummary" Core.$
      \x ->
        StackSummary'
          Core.<$> (x Core..:? "AppsCount")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "InstancesCount")
          Core.<*> (x Core..:? "LayersCount")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "StackId")
