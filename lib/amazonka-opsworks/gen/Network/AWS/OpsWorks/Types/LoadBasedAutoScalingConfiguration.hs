{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
  ( LoadBasedAutoScalingConfiguration (..)
  -- * Smart constructor
  , mkLoadBasedAutoScalingConfiguration
  -- * Lenses
  , lbascDownScaling
  , lbascEnable
  , lbascLayerId
  , lbascUpScaling
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.AutoScalingThresholds as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a layer's load-based auto scaling configuration.
--
-- /See:/ 'mkLoadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { downScaling :: Core.Maybe Types.AutoScalingThresholds
    -- ^ An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
  , enable :: Core.Maybe Core.Bool
    -- ^ Whether load-based auto scaling is enabled for the layer.
  , layerId :: Core.Maybe Core.Text
    -- ^ The layer ID.
  , upScaling :: Core.Maybe Types.AutoScalingThresholds
    -- ^ An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBasedAutoScalingConfiguration' value with any optional fields omitted.
mkLoadBasedAutoScalingConfiguration
    :: LoadBasedAutoScalingConfiguration
mkLoadBasedAutoScalingConfiguration
  = LoadBasedAutoScalingConfiguration'{downScaling = Core.Nothing,
                                       enable = Core.Nothing, layerId = Core.Nothing,
                                       upScaling = Core.Nothing}

-- | An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
--
-- /Note:/ Consider using 'downScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascDownScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Types.AutoScalingThresholds)
lbascDownScaling = Lens.field @"downScaling"
{-# INLINEABLE lbascDownScaling #-}
{-# DEPRECATED downScaling "Use generic-lens or generic-optics with 'downScaling' instead"  #-}

-- | Whether load-based auto scaling is enabled for the layer.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascEnable :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Core.Bool)
lbascEnable = Lens.field @"enable"
{-# INLINEABLE lbascEnable #-}
{-# DEPRECATED enable "Use generic-lens or generic-optics with 'enable' instead"  #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascLayerId :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Core.Text)
lbascLayerId = Lens.field @"layerId"
{-# INLINEABLE lbascLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

-- | An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
--
-- /Note:/ Consider using 'upScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascUpScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Types.AutoScalingThresholds)
lbascUpScaling = Lens.field @"upScaling"
{-# INLINEABLE lbascUpScaling #-}
{-# DEPRECATED upScaling "Use generic-lens or generic-optics with 'upScaling' instead"  #-}

instance Core.FromJSON LoadBasedAutoScalingConfiguration where
        parseJSON
          = Core.withObject "LoadBasedAutoScalingConfiguration" Core.$
              \ x ->
                LoadBasedAutoScalingConfiguration' Core.<$>
                  (x Core..:? "DownScaling") Core.<*> x Core..:? "Enable" Core.<*>
                    x Core..:? "LayerId"
                    Core.<*> x Core..:? "UpScaling"
