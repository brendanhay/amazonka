{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
  ( LoadBasedAutoScalingConfiguration (..),

    -- * Smart constructor
    mkLoadBasedAutoScalingConfiguration,

    -- * Lenses
    lbascUpScaling,
    lbascEnable,
    lbascDownScaling,
    lbascLayerId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import qualified Network.AWS.Prelude as Lude

-- | Describes a layer's load-based auto scaling configuration.
--
-- /See:/ 'mkLoadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { upScaling ::
      Lude.Maybe
        AutoScalingThresholds,
    enable ::
      Lude.Maybe Lude.Bool,
    downScaling ::
      Lude.Maybe
        AutoScalingThresholds,
    layerId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBasedAutoScalingConfiguration' with the minimum fields required to make a request.
--
-- * 'downScaling' - An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
-- * 'enable' - Whether load-based auto scaling is enabled for the layer.
-- * 'layerId' - The layer ID.
-- * 'upScaling' - An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
mkLoadBasedAutoScalingConfiguration ::
  LoadBasedAutoScalingConfiguration
mkLoadBasedAutoScalingConfiguration =
  LoadBasedAutoScalingConfiguration'
    { upScaling = Lude.Nothing,
      enable = Lude.Nothing,
      downScaling = Lude.Nothing,
      layerId = Lude.Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
--
-- /Note:/ Consider using 'upScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascUpScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Lude.Maybe AutoScalingThresholds)
lbascUpScaling = Lens.lens (upScaling :: LoadBasedAutoScalingConfiguration -> Lude.Maybe AutoScalingThresholds) (\s a -> s {upScaling = a} :: LoadBasedAutoScalingConfiguration)
{-# DEPRECATED lbascUpScaling "Use generic-lens or generic-optics with 'upScaling' instead." #-}

-- | Whether load-based auto scaling is enabled for the layer.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascEnable :: Lens.Lens' LoadBasedAutoScalingConfiguration (Lude.Maybe Lude.Bool)
lbascEnable = Lens.lens (enable :: LoadBasedAutoScalingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enable = a} :: LoadBasedAutoScalingConfiguration)
{-# DEPRECATED lbascEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
--
-- /Note:/ Consider using 'downScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascDownScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Lude.Maybe AutoScalingThresholds)
lbascDownScaling = Lens.lens (downScaling :: LoadBasedAutoScalingConfiguration -> Lude.Maybe AutoScalingThresholds) (\s a -> s {downScaling = a} :: LoadBasedAutoScalingConfiguration)
{-# DEPRECATED lbascDownScaling "Use generic-lens or generic-optics with 'downScaling' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbascLayerId :: Lens.Lens' LoadBasedAutoScalingConfiguration (Lude.Maybe Lude.Text)
lbascLayerId = Lens.lens (layerId :: LoadBasedAutoScalingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: LoadBasedAutoScalingConfiguration)
{-# DEPRECATED lbascLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.FromJSON LoadBasedAutoScalingConfiguration where
  parseJSON =
    Lude.withObject
      "LoadBasedAutoScalingConfiguration"
      ( \x ->
          LoadBasedAutoScalingConfiguration'
            Lude.<$> (x Lude..:? "UpScaling")
            Lude.<*> (x Lude..:? "Enable")
            Lude.<*> (x Lude..:? "DownScaling")
            Lude.<*> (x Lude..:? "LayerId")
      )
