{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AutoScalingThresholds

-- | Describes a layer\'s load-based auto scaling configuration.
--
-- /See:/ 'newLoadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { -- | An @AutoScalingThresholds@ object that describes the downscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks reduces
    -- the number of instances.
    downScaling :: Core.Maybe AutoScalingThresholds,
    -- | Whether load-based auto scaling is enabled for the layer.
    enable :: Core.Maybe Core.Bool,
    -- | The layer ID.
    layerId :: Core.Maybe Core.Text,
    -- | An @AutoScalingThresholds@ object that describes the upscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks increases
    -- the number of instances.
    upScaling :: Core.Maybe AutoScalingThresholds
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBasedAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downScaling', 'loadBasedAutoScalingConfiguration_downScaling' - An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks Stacks reduces
-- the number of instances.
--
-- 'enable', 'loadBasedAutoScalingConfiguration_enable' - Whether load-based auto scaling is enabled for the layer.
--
-- 'layerId', 'loadBasedAutoScalingConfiguration_layerId' - The layer ID.
--
-- 'upScaling', 'loadBasedAutoScalingConfiguration_upScaling' - An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks Stacks increases
-- the number of instances.
newLoadBasedAutoScalingConfiguration ::
  LoadBasedAutoScalingConfiguration
newLoadBasedAutoScalingConfiguration =
  LoadBasedAutoScalingConfiguration'
    { downScaling =
        Core.Nothing,
      enable = Core.Nothing,
      layerId = Core.Nothing,
      upScaling = Core.Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks Stacks reduces
-- the number of instances.
loadBasedAutoScalingConfiguration_downScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_downScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {downScaling} -> downScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {downScaling = a} :: LoadBasedAutoScalingConfiguration)

-- | Whether load-based auto scaling is enabled for the layer.
loadBasedAutoScalingConfiguration_enable :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Core.Bool)
loadBasedAutoScalingConfiguration_enable = Lens.lens (\LoadBasedAutoScalingConfiguration' {enable} -> enable) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {enable = a} :: LoadBasedAutoScalingConfiguration)

-- | The layer ID.
loadBasedAutoScalingConfiguration_layerId :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe Core.Text)
loadBasedAutoScalingConfiguration_layerId = Lens.lens (\LoadBasedAutoScalingConfiguration' {layerId} -> layerId) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {layerId = a} :: LoadBasedAutoScalingConfiguration)

-- | An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks Stacks increases
-- the number of instances.
loadBasedAutoScalingConfiguration_upScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Core.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_upScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {upScaling} -> upScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {upScaling = a} :: LoadBasedAutoScalingConfiguration)

instance
  Core.FromJSON
    LoadBasedAutoScalingConfiguration
  where
  parseJSON =
    Core.withObject
      "LoadBasedAutoScalingConfiguration"
      ( \x ->
          LoadBasedAutoScalingConfiguration'
            Core.<$> (x Core..:? "DownScaling")
            Core.<*> (x Core..:? "Enable")
            Core.<*> (x Core..:? "LayerId")
            Core.<*> (x Core..:? "UpScaling")
      )

instance
  Core.Hashable
    LoadBasedAutoScalingConfiguration

instance
  Core.NFData
    LoadBasedAutoScalingConfiguration
