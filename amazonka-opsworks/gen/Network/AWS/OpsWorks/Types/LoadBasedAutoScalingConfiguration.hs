{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import qualified Network.AWS.Prelude as Prelude

-- | Describes a layer\'s load-based auto scaling configuration.
--
-- /See:/ 'newLoadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { -- | An @AutoScalingThresholds@ object that describes the downscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks reduces
    -- the number of instances.
    downScaling :: Prelude.Maybe AutoScalingThresholds,
    -- | Whether load-based auto scaling is enabled for the layer.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | The layer ID.
    layerId :: Prelude.Maybe Prelude.Text,
    -- | An @AutoScalingThresholds@ object that describes the upscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks increases
    -- the number of instances.
    upScaling :: Prelude.Maybe AutoScalingThresholds
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      enable = Prelude.Nothing,
      layerId = Prelude.Nothing,
      upScaling = Prelude.Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks Stacks reduces
-- the number of instances.
loadBasedAutoScalingConfiguration_downScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_downScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {downScaling} -> downScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {downScaling = a} :: LoadBasedAutoScalingConfiguration)

-- | Whether load-based auto scaling is enabled for the layer.
loadBasedAutoScalingConfiguration_enable :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe Prelude.Bool)
loadBasedAutoScalingConfiguration_enable = Lens.lens (\LoadBasedAutoScalingConfiguration' {enable} -> enable) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {enable = a} :: LoadBasedAutoScalingConfiguration)

-- | The layer ID.
loadBasedAutoScalingConfiguration_layerId :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe Prelude.Text)
loadBasedAutoScalingConfiguration_layerId = Lens.lens (\LoadBasedAutoScalingConfiguration' {layerId} -> layerId) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {layerId = a} :: LoadBasedAutoScalingConfiguration)

-- | An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks Stacks increases
-- the number of instances.
loadBasedAutoScalingConfiguration_upScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_upScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {upScaling} -> upScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {upScaling = a} :: LoadBasedAutoScalingConfiguration)

instance
  Prelude.FromJSON
    LoadBasedAutoScalingConfiguration
  where
  parseJSON =
    Prelude.withObject
      "LoadBasedAutoScalingConfiguration"
      ( \x ->
          LoadBasedAutoScalingConfiguration'
            Prelude.<$> (x Prelude..:? "DownScaling")
            Prelude.<*> (x Prelude..:? "Enable")
            Prelude.<*> (x Prelude..:? "LayerId")
            Prelude.<*> (x Prelude..:? "UpScaling")
      )

instance
  Prelude.Hashable
    LoadBasedAutoScalingConfiguration

instance
  Prelude.NFData
    LoadBasedAutoScalingConfiguration
