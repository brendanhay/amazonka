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
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for a noise reducer filter
--
-- /See:/ 'newNoiseReducerFilterSettings' smart constructor.
data NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { -- | Relative strength of noise reducing filter. Higher values produce
    -- stronger filtering.
    strength :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NoiseReducerFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'strength', 'noiseReducerFilterSettings_strength' - Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
newNoiseReducerFilterSettings ::
  NoiseReducerFilterSettings
newNoiseReducerFilterSettings =
  NoiseReducerFilterSettings'
    { strength =
        Core.Nothing
    }

-- | Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
noiseReducerFilterSettings_strength :: Lens.Lens' NoiseReducerFilterSettings (Core.Maybe Core.Natural)
noiseReducerFilterSettings_strength = Lens.lens (\NoiseReducerFilterSettings' {strength} -> strength) (\s@NoiseReducerFilterSettings' {} a -> s {strength = a} :: NoiseReducerFilterSettings)

instance Core.FromJSON NoiseReducerFilterSettings where
  parseJSON =
    Core.withObject
      "NoiseReducerFilterSettings"
      ( \x ->
          NoiseReducerFilterSettings'
            Core.<$> (x Core..:? "strength")
      )

instance Core.Hashable NoiseReducerFilterSettings

instance Core.NFData NoiseReducerFilterSettings

instance Core.ToJSON NoiseReducerFilterSettings where
  toJSON NoiseReducerFilterSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("strength" Core..=) Core.<$> strength]
      )
