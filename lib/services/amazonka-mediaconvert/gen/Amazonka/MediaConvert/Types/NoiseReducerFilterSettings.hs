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
-- Module      : Amazonka.MediaConvert.Types.NoiseReducerFilterSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseReducerFilterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for a noise reducer filter
--
-- /See:/ 'newNoiseReducerFilterSettings' smart constructor.
data NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { -- | Relative strength of noise reducing filter. Higher values produce
    -- stronger filtering.
    strength :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
noiseReducerFilterSettings_strength :: Lens.Lens' NoiseReducerFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerFilterSettings_strength = Lens.lens (\NoiseReducerFilterSettings' {strength} -> strength) (\s@NoiseReducerFilterSettings' {} a -> s {strength = a} :: NoiseReducerFilterSettings)

instance Core.FromJSON NoiseReducerFilterSettings where
  parseJSON =
    Core.withObject
      "NoiseReducerFilterSettings"
      ( \x ->
          NoiseReducerFilterSettings'
            Prelude.<$> (x Core..:? "strength")
      )

instance Prelude.Hashable NoiseReducerFilterSettings where
  hashWithSalt _salt NoiseReducerFilterSettings' {..} =
    _salt `Prelude.hashWithSalt` strength

instance Prelude.NFData NoiseReducerFilterSettings where
  rnf NoiseReducerFilterSettings' {..} =
    Prelude.rnf strength

instance Core.ToJSON NoiseReducerFilterSettings where
  toJSON NoiseReducerFilterSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("strength" Core..=) Prelude.<$> strength]
      )
