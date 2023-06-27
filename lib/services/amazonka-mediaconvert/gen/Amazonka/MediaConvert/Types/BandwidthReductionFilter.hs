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
-- Module      : Amazonka.MediaConvert.Types.BandwidthReductionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BandwidthReductionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.BandwidthReductionFilterSharpening
import Amazonka.MediaConvert.Types.BandwidthReductionFilterStrength
import qualified Amazonka.Prelude as Prelude

-- | The Bandwidth reduction filter increases the video quality of your
-- output relative to its bitrate. Use to lower the bitrate of your
-- constant quality QVBR output, with little or no perceptual decrease in
-- quality. Or, use to increase the video quality of outputs with other
-- rate control modes relative to the bitrate that you specify. Bandwidth
-- reduction increases further when your input is low quality or noisy.
-- Outputs that use this feature incur pro-tier pricing. When you include
-- Bandwidth reduction filter, you cannot include the Noise reducer
-- preprocessor.
--
-- /See:/ 'newBandwidthReductionFilter' smart constructor.
data BandwidthReductionFilter = BandwidthReductionFilter'
  { -- | Optionally specify the level of sharpening to apply when you use the
    -- Bandwidth reduction filter. Sharpening adds contrast to the edges of
    -- your video content and can reduce softness. Keep the default value Off
    -- to apply no sharpening. Set Sharpening strength to Low to apply a
    -- minimal amount of sharpening, or High to apply a maximum amount of
    -- sharpening.
    sharpening :: Prelude.Maybe BandwidthReductionFilterSharpening,
    -- | Specify the strength of the Bandwidth reduction filter. For most
    -- workflows, we recommend that you choose Auto to reduce the bandwidth of
    -- your output with little to no perceptual decrease in video quality. For
    -- high quality and high bitrate outputs, choose Low. For the most
    -- bandwidth reduction, choose High. We recommend that you choose High for
    -- low bitrate outputs. Note that High may incur a slight increase in the
    -- softness of your output.
    strength :: Prelude.Maybe BandwidthReductionFilterStrength
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BandwidthReductionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharpening', 'bandwidthReductionFilter_sharpening' - Optionally specify the level of sharpening to apply when you use the
-- Bandwidth reduction filter. Sharpening adds contrast to the edges of
-- your video content and can reduce softness. Keep the default value Off
-- to apply no sharpening. Set Sharpening strength to Low to apply a
-- minimal amount of sharpening, or High to apply a maximum amount of
-- sharpening.
--
-- 'strength', 'bandwidthReductionFilter_strength' - Specify the strength of the Bandwidth reduction filter. For most
-- workflows, we recommend that you choose Auto to reduce the bandwidth of
-- your output with little to no perceptual decrease in video quality. For
-- high quality and high bitrate outputs, choose Low. For the most
-- bandwidth reduction, choose High. We recommend that you choose High for
-- low bitrate outputs. Note that High may incur a slight increase in the
-- softness of your output.
newBandwidthReductionFilter ::
  BandwidthReductionFilter
newBandwidthReductionFilter =
  BandwidthReductionFilter'
    { sharpening =
        Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | Optionally specify the level of sharpening to apply when you use the
-- Bandwidth reduction filter. Sharpening adds contrast to the edges of
-- your video content and can reduce softness. Keep the default value Off
-- to apply no sharpening. Set Sharpening strength to Low to apply a
-- minimal amount of sharpening, or High to apply a maximum amount of
-- sharpening.
bandwidthReductionFilter_sharpening :: Lens.Lens' BandwidthReductionFilter (Prelude.Maybe BandwidthReductionFilterSharpening)
bandwidthReductionFilter_sharpening = Lens.lens (\BandwidthReductionFilter' {sharpening} -> sharpening) (\s@BandwidthReductionFilter' {} a -> s {sharpening = a} :: BandwidthReductionFilter)

-- | Specify the strength of the Bandwidth reduction filter. For most
-- workflows, we recommend that you choose Auto to reduce the bandwidth of
-- your output with little to no perceptual decrease in video quality. For
-- high quality and high bitrate outputs, choose Low. For the most
-- bandwidth reduction, choose High. We recommend that you choose High for
-- low bitrate outputs. Note that High may incur a slight increase in the
-- softness of your output.
bandwidthReductionFilter_strength :: Lens.Lens' BandwidthReductionFilter (Prelude.Maybe BandwidthReductionFilterStrength)
bandwidthReductionFilter_strength = Lens.lens (\BandwidthReductionFilter' {strength} -> strength) (\s@BandwidthReductionFilter' {} a -> s {strength = a} :: BandwidthReductionFilter)

instance Data.FromJSON BandwidthReductionFilter where
  parseJSON =
    Data.withObject
      "BandwidthReductionFilter"
      ( \x ->
          BandwidthReductionFilter'
            Prelude.<$> (x Data..:? "sharpening")
            Prelude.<*> (x Data..:? "strength")
      )

instance Prelude.Hashable BandwidthReductionFilter where
  hashWithSalt _salt BandwidthReductionFilter' {..} =
    _salt
      `Prelude.hashWithSalt` sharpening
      `Prelude.hashWithSalt` strength

instance Prelude.NFData BandwidthReductionFilter where
  rnf BandwidthReductionFilter' {..} =
    Prelude.rnf sharpening
      `Prelude.seq` Prelude.rnf strength

instance Data.ToJSON BandwidthReductionFilter where
  toJSON BandwidthReductionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sharpening" Data..=) Prelude.<$> sharpening,
            ("strength" Data..=) Prelude.<$> strength
          ]
      )
