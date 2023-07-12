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
-- Module      : Amazonka.MediaConvert.Types.AiffSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AiffSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
--
-- /See:/ 'newAiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
    -- quality for this audio track.
    bitDepth :: Prelude.Maybe Prelude.Natural,
    -- | Specify the number of channels in this output audio track. Valid values
    -- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
    -- to 64.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Sample rate in hz.
    sampleRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AiffSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitDepth', 'aiffSettings_bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
--
-- 'channels', 'aiffSettings_channels' - Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
--
-- 'sampleRate', 'aiffSettings_sampleRate' - Sample rate in hz.
newAiffSettings ::
  AiffSettings
newAiffSettings =
  AiffSettings'
    { bitDepth = Prelude.Nothing,
      channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
aiffSettings_bitDepth :: Lens.Lens' AiffSettings (Prelude.Maybe Prelude.Natural)
aiffSettings_bitDepth = Lens.lens (\AiffSettings' {bitDepth} -> bitDepth) (\s@AiffSettings' {} a -> s {bitDepth = a} :: AiffSettings)

-- | Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
aiffSettings_channels :: Lens.Lens' AiffSettings (Prelude.Maybe Prelude.Natural)
aiffSettings_channels = Lens.lens (\AiffSettings' {channels} -> channels) (\s@AiffSettings' {} a -> s {channels = a} :: AiffSettings)

-- | Sample rate in hz.
aiffSettings_sampleRate :: Lens.Lens' AiffSettings (Prelude.Maybe Prelude.Natural)
aiffSettings_sampleRate = Lens.lens (\AiffSettings' {sampleRate} -> sampleRate) (\s@AiffSettings' {} a -> s {sampleRate = a} :: AiffSettings)

instance Data.FromJSON AiffSettings where
  parseJSON =
    Data.withObject
      "AiffSettings"
      ( \x ->
          AiffSettings'
            Prelude.<$> (x Data..:? "bitDepth")
            Prelude.<*> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable AiffSettings where
  hashWithSalt _salt AiffSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bitDepth
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData AiffSettings where
  rnf AiffSettings' {..} =
    Prelude.rnf bitDepth
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON AiffSettings where
  toJSON AiffSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitDepth" Data..=) Prelude.<$> bitDepth,
            ("channels" Data..=) Prelude.<$> channels,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
