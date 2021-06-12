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
-- Module      : Network.AWS.MediaConvert.Types.AiffSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AiffSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
--
-- /See:/ 'newAiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { -- | Specify the number of channels in this output audio track. Valid values
    -- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
    -- to 64.
    channels :: Core.Maybe Core.Natural,
    -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
    -- quality for this audio track.
    bitDepth :: Core.Maybe Core.Natural,
    -- | Sample rate in hz.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AiffSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'aiffSettings_channels' - Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
--
-- 'bitDepth', 'aiffSettings_bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
--
-- 'sampleRate', 'aiffSettings_sampleRate' - Sample rate in hz.
newAiffSettings ::
  AiffSettings
newAiffSettings =
  AiffSettings'
    { channels = Core.Nothing,
      bitDepth = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
aiffSettings_channels :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
aiffSettings_channels = Lens.lens (\AiffSettings' {channels} -> channels) (\s@AiffSettings' {} a -> s {channels = a} :: AiffSettings)

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
aiffSettings_bitDepth :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
aiffSettings_bitDepth = Lens.lens (\AiffSettings' {bitDepth} -> bitDepth) (\s@AiffSettings' {} a -> s {bitDepth = a} :: AiffSettings)

-- | Sample rate in hz.
aiffSettings_sampleRate :: Lens.Lens' AiffSettings (Core.Maybe Core.Natural)
aiffSettings_sampleRate = Lens.lens (\AiffSettings' {sampleRate} -> sampleRate) (\s@AiffSettings' {} a -> s {sampleRate = a} :: AiffSettings)

instance Core.FromJSON AiffSettings where
  parseJSON =
    Core.withObject
      "AiffSettings"
      ( \x ->
          AiffSettings'
            Core.<$> (x Core..:? "channels")
            Core.<*> (x Core..:? "bitDepth")
            Core.<*> (x Core..:? "sampleRate")
      )

instance Core.Hashable AiffSettings

instance Core.NFData AiffSettings

instance Core.ToJSON AiffSettings where
  toJSON AiffSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("channels" Core..=) Core.<$> channels,
            ("bitDepth" Core..=) Core.<$> bitDepth,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )
