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
-- Module      : Network.AWS.MediaConvert.Types.Mp2Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp2Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
--
-- /See:/ 'newMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Set Channels to specify the number of channels in this output audio
    -- track. Choosing Mono in the console will give you 1 output channel;
    -- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Sample rate in hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the average bitrate in bits per second.
    bitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mp2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'mp2Settings_channels' - Set Channels to specify the number of channels in this output audio
-- track. Choosing Mono in the console will give you 1 output channel;
-- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- 'sampleRate', 'mp2Settings_sampleRate' - Sample rate in hz.
--
-- 'bitrate', 'mp2Settings_bitrate' - Specify the average bitrate in bits per second.
newMp2Settings ::
  Mp2Settings
newMp2Settings =
  Mp2Settings'
    { channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      bitrate = Prelude.Nothing
    }

-- | Set Channels to specify the number of channels in this output audio
-- track. Choosing Mono in the console will give you 1 output channel;
-- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
mp2Settings_channels :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_channels = Lens.lens (\Mp2Settings' {channels} -> channels) (\s@Mp2Settings' {} a -> s {channels = a} :: Mp2Settings)

-- | Sample rate in hz.
mp2Settings_sampleRate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_sampleRate = Lens.lens (\Mp2Settings' {sampleRate} -> sampleRate) (\s@Mp2Settings' {} a -> s {sampleRate = a} :: Mp2Settings)

-- | Specify the average bitrate in bits per second.
mp2Settings_bitrate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_bitrate = Lens.lens (\Mp2Settings' {bitrate} -> bitrate) (\s@Mp2Settings' {} a -> s {bitrate = a} :: Mp2Settings)

instance Core.FromJSON Mp2Settings where
  parseJSON =
    Core.withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            Prelude.<$> (x Core..:? "channels")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "bitrate")
      )

instance Prelude.Hashable Mp2Settings

instance Prelude.NFData Mp2Settings

instance Core.ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("channels" Core..=) Prelude.<$> channels,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("bitrate" Core..=) Prelude.<$> bitrate
          ]
      )
