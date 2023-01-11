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
-- Module      : Amazonka.MediaConvert.Types.Mp2Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mp2Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
--
-- /See:/ 'newMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Specify the average bitrate in bits per second.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Set Channels to specify the number of channels in this output audio
    -- track. Choosing Mono in the console will give you 1 output channel;
    -- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Sample rate in hz.
    sampleRate :: Prelude.Maybe Prelude.Natural
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
-- 'bitrate', 'mp2Settings_bitrate' - Specify the average bitrate in bits per second.
--
-- 'channels', 'mp2Settings_channels' - Set Channels to specify the number of channels in this output audio
-- track. Choosing Mono in the console will give you 1 output channel;
-- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- 'sampleRate', 'mp2Settings_sampleRate' - Sample rate in hz.
newMp2Settings ::
  Mp2Settings
newMp2Settings =
  Mp2Settings'
    { bitrate = Prelude.Nothing,
      channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Specify the average bitrate in bits per second.
mp2Settings_bitrate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_bitrate = Lens.lens (\Mp2Settings' {bitrate} -> bitrate) (\s@Mp2Settings' {} a -> s {bitrate = a} :: Mp2Settings)

-- | Set Channels to specify the number of channels in this output audio
-- track. Choosing Mono in the console will give you 1 output channel;
-- choosing Stereo will give you 2. In the API, valid values are 1 and 2.
mp2Settings_channels :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_channels = Lens.lens (\Mp2Settings' {channels} -> channels) (\s@Mp2Settings' {} a -> s {channels = a} :: Mp2Settings)

-- | Sample rate in hz.
mp2Settings_sampleRate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Natural)
mp2Settings_sampleRate = Lens.lens (\Mp2Settings' {sampleRate} -> sampleRate) (\s@Mp2Settings' {} a -> s {sampleRate = a} :: Mp2Settings)

instance Data.FromJSON Mp2Settings where
  parseJSON =
    Data.withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable Mp2Settings where
  hashWithSalt _salt Mp2Settings' {..} =
    _salt `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData Mp2Settings where
  rnf Mp2Settings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("channels" Data..=) Prelude.<$> channels,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
