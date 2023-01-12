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
-- Module      : Amazonka.MediaConvert.Types.RemixSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.RemixSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.ChannelMapping
import qualified Amazonka.Prelude as Prelude

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for
-- each audio channel in each output of your job. With audio remixing, you
-- can output more or fewer audio channels than your input audio source
-- provides.
--
-- /See:/ 'newRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Channel mapping (ChannelMapping) contains the group of fields that hold
    -- the remixing value for each channel, in dB. Specify remix values to
    -- indicate how much of the content from your input audio channel you want
    -- in your output audio channels. Each instance of the InputChannels or
    -- InputChannelsFineTune array specifies these values for one output
    -- channel. Use one instance of this array for each output channel. In the
    -- console, each array corresponds to a column in the graphical depiction
    -- of the mapping matrix. The rows of the graphical matrix correspond to
    -- input channels. Valid values are within the range from -60 (mute)
    -- through 6. A setting of 0 passes the input channel unchanged to the
    -- output channel (no attenuation or amplification). Use InputChannels or
    -- InputChannelsFineTune to specify your remix values. Don\'t use both.
    channelMapping :: Prelude.Maybe ChannelMapping,
    -- | Specify the number of audio channels from your input that you want to
    -- use in your output. With remixing, you might combine or split the data
    -- in these channels, so the number of channels in your final output might
    -- be different. If you are doing both input channel mapping and output
    -- channel mapping, the number of output channels in your input mapping
    -- must be the same as the number of input channels in your output mapping.
    channelsIn :: Prelude.Maybe Prelude.Natural,
    -- | Specify the number of channels in this output after remixing. Valid
    -- values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.) If you are
    -- doing both input channel mapping and output channel mapping, the number
    -- of output channels in your input mapping must be the same as the number
    -- of input channels in your output mapping.
    channelsOut :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemixSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelMapping', 'remixSettings_channelMapping' - Channel mapping (ChannelMapping) contains the group of fields that hold
-- the remixing value for each channel, in dB. Specify remix values to
-- indicate how much of the content from your input audio channel you want
-- in your output audio channels. Each instance of the InputChannels or
-- InputChannelsFineTune array specifies these values for one output
-- channel. Use one instance of this array for each output channel. In the
-- console, each array corresponds to a column in the graphical depiction
-- of the mapping matrix. The rows of the graphical matrix correspond to
-- input channels. Valid values are within the range from -60 (mute)
-- through 6. A setting of 0 passes the input channel unchanged to the
-- output channel (no attenuation or amplification). Use InputChannels or
-- InputChannelsFineTune to specify your remix values. Don\'t use both.
--
-- 'channelsIn', 'remixSettings_channelsIn' - Specify the number of audio channels from your input that you want to
-- use in your output. With remixing, you might combine or split the data
-- in these channels, so the number of channels in your final output might
-- be different. If you are doing both input channel mapping and output
-- channel mapping, the number of output channels in your input mapping
-- must be the same as the number of input channels in your output mapping.
--
-- 'channelsOut', 'remixSettings_channelsOut' - Specify the number of channels in this output after remixing. Valid
-- values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.) If you are
-- doing both input channel mapping and output channel mapping, the number
-- of output channels in your input mapping must be the same as the number
-- of input channels in your output mapping.
newRemixSettings ::
  RemixSettings
newRemixSettings =
  RemixSettings'
    { channelMapping = Prelude.Nothing,
      channelsIn = Prelude.Nothing,
      channelsOut = Prelude.Nothing
    }

-- | Channel mapping (ChannelMapping) contains the group of fields that hold
-- the remixing value for each channel, in dB. Specify remix values to
-- indicate how much of the content from your input audio channel you want
-- in your output audio channels. Each instance of the InputChannels or
-- InputChannelsFineTune array specifies these values for one output
-- channel. Use one instance of this array for each output channel. In the
-- console, each array corresponds to a column in the graphical depiction
-- of the mapping matrix. The rows of the graphical matrix correspond to
-- input channels. Valid values are within the range from -60 (mute)
-- through 6. A setting of 0 passes the input channel unchanged to the
-- output channel (no attenuation or amplification). Use InputChannels or
-- InputChannelsFineTune to specify your remix values. Don\'t use both.
remixSettings_channelMapping :: Lens.Lens' RemixSettings (Prelude.Maybe ChannelMapping)
remixSettings_channelMapping = Lens.lens (\RemixSettings' {channelMapping} -> channelMapping) (\s@RemixSettings' {} a -> s {channelMapping = a} :: RemixSettings)

-- | Specify the number of audio channels from your input that you want to
-- use in your output. With remixing, you might combine or split the data
-- in these channels, so the number of channels in your final output might
-- be different. If you are doing both input channel mapping and output
-- channel mapping, the number of output channels in your input mapping
-- must be the same as the number of input channels in your output mapping.
remixSettings_channelsIn :: Lens.Lens' RemixSettings (Prelude.Maybe Prelude.Natural)
remixSettings_channelsIn = Lens.lens (\RemixSettings' {channelsIn} -> channelsIn) (\s@RemixSettings' {} a -> s {channelsIn = a} :: RemixSettings)

-- | Specify the number of channels in this output after remixing. Valid
-- values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.) If you are
-- doing both input channel mapping and output channel mapping, the number
-- of output channels in your input mapping must be the same as the number
-- of input channels in your output mapping.
remixSettings_channelsOut :: Lens.Lens' RemixSettings (Prelude.Maybe Prelude.Natural)
remixSettings_channelsOut = Lens.lens (\RemixSettings' {channelsOut} -> channelsOut) (\s@RemixSettings' {} a -> s {channelsOut = a} :: RemixSettings)

instance Data.FromJSON RemixSettings where
  parseJSON =
    Data.withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            Prelude.<$> (x Data..:? "channelMapping")
            Prelude.<*> (x Data..:? "channelsIn")
            Prelude.<*> (x Data..:? "channelsOut")
      )

instance Prelude.Hashable RemixSettings where
  hashWithSalt _salt RemixSettings' {..} =
    _salt `Prelude.hashWithSalt` channelMapping
      `Prelude.hashWithSalt` channelsIn
      `Prelude.hashWithSalt` channelsOut

instance Prelude.NFData RemixSettings where
  rnf RemixSettings' {..} =
    Prelude.rnf channelMapping
      `Prelude.seq` Prelude.rnf channelsIn
      `Prelude.seq` Prelude.rnf channelsOut

instance Data.ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channelMapping" Data..=)
              Prelude.<$> channelMapping,
            ("channelsIn" Data..=) Prelude.<$> channelsIn,
            ("channelsOut" Data..=) Prelude.<$> channelsOut
          ]
      )
