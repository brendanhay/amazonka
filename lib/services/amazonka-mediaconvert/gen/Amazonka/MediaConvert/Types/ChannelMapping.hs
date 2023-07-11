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
-- Module      : Amazonka.MediaConvert.Types.ChannelMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ChannelMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.OutputChannelMapping
import qualified Amazonka.Prelude as Prelude

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
--
-- /See:/ 'newChannelMapping' smart constructor.
data ChannelMapping = ChannelMapping'
  { -- | In your JSON job specification, include one child of OutputChannels for
    -- each audio channel that you want in your output. Each child should
    -- contain one instance of InputChannels or InputChannelsFineTune.
    outputChannels :: Prelude.Maybe [OutputChannelMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputChannels', 'channelMapping_outputChannels' - In your JSON job specification, include one child of OutputChannels for
-- each audio channel that you want in your output. Each child should
-- contain one instance of InputChannels or InputChannelsFineTune.
newChannelMapping ::
  ChannelMapping
newChannelMapping =
  ChannelMapping' {outputChannels = Prelude.Nothing}

-- | In your JSON job specification, include one child of OutputChannels for
-- each audio channel that you want in your output. Each child should
-- contain one instance of InputChannels or InputChannelsFineTune.
channelMapping_outputChannels :: Lens.Lens' ChannelMapping (Prelude.Maybe [OutputChannelMapping])
channelMapping_outputChannels = Lens.lens (\ChannelMapping' {outputChannels} -> outputChannels) (\s@ChannelMapping' {} a -> s {outputChannels = a} :: ChannelMapping) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ChannelMapping where
  parseJSON =
    Data.withObject
      "ChannelMapping"
      ( \x ->
          ChannelMapping'
            Prelude.<$> ( x
                            Data..:? "outputChannels"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ChannelMapping where
  hashWithSalt _salt ChannelMapping' {..} =
    _salt `Prelude.hashWithSalt` outputChannels

instance Prelude.NFData ChannelMapping where
  rnf ChannelMapping' {..} = Prelude.rnf outputChannels

instance Data.ToJSON ChannelMapping where
  toJSON ChannelMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("outputChannels" Data..=)
              Prelude.<$> outputChannels
          ]
      )
