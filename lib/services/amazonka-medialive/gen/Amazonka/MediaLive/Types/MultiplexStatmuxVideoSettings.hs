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
-- Module      : Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statmux rate control settings
--
-- /See:/ 'newMultiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'
  { -- | Maximum statmux bitrate.
    maximumBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Minimum statmux bitrate.
    minimumBitrate :: Prelude.Maybe Prelude.Natural,
    -- | The purpose of the priority is to use a combination of the\\nmultiplex
    -- rate control algorithm and the QVBR capability of the\\nencoder to
    -- prioritize the video quality of some channels in a\\nmultiplex over
    -- others. Channels that have a higher priority will\\nget higher video
    -- quality at the expense of the video quality of\\nother channels in the
    -- multiplex with lower priority.
    priority :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexStatmuxVideoSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumBitrate', 'multiplexStatmuxVideoSettings_maximumBitrate' - Maximum statmux bitrate.
--
-- 'minimumBitrate', 'multiplexStatmuxVideoSettings_minimumBitrate' - Minimum statmux bitrate.
--
-- 'priority', 'multiplexStatmuxVideoSettings_priority' - The purpose of the priority is to use a combination of the\\nmultiplex
-- rate control algorithm and the QVBR capability of the\\nencoder to
-- prioritize the video quality of some channels in a\\nmultiplex over
-- others. Channels that have a higher priority will\\nget higher video
-- quality at the expense of the video quality of\\nother channels in the
-- multiplex with lower priority.
newMultiplexStatmuxVideoSettings ::
  MultiplexStatmuxVideoSettings
newMultiplexStatmuxVideoSettings =
  MultiplexStatmuxVideoSettings'
    { maximumBitrate =
        Prelude.Nothing,
      minimumBitrate = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | Maximum statmux bitrate.
multiplexStatmuxVideoSettings_maximumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Prelude.Maybe Prelude.Natural)
multiplexStatmuxVideoSettings_maximumBitrate = Lens.lens (\MultiplexStatmuxVideoSettings' {maximumBitrate} -> maximumBitrate) (\s@MultiplexStatmuxVideoSettings' {} a -> s {maximumBitrate = a} :: MultiplexStatmuxVideoSettings)

-- | Minimum statmux bitrate.
multiplexStatmuxVideoSettings_minimumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Prelude.Maybe Prelude.Natural)
multiplexStatmuxVideoSettings_minimumBitrate = Lens.lens (\MultiplexStatmuxVideoSettings' {minimumBitrate} -> minimumBitrate) (\s@MultiplexStatmuxVideoSettings' {} a -> s {minimumBitrate = a} :: MultiplexStatmuxVideoSettings)

-- | The purpose of the priority is to use a combination of the\\nmultiplex
-- rate control algorithm and the QVBR capability of the\\nencoder to
-- prioritize the video quality of some channels in a\\nmultiplex over
-- others. Channels that have a higher priority will\\nget higher video
-- quality at the expense of the video quality of\\nother channels in the
-- multiplex with lower priority.
multiplexStatmuxVideoSettings_priority :: Lens.Lens' MultiplexStatmuxVideoSettings (Prelude.Maybe Prelude.Int)
multiplexStatmuxVideoSettings_priority = Lens.lens (\MultiplexStatmuxVideoSettings' {priority} -> priority) (\s@MultiplexStatmuxVideoSettings' {} a -> s {priority = a} :: MultiplexStatmuxVideoSettings)

instance Data.FromJSON MultiplexStatmuxVideoSettings where
  parseJSON =
    Data.withObject
      "MultiplexStatmuxVideoSettings"
      ( \x ->
          MultiplexStatmuxVideoSettings'
            Prelude.<$> (x Data..:? "maximumBitrate")
            Prelude.<*> (x Data..:? "minimumBitrate")
            Prelude.<*> (x Data..:? "priority")
      )

instance
  Prelude.Hashable
    MultiplexStatmuxVideoSettings
  where
  hashWithSalt _salt MultiplexStatmuxVideoSettings' {..} =
    _salt
      `Prelude.hashWithSalt` maximumBitrate
      `Prelude.hashWithSalt` minimumBitrate
      `Prelude.hashWithSalt` priority

instance Prelude.NFData MultiplexStatmuxVideoSettings where
  rnf MultiplexStatmuxVideoSettings' {..} =
    Prelude.rnf maximumBitrate `Prelude.seq`
      Prelude.rnf minimumBitrate `Prelude.seq`
        Prelude.rnf priority

instance Data.ToJSON MultiplexStatmuxVideoSettings where
  toJSON MultiplexStatmuxVideoSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maximumBitrate" Data..=)
              Prelude.<$> maximumBitrate,
            ("minimumBitrate" Data..=)
              Prelude.<$> minimumBitrate,
            ("priority" Data..=) Prelude.<$> priority
          ]
      )
