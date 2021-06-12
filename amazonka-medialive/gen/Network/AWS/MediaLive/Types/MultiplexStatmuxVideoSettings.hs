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
-- Module      : Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Statmux rate control settings
--
-- /See:/ 'newMultiplexStatmuxVideoSettings' smart constructor.
data MultiplexStatmuxVideoSettings = MultiplexStatmuxVideoSettings'
  { -- | Minimum statmux bitrate.
    minimumBitrate :: Core.Maybe Core.Natural,
    -- | Maximum statmux bitrate.
    maximumBitrate :: Core.Maybe Core.Natural,
    -- | The purpose of the priority is to use a combination of the\\nmultiplex
    -- rate control algorithm and the QVBR capability of the\\nencoder to
    -- prioritize the video quality of some channels in a\\nmultiplex over
    -- others. Channels that have a higher priority will\\nget higher video
    -- quality at the expense of the video quality of\\nother channels in the
    -- multiplex with lower priority.
    priority :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexStatmuxVideoSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumBitrate', 'multiplexStatmuxVideoSettings_minimumBitrate' - Minimum statmux bitrate.
--
-- 'maximumBitrate', 'multiplexStatmuxVideoSettings_maximumBitrate' - Maximum statmux bitrate.
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
    { minimumBitrate =
        Core.Nothing,
      maximumBitrate = Core.Nothing,
      priority = Core.Nothing
    }

-- | Minimum statmux bitrate.
multiplexStatmuxVideoSettings_minimumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Natural)
multiplexStatmuxVideoSettings_minimumBitrate = Lens.lens (\MultiplexStatmuxVideoSettings' {minimumBitrate} -> minimumBitrate) (\s@MultiplexStatmuxVideoSettings' {} a -> s {minimumBitrate = a} :: MultiplexStatmuxVideoSettings)

-- | Maximum statmux bitrate.
multiplexStatmuxVideoSettings_maximumBitrate :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Natural)
multiplexStatmuxVideoSettings_maximumBitrate = Lens.lens (\MultiplexStatmuxVideoSettings' {maximumBitrate} -> maximumBitrate) (\s@MultiplexStatmuxVideoSettings' {} a -> s {maximumBitrate = a} :: MultiplexStatmuxVideoSettings)

-- | The purpose of the priority is to use a combination of the\\nmultiplex
-- rate control algorithm and the QVBR capability of the\\nencoder to
-- prioritize the video quality of some channels in a\\nmultiplex over
-- others. Channels that have a higher priority will\\nget higher video
-- quality at the expense of the video quality of\\nother channels in the
-- multiplex with lower priority.
multiplexStatmuxVideoSettings_priority :: Lens.Lens' MultiplexStatmuxVideoSettings (Core.Maybe Core.Int)
multiplexStatmuxVideoSettings_priority = Lens.lens (\MultiplexStatmuxVideoSettings' {priority} -> priority) (\s@MultiplexStatmuxVideoSettings' {} a -> s {priority = a} :: MultiplexStatmuxVideoSettings)

instance Core.FromJSON MultiplexStatmuxVideoSettings where
  parseJSON =
    Core.withObject
      "MultiplexStatmuxVideoSettings"
      ( \x ->
          MultiplexStatmuxVideoSettings'
            Core.<$> (x Core..:? "minimumBitrate")
            Core.<*> (x Core..:? "maximumBitrate")
            Core.<*> (x Core..:? "priority")
      )

instance Core.Hashable MultiplexStatmuxVideoSettings

instance Core.NFData MultiplexStatmuxVideoSettings

instance Core.ToJSON MultiplexStatmuxVideoSettings where
  toJSON MultiplexStatmuxVideoSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("minimumBitrate" Core..=) Core.<$> minimumBitrate,
            ("maximumBitrate" Core..=) Core.<$> maximumBitrate,
            ("priority" Core..=) Core.<$> priority
          ]
      )
