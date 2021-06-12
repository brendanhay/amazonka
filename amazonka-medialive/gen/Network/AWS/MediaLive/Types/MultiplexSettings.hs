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
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'newMultiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { -- | Transport stream reserved bit rate.
    transportStreamReservedBitrate :: Core.Maybe Core.Natural,
    -- | Maximum video buffer delay in milliseconds.
    maximumVideoBufferDelayMilliseconds :: Core.Maybe Core.Natural,
    -- | Transport stream bit rate.
    transportStreamBitrate :: Core.Natural,
    -- | Transport stream ID.
    transportStreamId :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transportStreamReservedBitrate', 'multiplexSettings_transportStreamReservedBitrate' - Transport stream reserved bit rate.
--
-- 'maximumVideoBufferDelayMilliseconds', 'multiplexSettings_maximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
--
-- 'transportStreamBitrate', 'multiplexSettings_transportStreamBitrate' - Transport stream bit rate.
--
-- 'transportStreamId', 'multiplexSettings_transportStreamId' - Transport stream ID.
newMultiplexSettings ::
  -- | 'transportStreamBitrate'
  Core.Natural ->
  -- | 'transportStreamId'
  Core.Natural ->
  MultiplexSettings
newMultiplexSettings
  pTransportStreamBitrate_
  pTransportStreamId_ =
    MultiplexSettings'
      { transportStreamReservedBitrate =
          Core.Nothing,
        maximumVideoBufferDelayMilliseconds = Core.Nothing,
        transportStreamBitrate = pTransportStreamBitrate_,
        transportStreamId = pTransportStreamId_
      }

-- | Transport stream reserved bit rate.
multiplexSettings_transportStreamReservedBitrate :: Lens.Lens' MultiplexSettings (Core.Maybe Core.Natural)
multiplexSettings_transportStreamReservedBitrate = Lens.lens (\MultiplexSettings' {transportStreamReservedBitrate} -> transportStreamReservedBitrate) (\s@MultiplexSettings' {} a -> s {transportStreamReservedBitrate = a} :: MultiplexSettings)

-- | Maximum video buffer delay in milliseconds.
multiplexSettings_maximumVideoBufferDelayMilliseconds :: Lens.Lens' MultiplexSettings (Core.Maybe Core.Natural)
multiplexSettings_maximumVideoBufferDelayMilliseconds = Lens.lens (\MultiplexSettings' {maximumVideoBufferDelayMilliseconds} -> maximumVideoBufferDelayMilliseconds) (\s@MultiplexSettings' {} a -> s {maximumVideoBufferDelayMilliseconds = a} :: MultiplexSettings)

-- | Transport stream bit rate.
multiplexSettings_transportStreamBitrate :: Lens.Lens' MultiplexSettings Core.Natural
multiplexSettings_transportStreamBitrate = Lens.lens (\MultiplexSettings' {transportStreamBitrate} -> transportStreamBitrate) (\s@MultiplexSettings' {} a -> s {transportStreamBitrate = a} :: MultiplexSettings)

-- | Transport stream ID.
multiplexSettings_transportStreamId :: Lens.Lens' MultiplexSettings Core.Natural
multiplexSettings_transportStreamId = Lens.lens (\MultiplexSettings' {transportStreamId} -> transportStreamId) (\s@MultiplexSettings' {} a -> s {transportStreamId = a} :: MultiplexSettings)

instance Core.FromJSON MultiplexSettings where
  parseJSON =
    Core.withObject
      "MultiplexSettings"
      ( \x ->
          MultiplexSettings'
            Core.<$> (x Core..:? "transportStreamReservedBitrate")
            Core.<*> (x Core..:? "maximumVideoBufferDelayMilliseconds")
            Core.<*> (x Core..: "transportStreamBitrate")
            Core.<*> (x Core..: "transportStreamId")
      )

instance Core.Hashable MultiplexSettings

instance Core.NFData MultiplexSettings

instance Core.ToJSON MultiplexSettings where
  toJSON MultiplexSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("transportStreamReservedBitrate" Core..=)
              Core.<$> transportStreamReservedBitrate,
            ("maximumVideoBufferDelayMilliseconds" Core..=)
              Core.<$> maximumVideoBufferDelayMilliseconds,
            Core.Just
              ( "transportStreamBitrate"
                  Core..= transportStreamBitrate
              ),
            Core.Just
              ("transportStreamId" Core..= transportStreamId)
          ]
      )
