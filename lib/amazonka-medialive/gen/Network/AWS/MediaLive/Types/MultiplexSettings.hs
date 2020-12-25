{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettings
  ( MultiplexSettings (..),

    -- * Smart constructor
    mkMultiplexSettings,

    -- * Lenses
    mssTransportStreamBitrate,
    mssTransportStreamId,
    mssMaximumVideoBufferDelayMilliseconds,
    mssTransportStreamReservedBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'mkMultiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { -- | Transport stream bit rate.
    transportStreamBitrate :: Core.Natural,
    -- | Transport stream ID.
    transportStreamId :: Core.Natural,
    -- | Maximum video buffer delay in milliseconds.
    maximumVideoBufferDelayMilliseconds :: Core.Maybe Core.Natural,
    -- | Transport stream reserved bit rate.
    transportStreamReservedBitrate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexSettings' value with any optional fields omitted.
mkMultiplexSettings ::
  -- | 'transportStreamBitrate'
  Core.Natural ->
  -- | 'transportStreamId'
  Core.Natural ->
  MultiplexSettings
mkMultiplexSettings transportStreamBitrate transportStreamId =
  MultiplexSettings'
    { transportStreamBitrate,
      transportStreamId,
      maximumVideoBufferDelayMilliseconds = Core.Nothing,
      transportStreamReservedBitrate = Core.Nothing
    }

-- | Transport stream bit rate.
--
-- /Note:/ Consider using 'transportStreamBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamBitrate :: Lens.Lens' MultiplexSettings Core.Natural
mssTransportStreamBitrate = Lens.field @"transportStreamBitrate"
{-# DEPRECATED mssTransportStreamBitrate "Use generic-lens or generic-optics with 'transportStreamBitrate' instead." #-}

-- | Transport stream ID.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamId :: Lens.Lens' MultiplexSettings Core.Natural
mssTransportStreamId = Lens.field @"transportStreamId"
{-# DEPRECATED mssTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Maximum video buffer delay in milliseconds.
--
-- /Note:/ Consider using 'maximumVideoBufferDelayMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMaximumVideoBufferDelayMilliseconds :: Lens.Lens' MultiplexSettings (Core.Maybe Core.Natural)
mssMaximumVideoBufferDelayMilliseconds = Lens.field @"maximumVideoBufferDelayMilliseconds"
{-# DEPRECATED mssMaximumVideoBufferDelayMilliseconds "Use generic-lens or generic-optics with 'maximumVideoBufferDelayMilliseconds' instead." #-}

-- | Transport stream reserved bit rate.
--
-- /Note:/ Consider using 'transportStreamReservedBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamReservedBitrate :: Lens.Lens' MultiplexSettings (Core.Maybe Core.Natural)
mssTransportStreamReservedBitrate = Lens.field @"transportStreamReservedBitrate"
{-# DEPRECATED mssTransportStreamReservedBitrate "Use generic-lens or generic-optics with 'transportStreamReservedBitrate' instead." #-}

instance Core.FromJSON MultiplexSettings where
  toJSON MultiplexSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("transportStreamBitrate" Core..= transportStreamBitrate),
            Core.Just ("transportStreamId" Core..= transportStreamId),
            ("maximumVideoBufferDelayMilliseconds" Core..=)
              Core.<$> maximumVideoBufferDelayMilliseconds,
            ("transportStreamReservedBitrate" Core..=)
              Core.<$> transportStreamReservedBitrate
          ]
      )

instance Core.FromJSON MultiplexSettings where
  parseJSON =
    Core.withObject "MultiplexSettings" Core.$
      \x ->
        MultiplexSettings'
          Core.<$> (x Core..: "transportStreamBitrate")
          Core.<*> (x Core..: "transportStreamId")
          Core.<*> (x Core..:? "maximumVideoBufferDelayMilliseconds")
          Core.<*> (x Core..:? "transportStreamReservedBitrate")
