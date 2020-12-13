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
    mssTransportStreamId,
    mssMaximumVideoBufferDelayMilliseconds,
    mssTransportStreamBitrate,
    mssTransportStreamReservedBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'mkMultiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { -- | Transport stream ID.
    transportStreamId :: Lude.Natural,
    -- | Maximum video buffer delay in milliseconds.
    maximumVideoBufferDelayMilliseconds :: Lude.Maybe Lude.Natural,
    -- | Transport stream bit rate.
    transportStreamBitrate :: Lude.Natural,
    -- | Transport stream reserved bit rate.
    transportStreamReservedBitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexSettings' with the minimum fields required to make a request.
--
-- * 'transportStreamId' - Transport stream ID.
-- * 'maximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
-- * 'transportStreamBitrate' - Transport stream bit rate.
-- * 'transportStreamReservedBitrate' - Transport stream reserved bit rate.
mkMultiplexSettings ::
  -- | 'transportStreamId'
  Lude.Natural ->
  -- | 'transportStreamBitrate'
  Lude.Natural ->
  MultiplexSettings
mkMultiplexSettings pTransportStreamId_ pTransportStreamBitrate_ =
  MultiplexSettings'
    { transportStreamId = pTransportStreamId_,
      maximumVideoBufferDelayMilliseconds = Lude.Nothing,
      transportStreamBitrate = pTransportStreamBitrate_,
      transportStreamReservedBitrate = Lude.Nothing
    }

-- | Transport stream ID.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamId :: Lens.Lens' MultiplexSettings Lude.Natural
mssTransportStreamId = Lens.lens (transportStreamId :: MultiplexSettings -> Lude.Natural) (\s a -> s {transportStreamId = a} :: MultiplexSettings)
{-# DEPRECATED mssTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

-- | Maximum video buffer delay in milliseconds.
--
-- /Note:/ Consider using 'maximumVideoBufferDelayMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMaximumVideoBufferDelayMilliseconds :: Lens.Lens' MultiplexSettings (Lude.Maybe Lude.Natural)
mssMaximumVideoBufferDelayMilliseconds = Lens.lens (maximumVideoBufferDelayMilliseconds :: MultiplexSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maximumVideoBufferDelayMilliseconds = a} :: MultiplexSettings)
{-# DEPRECATED mssMaximumVideoBufferDelayMilliseconds "Use generic-lens or generic-optics with 'maximumVideoBufferDelayMilliseconds' instead." #-}

-- | Transport stream bit rate.
--
-- /Note:/ Consider using 'transportStreamBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamBitrate :: Lens.Lens' MultiplexSettings Lude.Natural
mssTransportStreamBitrate = Lens.lens (transportStreamBitrate :: MultiplexSettings -> Lude.Natural) (\s a -> s {transportStreamBitrate = a} :: MultiplexSettings)
{-# DEPRECATED mssTransportStreamBitrate "Use generic-lens or generic-optics with 'transportStreamBitrate' instead." #-}

-- | Transport stream reserved bit rate.
--
-- /Note:/ Consider using 'transportStreamReservedBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssTransportStreamReservedBitrate :: Lens.Lens' MultiplexSettings (Lude.Maybe Lude.Natural)
mssTransportStreamReservedBitrate = Lens.lens (transportStreamReservedBitrate :: MultiplexSettings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamReservedBitrate = a} :: MultiplexSettings)
{-# DEPRECATED mssTransportStreamReservedBitrate "Use generic-lens or generic-optics with 'transportStreamReservedBitrate' instead." #-}

instance Lude.FromJSON MultiplexSettings where
  parseJSON =
    Lude.withObject
      "MultiplexSettings"
      ( \x ->
          MultiplexSettings'
            Lude.<$> (x Lude..: "transportStreamId")
            Lude.<*> (x Lude..:? "maximumVideoBufferDelayMilliseconds")
            Lude.<*> (x Lude..: "transportStreamBitrate")
            Lude.<*> (x Lude..:? "transportStreamReservedBitrate")
      )

instance Lude.ToJSON MultiplexSettings where
  toJSON MultiplexSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("transportStreamId" Lude..= transportStreamId),
            ("maximumVideoBufferDelayMilliseconds" Lude..=)
              Lude.<$> maximumVideoBufferDelayMilliseconds,
            Lude.Just
              ("transportStreamBitrate" Lude..= transportStreamBitrate),
            ("transportStreamReservedBitrate" Lude..=)
              Lude.<$> transportStreamReservedBitrate
          ]
      )
