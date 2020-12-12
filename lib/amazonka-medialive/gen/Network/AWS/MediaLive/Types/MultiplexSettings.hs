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
    msMaximumVideoBufferDelayMilliseconds,
    msTransportStreamReservedBitrate,
    msTransportStreamBitrate,
    msTransportStreamId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'mkMultiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { maximumVideoBufferDelayMilliseconds ::
      Lude.Maybe Lude.Natural,
    transportStreamReservedBitrate ::
      Lude.Maybe Lude.Natural,
    transportStreamBitrate :: Lude.Natural,
    transportStreamId :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexSettings' with the minimum fields required to make a request.
--
-- * 'maximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
-- * 'transportStreamBitrate' - Transport stream bit rate.
-- * 'transportStreamId' - Transport stream ID.
-- * 'transportStreamReservedBitrate' - Transport stream reserved bit rate.
mkMultiplexSettings ::
  -- | 'transportStreamBitrate'
  Lude.Natural ->
  -- | 'transportStreamId'
  Lude.Natural ->
  MultiplexSettings
mkMultiplexSettings pTransportStreamBitrate_ pTransportStreamId_ =
  MultiplexSettings'
    { maximumVideoBufferDelayMilliseconds =
        Lude.Nothing,
      transportStreamReservedBitrate = Lude.Nothing,
      transportStreamBitrate = pTransportStreamBitrate_,
      transportStreamId = pTransportStreamId_
    }

-- | Maximum video buffer delay in milliseconds.
--
-- /Note:/ Consider using 'maximumVideoBufferDelayMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMaximumVideoBufferDelayMilliseconds :: Lens.Lens' MultiplexSettings (Lude.Maybe Lude.Natural)
msMaximumVideoBufferDelayMilliseconds = Lens.lens (maximumVideoBufferDelayMilliseconds :: MultiplexSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maximumVideoBufferDelayMilliseconds = a} :: MultiplexSettings)
{-# DEPRECATED msMaximumVideoBufferDelayMilliseconds "Use generic-lens or generic-optics with 'maximumVideoBufferDelayMilliseconds' instead." #-}

-- | Transport stream reserved bit rate.
--
-- /Note:/ Consider using 'transportStreamReservedBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamReservedBitrate :: Lens.Lens' MultiplexSettings (Lude.Maybe Lude.Natural)
msTransportStreamReservedBitrate = Lens.lens (transportStreamReservedBitrate :: MultiplexSettings -> Lude.Maybe Lude.Natural) (\s a -> s {transportStreamReservedBitrate = a} :: MultiplexSettings)
{-# DEPRECATED msTransportStreamReservedBitrate "Use generic-lens or generic-optics with 'transportStreamReservedBitrate' instead." #-}

-- | Transport stream bit rate.
--
-- /Note:/ Consider using 'transportStreamBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamBitrate :: Lens.Lens' MultiplexSettings Lude.Natural
msTransportStreamBitrate = Lens.lens (transportStreamBitrate :: MultiplexSettings -> Lude.Natural) (\s a -> s {transportStreamBitrate = a} :: MultiplexSettings)
{-# DEPRECATED msTransportStreamBitrate "Use generic-lens or generic-optics with 'transportStreamBitrate' instead." #-}

-- | Transport stream ID.
--
-- /Note:/ Consider using 'transportStreamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTransportStreamId :: Lens.Lens' MultiplexSettings Lude.Natural
msTransportStreamId = Lens.lens (transportStreamId :: MultiplexSettings -> Lude.Natural) (\s a -> s {transportStreamId = a} :: MultiplexSettings)
{-# DEPRECATED msTransportStreamId "Use generic-lens or generic-optics with 'transportStreamId' instead." #-}

instance Lude.FromJSON MultiplexSettings where
  parseJSON =
    Lude.withObject
      "MultiplexSettings"
      ( \x ->
          MultiplexSettings'
            Lude.<$> (x Lude..:? "maximumVideoBufferDelayMilliseconds")
            Lude.<*> (x Lude..:? "transportStreamReservedBitrate")
            Lude.<*> (x Lude..: "transportStreamBitrate")
            Lude.<*> (x Lude..: "transportStreamId")
      )

instance Lude.ToJSON MultiplexSettings where
  toJSON MultiplexSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maximumVideoBufferDelayMilliseconds" Lude..=)
              Lude.<$> maximumVideoBufferDelayMilliseconds,
            ("transportStreamReservedBitrate" Lude..=)
              Lude.<$> transportStreamReservedBitrate,
            Lude.Just
              ("transportStreamBitrate" Lude..= transportStreamBitrate),
            Lude.Just ("transportStreamId" Lude..= transportStreamId)
          ]
      )
