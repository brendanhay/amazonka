{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSpecification
  ( InputSpecification (..),

    -- * Smart constructor
    mkInputSpecification,

    -- * Lenses
    isResolution,
    isCodec,
    isMaximumBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputCodec
import Network.AWS.MediaLive.Types.InputMaximumBitrate
import Network.AWS.MediaLive.Types.InputResolution
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for InputSpecification
--
-- /See:/ 'mkInputSpecification' smart constructor.
data InputSpecification = InputSpecification'
  { resolution ::
      Lude.Maybe InputResolution,
    codec :: Lude.Maybe InputCodec,
    maximumBitrate :: Lude.Maybe InputMaximumBitrate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSpecification' with the minimum fields required to make a request.
--
-- * 'codec' - Input codec
-- * 'maximumBitrate' - Maximum input bitrate, categorized coarsely
-- * 'resolution' - Input resolution, categorized coarsely
mkInputSpecification ::
  InputSpecification
mkInputSpecification =
  InputSpecification'
    { resolution = Lude.Nothing,
      codec = Lude.Nothing,
      maximumBitrate = Lude.Nothing
    }

-- | Input resolution, categorized coarsely
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isResolution :: Lens.Lens' InputSpecification (Lude.Maybe InputResolution)
isResolution = Lens.lens (resolution :: InputSpecification -> Lude.Maybe InputResolution) (\s a -> s {resolution = a} :: InputSpecification)
{-# DEPRECATED isResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | Input codec
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCodec :: Lens.Lens' InputSpecification (Lude.Maybe InputCodec)
isCodec = Lens.lens (codec :: InputSpecification -> Lude.Maybe InputCodec) (\s a -> s {codec = a} :: InputSpecification)
{-# DEPRECATED isCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Maximum input bitrate, categorized coarsely
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isMaximumBitrate :: Lens.Lens' InputSpecification (Lude.Maybe InputMaximumBitrate)
isMaximumBitrate = Lens.lens (maximumBitrate :: InputSpecification -> Lude.Maybe InputMaximumBitrate) (\s a -> s {maximumBitrate = a} :: InputSpecification)
{-# DEPRECATED isMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

instance Lude.FromJSON InputSpecification where
  parseJSON =
    Lude.withObject
      "InputSpecification"
      ( \x ->
          InputSpecification'
            Lude.<$> (x Lude..:? "resolution")
            Lude.<*> (x Lude..:? "codec")
            Lude.<*> (x Lude..:? "maximumBitrate")
      )

instance Lude.ToJSON InputSpecification where
  toJSON InputSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resolution" Lude..=) Lude.<$> resolution,
            ("codec" Lude..=) Lude.<$> codec,
            ("maximumBitrate" Lude..=) Lude.<$> maximumBitrate
          ]
      )
