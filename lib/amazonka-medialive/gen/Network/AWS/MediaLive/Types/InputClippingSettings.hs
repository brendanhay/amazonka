-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputClippingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputClippingSettings
  ( InputClippingSettings (..),

    -- * Smart constructor
    mkInputClippingSettings,

    -- * Lenses
    icsStopTimecode,
    icsStartTimecode,
    icsInputTimecodeSource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputTimecodeSource
import Network.AWS.MediaLive.Types.StartTimecode
import Network.AWS.MediaLive.Types.StopTimecode
import qualified Network.AWS.Prelude as Lude

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /See:/ 'mkInputClippingSettings' smart constructor.
data InputClippingSettings = InputClippingSettings'
  { stopTimecode ::
      Lude.Maybe StopTimecode,
    startTimecode :: Lude.Maybe StartTimecode,
    inputTimecodeSource :: InputTimecodeSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputClippingSettings' with the minimum fields required to make a request.
--
-- * 'inputTimecodeSource' - The source of the timecodes in the source being clipped.
-- * 'startTimecode' - Settings to identify the start of the clip.
-- * 'stopTimecode' - Settings to identify the end of the clip.
mkInputClippingSettings ::
  -- | 'inputTimecodeSource'
  InputTimecodeSource ->
  InputClippingSettings
mkInputClippingSettings pInputTimecodeSource_ =
  InputClippingSettings'
    { stopTimecode = Lude.Nothing,
      startTimecode = Lude.Nothing,
      inputTimecodeSource = pInputTimecodeSource_
    }

-- | Settings to identify the end of the clip.
--
-- /Note:/ Consider using 'stopTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsStopTimecode :: Lens.Lens' InputClippingSettings (Lude.Maybe StopTimecode)
icsStopTimecode = Lens.lens (stopTimecode :: InputClippingSettings -> Lude.Maybe StopTimecode) (\s a -> s {stopTimecode = a} :: InputClippingSettings)
{-# DEPRECATED icsStopTimecode "Use generic-lens or generic-optics with 'stopTimecode' instead." #-}

-- | Settings to identify the start of the clip.
--
-- /Note:/ Consider using 'startTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsStartTimecode :: Lens.Lens' InputClippingSettings (Lude.Maybe StartTimecode)
icsStartTimecode = Lens.lens (startTimecode :: InputClippingSettings -> Lude.Maybe StartTimecode) (\s a -> s {startTimecode = a} :: InputClippingSettings)
{-# DEPRECATED icsStartTimecode "Use generic-lens or generic-optics with 'startTimecode' instead." #-}

-- | The source of the timecodes in the source being clipped.
--
-- /Note:/ Consider using 'inputTimecodeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsInputTimecodeSource :: Lens.Lens' InputClippingSettings InputTimecodeSource
icsInputTimecodeSource = Lens.lens (inputTimecodeSource :: InputClippingSettings -> InputTimecodeSource) (\s a -> s {inputTimecodeSource = a} :: InputClippingSettings)
{-# DEPRECATED icsInputTimecodeSource "Use generic-lens or generic-optics with 'inputTimecodeSource' instead." #-}

instance Lude.FromJSON InputClippingSettings where
  parseJSON =
    Lude.withObject
      "InputClippingSettings"
      ( \x ->
          InputClippingSettings'
            Lude.<$> (x Lude..:? "stopTimecode")
            Lude.<*> (x Lude..:? "startTimecode")
            Lude.<*> (x Lude..: "inputTimecodeSource")
      )

instance Lude.ToJSON InputClippingSettings where
  toJSON InputClippingSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stopTimecode" Lude..=) Lude.<$> stopTimecode,
            ("startTimecode" Lude..=) Lude.<$> startTimecode,
            Lude.Just ("inputTimecodeSource" Lude..= inputTimecodeSource)
          ]
      )
