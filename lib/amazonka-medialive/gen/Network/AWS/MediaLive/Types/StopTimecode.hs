{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StopTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StopTimecode
  ( StopTimecode (..),

    -- * Smart constructor
    mkStopTimecode,

    -- * Lenses
    stLastFrameClippingBehavior,
    stTimecode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.LastFrameClippingBehavior
import qualified Network.AWS.Prelude as Lude

-- | Settings to identify the end of the clip.
--
-- /See:/ 'mkStopTimecode' smart constructor.
data StopTimecode = StopTimecode'
  { lastFrameClippingBehavior ::
      Lude.Maybe LastFrameClippingBehavior,
    timecode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTimecode' with the minimum fields required to make a request.
--
-- * 'lastFrameClippingBehavior' - If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
-- * 'timecode' - The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
mkStopTimecode ::
  StopTimecode
mkStopTimecode =
  StopTimecode'
    { lastFrameClippingBehavior = Lude.Nothing,
      timecode = Lude.Nothing
    }

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
--
-- /Note:/ Consider using 'lastFrameClippingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stLastFrameClippingBehavior :: Lens.Lens' StopTimecode (Lude.Maybe LastFrameClippingBehavior)
stLastFrameClippingBehavior = Lens.lens (lastFrameClippingBehavior :: StopTimecode -> Lude.Maybe LastFrameClippingBehavior) (\s a -> s {lastFrameClippingBehavior = a} :: StopTimecode)
{-# DEPRECATED stLastFrameClippingBehavior "Use generic-lens or generic-optics with 'lastFrameClippingBehavior' instead." #-}

-- | The timecode for the frame where you want to stop the clip. Optional; if not specified, the clip continues to the end of the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTimecode :: Lens.Lens' StopTimecode (Lude.Maybe Lude.Text)
stTimecode = Lens.lens (timecode :: StopTimecode -> Lude.Maybe Lude.Text) (\s a -> s {timecode = a} :: StopTimecode)
{-# DEPRECATED stTimecode "Use generic-lens or generic-optics with 'timecode' instead." #-}

instance Lude.FromJSON StopTimecode where
  parseJSON =
    Lude.withObject
      "StopTimecode"
      ( \x ->
          StopTimecode'
            Lude.<$> (x Lude..:? "lastFrameClippingBehavior")
            Lude.<*> (x Lude..:? "timecode")
      )

instance Lude.ToJSON StopTimecode where
  toJSON StopTimecode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lastFrameClippingBehavior" Lude..=)
              Lude.<$> lastFrameClippingBehavior,
            ("timecode" Lude..=) Lude.<$> timecode
          ]
      )
