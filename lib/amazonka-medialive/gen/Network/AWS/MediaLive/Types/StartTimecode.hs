{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StartTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StartTimecode
  ( StartTimecode (..),

    -- * Smart constructor
    mkStartTimecode,

    -- * Lenses
    sTimecode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings to identify the start of the clip.
--
-- /See:/ 'mkStartTimecode' smart constructor.
newtype StartTimecode = StartTimecode'
  { -- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
    timecode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTimecode' with the minimum fields required to make a request.
--
-- * 'timecode' - The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
mkStartTimecode ::
  StartTimecode
mkStartTimecode = StartTimecode' {timecode = Lude.Nothing}

-- | The timecode for the frame where you want to start the clip. Optional; if not specified, the clip starts at first frame in the file. Enter the timecode as HH:MM:SS:FF or HH:MM:SS;FF.
--
-- /Note:/ Consider using 'timecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimecode :: Lens.Lens' StartTimecode (Lude.Maybe Lude.Text)
sTimecode = Lens.lens (timecode :: StartTimecode -> Lude.Maybe Lude.Text) (\s a -> s {timecode = a} :: StartTimecode)
{-# DEPRECATED sTimecode "Use generic-lens or generic-optics with 'timecode' instead." #-}

instance Lude.FromJSON StartTimecode where
  parseJSON =
    Lude.withObject
      "StartTimecode"
      (\x -> StartTimecode' Lude.<$> (x Lude..:? "timecode"))

instance Lude.ToJSON StartTimecode where
  toJSON StartTimecode' {..} =
    Lude.object
      (Lude.catMaybes [("timecode" Lude..=) Lude.<$> timecode])
