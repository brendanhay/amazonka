{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorPid
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorPid
  ( VideoSelectorPid (..),

    -- * Smart constructor
    mkVideoSelectorPid,

    -- * Lenses
    vspPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Video Selector Pid
--
-- /See:/ 'mkVideoSelectorPid' smart constructor.
newtype VideoSelectorPid = VideoSelectorPid'
  { -- | Selects a specific PID from within a video source.
    pid :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoSelectorPid' with the minimum fields required to make a request.
--
-- * 'pid' - Selects a specific PID from within a video source.
mkVideoSelectorPid ::
  VideoSelectorPid
mkVideoSelectorPid = VideoSelectorPid' {pid = Lude.Nothing}

-- | Selects a specific PID from within a video source.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspPid :: Lens.Lens' VideoSelectorPid (Lude.Maybe Lude.Natural)
vspPid = Lens.lens (pid :: VideoSelectorPid -> Lude.Maybe Lude.Natural) (\s a -> s {pid = a} :: VideoSelectorPid)
{-# DEPRECATED vspPid "Use generic-lens or generic-optics with 'pid' instead." #-}

instance Lude.FromJSON VideoSelectorPid where
  parseJSON =
    Lude.withObject
      "VideoSelectorPid"
      (\x -> VideoSelectorPid' Lude.<$> (x Lude..:? "pid"))

instance Lude.ToJSON VideoSelectorPid where
  toJSON VideoSelectorPid' {..} =
    Lude.object (Lude.catMaybes [("pid" Lude..=) Lude.<$> pid])
