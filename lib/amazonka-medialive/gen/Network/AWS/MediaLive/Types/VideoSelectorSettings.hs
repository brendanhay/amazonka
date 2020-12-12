{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorSettings
  ( VideoSelectorSettings (..),

    -- * Smart constructor
    mkVideoSelectorSettings,

    -- * Lenses
    vssVideoSelectorProgramId,
    vssVideoSelectorPid,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoSelectorPid
import Network.AWS.MediaLive.Types.VideoSelectorProgramId
import qualified Network.AWS.Prelude as Lude

-- | Video Selector Settings
--
-- /See:/ 'mkVideoSelectorSettings' smart constructor.
data VideoSelectorSettings = VideoSelectorSettings'
  { videoSelectorProgramId ::
      Lude.Maybe VideoSelectorProgramId,
    videoSelectorPid :: Lude.Maybe VideoSelectorPid
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoSelectorSettings' with the minimum fields required to make a request.
--
-- * 'videoSelectorPid' - Undocumented field.
-- * 'videoSelectorProgramId' - Undocumented field.
mkVideoSelectorSettings ::
  VideoSelectorSettings
mkVideoSelectorSettings =
  VideoSelectorSettings'
    { videoSelectorProgramId = Lude.Nothing,
      videoSelectorPid = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoSelectorProgramId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssVideoSelectorProgramId :: Lens.Lens' VideoSelectorSettings (Lude.Maybe VideoSelectorProgramId)
vssVideoSelectorProgramId = Lens.lens (videoSelectorProgramId :: VideoSelectorSettings -> Lude.Maybe VideoSelectorProgramId) (\s a -> s {videoSelectorProgramId = a} :: VideoSelectorSettings)
{-# DEPRECATED vssVideoSelectorProgramId "Use generic-lens or generic-optics with 'videoSelectorProgramId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoSelectorPid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssVideoSelectorPid :: Lens.Lens' VideoSelectorSettings (Lude.Maybe VideoSelectorPid)
vssVideoSelectorPid = Lens.lens (videoSelectorPid :: VideoSelectorSettings -> Lude.Maybe VideoSelectorPid) (\s a -> s {videoSelectorPid = a} :: VideoSelectorSettings)
{-# DEPRECATED vssVideoSelectorPid "Use generic-lens or generic-optics with 'videoSelectorPid' instead." #-}

instance Lude.FromJSON VideoSelectorSettings where
  parseJSON =
    Lude.withObject
      "VideoSelectorSettings"
      ( \x ->
          VideoSelectorSettings'
            Lude.<$> (x Lude..:? "videoSelectorProgramId")
            Lude.<*> (x Lude..:? "videoSelectorPid")
      )

instance Lude.ToJSON VideoSelectorSettings where
  toJSON VideoSelectorSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("videoSelectorProgramId" Lude..=)
              Lude.<$> videoSelectorProgramId,
            ("videoSelectorPid" Lude..=) Lude.<$> videoSelectorPid
          ]
      )
