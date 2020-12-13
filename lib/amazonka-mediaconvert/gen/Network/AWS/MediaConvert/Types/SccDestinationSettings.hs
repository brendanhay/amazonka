{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SccDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SccDestinationSettings
  ( SccDestinationSettings (..),

    -- * Smart constructor
    mkSccDestinationSettings,

    -- * Lenses
    sdsFramerate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.SccDestinationFramerate
import qualified Network.AWS.Prelude as Lude

-- | Settings for SCC caption output.
--
-- /See:/ 'mkSccDestinationSettings' smart constructor.
newtype SccDestinationSettings = SccDestinationSettings'
  { -- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
    framerate :: Lude.Maybe SccDestinationFramerate
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SccDestinationSettings' with the minimum fields required to make a request.
--
-- * 'framerate' - Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
mkSccDestinationSettings ::
  SccDestinationSettings
mkSccDestinationSettings =
  SccDestinationSettings' {framerate = Lude.Nothing}

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFramerate :: Lens.Lens' SccDestinationSettings (Lude.Maybe SccDestinationFramerate)
sdsFramerate = Lens.lens (framerate :: SccDestinationSettings -> Lude.Maybe SccDestinationFramerate) (\s a -> s {framerate = a} :: SccDestinationSettings)
{-# DEPRECATED sdsFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

instance Lude.FromJSON SccDestinationSettings where
  parseJSON =
    Lude.withObject
      "SccDestinationSettings"
      (\x -> SccDestinationSettings' Lude.<$> (x Lude..:? "framerate"))

instance Lude.ToJSON SccDestinationSettings where
  toJSON SccDestinationSettings' {..} =
    Lude.object
      (Lude.catMaybes [("framerate" Lude..=) Lude.<$> framerate])
