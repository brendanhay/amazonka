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
import qualified Network.AWS.MediaConvert.Types.SccDestinationFramerate as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for SCC caption output.
--
-- /See:/ 'mkSccDestinationSettings' smart constructor.
newtype SccDestinationSettings = SccDestinationSettings'
  { -- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
    framerate :: Core.Maybe Types.SccDestinationFramerate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SccDestinationSettings' value with any optional fields omitted.
mkSccDestinationSettings ::
  SccDestinationSettings
mkSccDestinationSettings =
  SccDestinationSettings' {framerate = Core.Nothing}

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFramerate :: Lens.Lens' SccDestinationSettings (Core.Maybe Types.SccDestinationFramerate)
sdsFramerate = Lens.field @"framerate"
{-# DEPRECATED sdsFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

instance Core.FromJSON SccDestinationSettings where
  toJSON SccDestinationSettings {..} =
    Core.object
      (Core.catMaybes [("framerate" Core..=) Core.<$> framerate])

instance Core.FromJSON SccDestinationSettings where
  parseJSON =
    Core.withObject "SccDestinationSettings" Core.$
      \x -> SccDestinationSettings' Core.<$> (x Core..:? "framerate")
