{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescription
  ( VideoDescription (..),

    -- * Smart constructor
    mkVideoDescription,

    -- * Lenses
    vdName,
    vdCodecSettings,
    vdHeight,
    vdRespondToAfd,
    vdScalingBehavior,
    vdSharpness,
    vdWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.VideoCodecSettings as Types
import qualified Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd as Types
import qualified Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Video settings for this stream.
--
-- /See:/ 'mkVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
    name :: Core.Text,
    -- | Video codec settings.
    codecSettings :: Core.Maybe Types.VideoCodecSettings,
    -- | Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
    height :: Core.Maybe Core.Int,
    -- | Indicates how MediaLive will respond to the AFD values that might be in the input video. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose PASSTHROUGH.
    --
    -- RESPOND: MediaLive clips the input video using a formula that uses the AFD values (configured in afdSignaling ), the input display aspect ratio, and the output display aspect ratio. MediaLive also includes the AFD values in the output, unless the codec for this encode is FRAME_CAPTURE.
    -- PASSTHROUGH: MediaLive ignores the AFD values and does not clip the video. But MediaLive does include the values in the output.
    -- NONE: MediaLive does not clip the input video and does not include the AFD values in the output
    respondToAfd :: Core.Maybe Types.VideoDescriptionRespondToAfd,
    -- | STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
    scalingBehavior :: Core.Maybe Types.VideoDescriptionScalingBehavior,
    -- | Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
    sharpness :: Core.Maybe Core.Natural,
    -- | Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
    width :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoDescription' value with any optional fields omitted.
mkVideoDescription ::
  -- | 'name'
  Core.Text ->
  VideoDescription
mkVideoDescription name =
  VideoDescription'
    { name,
      codecSettings = Core.Nothing,
      height = Core.Nothing,
      respondToAfd = Core.Nothing,
      scalingBehavior = Core.Nothing,
      sharpness = Core.Nothing,
      width = Core.Nothing
    }

-- | The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdName :: Lens.Lens' VideoDescription Core.Text
vdName = Lens.field @"name"
{-# DEPRECATED vdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Video codec settings.
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCodecSettings :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoCodecSettings)
vdCodecSettings = Lens.field @"codecSettings"
{-# DEPRECATED vdCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeight :: Lens.Lens' VideoDescription (Core.Maybe Core.Int)
vdHeight = Lens.field @"height"
{-# DEPRECATED vdHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Indicates how MediaLive will respond to the AFD values that might be in the input video. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose PASSTHROUGH.
--
-- RESPOND: MediaLive clips the input video using a formula that uses the AFD values (configured in afdSignaling ), the input display aspect ratio, and the output display aspect ratio. MediaLive also includes the AFD values in the output, unless the codec for this encode is FRAME_CAPTURE.
-- PASSTHROUGH: MediaLive ignores the AFD values and does not clip the video. But MediaLive does include the values in the output.
-- NONE: MediaLive does not clip the input video and does not include the AFD values in the output
--
-- /Note:/ Consider using 'respondToAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdRespondToAfd :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoDescriptionRespondToAfd)
vdRespondToAfd = Lens.field @"respondToAfd"
{-# DEPRECATED vdRespondToAfd "Use generic-lens or generic-optics with 'respondToAfd' instead." #-}

-- | STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
--
-- /Note:/ Consider using 'scalingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdScalingBehavior :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoDescriptionScalingBehavior)
vdScalingBehavior = Lens.field @"scalingBehavior"
{-# DEPRECATED vdScalingBehavior "Use generic-lens or generic-optics with 'scalingBehavior' instead." #-}

-- | Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSharpness :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
vdSharpness = Lens.field @"sharpness"
{-# DEPRECATED vdSharpness "Use generic-lens or generic-optics with 'sharpness' instead." #-}

-- | Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidth :: Lens.Lens' VideoDescription (Core.Maybe Core.Int)
vdWidth = Lens.field @"width"
{-# DEPRECATED vdWidth "Use generic-lens or generic-optics with 'width' instead." #-}

instance Core.FromJSON VideoDescription where
  toJSON VideoDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("codecSettings" Core..=) Core.<$> codecSettings,
            ("height" Core..=) Core.<$> height,
            ("respondToAfd" Core..=) Core.<$> respondToAfd,
            ("scalingBehavior" Core..=) Core.<$> scalingBehavior,
            ("sharpness" Core..=) Core.<$> sharpness,
            ("width" Core..=) Core.<$> width
          ]
      )

instance Core.FromJSON VideoDescription where
  parseJSON =
    Core.withObject "VideoDescription" Core.$
      \x ->
        VideoDescription'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..:? "codecSettings")
          Core.<*> (x Core..:? "height")
          Core.<*> (x Core..:? "respondToAfd")
          Core.<*> (x Core..:? "scalingBehavior")
          Core.<*> (x Core..:? "sharpness")
          Core.<*> (x Core..:? "width")
