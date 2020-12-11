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
    vdHeight,
    vdSharpness,
    vdWidth,
    vdScalingBehavior,
    vdRespondToAfd,
    vdCodecSettings,
    vdName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoCodecSettings
import Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
import Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
import qualified Network.AWS.Prelude as Lude

-- | Video settings for this stream.
--
-- /See:/ 'mkVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { height ::
      Lude.Maybe Lude.Int,
    sharpness :: Lude.Maybe Lude.Natural,
    width :: Lude.Maybe Lude.Int,
    scalingBehavior ::
      Lude.Maybe VideoDescriptionScalingBehavior,
    respondToAfd :: Lude.Maybe VideoDescriptionRespondToAfd,
    codecSettings :: Lude.Maybe VideoCodecSettings,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoDescription' with the minimum fields required to make a request.
--
-- * 'codecSettings' - Video codec settings.
-- * 'height' - Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
-- * 'name' - The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
-- * 'respondToAfd' - Indicates how MediaLive will respond to the AFD values that might be in the input video. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose PASSTHROUGH.
--
-- RESPOND: MediaLive clips the input video using a formula that uses the AFD values (configured in afdSignaling ), the input display aspect ratio, and the output display aspect ratio. MediaLive also includes the AFD values in the output, unless the codec for this encode is FRAME_CAPTURE.
-- PASSTHROUGH: MediaLive ignores the AFD values and does not clip the video. But MediaLive does include the values in the output.
-- NONE: MediaLive does not clip the input video and does not include the AFD values in the output
-- * 'scalingBehavior' - STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
-- * 'sharpness' - Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
-- * 'width' - Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
mkVideoDescription ::
  -- | 'name'
  Lude.Text ->
  VideoDescription
mkVideoDescription pName_ =
  VideoDescription'
    { height = Lude.Nothing,
      sharpness = Lude.Nothing,
      width = Lude.Nothing,
      scalingBehavior = Lude.Nothing,
      respondToAfd = Lude.Nothing,
      codecSettings = Lude.Nothing,
      name = pName_
    }

-- | Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeight :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Int)
vdHeight = Lens.lens (height :: VideoDescription -> Lude.Maybe Lude.Int) (\s a -> s {height = a} :: VideoDescription)
{-# DEPRECATED vdHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSharpness :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Natural)
vdSharpness = Lens.lens (sharpness :: VideoDescription -> Lude.Maybe Lude.Natural) (\s a -> s {sharpness = a} :: VideoDescription)
{-# DEPRECATED vdSharpness "Use generic-lens or generic-optics with 'sharpness' instead." #-}

-- | Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidth :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Int)
vdWidth = Lens.lens (width :: VideoDescription -> Lude.Maybe Lude.Int) (\s a -> s {width = a} :: VideoDescription)
{-# DEPRECATED vdWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | STRETCH_TO_OUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
--
-- /Note:/ Consider using 'scalingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdScalingBehavior :: Lens.Lens' VideoDescription (Lude.Maybe VideoDescriptionScalingBehavior)
vdScalingBehavior = Lens.lens (scalingBehavior :: VideoDescription -> Lude.Maybe VideoDescriptionScalingBehavior) (\s a -> s {scalingBehavior = a} :: VideoDescription)
{-# DEPRECATED vdScalingBehavior "Use generic-lens or generic-optics with 'scalingBehavior' instead." #-}

-- | Indicates how MediaLive will respond to the AFD values that might be in the input video. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose PASSTHROUGH.
--
-- RESPOND: MediaLive clips the input video using a formula that uses the AFD values (configured in afdSignaling ), the input display aspect ratio, and the output display aspect ratio. MediaLive also includes the AFD values in the output, unless the codec for this encode is FRAME_CAPTURE.
-- PASSTHROUGH: MediaLive ignores the AFD values and does not clip the video. But MediaLive does include the values in the output.
-- NONE: MediaLive does not clip the input video and does not include the AFD values in the output
--
-- /Note:/ Consider using 'respondToAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdRespondToAfd :: Lens.Lens' VideoDescription (Lude.Maybe VideoDescriptionRespondToAfd)
vdRespondToAfd = Lens.lens (respondToAfd :: VideoDescription -> Lude.Maybe VideoDescriptionRespondToAfd) (\s a -> s {respondToAfd = a} :: VideoDescription)
{-# DEPRECATED vdRespondToAfd "Use generic-lens or generic-optics with 'respondToAfd' instead." #-}

-- | Video codec settings.
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCodecSettings :: Lens.Lens' VideoDescription (Lude.Maybe VideoCodecSettings)
vdCodecSettings = Lens.lens (codecSettings :: VideoDescription -> Lude.Maybe VideoCodecSettings) (\s a -> s {codecSettings = a} :: VideoDescription)
{-# DEPRECATED vdCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdName :: Lens.Lens' VideoDescription Lude.Text
vdName = Lens.lens (name :: VideoDescription -> Lude.Text) (\s a -> s {name = a} :: VideoDescription)
{-# DEPRECATED vdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON VideoDescription where
  parseJSON =
    Lude.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Lude.<$> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "sharpness")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "scalingBehavior")
            Lude.<*> (x Lude..:? "respondToAfd")
            Lude.<*> (x Lude..:? "codecSettings")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("height" Lude..=) Lude.<$> height,
            ("sharpness" Lude..=) Lude.<$> sharpness,
            ("width" Lude..=) Lude.<$> width,
            ("scalingBehavior" Lude..=) Lude.<$> scalingBehavior,
            ("respondToAfd" Lude..=) Lude.<$> respondToAfd,
            ("codecSettings" Lude..=) Lude.<$> codecSettings,
            Lude.Just ("name" Lude..= name)
          ]
      )
