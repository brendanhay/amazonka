{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoCodecSettings
import Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
import Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
import qualified Network.AWS.Prelude as Prelude

-- | Video settings for this stream.
--
-- /See:/ 'newVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | Output video height, in pixels. Must be an even number. For most codecs,
    -- you can leave this field and width blank in order to use the height and
    -- width (resolution) from the source. Note, however, that leaving blank is
    -- not recommended. For the Frame Capture codec, height and width are
    -- required.
    height :: Prelude.Maybe Prelude.Int,
    -- | Indicates how MediaLive will respond to the AFD values that might be in
    -- the input video. If you do not know what AFD signaling is, or if your
    -- downstream system has not given you guidance, choose PASSTHROUGH.
    -- RESPOND: MediaLive clips the input video using a formula that uses the
    -- AFD values (configured in afdSignaling ), the input display aspect
    -- ratio, and the output display aspect ratio. MediaLive also includes the
    -- AFD values in the output, unless the codec for this encode is
    -- FRAME_CAPTURE. PASSTHROUGH: MediaLive ignores the AFD values and does
    -- not clip the video. But MediaLive does include the values in the output.
    -- NONE: MediaLive does not clip the input video and does not include the
    -- AFD values in the output
    respondToAfd :: Prelude.Maybe VideoDescriptionRespondToAfd,
    -- | Output video width, in pixels. Must be an even number. For most codecs,
    -- you can leave this field and height blank in order to use the height and
    -- width (resolution) from the source. Note, however, that leaving blank is
    -- not recommended. For the Frame Capture codec, height and width are
    -- required.
    width :: Prelude.Maybe Prelude.Int,
    -- | Video codec settings.
    codecSettings :: Prelude.Maybe VideoCodecSettings,
    -- | STRETCH_TO_OUTPUT configures the output position to stretch the video to
    -- the specified output resolution (height and width). This option will
    -- override any position value. DEFAULT may insert black boxes (pillar
    -- boxes or letter boxes) around the video to provide the specified output
    -- resolution.
    scalingBehavior :: Prelude.Maybe VideoDescriptionScalingBehavior,
    -- | Changes the strength of the anti-alias filter used for scaling. 0 is the
    -- softest setting, 100 is the sharpest. A setting of 50 is recommended for
    -- most content.
    sharpness :: Prelude.Maybe Prelude.Natural,
    -- | The name of this VideoDescription. Outputs will use this name to
    -- uniquely identify this Description. Description names should be unique
    -- within this Live Event.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'videoDescription_height' - Output video height, in pixels. Must be an even number. For most codecs,
-- you can leave this field and width blank in order to use the height and
-- width (resolution) from the source. Note, however, that leaving blank is
-- not recommended. For the Frame Capture codec, height and width are
-- required.
--
-- 'respondToAfd', 'videoDescription_respondToAfd' - Indicates how MediaLive will respond to the AFD values that might be in
-- the input video. If you do not know what AFD signaling is, or if your
-- downstream system has not given you guidance, choose PASSTHROUGH.
-- RESPOND: MediaLive clips the input video using a formula that uses the
-- AFD values (configured in afdSignaling ), the input display aspect
-- ratio, and the output display aspect ratio. MediaLive also includes the
-- AFD values in the output, unless the codec for this encode is
-- FRAME_CAPTURE. PASSTHROUGH: MediaLive ignores the AFD values and does
-- not clip the video. But MediaLive does include the values in the output.
-- NONE: MediaLive does not clip the input video and does not include the
-- AFD values in the output
--
-- 'width', 'videoDescription_width' - Output video width, in pixels. Must be an even number. For most codecs,
-- you can leave this field and height blank in order to use the height and
-- width (resolution) from the source. Note, however, that leaving blank is
-- not recommended. For the Frame Capture codec, height and width are
-- required.
--
-- 'codecSettings', 'videoDescription_codecSettings' - Video codec settings.
--
-- 'scalingBehavior', 'videoDescription_scalingBehavior' - STRETCH_TO_OUTPUT configures the output position to stretch the video to
-- the specified output resolution (height and width). This option will
-- override any position value. DEFAULT may insert black boxes (pillar
-- boxes or letter boxes) around the video to provide the specified output
-- resolution.
--
-- 'sharpness', 'videoDescription_sharpness' - Changes the strength of the anti-alias filter used for scaling. 0 is the
-- softest setting, 100 is the sharpest. A setting of 50 is recommended for
-- most content.
--
-- 'name', 'videoDescription_name' - The name of this VideoDescription. Outputs will use this name to
-- uniquely identify this Description. Description names should be unique
-- within this Live Event.
newVideoDescription ::
  -- | 'name'
  Prelude.Text ->
  VideoDescription
newVideoDescription pName_ =
  VideoDescription'
    { height = Prelude.Nothing,
      respondToAfd = Prelude.Nothing,
      width = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      scalingBehavior = Prelude.Nothing,
      sharpness = Prelude.Nothing,
      name = pName_
    }

-- | Output video height, in pixels. Must be an even number. For most codecs,
-- you can leave this field and width blank in order to use the height and
-- width (resolution) from the source. Note, however, that leaving blank is
-- not recommended. For the Frame Capture codec, height and width are
-- required.
videoDescription_height :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Int)
videoDescription_height = Lens.lens (\VideoDescription' {height} -> height) (\s@VideoDescription' {} a -> s {height = a} :: VideoDescription)

-- | Indicates how MediaLive will respond to the AFD values that might be in
-- the input video. If you do not know what AFD signaling is, or if your
-- downstream system has not given you guidance, choose PASSTHROUGH.
-- RESPOND: MediaLive clips the input video using a formula that uses the
-- AFD values (configured in afdSignaling ), the input display aspect
-- ratio, and the output display aspect ratio. MediaLive also includes the
-- AFD values in the output, unless the codec for this encode is
-- FRAME_CAPTURE. PASSTHROUGH: MediaLive ignores the AFD values and does
-- not clip the video. But MediaLive does include the values in the output.
-- NONE: MediaLive does not clip the input video and does not include the
-- AFD values in the output
videoDescription_respondToAfd :: Lens.Lens' VideoDescription (Prelude.Maybe VideoDescriptionRespondToAfd)
videoDescription_respondToAfd = Lens.lens (\VideoDescription' {respondToAfd} -> respondToAfd) (\s@VideoDescription' {} a -> s {respondToAfd = a} :: VideoDescription)

-- | Output video width, in pixels. Must be an even number. For most codecs,
-- you can leave this field and height blank in order to use the height and
-- width (resolution) from the source. Note, however, that leaving blank is
-- not recommended. For the Frame Capture codec, height and width are
-- required.
videoDescription_width :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Int)
videoDescription_width = Lens.lens (\VideoDescription' {width} -> width) (\s@VideoDescription' {} a -> s {width = a} :: VideoDescription)

-- | Video codec settings.
videoDescription_codecSettings :: Lens.Lens' VideoDescription (Prelude.Maybe VideoCodecSettings)
videoDescription_codecSettings = Lens.lens (\VideoDescription' {codecSettings} -> codecSettings) (\s@VideoDescription' {} a -> s {codecSettings = a} :: VideoDescription)

-- | STRETCH_TO_OUTPUT configures the output position to stretch the video to
-- the specified output resolution (height and width). This option will
-- override any position value. DEFAULT may insert black boxes (pillar
-- boxes or letter boxes) around the video to provide the specified output
-- resolution.
videoDescription_scalingBehavior :: Lens.Lens' VideoDescription (Prelude.Maybe VideoDescriptionScalingBehavior)
videoDescription_scalingBehavior = Lens.lens (\VideoDescription' {scalingBehavior} -> scalingBehavior) (\s@VideoDescription' {} a -> s {scalingBehavior = a} :: VideoDescription)

-- | Changes the strength of the anti-alias filter used for scaling. 0 is the
-- softest setting, 100 is the sharpest. A setting of 50 is recommended for
-- most content.
videoDescription_sharpness :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_sharpness = Lens.lens (\VideoDescription' {sharpness} -> sharpness) (\s@VideoDescription' {} a -> s {sharpness = a} :: VideoDescription)

-- | The name of this VideoDescription. Outputs will use this name to
-- uniquely identify this Description. Description names should be unique
-- within this Live Event.
videoDescription_name :: Lens.Lens' VideoDescription Prelude.Text
videoDescription_name = Lens.lens (\VideoDescription' {name} -> name) (\s@VideoDescription' {} a -> s {name = a} :: VideoDescription)

instance Core.FromJSON VideoDescription where
  parseJSON =
    Core.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Prelude.<$> (x Core..:? "height")
            Prelude.<*> (x Core..:? "respondToAfd")
            Prelude.<*> (x Core..:? "width")
            Prelude.<*> (x Core..:? "codecSettings")
            Prelude.<*> (x Core..:? "scalingBehavior")
            Prelude.<*> (x Core..:? "sharpness")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable VideoDescription

instance Prelude.NFData VideoDescription

instance Core.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("height" Core..=) Prelude.<$> height,
            ("respondToAfd" Core..=) Prelude.<$> respondToAfd,
            ("width" Core..=) Prelude.<$> width,
            ("codecSettings" Core..=) Prelude.<$> codecSettings,
            ("scalingBehavior" Core..=)
              Prelude.<$> scalingBehavior,
            ("sharpness" Core..=) Prelude.<$> sharpness,
            Prelude.Just ("name" Core..= name)
          ]
      )
