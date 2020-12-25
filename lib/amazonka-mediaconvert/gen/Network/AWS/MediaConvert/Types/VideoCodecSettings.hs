{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoCodecSettings
  ( VideoCodecSettings (..),

    -- * Smart constructor
    mkVideoCodecSettings,

    -- * Lenses
    vcsAv1Settings,
    vcsAvcIntraSettings,
    vcsCodec,
    vcsFrameCaptureSettings,
    vcsH264Settings,
    vcsH265Settings,
    vcsMpeg2Settings,
    vcsProresSettings,
    vcsVc3Settings,
    vcsVp8Settings,
    vcsVp9Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Av1Settings as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraSettings as Types
import qualified Network.AWS.MediaConvert.Types.FrameCaptureSettings as Types
import qualified Network.AWS.MediaConvert.Types.H264Settings as Types
import qualified Network.AWS.MediaConvert.Types.H265Settings as Types
import qualified Network.AWS.MediaConvert.Types.Mpeg2Settings as Types
import qualified Network.AWS.MediaConvert.Types.ProresSettings as Types
import qualified Network.AWS.MediaConvert.Types.Vc3Settings as Types
import qualified Network.AWS.MediaConvert.Types.VideoCodec as Types
import qualified Network.AWS.MediaConvert.Types.Vp8Settings as Types
import qualified Network.AWS.MediaConvert.Types.Vp9Settings as Types
import qualified Network.AWS.Prelude as Core

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
--
-- /See:/ 'mkVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { -- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
    av1Settings :: Core.Maybe Types.Av1Settings,
    -- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
    avcIntraSettings :: Core.Maybe Types.AvcIntraSettings,
    -- | Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
    codec :: Core.Maybe Types.VideoCodec,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
    frameCaptureSettings :: Core.Maybe Types.FrameCaptureSettings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
    h264Settings :: Core.Maybe Types.H264Settings,
    -- | Settings for H265 codec
    h265Settings :: Core.Maybe Types.H265Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
    mpeg2Settings :: Core.Maybe Types.Mpeg2Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
    proresSettings :: Core.Maybe Types.ProresSettings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
    vc3Settings :: Core.Maybe Types.Vc3Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
    vp8Settings :: Core.Maybe Types.Vp8Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
    vp9Settings :: Core.Maybe Types.Vp9Settings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoCodecSettings' value with any optional fields omitted.
mkVideoCodecSettings ::
  VideoCodecSettings
mkVideoCodecSettings =
  VideoCodecSettings'
    { av1Settings = Core.Nothing,
      avcIntraSettings = Core.Nothing,
      codec = Core.Nothing,
      frameCaptureSettings = Core.Nothing,
      h264Settings = Core.Nothing,
      h265Settings = Core.Nothing,
      mpeg2Settings = Core.Nothing,
      proresSettings = Core.Nothing,
      vc3Settings = Core.Nothing,
      vp8Settings = Core.Nothing,
      vp9Settings = Core.Nothing
    }

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- /Note:/ Consider using 'av1Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsAv1Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Av1Settings)
vcsAv1Settings = Lens.field @"av1Settings"
{-# DEPRECATED vcsAv1Settings "Use generic-lens or generic-optics with 'av1Settings' instead." #-}

-- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
--
-- /Note:/ Consider using 'avcIntraSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsAvcIntraSettings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.AvcIntraSettings)
vcsAvcIntraSettings = Lens.field @"avcIntraSettings"
{-# DEPRECATED vcsAvcIntraSettings "Use generic-lens or generic-optics with 'avcIntraSettings' instead." #-}

-- | Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsCodec :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.VideoCodec)
vcsCodec = Lens.field @"codec"
{-# DEPRECATED vcsCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- /Note:/ Consider using 'frameCaptureSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsFrameCaptureSettings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.FrameCaptureSettings)
vcsFrameCaptureSettings = Lens.field @"frameCaptureSettings"
{-# DEPRECATED vcsFrameCaptureSettings "Use generic-lens or generic-optics with 'frameCaptureSettings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /Note:/ Consider using 'h264Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH264Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.H264Settings)
vcsH264Settings = Lens.field @"h264Settings"
{-# DEPRECATED vcsH264Settings "Use generic-lens or generic-optics with 'h264Settings' instead." #-}

-- | Settings for H265 codec
--
-- /Note:/ Consider using 'h265Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH265Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.H265Settings)
vcsH265Settings = Lens.field @"h265Settings"
{-# DEPRECATED vcsH265Settings "Use generic-lens or generic-optics with 'h265Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /Note:/ Consider using 'mpeg2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsMpeg2Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Mpeg2Settings)
vcsMpeg2Settings = Lens.field @"mpeg2Settings"
{-# DEPRECATED vcsMpeg2Settings "Use generic-lens or generic-optics with 'mpeg2Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /Note:/ Consider using 'proresSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsProresSettings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.ProresSettings)
vcsProresSettings = Lens.field @"proresSettings"
{-# DEPRECATED vcsProresSettings "Use generic-lens or generic-optics with 'proresSettings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
--
-- /Note:/ Consider using 'vc3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVc3Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Vc3Settings)
vcsVc3Settings = Lens.field @"vc3Settings"
{-# DEPRECATED vcsVc3Settings "Use generic-lens or generic-optics with 'vc3Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
--
-- /Note:/ Consider using 'vp8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVp8Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Vp8Settings)
vcsVp8Settings = Lens.field @"vp8Settings"
{-# DEPRECATED vcsVp8Settings "Use generic-lens or generic-optics with 'vp8Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
--
-- /Note:/ Consider using 'vp9Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVp9Settings :: Lens.Lens' VideoCodecSettings (Core.Maybe Types.Vp9Settings)
vcsVp9Settings = Lens.field @"vp9Settings"
{-# DEPRECATED vcsVp9Settings "Use generic-lens or generic-optics with 'vp9Settings' instead." #-}

instance Core.FromJSON VideoCodecSettings where
  toJSON VideoCodecSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("av1Settings" Core..=) Core.<$> av1Settings,
            ("avcIntraSettings" Core..=) Core.<$> avcIntraSettings,
            ("codec" Core..=) Core.<$> codec,
            ("frameCaptureSettings" Core..=) Core.<$> frameCaptureSettings,
            ("h264Settings" Core..=) Core.<$> h264Settings,
            ("h265Settings" Core..=) Core.<$> h265Settings,
            ("mpeg2Settings" Core..=) Core.<$> mpeg2Settings,
            ("proresSettings" Core..=) Core.<$> proresSettings,
            ("vc3Settings" Core..=) Core.<$> vc3Settings,
            ("vp8Settings" Core..=) Core.<$> vp8Settings,
            ("vp9Settings" Core..=) Core.<$> vp9Settings
          ]
      )

instance Core.FromJSON VideoCodecSettings where
  parseJSON =
    Core.withObject "VideoCodecSettings" Core.$
      \x ->
        VideoCodecSettings'
          Core.<$> (x Core..:? "av1Settings")
          Core.<*> (x Core..:? "avcIntraSettings")
          Core.<*> (x Core..:? "codec")
          Core.<*> (x Core..:? "frameCaptureSettings")
          Core.<*> (x Core..:? "h264Settings")
          Core.<*> (x Core..:? "h265Settings")
          Core.<*> (x Core..:? "mpeg2Settings")
          Core.<*> (x Core..:? "proresSettings")
          Core.<*> (x Core..:? "vc3Settings")
          Core.<*> (x Core..:? "vp8Settings")
          Core.<*> (x Core..:? "vp9Settings")
