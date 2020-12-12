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
    vcsFrameCaptureSettings,
    vcsAv1Settings,
    vcsCodec,
    vcsH265Settings,
    vcsProresSettings,
    vcsVp9Settings,
    vcsH264Settings,
    vcsMpeg2Settings,
    vcsVp8Settings,
    vcsVc3Settings,
    vcsAvcIntraSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Av1Settings
import Network.AWS.MediaConvert.Types.AvcIntraSettings
import Network.AWS.MediaConvert.Types.FrameCaptureSettings
import Network.AWS.MediaConvert.Types.H264Settings
import Network.AWS.MediaConvert.Types.H265Settings
import Network.AWS.MediaConvert.Types.Mpeg2Settings
import Network.AWS.MediaConvert.Types.ProresSettings
import Network.AWS.MediaConvert.Types.Vc3Settings
import Network.AWS.MediaConvert.Types.VideoCodec
import Network.AWS.MediaConvert.Types.Vp8Settings
import Network.AWS.MediaConvert.Types.Vp9Settings
import qualified Network.AWS.Prelude as Lude

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
--
-- /See:/ 'mkVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { frameCaptureSettings ::
      Lude.Maybe FrameCaptureSettings,
    av1Settings :: Lude.Maybe Av1Settings,
    codec :: Lude.Maybe VideoCodec,
    h265Settings :: Lude.Maybe H265Settings,
    proresSettings :: Lude.Maybe ProresSettings,
    vp9Settings :: Lude.Maybe Vp9Settings,
    h264Settings :: Lude.Maybe H264Settings,
    mpeg2Settings :: Lude.Maybe Mpeg2Settings,
    vp8Settings :: Lude.Maybe Vp8Settings,
    vc3Settings :: Lude.Maybe Vc3Settings,
    avcIntraSettings :: Lude.Maybe AvcIntraSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- * 'av1Settings' - Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
-- * 'avcIntraSettings' - Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
-- * 'codec' - Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
-- * 'frameCaptureSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
-- * 'h264Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
-- * 'h265Settings' - Settings for H265 codec
-- * 'mpeg2Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
-- * 'proresSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
-- * 'vc3Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
-- * 'vp8Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
-- * 'vp9Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
mkVideoCodecSettings ::
  VideoCodecSettings
mkVideoCodecSettings =
  VideoCodecSettings'
    { frameCaptureSettings = Lude.Nothing,
      av1Settings = Lude.Nothing,
      codec = Lude.Nothing,
      h265Settings = Lude.Nothing,
      proresSettings = Lude.Nothing,
      vp9Settings = Lude.Nothing,
      h264Settings = Lude.Nothing,
      mpeg2Settings = Lude.Nothing,
      vp8Settings = Lude.Nothing,
      vc3Settings = Lude.Nothing,
      avcIntraSettings = Lude.Nothing
    }

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- /Note:/ Consider using 'frameCaptureSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsFrameCaptureSettings :: Lens.Lens' VideoCodecSettings (Lude.Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = Lens.lens (frameCaptureSettings :: VideoCodecSettings -> Lude.Maybe FrameCaptureSettings) (\s a -> s {frameCaptureSettings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsFrameCaptureSettings "Use generic-lens or generic-optics with 'frameCaptureSettings' instead." #-}

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- /Note:/ Consider using 'av1Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsAv1Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Av1Settings)
vcsAv1Settings = Lens.lens (av1Settings :: VideoCodecSettings -> Lude.Maybe Av1Settings) (\s a -> s {av1Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsAv1Settings "Use generic-lens or generic-optics with 'av1Settings' instead." #-}

-- | Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsCodec :: Lens.Lens' VideoCodecSettings (Lude.Maybe VideoCodec)
vcsCodec = Lens.lens (codec :: VideoCodecSettings -> Lude.Maybe VideoCodec) (\s a -> s {codec = a} :: VideoCodecSettings)
{-# DEPRECATED vcsCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Settings for H265 codec
--
-- /Note:/ Consider using 'h265Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH265Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe H265Settings)
vcsH265Settings = Lens.lens (h265Settings :: VideoCodecSettings -> Lude.Maybe H265Settings) (\s a -> s {h265Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsH265Settings "Use generic-lens or generic-optics with 'h265Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /Note:/ Consider using 'proresSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsProresSettings :: Lens.Lens' VideoCodecSettings (Lude.Maybe ProresSettings)
vcsProresSettings = Lens.lens (proresSettings :: VideoCodecSettings -> Lude.Maybe ProresSettings) (\s a -> s {proresSettings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsProresSettings "Use generic-lens or generic-optics with 'proresSettings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
--
-- /Note:/ Consider using 'vp9Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVp9Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Vp9Settings)
vcsVp9Settings = Lens.lens (vp9Settings :: VideoCodecSettings -> Lude.Maybe Vp9Settings) (\s a -> s {vp9Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsVp9Settings "Use generic-lens or generic-optics with 'vp9Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /Note:/ Consider using 'h264Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsH264Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe H264Settings)
vcsH264Settings = Lens.lens (h264Settings :: VideoCodecSettings -> Lude.Maybe H264Settings) (\s a -> s {h264Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsH264Settings "Use generic-lens or generic-optics with 'h264Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /Note:/ Consider using 'mpeg2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsMpeg2Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Mpeg2Settings)
vcsMpeg2Settings = Lens.lens (mpeg2Settings :: VideoCodecSettings -> Lude.Maybe Mpeg2Settings) (\s a -> s {mpeg2Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsMpeg2Settings "Use generic-lens or generic-optics with 'mpeg2Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
--
-- /Note:/ Consider using 'vp8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVp8Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Vp8Settings)
vcsVp8Settings = Lens.lens (vp8Settings :: VideoCodecSettings -> Lude.Maybe Vp8Settings) (\s a -> s {vp8Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsVp8Settings "Use generic-lens or generic-optics with 'vp8Settings' instead." #-}

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
--
-- /Note:/ Consider using 'vc3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsVc3Settings :: Lens.Lens' VideoCodecSettings (Lude.Maybe Vc3Settings)
vcsVc3Settings = Lens.lens (vc3Settings :: VideoCodecSettings -> Lude.Maybe Vc3Settings) (\s a -> s {vc3Settings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsVc3Settings "Use generic-lens or generic-optics with 'vc3Settings' instead." #-}

-- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
--
-- /Note:/ Consider using 'avcIntraSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsAvcIntraSettings :: Lens.Lens' VideoCodecSettings (Lude.Maybe AvcIntraSettings)
vcsAvcIntraSettings = Lens.lens (avcIntraSettings :: VideoCodecSettings -> Lude.Maybe AvcIntraSettings) (\s a -> s {avcIntraSettings = a} :: VideoCodecSettings)
{-# DEPRECATED vcsAvcIntraSettings "Use generic-lens or generic-optics with 'avcIntraSettings' instead." #-}

instance Lude.FromJSON VideoCodecSettings where
  parseJSON =
    Lude.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Lude.<$> (x Lude..:? "frameCaptureSettings")
            Lude.<*> (x Lude..:? "av1Settings")
            Lude.<*> (x Lude..:? "codec")
            Lude.<*> (x Lude..:? "h265Settings")
            Lude.<*> (x Lude..:? "proresSettings")
            Lude.<*> (x Lude..:? "vp9Settings")
            Lude.<*> (x Lude..:? "h264Settings")
            Lude.<*> (x Lude..:? "mpeg2Settings")
            Lude.<*> (x Lude..:? "vp8Settings")
            Lude.<*> (x Lude..:? "vc3Settings")
            Lude.<*> (x Lude..:? "avcIntraSettings")
      )

instance Lude.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("frameCaptureSettings" Lude..=) Lude.<$> frameCaptureSettings,
            ("av1Settings" Lude..=) Lude.<$> av1Settings,
            ("codec" Lude..=) Lude.<$> codec,
            ("h265Settings" Lude..=) Lude.<$> h265Settings,
            ("proresSettings" Lude..=) Lude.<$> proresSettings,
            ("vp9Settings" Lude..=) Lude.<$> vp9Settings,
            ("h264Settings" Lude..=) Lude.<$> h264Settings,
            ("mpeg2Settings" Lude..=) Lude.<$> mpeg2Settings,
            ("vp8Settings" Lude..=) Lude.<$> vp8Settings,
            ("vc3Settings" Lude..=) Lude.<$> vc3Settings,
            ("avcIntraSettings" Lude..=) Lude.<$> avcIntraSettings
          ]
      )
