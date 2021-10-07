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
-- Module      : Network.AWS.MediaConvert.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoCodecSettings where

import qualified Network.AWS.Core as Core
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
import Network.AWS.MediaConvert.Types.XavcSettings
import qualified Network.AWS.Prelude as Prelude

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
--
-- /See:/ 'newVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value FRAME_CAPTURE.
    frameCaptureSettings :: Prelude.Maybe FrameCaptureSettings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value XAVC.
    xavcSettings :: Prelude.Maybe XavcSettings,
    -- | Specifies the video codec. This must be equal to one of the enum values
    -- defined by the object VideoCodec.
    codec :: Prelude.Maybe VideoCodec,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VC3
    vc3Settings :: Prelude.Maybe Vc3Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VP8.
    vp8Settings :: Prelude.Maybe Vp8Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value MPEG2.
    mpeg2Settings :: Prelude.Maybe Mpeg2Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value H_264.
    h264Settings :: Prelude.Maybe H264Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VP9.
    vp9Settings :: Prelude.Maybe Vp9Settings,
    -- | Settings for H265 codec
    h265Settings :: Prelude.Maybe H265Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value PRORES.
    proresSettings :: Prelude.Maybe ProresSettings,
    -- | Required when you choose AVC-Intra for your output video codec. For more
    -- information about the AVC-Intra settings, see the relevant
    -- specification. For detailed information about SD and HD in AVC-Intra,
    -- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
    -- about 4K\/2K in AVC-Intra, see
    -- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
    avcIntraSettings :: Prelude.Maybe AvcIntraSettings,
    -- | Required when you set Codec, under VideoDescription>CodecSettings to the
    -- value AV1.
    av1Settings :: Prelude.Maybe Av1Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameCaptureSettings', 'videoCodecSettings_frameCaptureSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value FRAME_CAPTURE.
--
-- 'xavcSettings', 'videoCodecSettings_xavcSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value XAVC.
--
-- 'codec', 'videoCodecSettings_codec' - Specifies the video codec. This must be equal to one of the enum values
-- defined by the object VideoCodec.
--
-- 'vc3Settings', 'videoCodecSettings_vc3Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
--
-- 'vp8Settings', 'videoCodecSettings_vp8Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP8.
--
-- 'mpeg2Settings', 'videoCodecSettings_mpeg2Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value MPEG2.
--
-- 'h264Settings', 'videoCodecSettings_h264Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
--
-- 'vp9Settings', 'videoCodecSettings_vp9Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP9.
--
-- 'h265Settings', 'videoCodecSettings_h265Settings' - Settings for H265 codec
--
-- 'proresSettings', 'videoCodecSettings_proresSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value PRORES.
--
-- 'avcIntraSettings', 'videoCodecSettings_avcIntraSettings' - Required when you choose AVC-Intra for your output video codec. For more
-- information about the AVC-Intra settings, see the relevant
-- specification. For detailed information about SD and HD in AVC-Intra,
-- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
-- about 4K\/2K in AVC-Intra, see
-- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
--
-- 'av1Settings', 'videoCodecSettings_av1Settings' - Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
newVideoCodecSettings ::
  VideoCodecSettings
newVideoCodecSettings =
  VideoCodecSettings'
    { frameCaptureSettings =
        Prelude.Nothing,
      xavcSettings = Prelude.Nothing,
      codec = Prelude.Nothing,
      vc3Settings = Prelude.Nothing,
      vp8Settings = Prelude.Nothing,
      mpeg2Settings = Prelude.Nothing,
      h264Settings = Prelude.Nothing,
      vp9Settings = Prelude.Nothing,
      h265Settings = Prelude.Nothing,
      proresSettings = Prelude.Nothing,
      avcIntraSettings = Prelude.Nothing,
      av1Settings = Prelude.Nothing
    }

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value FRAME_CAPTURE.
videoCodecSettings_frameCaptureSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe FrameCaptureSettings)
videoCodecSettings_frameCaptureSettings = Lens.lens (\VideoCodecSettings' {frameCaptureSettings} -> frameCaptureSettings) (\s@VideoCodecSettings' {} a -> s {frameCaptureSettings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value XAVC.
videoCodecSettings_xavcSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe XavcSettings)
videoCodecSettings_xavcSettings = Lens.lens (\VideoCodecSettings' {xavcSettings} -> xavcSettings) (\s@VideoCodecSettings' {} a -> s {xavcSettings = a} :: VideoCodecSettings)

-- | Specifies the video codec. This must be equal to one of the enum values
-- defined by the object VideoCodec.
videoCodecSettings_codec :: Lens.Lens' VideoCodecSettings (Prelude.Maybe VideoCodec)
videoCodecSettings_codec = Lens.lens (\VideoCodecSettings' {codec} -> codec) (\s@VideoCodecSettings' {} a -> s {codec = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
videoCodecSettings_vc3Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vc3Settings)
videoCodecSettings_vc3Settings = Lens.lens (\VideoCodecSettings' {vc3Settings} -> vc3Settings) (\s@VideoCodecSettings' {} a -> s {vc3Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP8.
videoCodecSettings_vp8Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vp8Settings)
videoCodecSettings_vp8Settings = Lens.lens (\VideoCodecSettings' {vp8Settings} -> vp8Settings) (\s@VideoCodecSettings' {} a -> s {vp8Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value MPEG2.
videoCodecSettings_mpeg2Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Mpeg2Settings)
videoCodecSettings_mpeg2Settings = Lens.lens (\VideoCodecSettings' {mpeg2Settings} -> mpeg2Settings) (\s@VideoCodecSettings' {} a -> s {mpeg2Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
videoCodecSettings_h264Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H264Settings)
videoCodecSettings_h264Settings = Lens.lens (\VideoCodecSettings' {h264Settings} -> h264Settings) (\s@VideoCodecSettings' {} a -> s {h264Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP9.
videoCodecSettings_vp9Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vp9Settings)
videoCodecSettings_vp9Settings = Lens.lens (\VideoCodecSettings' {vp9Settings} -> vp9Settings) (\s@VideoCodecSettings' {} a -> s {vp9Settings = a} :: VideoCodecSettings)

-- | Settings for H265 codec
videoCodecSettings_h265Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H265Settings)
videoCodecSettings_h265Settings = Lens.lens (\VideoCodecSettings' {h265Settings} -> h265Settings) (\s@VideoCodecSettings' {} a -> s {h265Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value PRORES.
videoCodecSettings_proresSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe ProresSettings)
videoCodecSettings_proresSettings = Lens.lens (\VideoCodecSettings' {proresSettings} -> proresSettings) (\s@VideoCodecSettings' {} a -> s {proresSettings = a} :: VideoCodecSettings)

-- | Required when you choose AVC-Intra for your output video codec. For more
-- information about the AVC-Intra settings, see the relevant
-- specification. For detailed information about SD and HD in AVC-Intra,
-- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
-- about 4K\/2K in AVC-Intra, see
-- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
videoCodecSettings_avcIntraSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe AvcIntraSettings)
videoCodecSettings_avcIntraSettings = Lens.lens (\VideoCodecSettings' {avcIntraSettings} -> avcIntraSettings) (\s@VideoCodecSettings' {} a -> s {avcIntraSettings = a} :: VideoCodecSettings)

-- | Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
videoCodecSettings_av1Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Av1Settings)
videoCodecSettings_av1Settings = Lens.lens (\VideoCodecSettings' {av1Settings} -> av1Settings) (\s@VideoCodecSettings' {} a -> s {av1Settings = a} :: VideoCodecSettings)

instance Core.FromJSON VideoCodecSettings where
  parseJSON =
    Core.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Prelude.<$> (x Core..:? "frameCaptureSettings")
            Prelude.<*> (x Core..:? "xavcSettings")
            Prelude.<*> (x Core..:? "codec")
            Prelude.<*> (x Core..:? "vc3Settings")
            Prelude.<*> (x Core..:? "vp8Settings")
            Prelude.<*> (x Core..:? "mpeg2Settings")
            Prelude.<*> (x Core..:? "h264Settings")
            Prelude.<*> (x Core..:? "vp9Settings")
            Prelude.<*> (x Core..:? "h265Settings")
            Prelude.<*> (x Core..:? "proresSettings")
            Prelude.<*> (x Core..:? "avcIntraSettings")
            Prelude.<*> (x Core..:? "av1Settings")
      )

instance Prelude.Hashable VideoCodecSettings

instance Prelude.NFData VideoCodecSettings

instance Core.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("frameCaptureSettings" Core..=)
              Prelude.<$> frameCaptureSettings,
            ("xavcSettings" Core..=) Prelude.<$> xavcSettings,
            ("codec" Core..=) Prelude.<$> codec,
            ("vc3Settings" Core..=) Prelude.<$> vc3Settings,
            ("vp8Settings" Core..=) Prelude.<$> vp8Settings,
            ("mpeg2Settings" Core..=) Prelude.<$> mpeg2Settings,
            ("h264Settings" Core..=) Prelude.<$> h264Settings,
            ("vp9Settings" Core..=) Prelude.<$> vp9Settings,
            ("h265Settings" Core..=) Prelude.<$> h265Settings,
            ("proresSettings" Core..=)
              Prelude.<$> proresSettings,
            ("avcIntraSettings" Core..=)
              Prelude.<$> avcIntraSettings,
            ("av1Settings" Core..=) Prelude.<$> av1Settings
          ]
      )
