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
-- Module      : Amazonka.MediaConvert.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoCodecSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Av1Settings
import Amazonka.MediaConvert.Types.AvcIntraSettings
import Amazonka.MediaConvert.Types.FrameCaptureSettings
import Amazonka.MediaConvert.Types.H264Settings
import Amazonka.MediaConvert.Types.H265Settings
import Amazonka.MediaConvert.Types.Mpeg2Settings
import Amazonka.MediaConvert.Types.ProresSettings
import Amazonka.MediaConvert.Types.Vc3Settings
import Amazonka.MediaConvert.Types.VideoCodec
import Amazonka.MediaConvert.Types.Vp8Settings
import Amazonka.MediaConvert.Types.Vp9Settings
import Amazonka.MediaConvert.Types.XavcSettings
import qualified Amazonka.Prelude as Prelude

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
  { -- | Required when you set Codec, under VideoDescription>CodecSettings to the
    -- value AV1.
    av1Settings :: Prelude.Maybe Av1Settings,
    -- | Required when you choose AVC-Intra for your output video codec. For more
    -- information about the AVC-Intra settings, see the relevant
    -- specification. For detailed information about SD and HD in AVC-Intra,
    -- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
    -- about 4K\/2K in AVC-Intra, see
    -- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
    avcIntraSettings :: Prelude.Maybe AvcIntraSettings,
    -- | Specifies the video codec. This must be equal to one of the enum values
    -- defined by the object VideoCodec.
    codec :: Prelude.Maybe VideoCodec,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value FRAME_CAPTURE.
    frameCaptureSettings :: Prelude.Maybe FrameCaptureSettings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value H_264.
    h264Settings :: Prelude.Maybe H264Settings,
    -- | Settings for H265 codec
    h265Settings :: Prelude.Maybe H265Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value MPEG2.
    mpeg2Settings :: Prelude.Maybe Mpeg2Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value PRORES.
    proresSettings :: Prelude.Maybe ProresSettings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VC3
    vc3Settings :: Prelude.Maybe Vc3Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VP8.
    vp8Settings :: Prelude.Maybe Vp8Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value VP9.
    vp9Settings :: Prelude.Maybe Vp9Settings,
    -- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
    -- to the value XAVC.
    xavcSettings :: Prelude.Maybe XavcSettings
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
-- 'av1Settings', 'videoCodecSettings_av1Settings' - Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
--
-- 'avcIntraSettings', 'videoCodecSettings_avcIntraSettings' - Required when you choose AVC-Intra for your output video codec. For more
-- information about the AVC-Intra settings, see the relevant
-- specification. For detailed information about SD and HD in AVC-Intra,
-- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
-- about 4K\/2K in AVC-Intra, see
-- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
--
-- 'codec', 'videoCodecSettings_codec' - Specifies the video codec. This must be equal to one of the enum values
-- defined by the object VideoCodec.
--
-- 'frameCaptureSettings', 'videoCodecSettings_frameCaptureSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value FRAME_CAPTURE.
--
-- 'h264Settings', 'videoCodecSettings_h264Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
--
-- 'h265Settings', 'videoCodecSettings_h265Settings' - Settings for H265 codec
--
-- 'mpeg2Settings', 'videoCodecSettings_mpeg2Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value MPEG2.
--
-- 'proresSettings', 'videoCodecSettings_proresSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value PRORES.
--
-- 'vc3Settings', 'videoCodecSettings_vc3Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
--
-- 'vp8Settings', 'videoCodecSettings_vp8Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP8.
--
-- 'vp9Settings', 'videoCodecSettings_vp9Settings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP9.
--
-- 'xavcSettings', 'videoCodecSettings_xavcSettings' - Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value XAVC.
newVideoCodecSettings ::
  VideoCodecSettings
newVideoCodecSettings =
  VideoCodecSettings'
    { av1Settings = Prelude.Nothing,
      avcIntraSettings = Prelude.Nothing,
      codec = Prelude.Nothing,
      frameCaptureSettings = Prelude.Nothing,
      h264Settings = Prelude.Nothing,
      h265Settings = Prelude.Nothing,
      mpeg2Settings = Prelude.Nothing,
      proresSettings = Prelude.Nothing,
      vc3Settings = Prelude.Nothing,
      vp8Settings = Prelude.Nothing,
      vp9Settings = Prelude.Nothing,
      xavcSettings = Prelude.Nothing
    }

-- | Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
videoCodecSettings_av1Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Av1Settings)
videoCodecSettings_av1Settings = Lens.lens (\VideoCodecSettings' {av1Settings} -> av1Settings) (\s@VideoCodecSettings' {} a -> s {av1Settings = a} :: VideoCodecSettings)

-- | Required when you choose AVC-Intra for your output video codec. For more
-- information about the AVC-Intra settings, see the relevant
-- specification. For detailed information about SD and HD in AVC-Intra,
-- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
-- about 4K\/2K in AVC-Intra, see
-- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
videoCodecSettings_avcIntraSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe AvcIntraSettings)
videoCodecSettings_avcIntraSettings = Lens.lens (\VideoCodecSettings' {avcIntraSettings} -> avcIntraSettings) (\s@VideoCodecSettings' {} a -> s {avcIntraSettings = a} :: VideoCodecSettings)

-- | Specifies the video codec. This must be equal to one of the enum values
-- defined by the object VideoCodec.
videoCodecSettings_codec :: Lens.Lens' VideoCodecSettings (Prelude.Maybe VideoCodec)
videoCodecSettings_codec = Lens.lens (\VideoCodecSettings' {codec} -> codec) (\s@VideoCodecSettings' {} a -> s {codec = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value FRAME_CAPTURE.
videoCodecSettings_frameCaptureSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe FrameCaptureSettings)
videoCodecSettings_frameCaptureSettings = Lens.lens (\VideoCodecSettings' {frameCaptureSettings} -> frameCaptureSettings) (\s@VideoCodecSettings' {} a -> s {frameCaptureSettings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value H_264.
videoCodecSettings_h264Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H264Settings)
videoCodecSettings_h264Settings = Lens.lens (\VideoCodecSettings' {h264Settings} -> h264Settings) (\s@VideoCodecSettings' {} a -> s {h264Settings = a} :: VideoCodecSettings)

-- | Settings for H265 codec
videoCodecSettings_h265Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H265Settings)
videoCodecSettings_h265Settings = Lens.lens (\VideoCodecSettings' {h265Settings} -> h265Settings) (\s@VideoCodecSettings' {} a -> s {h265Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value MPEG2.
videoCodecSettings_mpeg2Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Mpeg2Settings)
videoCodecSettings_mpeg2Settings = Lens.lens (\VideoCodecSettings' {mpeg2Settings} -> mpeg2Settings) (\s@VideoCodecSettings' {} a -> s {mpeg2Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value PRORES.
videoCodecSettings_proresSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe ProresSettings)
videoCodecSettings_proresSettings = Lens.lens (\VideoCodecSettings' {proresSettings} -> proresSettings) (\s@VideoCodecSettings' {} a -> s {proresSettings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
videoCodecSettings_vc3Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vc3Settings)
videoCodecSettings_vc3Settings = Lens.lens (\VideoCodecSettings' {vc3Settings} -> vc3Settings) (\s@VideoCodecSettings' {} a -> s {vc3Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP8.
videoCodecSettings_vp8Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vp8Settings)
videoCodecSettings_vp8Settings = Lens.lens (\VideoCodecSettings' {vp8Settings} -> vp8Settings) (\s@VideoCodecSettings' {} a -> s {vp8Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP9.
videoCodecSettings_vp9Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Vp9Settings)
videoCodecSettings_vp9Settings = Lens.lens (\VideoCodecSettings' {vp9Settings} -> vp9Settings) (\s@VideoCodecSettings' {} a -> s {vp9Settings = a} :: VideoCodecSettings)

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value XAVC.
videoCodecSettings_xavcSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe XavcSettings)
videoCodecSettings_xavcSettings = Lens.lens (\VideoCodecSettings' {xavcSettings} -> xavcSettings) (\s@VideoCodecSettings' {} a -> s {xavcSettings = a} :: VideoCodecSettings)

instance Data.FromJSON VideoCodecSettings where
  parseJSON =
    Data.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Prelude.<$> (x Data..:? "av1Settings")
            Prelude.<*> (x Data..:? "avcIntraSettings")
            Prelude.<*> (x Data..:? "codec")
            Prelude.<*> (x Data..:? "frameCaptureSettings")
            Prelude.<*> (x Data..:? "h264Settings")
            Prelude.<*> (x Data..:? "h265Settings")
            Prelude.<*> (x Data..:? "mpeg2Settings")
            Prelude.<*> (x Data..:? "proresSettings")
            Prelude.<*> (x Data..:? "vc3Settings")
            Prelude.<*> (x Data..:? "vp8Settings")
            Prelude.<*> (x Data..:? "vp9Settings")
            Prelude.<*> (x Data..:? "xavcSettings")
      )

instance Prelude.Hashable VideoCodecSettings where
  hashWithSalt _salt VideoCodecSettings' {..} =
    _salt `Prelude.hashWithSalt` av1Settings
      `Prelude.hashWithSalt` avcIntraSettings
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` frameCaptureSettings
      `Prelude.hashWithSalt` h264Settings
      `Prelude.hashWithSalt` h265Settings
      `Prelude.hashWithSalt` mpeg2Settings
      `Prelude.hashWithSalt` proresSettings
      `Prelude.hashWithSalt` vc3Settings
      `Prelude.hashWithSalt` vp8Settings
      `Prelude.hashWithSalt` vp9Settings
      `Prelude.hashWithSalt` xavcSettings

instance Prelude.NFData VideoCodecSettings where
  rnf VideoCodecSettings' {..} =
    Prelude.rnf av1Settings
      `Prelude.seq` Prelude.rnf avcIntraSettings
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf frameCaptureSettings
      `Prelude.seq` Prelude.rnf h264Settings
      `Prelude.seq` Prelude.rnf h265Settings
      `Prelude.seq` Prelude.rnf mpeg2Settings
      `Prelude.seq` Prelude.rnf proresSettings
      `Prelude.seq` Prelude.rnf vc3Settings
      `Prelude.seq` Prelude.rnf vp8Settings
      `Prelude.seq` Prelude.rnf vp9Settings
      `Prelude.seq` Prelude.rnf xavcSettings

instance Data.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("av1Settings" Data..=) Prelude.<$> av1Settings,
            ("avcIntraSettings" Data..=)
              Prelude.<$> avcIntraSettings,
            ("codec" Data..=) Prelude.<$> codec,
            ("frameCaptureSettings" Data..=)
              Prelude.<$> frameCaptureSettings,
            ("h264Settings" Data..=) Prelude.<$> h264Settings,
            ("h265Settings" Data..=) Prelude.<$> h265Settings,
            ("mpeg2Settings" Data..=) Prelude.<$> mpeg2Settings,
            ("proresSettings" Data..=)
              Prelude.<$> proresSettings,
            ("vc3Settings" Data..=) Prelude.<$> vc3Settings,
            ("vp8Settings" Data..=) Prelude.<$> vp8Settings,
            ("vp9Settings" Data..=) Prelude.<$> vp9Settings,
            ("xavcSettings" Data..=) Prelude.<$> xavcSettings
          ]
      )
