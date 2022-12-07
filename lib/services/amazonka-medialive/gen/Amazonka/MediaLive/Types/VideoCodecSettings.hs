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
-- Module      : Amazonka.MediaLive.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoCodecSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FrameCaptureSettings
import Amazonka.MediaLive.Types.H264Settings
import Amazonka.MediaLive.Types.H265Settings
import Amazonka.MediaLive.Types.Mpeg2Settings
import qualified Amazonka.Prelude as Prelude

-- | Video Codec Settings
--
-- /See:/ 'newVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { mpeg2Settings :: Prelude.Maybe Mpeg2Settings,
    h264Settings :: Prelude.Maybe H264Settings,
    h265Settings :: Prelude.Maybe H265Settings,
    frameCaptureSettings :: Prelude.Maybe FrameCaptureSettings
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
-- 'mpeg2Settings', 'videoCodecSettings_mpeg2Settings' - Undocumented member.
--
-- 'h264Settings', 'videoCodecSettings_h264Settings' - Undocumented member.
--
-- 'h265Settings', 'videoCodecSettings_h265Settings' - Undocumented member.
--
-- 'frameCaptureSettings', 'videoCodecSettings_frameCaptureSettings' - Undocumented member.
newVideoCodecSettings ::
  VideoCodecSettings
newVideoCodecSettings =
  VideoCodecSettings'
    { mpeg2Settings =
        Prelude.Nothing,
      h264Settings = Prelude.Nothing,
      h265Settings = Prelude.Nothing,
      frameCaptureSettings = Prelude.Nothing
    }

-- | Undocumented member.
videoCodecSettings_mpeg2Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Mpeg2Settings)
videoCodecSettings_mpeg2Settings = Lens.lens (\VideoCodecSettings' {mpeg2Settings} -> mpeg2Settings) (\s@VideoCodecSettings' {} a -> s {mpeg2Settings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_h264Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H264Settings)
videoCodecSettings_h264Settings = Lens.lens (\VideoCodecSettings' {h264Settings} -> h264Settings) (\s@VideoCodecSettings' {} a -> s {h264Settings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_h265Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H265Settings)
videoCodecSettings_h265Settings = Lens.lens (\VideoCodecSettings' {h265Settings} -> h265Settings) (\s@VideoCodecSettings' {} a -> s {h265Settings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_frameCaptureSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe FrameCaptureSettings)
videoCodecSettings_frameCaptureSettings = Lens.lens (\VideoCodecSettings' {frameCaptureSettings} -> frameCaptureSettings) (\s@VideoCodecSettings' {} a -> s {frameCaptureSettings = a} :: VideoCodecSettings)

instance Data.FromJSON VideoCodecSettings where
  parseJSON =
    Data.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Prelude.<$> (x Data..:? "mpeg2Settings")
            Prelude.<*> (x Data..:? "h264Settings")
            Prelude.<*> (x Data..:? "h265Settings")
            Prelude.<*> (x Data..:? "frameCaptureSettings")
      )

instance Prelude.Hashable VideoCodecSettings where
  hashWithSalt _salt VideoCodecSettings' {..} =
    _salt `Prelude.hashWithSalt` mpeg2Settings
      `Prelude.hashWithSalt` h264Settings
      `Prelude.hashWithSalt` h265Settings
      `Prelude.hashWithSalt` frameCaptureSettings

instance Prelude.NFData VideoCodecSettings where
  rnf VideoCodecSettings' {..} =
    Prelude.rnf mpeg2Settings
      `Prelude.seq` Prelude.rnf h264Settings
      `Prelude.seq` Prelude.rnf h265Settings
      `Prelude.seq` Prelude.rnf frameCaptureSettings

instance Data.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mpeg2Settings" Data..=) Prelude.<$> mpeg2Settings,
            ("h264Settings" Data..=) Prelude.<$> h264Settings,
            ("h265Settings" Data..=) Prelude.<$> h265Settings,
            ("frameCaptureSettings" Data..=)
              Prelude.<$> frameCaptureSettings
          ]
      )
