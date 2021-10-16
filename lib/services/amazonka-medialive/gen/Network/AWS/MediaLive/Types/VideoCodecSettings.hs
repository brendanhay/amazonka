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
-- Module      : Network.AWS.MediaLive.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoCodecSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FrameCaptureSettings
import Network.AWS.MediaLive.Types.H264Settings
import Network.AWS.MediaLive.Types.H265Settings
import Network.AWS.MediaLive.Types.Mpeg2Settings
import qualified Network.AWS.Prelude as Prelude

-- | Video Codec Settings
--
-- /See:/ 'newVideoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { frameCaptureSettings :: Prelude.Maybe FrameCaptureSettings,
    mpeg2Settings :: Prelude.Maybe Mpeg2Settings,
    h264Settings :: Prelude.Maybe H264Settings,
    h265Settings :: Prelude.Maybe H265Settings
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
-- 'frameCaptureSettings', 'videoCodecSettings_frameCaptureSettings' - Undocumented member.
--
-- 'mpeg2Settings', 'videoCodecSettings_mpeg2Settings' - Undocumented member.
--
-- 'h264Settings', 'videoCodecSettings_h264Settings' - Undocumented member.
--
-- 'h265Settings', 'videoCodecSettings_h265Settings' - Undocumented member.
newVideoCodecSettings ::
  VideoCodecSettings
newVideoCodecSettings =
  VideoCodecSettings'
    { frameCaptureSettings =
        Prelude.Nothing,
      mpeg2Settings = Prelude.Nothing,
      h264Settings = Prelude.Nothing,
      h265Settings = Prelude.Nothing
    }

-- | Undocumented member.
videoCodecSettings_frameCaptureSettings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe FrameCaptureSettings)
videoCodecSettings_frameCaptureSettings = Lens.lens (\VideoCodecSettings' {frameCaptureSettings} -> frameCaptureSettings) (\s@VideoCodecSettings' {} a -> s {frameCaptureSettings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_mpeg2Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe Mpeg2Settings)
videoCodecSettings_mpeg2Settings = Lens.lens (\VideoCodecSettings' {mpeg2Settings} -> mpeg2Settings) (\s@VideoCodecSettings' {} a -> s {mpeg2Settings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_h264Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H264Settings)
videoCodecSettings_h264Settings = Lens.lens (\VideoCodecSettings' {h264Settings} -> h264Settings) (\s@VideoCodecSettings' {} a -> s {h264Settings = a} :: VideoCodecSettings)

-- | Undocumented member.
videoCodecSettings_h265Settings :: Lens.Lens' VideoCodecSettings (Prelude.Maybe H265Settings)
videoCodecSettings_h265Settings = Lens.lens (\VideoCodecSettings' {h265Settings} -> h265Settings) (\s@VideoCodecSettings' {} a -> s {h265Settings = a} :: VideoCodecSettings)

instance Core.FromJSON VideoCodecSettings where
  parseJSON =
    Core.withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            Prelude.<$> (x Core..:? "frameCaptureSettings")
            Prelude.<*> (x Core..:? "mpeg2Settings")
            Prelude.<*> (x Core..:? "h264Settings")
            Prelude.<*> (x Core..:? "h265Settings")
      )

instance Prelude.Hashable VideoCodecSettings

instance Prelude.NFData VideoCodecSettings

instance Core.ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("frameCaptureSettings" Core..=)
              Prelude.<$> frameCaptureSettings,
            ("mpeg2Settings" Core..=) Prelude.<$> mpeg2Settings,
            ("h264Settings" Core..=) Prelude.<$> h264Settings,
            ("h265Settings" Core..=) Prelude.<$> h265Settings
          ]
      )
