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
-- Module      : Network.AWS.MediaLive.Types.HlsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
import Network.AWS.MediaLive.Types.Fmp4HlsSettings
import Network.AWS.MediaLive.Types.FrameCaptureHlsSettings
import Network.AWS.MediaLive.Types.StandardHlsSettings

-- | Hls Settings
--
-- /See:/ 'newHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { standardHlsSettings :: Core.Maybe StandardHlsSettings,
    frameCaptureHlsSettings :: Core.Maybe FrameCaptureHlsSettings,
    audioOnlyHlsSettings :: Core.Maybe AudioOnlyHlsSettings,
    fmp4HlsSettings :: Core.Maybe Fmp4HlsSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardHlsSettings', 'hlsSettings_standardHlsSettings' - Undocumented member.
--
-- 'frameCaptureHlsSettings', 'hlsSettings_frameCaptureHlsSettings' - Undocumented member.
--
-- 'audioOnlyHlsSettings', 'hlsSettings_audioOnlyHlsSettings' - Undocumented member.
--
-- 'fmp4HlsSettings', 'hlsSettings_fmp4HlsSettings' - Undocumented member.
newHlsSettings ::
  HlsSettings
newHlsSettings =
  HlsSettings'
    { standardHlsSettings = Core.Nothing,
      frameCaptureHlsSettings = Core.Nothing,
      audioOnlyHlsSettings = Core.Nothing,
      fmp4HlsSettings = Core.Nothing
    }

-- | Undocumented member.
hlsSettings_standardHlsSettings :: Lens.Lens' HlsSettings (Core.Maybe StandardHlsSettings)
hlsSettings_standardHlsSettings = Lens.lens (\HlsSettings' {standardHlsSettings} -> standardHlsSettings) (\s@HlsSettings' {} a -> s {standardHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_frameCaptureHlsSettings :: Lens.Lens' HlsSettings (Core.Maybe FrameCaptureHlsSettings)
hlsSettings_frameCaptureHlsSettings = Lens.lens (\HlsSettings' {frameCaptureHlsSettings} -> frameCaptureHlsSettings) (\s@HlsSettings' {} a -> s {frameCaptureHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_audioOnlyHlsSettings :: Lens.Lens' HlsSettings (Core.Maybe AudioOnlyHlsSettings)
hlsSettings_audioOnlyHlsSettings = Lens.lens (\HlsSettings' {audioOnlyHlsSettings} -> audioOnlyHlsSettings) (\s@HlsSettings' {} a -> s {audioOnlyHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_fmp4HlsSettings :: Lens.Lens' HlsSettings (Core.Maybe Fmp4HlsSettings)
hlsSettings_fmp4HlsSettings = Lens.lens (\HlsSettings' {fmp4HlsSettings} -> fmp4HlsSettings) (\s@HlsSettings' {} a -> s {fmp4HlsSettings = a} :: HlsSettings)

instance Core.FromJSON HlsSettings where
  parseJSON =
    Core.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Core.<$> (x Core..:? "standardHlsSettings")
            Core.<*> (x Core..:? "frameCaptureHlsSettings")
            Core.<*> (x Core..:? "audioOnlyHlsSettings")
            Core.<*> (x Core..:? "fmp4HlsSettings")
      )

instance Core.Hashable HlsSettings

instance Core.NFData HlsSettings

instance Core.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("standardHlsSettings" Core..=)
              Core.<$> standardHlsSettings,
            ("frameCaptureHlsSettings" Core..=)
              Core.<$> frameCaptureHlsSettings,
            ("audioOnlyHlsSettings" Core..=)
              Core.<$> audioOnlyHlsSettings,
            ("fmp4HlsSettings" Core..=)
              Core.<$> fmp4HlsSettings
          ]
      )
