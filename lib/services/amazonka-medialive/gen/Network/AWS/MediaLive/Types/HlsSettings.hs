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
import qualified Network.AWS.Prelude as Prelude

-- | Hls Settings
--
-- /See:/ 'newHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { fmp4HlsSettings :: Prelude.Maybe Fmp4HlsSettings,
    audioOnlyHlsSettings :: Prelude.Maybe AudioOnlyHlsSettings,
    frameCaptureHlsSettings :: Prelude.Maybe FrameCaptureHlsSettings,
    standardHlsSettings :: Prelude.Maybe StandardHlsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fmp4HlsSettings', 'hlsSettings_fmp4HlsSettings' - Undocumented member.
--
-- 'audioOnlyHlsSettings', 'hlsSettings_audioOnlyHlsSettings' - Undocumented member.
--
-- 'frameCaptureHlsSettings', 'hlsSettings_frameCaptureHlsSettings' - Undocumented member.
--
-- 'standardHlsSettings', 'hlsSettings_standardHlsSettings' - Undocumented member.
newHlsSettings ::
  HlsSettings
newHlsSettings =
  HlsSettings'
    { fmp4HlsSettings = Prelude.Nothing,
      audioOnlyHlsSettings = Prelude.Nothing,
      frameCaptureHlsSettings = Prelude.Nothing,
      standardHlsSettings = Prelude.Nothing
    }

-- | Undocumented member.
hlsSettings_fmp4HlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe Fmp4HlsSettings)
hlsSettings_fmp4HlsSettings = Lens.lens (\HlsSettings' {fmp4HlsSettings} -> fmp4HlsSettings) (\s@HlsSettings' {} a -> s {fmp4HlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_audioOnlyHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe AudioOnlyHlsSettings)
hlsSettings_audioOnlyHlsSettings = Lens.lens (\HlsSettings' {audioOnlyHlsSettings} -> audioOnlyHlsSettings) (\s@HlsSettings' {} a -> s {audioOnlyHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_frameCaptureHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe FrameCaptureHlsSettings)
hlsSettings_frameCaptureHlsSettings = Lens.lens (\HlsSettings' {frameCaptureHlsSettings} -> frameCaptureHlsSettings) (\s@HlsSettings' {} a -> s {frameCaptureHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_standardHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe StandardHlsSettings)
hlsSettings_standardHlsSettings = Lens.lens (\HlsSettings' {standardHlsSettings} -> standardHlsSettings) (\s@HlsSettings' {} a -> s {standardHlsSettings = a} :: HlsSettings)

instance Core.FromJSON HlsSettings where
  parseJSON =
    Core.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Prelude.<$> (x Core..:? "fmp4HlsSettings")
            Prelude.<*> (x Core..:? "audioOnlyHlsSettings")
            Prelude.<*> (x Core..:? "frameCaptureHlsSettings")
            Prelude.<*> (x Core..:? "standardHlsSettings")
      )

instance Prelude.Hashable HlsSettings

instance Prelude.NFData HlsSettings

instance Core.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fmp4HlsSettings" Core..=)
              Prelude.<$> fmp4HlsSettings,
            ("audioOnlyHlsSettings" Core..=)
              Prelude.<$> audioOnlyHlsSettings,
            ("frameCaptureHlsSettings" Core..=)
              Prelude.<$> frameCaptureHlsSettings,
            ("standardHlsSettings" Core..=)
              Prelude.<$> standardHlsSettings
          ]
      )
