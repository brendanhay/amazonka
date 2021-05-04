{-# LANGUAGE DeriveDataTypeable #-}
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
  { standardHlsSettings :: Prelude.Maybe StandardHlsSettings,
    frameCaptureHlsSettings :: Prelude.Maybe FrameCaptureHlsSettings,
    audioOnlyHlsSettings :: Prelude.Maybe AudioOnlyHlsSettings,
    fmp4HlsSettings :: Prelude.Maybe Fmp4HlsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { standardHlsSettings = Prelude.Nothing,
      frameCaptureHlsSettings = Prelude.Nothing,
      audioOnlyHlsSettings = Prelude.Nothing,
      fmp4HlsSettings = Prelude.Nothing
    }

-- | Undocumented member.
hlsSettings_standardHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe StandardHlsSettings)
hlsSettings_standardHlsSettings = Lens.lens (\HlsSettings' {standardHlsSettings} -> standardHlsSettings) (\s@HlsSettings' {} a -> s {standardHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_frameCaptureHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe FrameCaptureHlsSettings)
hlsSettings_frameCaptureHlsSettings = Lens.lens (\HlsSettings' {frameCaptureHlsSettings} -> frameCaptureHlsSettings) (\s@HlsSettings' {} a -> s {frameCaptureHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_audioOnlyHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe AudioOnlyHlsSettings)
hlsSettings_audioOnlyHlsSettings = Lens.lens (\HlsSettings' {audioOnlyHlsSettings} -> audioOnlyHlsSettings) (\s@HlsSettings' {} a -> s {audioOnlyHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_fmp4HlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe Fmp4HlsSettings)
hlsSettings_fmp4HlsSettings = Lens.lens (\HlsSettings' {fmp4HlsSettings} -> fmp4HlsSettings) (\s@HlsSettings' {} a -> s {fmp4HlsSettings = a} :: HlsSettings)

instance Prelude.FromJSON HlsSettings where
  parseJSON =
    Prelude.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Prelude.<$> (x Prelude..:? "standardHlsSettings")
            Prelude.<*> (x Prelude..:? "frameCaptureHlsSettings")
            Prelude.<*> (x Prelude..:? "audioOnlyHlsSettings")
            Prelude.<*> (x Prelude..:? "fmp4HlsSettings")
      )

instance Prelude.Hashable HlsSettings

instance Prelude.NFData HlsSettings

instance Prelude.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("standardHlsSettings" Prelude..=)
              Prelude.<$> standardHlsSettings,
            ("frameCaptureHlsSettings" Prelude..=)
              Prelude.<$> frameCaptureHlsSettings,
            ("audioOnlyHlsSettings" Prelude..=)
              Prelude.<$> audioOnlyHlsSettings,
            ("fmp4HlsSettings" Prelude..=)
              Prelude.<$> fmp4HlsSettings
          ]
      )
