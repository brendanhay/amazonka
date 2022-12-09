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
-- Module      : Amazonka.MediaLive.Types.HlsSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioOnlyHlsSettings
import Amazonka.MediaLive.Types.Fmp4HlsSettings
import Amazonka.MediaLive.Types.FrameCaptureHlsSettings
import Amazonka.MediaLive.Types.StandardHlsSettings
import qualified Amazonka.Prelude as Prelude

-- | Hls Settings
--
-- /See:/ 'newHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { audioOnlyHlsSettings :: Prelude.Maybe AudioOnlyHlsSettings,
    fmp4HlsSettings :: Prelude.Maybe Fmp4HlsSettings,
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
-- 'audioOnlyHlsSettings', 'hlsSettings_audioOnlyHlsSettings' - Undocumented member.
--
-- 'fmp4HlsSettings', 'hlsSettings_fmp4HlsSettings' - Undocumented member.
--
-- 'frameCaptureHlsSettings', 'hlsSettings_frameCaptureHlsSettings' - Undocumented member.
--
-- 'standardHlsSettings', 'hlsSettings_standardHlsSettings' - Undocumented member.
newHlsSettings ::
  HlsSettings
newHlsSettings =
  HlsSettings'
    { audioOnlyHlsSettings =
        Prelude.Nothing,
      fmp4HlsSettings = Prelude.Nothing,
      frameCaptureHlsSettings = Prelude.Nothing,
      standardHlsSettings = Prelude.Nothing
    }

-- | Undocumented member.
hlsSettings_audioOnlyHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe AudioOnlyHlsSettings)
hlsSettings_audioOnlyHlsSettings = Lens.lens (\HlsSettings' {audioOnlyHlsSettings} -> audioOnlyHlsSettings) (\s@HlsSettings' {} a -> s {audioOnlyHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_fmp4HlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe Fmp4HlsSettings)
hlsSettings_fmp4HlsSettings = Lens.lens (\HlsSettings' {fmp4HlsSettings} -> fmp4HlsSettings) (\s@HlsSettings' {} a -> s {fmp4HlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_frameCaptureHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe FrameCaptureHlsSettings)
hlsSettings_frameCaptureHlsSettings = Lens.lens (\HlsSettings' {frameCaptureHlsSettings} -> frameCaptureHlsSettings) (\s@HlsSettings' {} a -> s {frameCaptureHlsSettings = a} :: HlsSettings)

-- | Undocumented member.
hlsSettings_standardHlsSettings :: Lens.Lens' HlsSettings (Prelude.Maybe StandardHlsSettings)
hlsSettings_standardHlsSettings = Lens.lens (\HlsSettings' {standardHlsSettings} -> standardHlsSettings) (\s@HlsSettings' {} a -> s {standardHlsSettings = a} :: HlsSettings)

instance Data.FromJSON HlsSettings where
  parseJSON =
    Data.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Prelude.<$> (x Data..:? "audioOnlyHlsSettings")
            Prelude.<*> (x Data..:? "fmp4HlsSettings")
            Prelude.<*> (x Data..:? "frameCaptureHlsSettings")
            Prelude.<*> (x Data..:? "standardHlsSettings")
      )

instance Prelude.Hashable HlsSettings where
  hashWithSalt _salt HlsSettings' {..} =
    _salt `Prelude.hashWithSalt` audioOnlyHlsSettings
      `Prelude.hashWithSalt` fmp4HlsSettings
      `Prelude.hashWithSalt` frameCaptureHlsSettings
      `Prelude.hashWithSalt` standardHlsSettings

instance Prelude.NFData HlsSettings where
  rnf HlsSettings' {..} =
    Prelude.rnf audioOnlyHlsSettings
      `Prelude.seq` Prelude.rnf fmp4HlsSettings
      `Prelude.seq` Prelude.rnf frameCaptureHlsSettings
      `Prelude.seq` Prelude.rnf standardHlsSettings

instance Data.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioOnlyHlsSettings" Data..=)
              Prelude.<$> audioOnlyHlsSettings,
            ("fmp4HlsSettings" Data..=)
              Prelude.<$> fmp4HlsSettings,
            ("frameCaptureHlsSettings" Data..=)
              Prelude.<$> frameCaptureHlsSettings,
            ("standardHlsSettings" Data..=)
              Prelude.<$> standardHlsSettings
          ]
      )
