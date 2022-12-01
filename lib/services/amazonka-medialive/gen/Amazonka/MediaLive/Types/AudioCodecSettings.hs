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
-- Module      : Amazonka.MediaLive.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioCodecSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.AacSettings
import Amazonka.MediaLive.Types.Ac3Settings
import Amazonka.MediaLive.Types.Eac3AtmosSettings
import Amazonka.MediaLive.Types.Eac3Settings
import Amazonka.MediaLive.Types.Mp2Settings
import Amazonka.MediaLive.Types.PassThroughSettings
import Amazonka.MediaLive.Types.WavSettings
import qualified Amazonka.Prelude as Prelude

-- | Audio Codec Settings
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { eac3Settings :: Prelude.Maybe Eac3Settings,
    passThroughSettings :: Prelude.Maybe PassThroughSettings,
    mp2Settings :: Prelude.Maybe Mp2Settings,
    wavSettings :: Prelude.Maybe WavSettings,
    ac3Settings :: Prelude.Maybe Ac3Settings,
    eac3AtmosSettings :: Prelude.Maybe Eac3AtmosSettings,
    aacSettings :: Prelude.Maybe AacSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eac3Settings', 'audioCodecSettings_eac3Settings' - Undocumented member.
--
-- 'passThroughSettings', 'audioCodecSettings_passThroughSettings' - Undocumented member.
--
-- 'mp2Settings', 'audioCodecSettings_mp2Settings' - Undocumented member.
--
-- 'wavSettings', 'audioCodecSettings_wavSettings' - Undocumented member.
--
-- 'ac3Settings', 'audioCodecSettings_ac3Settings' - Undocumented member.
--
-- 'eac3AtmosSettings', 'audioCodecSettings_eac3AtmosSettings' - Undocumented member.
--
-- 'aacSettings', 'audioCodecSettings_aacSettings' - Undocumented member.
newAudioCodecSettings ::
  AudioCodecSettings
newAudioCodecSettings =
  AudioCodecSettings'
    { eac3Settings = Prelude.Nothing,
      passThroughSettings = Prelude.Nothing,
      mp2Settings = Prelude.Nothing,
      wavSettings = Prelude.Nothing,
      ac3Settings = Prelude.Nothing,
      eac3AtmosSettings = Prelude.Nothing,
      aacSettings = Prelude.Nothing
    }

-- | Undocumented member.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_passThroughSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe PassThroughSettings)
audioCodecSettings_passThroughSettings = Lens.lens (\AudioCodecSettings' {passThroughSettings} -> passThroughSettings) (\s@AudioCodecSettings' {} a -> s {passThroughSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_eac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3AtmosSettings)
audioCodecSettings_eac3AtmosSettings = Lens.lens (\AudioCodecSettings' {eac3AtmosSettings} -> eac3AtmosSettings) (\s@AudioCodecSettings' {} a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

instance Core.FromJSON AudioCodecSettings where
  parseJSON =
    Core.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Prelude.<$> (x Core..:? "eac3Settings")
            Prelude.<*> (x Core..:? "passThroughSettings")
            Prelude.<*> (x Core..:? "mp2Settings")
            Prelude.<*> (x Core..:? "wavSettings")
            Prelude.<*> (x Core..:? "ac3Settings")
            Prelude.<*> (x Core..:? "eac3AtmosSettings")
            Prelude.<*> (x Core..:? "aacSettings")
      )

instance Prelude.Hashable AudioCodecSettings where
  hashWithSalt _salt AudioCodecSettings' {..} =
    _salt `Prelude.hashWithSalt` eac3Settings
      `Prelude.hashWithSalt` passThroughSettings
      `Prelude.hashWithSalt` mp2Settings
      `Prelude.hashWithSalt` wavSettings
      `Prelude.hashWithSalt` ac3Settings
      `Prelude.hashWithSalt` eac3AtmosSettings
      `Prelude.hashWithSalt` aacSettings

instance Prelude.NFData AudioCodecSettings where
  rnf AudioCodecSettings' {..} =
    Prelude.rnf eac3Settings
      `Prelude.seq` Prelude.rnf passThroughSettings
      `Prelude.seq` Prelude.rnf mp2Settings
      `Prelude.seq` Prelude.rnf wavSettings
      `Prelude.seq` Prelude.rnf ac3Settings
      `Prelude.seq` Prelude.rnf eac3AtmosSettings
      `Prelude.seq` Prelude.rnf aacSettings

instance Core.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("eac3Settings" Core..=) Prelude.<$> eac3Settings,
            ("passThroughSettings" Core..=)
              Prelude.<$> passThroughSettings,
            ("mp2Settings" Core..=) Prelude.<$> mp2Settings,
            ("wavSettings" Core..=) Prelude.<$> wavSettings,
            ("ac3Settings" Core..=) Prelude.<$> ac3Settings,
            ("eac3AtmosSettings" Core..=)
              Prelude.<$> eac3AtmosSettings,
            ("aacSettings" Core..=) Prelude.<$> aacSettings
          ]
      )
