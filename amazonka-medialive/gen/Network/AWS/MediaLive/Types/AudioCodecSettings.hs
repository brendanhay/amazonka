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
-- Module      : Network.AWS.MediaLive.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioCodecSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.PassThroughSettings
import Network.AWS.MediaLive.Types.WavSettings
import qualified Network.AWS.Prelude as Prelude

-- | Audio Codec Settings
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { ac3Settings :: Prelude.Maybe Ac3Settings,
    mp2Settings :: Prelude.Maybe Mp2Settings,
    passThroughSettings :: Prelude.Maybe PassThroughSettings,
    eac3Settings :: Prelude.Maybe Eac3Settings,
    aacSettings :: Prelude.Maybe AacSettings,
    wavSettings :: Prelude.Maybe WavSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ac3Settings', 'audioCodecSettings_ac3Settings' - Undocumented member.
--
-- 'mp2Settings', 'audioCodecSettings_mp2Settings' - Undocumented member.
--
-- 'passThroughSettings', 'audioCodecSettings_passThroughSettings' - Undocumented member.
--
-- 'eac3Settings', 'audioCodecSettings_eac3Settings' - Undocumented member.
--
-- 'aacSettings', 'audioCodecSettings_aacSettings' - Undocumented member.
--
-- 'wavSettings', 'audioCodecSettings_wavSettings' - Undocumented member.
newAudioCodecSettings ::
  AudioCodecSettings
newAudioCodecSettings =
  AudioCodecSettings'
    { ac3Settings = Prelude.Nothing,
      mp2Settings = Prelude.Nothing,
      passThroughSettings = Prelude.Nothing,
      eac3Settings = Prelude.Nothing,
      aacSettings = Prelude.Nothing,
      wavSettings = Prelude.Nothing
    }

-- | Undocumented member.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_passThroughSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe PassThroughSettings)
audioCodecSettings_passThroughSettings = Lens.lens (\AudioCodecSettings' {passThroughSettings} -> passThroughSettings) (\s@AudioCodecSettings' {} a -> s {passThroughSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

instance Prelude.FromJSON AudioCodecSettings where
  parseJSON =
    Prelude.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Prelude.<$> (x Prelude..:? "ac3Settings")
            Prelude.<*> (x Prelude..:? "mp2Settings")
            Prelude.<*> (x Prelude..:? "passThroughSettings")
            Prelude.<*> (x Prelude..:? "eac3Settings")
            Prelude.<*> (x Prelude..:? "aacSettings")
            Prelude.<*> (x Prelude..:? "wavSettings")
      )

instance Prelude.Hashable AudioCodecSettings

instance Prelude.NFData AudioCodecSettings

instance Prelude.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ac3Settings" Prelude..=) Prelude.<$> ac3Settings,
            ("mp2Settings" Prelude..=) Prelude.<$> mp2Settings,
            ("passThroughSettings" Prelude..=)
              Prelude.<$> passThroughSettings,
            ("eac3Settings" Prelude..=) Prelude.<$> eac3Settings,
            ("aacSettings" Prelude..=) Prelude.<$> aacSettings,
            ("wavSettings" Prelude..=) Prelude.<$> wavSettings
          ]
      )
