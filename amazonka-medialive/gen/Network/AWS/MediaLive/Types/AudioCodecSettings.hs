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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.PassThroughSettings
import Network.AWS.MediaLive.Types.WavSettings

-- | Audio Codec Settings
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { ac3Settings :: Core.Maybe Ac3Settings,
    mp2Settings :: Core.Maybe Mp2Settings,
    passThroughSettings :: Core.Maybe PassThroughSettings,
    eac3Settings :: Core.Maybe Eac3Settings,
    aacSettings :: Core.Maybe AacSettings,
    wavSettings :: Core.Maybe WavSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ac3Settings = Core.Nothing,
      mp2Settings = Core.Nothing,
      passThroughSettings = Core.Nothing,
      eac3Settings = Core.Nothing,
      aacSettings = Core.Nothing,
      wavSettings = Core.Nothing
    }

-- | Undocumented member.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_passThroughSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe PassThroughSettings)
audioCodecSettings_passThroughSettings = Lens.lens (\AudioCodecSettings' {passThroughSettings} -> passThroughSettings) (\s@AudioCodecSettings' {} a -> s {passThroughSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

-- | Undocumented member.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

instance Core.FromJSON AudioCodecSettings where
  parseJSON =
    Core.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Core.<$> (x Core..:? "ac3Settings")
            Core.<*> (x Core..:? "mp2Settings")
            Core.<*> (x Core..:? "passThroughSettings")
            Core.<*> (x Core..:? "eac3Settings")
            Core.<*> (x Core..:? "aacSettings")
            Core.<*> (x Core..:? "wavSettings")
      )

instance Core.Hashable AudioCodecSettings

instance Core.NFData AudioCodecSettings

instance Core.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ac3Settings" Core..=) Core.<$> ac3Settings,
            ("mp2Settings" Core..=) Core.<$> mp2Settings,
            ("passThroughSettings" Core..=)
              Core.<$> passThroughSettings,
            ("eac3Settings" Core..=) Core.<$> eac3Settings,
            ("aacSettings" Core..=) Core.<$> aacSettings,
            ("wavSettings" Core..=) Core.<$> wavSettings
          ]
      )
