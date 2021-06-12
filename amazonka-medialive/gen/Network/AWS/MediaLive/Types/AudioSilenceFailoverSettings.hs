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
-- Module      : Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSilenceFailoverSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Placeholder documentation for AudioSilenceFailoverSettings
--
-- /See:/ 'newAudioSilenceFailoverSettings' smart constructor.
data AudioSilenceFailoverSettings = AudioSilenceFailoverSettings'
  { -- | The amount of time (in milliseconds) that the active input must be
    -- silent before automatic input failover occurs. Silence is defined as
    -- audio loss or audio quieter than -50 dBFS.
    audioSilenceThresholdMsec :: Core.Maybe Core.Natural,
    -- | The name of the audio selector in the input that MediaLive should
    -- monitor to detect silence. Select your most important rendition. If you
    -- didn\'t create an audio selector in this input, leave blank.
    audioSelectorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioSilenceFailoverSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioSilenceThresholdMsec', 'audioSilenceFailoverSettings_audioSilenceThresholdMsec' - The amount of time (in milliseconds) that the active input must be
-- silent before automatic input failover occurs. Silence is defined as
-- audio loss or audio quieter than -50 dBFS.
--
-- 'audioSelectorName', 'audioSilenceFailoverSettings_audioSelectorName' - The name of the audio selector in the input that MediaLive should
-- monitor to detect silence. Select your most important rendition. If you
-- didn\'t create an audio selector in this input, leave blank.
newAudioSilenceFailoverSettings ::
  -- | 'audioSelectorName'
  Core.Text ->
  AudioSilenceFailoverSettings
newAudioSilenceFailoverSettings pAudioSelectorName_ =
  AudioSilenceFailoverSettings'
    { audioSilenceThresholdMsec =
        Core.Nothing,
      audioSelectorName = pAudioSelectorName_
    }

-- | The amount of time (in milliseconds) that the active input must be
-- silent before automatic input failover occurs. Silence is defined as
-- audio loss or audio quieter than -50 dBFS.
audioSilenceFailoverSettings_audioSilenceThresholdMsec :: Lens.Lens' AudioSilenceFailoverSettings (Core.Maybe Core.Natural)
audioSilenceFailoverSettings_audioSilenceThresholdMsec = Lens.lens (\AudioSilenceFailoverSettings' {audioSilenceThresholdMsec} -> audioSilenceThresholdMsec) (\s@AudioSilenceFailoverSettings' {} a -> s {audioSilenceThresholdMsec = a} :: AudioSilenceFailoverSettings)

-- | The name of the audio selector in the input that MediaLive should
-- monitor to detect silence. Select your most important rendition. If you
-- didn\'t create an audio selector in this input, leave blank.
audioSilenceFailoverSettings_audioSelectorName :: Lens.Lens' AudioSilenceFailoverSettings Core.Text
audioSilenceFailoverSettings_audioSelectorName = Lens.lens (\AudioSilenceFailoverSettings' {audioSelectorName} -> audioSelectorName) (\s@AudioSilenceFailoverSettings' {} a -> s {audioSelectorName = a} :: AudioSilenceFailoverSettings)

instance Core.FromJSON AudioSilenceFailoverSettings where
  parseJSON =
    Core.withObject
      "AudioSilenceFailoverSettings"
      ( \x ->
          AudioSilenceFailoverSettings'
            Core.<$> (x Core..:? "audioSilenceThresholdMsec")
            Core.<*> (x Core..: "audioSelectorName")
      )

instance Core.Hashable AudioSilenceFailoverSettings

instance Core.NFData AudioSilenceFailoverSettings

instance Core.ToJSON AudioSilenceFailoverSettings where
  toJSON AudioSilenceFailoverSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioSilenceThresholdMsec" Core..=)
              Core.<$> audioSilenceThresholdMsec,
            Core.Just
              ("audioSelectorName" Core..= audioSelectorName)
          ]
      )
