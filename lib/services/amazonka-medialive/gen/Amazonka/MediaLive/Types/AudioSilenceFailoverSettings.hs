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
-- Module      : Amazonka.MediaLive.Types.AudioSilenceFailoverSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioSilenceFailoverSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for AudioSilenceFailoverSettings
--
-- /See:/ 'newAudioSilenceFailoverSettings' smart constructor.
data AudioSilenceFailoverSettings = AudioSilenceFailoverSettings'
  { -- | The amount of time (in milliseconds) that the active input must be
    -- silent before automatic input failover occurs. Silence is defined as
    -- audio loss or audio quieter than -50 dBFS.
    audioSilenceThresholdMsec :: Prelude.Maybe Prelude.Natural,
    -- | The name of the audio selector in the input that MediaLive should
    -- monitor to detect silence. Select your most important rendition. If you
    -- didn\'t create an audio selector in this input, leave blank.
    audioSelectorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AudioSilenceFailoverSettings
newAudioSilenceFailoverSettings pAudioSelectorName_ =
  AudioSilenceFailoverSettings'
    { audioSilenceThresholdMsec =
        Prelude.Nothing,
      audioSelectorName = pAudioSelectorName_
    }

-- | The amount of time (in milliseconds) that the active input must be
-- silent before automatic input failover occurs. Silence is defined as
-- audio loss or audio quieter than -50 dBFS.
audioSilenceFailoverSettings_audioSilenceThresholdMsec :: Lens.Lens' AudioSilenceFailoverSettings (Prelude.Maybe Prelude.Natural)
audioSilenceFailoverSettings_audioSilenceThresholdMsec = Lens.lens (\AudioSilenceFailoverSettings' {audioSilenceThresholdMsec} -> audioSilenceThresholdMsec) (\s@AudioSilenceFailoverSettings' {} a -> s {audioSilenceThresholdMsec = a} :: AudioSilenceFailoverSettings)

-- | The name of the audio selector in the input that MediaLive should
-- monitor to detect silence. Select your most important rendition. If you
-- didn\'t create an audio selector in this input, leave blank.
audioSilenceFailoverSettings_audioSelectorName :: Lens.Lens' AudioSilenceFailoverSettings Prelude.Text
audioSilenceFailoverSettings_audioSelectorName = Lens.lens (\AudioSilenceFailoverSettings' {audioSelectorName} -> audioSelectorName) (\s@AudioSilenceFailoverSettings' {} a -> s {audioSelectorName = a} :: AudioSilenceFailoverSettings)

instance Data.FromJSON AudioSilenceFailoverSettings where
  parseJSON =
    Data.withObject
      "AudioSilenceFailoverSettings"
      ( \x ->
          AudioSilenceFailoverSettings'
            Prelude.<$> (x Data..:? "audioSilenceThresholdMsec")
            Prelude.<*> (x Data..: "audioSelectorName")
      )

instance
  Prelude.Hashable
    AudioSilenceFailoverSettings
  where
  hashWithSalt _salt AudioSilenceFailoverSettings' {..} =
    _salt
      `Prelude.hashWithSalt` audioSilenceThresholdMsec
      `Prelude.hashWithSalt` audioSelectorName

instance Prelude.NFData AudioSilenceFailoverSettings where
  rnf AudioSilenceFailoverSettings' {..} =
    Prelude.rnf audioSilenceThresholdMsec
      `Prelude.seq` Prelude.rnf audioSelectorName

instance Data.ToJSON AudioSilenceFailoverSettings where
  toJSON AudioSilenceFailoverSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioSilenceThresholdMsec" Data..=)
              Prelude.<$> audioSilenceThresholdMsec,
            Prelude.Just
              ("audioSelectorName" Data..= audioSelectorName)
          ]
      )
