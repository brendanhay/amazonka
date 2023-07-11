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
-- Module      : Amazonka.MediaLive.Types.AudioSelectorSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioSelectorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioHlsRenditionSelection
import Amazonka.MediaLive.Types.AudioLanguageSelection
import Amazonka.MediaLive.Types.AudioPidSelection
import Amazonka.MediaLive.Types.AudioTrackSelection
import qualified Amazonka.Prelude as Prelude

-- | Audio Selector Settings
--
-- /See:/ 'newAudioSelectorSettings' smart constructor.
data AudioSelectorSettings = AudioSelectorSettings'
  { audioHlsRenditionSelection :: Prelude.Maybe AudioHlsRenditionSelection,
    audioLanguageSelection :: Prelude.Maybe AudioLanguageSelection,
    audioPidSelection :: Prelude.Maybe AudioPidSelection,
    audioTrackSelection :: Prelude.Maybe AudioTrackSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioSelectorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioHlsRenditionSelection', 'audioSelectorSettings_audioHlsRenditionSelection' - Undocumented member.
--
-- 'audioLanguageSelection', 'audioSelectorSettings_audioLanguageSelection' - Undocumented member.
--
-- 'audioPidSelection', 'audioSelectorSettings_audioPidSelection' - Undocumented member.
--
-- 'audioTrackSelection', 'audioSelectorSettings_audioTrackSelection' - Undocumented member.
newAudioSelectorSettings ::
  AudioSelectorSettings
newAudioSelectorSettings =
  AudioSelectorSettings'
    { audioHlsRenditionSelection =
        Prelude.Nothing,
      audioLanguageSelection = Prelude.Nothing,
      audioPidSelection = Prelude.Nothing,
      audioTrackSelection = Prelude.Nothing
    }

-- | Undocumented member.
audioSelectorSettings_audioHlsRenditionSelection :: Lens.Lens' AudioSelectorSettings (Prelude.Maybe AudioHlsRenditionSelection)
audioSelectorSettings_audioHlsRenditionSelection = Lens.lens (\AudioSelectorSettings' {audioHlsRenditionSelection} -> audioHlsRenditionSelection) (\s@AudioSelectorSettings' {} a -> s {audioHlsRenditionSelection = a} :: AudioSelectorSettings)

-- | Undocumented member.
audioSelectorSettings_audioLanguageSelection :: Lens.Lens' AudioSelectorSettings (Prelude.Maybe AudioLanguageSelection)
audioSelectorSettings_audioLanguageSelection = Lens.lens (\AudioSelectorSettings' {audioLanguageSelection} -> audioLanguageSelection) (\s@AudioSelectorSettings' {} a -> s {audioLanguageSelection = a} :: AudioSelectorSettings)

-- | Undocumented member.
audioSelectorSettings_audioPidSelection :: Lens.Lens' AudioSelectorSettings (Prelude.Maybe AudioPidSelection)
audioSelectorSettings_audioPidSelection = Lens.lens (\AudioSelectorSettings' {audioPidSelection} -> audioPidSelection) (\s@AudioSelectorSettings' {} a -> s {audioPidSelection = a} :: AudioSelectorSettings)

-- | Undocumented member.
audioSelectorSettings_audioTrackSelection :: Lens.Lens' AudioSelectorSettings (Prelude.Maybe AudioTrackSelection)
audioSelectorSettings_audioTrackSelection = Lens.lens (\AudioSelectorSettings' {audioTrackSelection} -> audioTrackSelection) (\s@AudioSelectorSettings' {} a -> s {audioTrackSelection = a} :: AudioSelectorSettings)

instance Data.FromJSON AudioSelectorSettings where
  parseJSON =
    Data.withObject
      "AudioSelectorSettings"
      ( \x ->
          AudioSelectorSettings'
            Prelude.<$> (x Data..:? "audioHlsRenditionSelection")
            Prelude.<*> (x Data..:? "audioLanguageSelection")
            Prelude.<*> (x Data..:? "audioPidSelection")
            Prelude.<*> (x Data..:? "audioTrackSelection")
      )

instance Prelude.Hashable AudioSelectorSettings where
  hashWithSalt _salt AudioSelectorSettings' {..} =
    _salt
      `Prelude.hashWithSalt` audioHlsRenditionSelection
      `Prelude.hashWithSalt` audioLanguageSelection
      `Prelude.hashWithSalt` audioPidSelection
      `Prelude.hashWithSalt` audioTrackSelection

instance Prelude.NFData AudioSelectorSettings where
  rnf AudioSelectorSettings' {..} =
    Prelude.rnf audioHlsRenditionSelection
      `Prelude.seq` Prelude.rnf audioLanguageSelection
      `Prelude.seq` Prelude.rnf audioPidSelection
      `Prelude.seq` Prelude.rnf audioTrackSelection

instance Data.ToJSON AudioSelectorSettings where
  toJSON AudioSelectorSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioHlsRenditionSelection" Data..=)
              Prelude.<$> audioHlsRenditionSelection,
            ("audioLanguageSelection" Data..=)
              Prelude.<$> audioLanguageSelection,
            ("audioPidSelection" Data..=)
              Prelude.<$> audioPidSelection,
            ("audioTrackSelection" Data..=)
              Prelude.<$> audioTrackSelection
          ]
      )
