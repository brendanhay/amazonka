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
-- Module      : Network.AWS.MediaLive.Types.AudioLanguageSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioLanguageSelection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy

-- | Audio Language Selection
--
-- /See:/ 'newAudioLanguageSelection' smart constructor.
data AudioLanguageSelection = AudioLanguageSelection'
  { -- | When set to \"strict\", the transport stream demux strictly identifies
    -- audio streams by their language descriptor. If a PMT update occurs such
    -- that an audio stream matching the initially selected language is no
    -- longer present then mute will be encoded until the language returns. If
    -- \"loose\", then on a PMT update the demux will choose another audio
    -- stream in the program with the same stream type if it can\'t find one
    -- with the same language.
    languageSelectionPolicy :: Core.Maybe AudioLanguageSelectionPolicy,
    -- | Selects a specific three-letter language code from within an audio
    -- source.
    languageCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioLanguageSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageSelectionPolicy', 'audioLanguageSelection_languageSelectionPolicy' - When set to \"strict\", the transport stream demux strictly identifies
-- audio streams by their language descriptor. If a PMT update occurs such
-- that an audio stream matching the initially selected language is no
-- longer present then mute will be encoded until the language returns. If
-- \"loose\", then on a PMT update the demux will choose another audio
-- stream in the program with the same stream type if it can\'t find one
-- with the same language.
--
-- 'languageCode', 'audioLanguageSelection_languageCode' - Selects a specific three-letter language code from within an audio
-- source.
newAudioLanguageSelection ::
  -- | 'languageCode'
  Core.Text ->
  AudioLanguageSelection
newAudioLanguageSelection pLanguageCode_ =
  AudioLanguageSelection'
    { languageSelectionPolicy =
        Core.Nothing,
      languageCode = pLanguageCode_
    }

-- | When set to \"strict\", the transport stream demux strictly identifies
-- audio streams by their language descriptor. If a PMT update occurs such
-- that an audio stream matching the initially selected language is no
-- longer present then mute will be encoded until the language returns. If
-- \"loose\", then on a PMT update the demux will choose another audio
-- stream in the program with the same stream type if it can\'t find one
-- with the same language.
audioLanguageSelection_languageSelectionPolicy :: Lens.Lens' AudioLanguageSelection (Core.Maybe AudioLanguageSelectionPolicy)
audioLanguageSelection_languageSelectionPolicy = Lens.lens (\AudioLanguageSelection' {languageSelectionPolicy} -> languageSelectionPolicy) (\s@AudioLanguageSelection' {} a -> s {languageSelectionPolicy = a} :: AudioLanguageSelection)

-- | Selects a specific three-letter language code from within an audio
-- source.
audioLanguageSelection_languageCode :: Lens.Lens' AudioLanguageSelection Core.Text
audioLanguageSelection_languageCode = Lens.lens (\AudioLanguageSelection' {languageCode} -> languageCode) (\s@AudioLanguageSelection' {} a -> s {languageCode = a} :: AudioLanguageSelection)

instance Core.FromJSON AudioLanguageSelection where
  parseJSON =
    Core.withObject
      "AudioLanguageSelection"
      ( \x ->
          AudioLanguageSelection'
            Core.<$> (x Core..:? "languageSelectionPolicy")
            Core.<*> (x Core..: "languageCode")
      )

instance Core.Hashable AudioLanguageSelection

instance Core.NFData AudioLanguageSelection

instance Core.ToJSON AudioLanguageSelection where
  toJSON AudioLanguageSelection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageSelectionPolicy" Core..=)
              Core.<$> languageSelectionPolicy,
            Core.Just ("languageCode" Core..= languageCode)
          ]
      )
