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
-- Module      : Amazonka.LexV2Models.Types.UtteranceInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UtteranceInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.UtteranceAudioInputSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains information about input of an utterance.
--
-- /See:/ 'newUtteranceInputSpecification' smart constructor.
data UtteranceInputSpecification = UtteranceInputSpecification'
  { -- | Contains information about the audio input for an utterance.
    audioInput :: Prelude.Maybe UtteranceAudioInputSpecification,
    -- | A text input transcription of the utterance. It is only applicable for
    -- test-sets containing text data.
    textInput :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioInput', 'utteranceInputSpecification_audioInput' - Contains information about the audio input for an utterance.
--
-- 'textInput', 'utteranceInputSpecification_textInput' - A text input transcription of the utterance. It is only applicable for
-- test-sets containing text data.
newUtteranceInputSpecification ::
  UtteranceInputSpecification
newUtteranceInputSpecification =
  UtteranceInputSpecification'
    { audioInput =
        Prelude.Nothing,
      textInput = Prelude.Nothing
    }

-- | Contains information about the audio input for an utterance.
utteranceInputSpecification_audioInput :: Lens.Lens' UtteranceInputSpecification (Prelude.Maybe UtteranceAudioInputSpecification)
utteranceInputSpecification_audioInput = Lens.lens (\UtteranceInputSpecification' {audioInput} -> audioInput) (\s@UtteranceInputSpecification' {} a -> s {audioInput = a} :: UtteranceInputSpecification)

-- | A text input transcription of the utterance. It is only applicable for
-- test-sets containing text data.
utteranceInputSpecification_textInput :: Lens.Lens' UtteranceInputSpecification (Prelude.Maybe Prelude.Text)
utteranceInputSpecification_textInput = Lens.lens (\UtteranceInputSpecification' {textInput} -> textInput) (\s@UtteranceInputSpecification' {} a -> s {textInput = a} :: UtteranceInputSpecification)

instance Data.FromJSON UtteranceInputSpecification where
  parseJSON =
    Data.withObject
      "UtteranceInputSpecification"
      ( \x ->
          UtteranceInputSpecification'
            Prelude.<$> (x Data..:? "audioInput")
            Prelude.<*> (x Data..:? "textInput")
      )

instance Prelude.Hashable UtteranceInputSpecification where
  hashWithSalt _salt UtteranceInputSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` audioInput
      `Prelude.hashWithSalt` textInput

instance Prelude.NFData UtteranceInputSpecification where
  rnf UtteranceInputSpecification' {..} =
    Prelude.rnf audioInput
      `Prelude.seq` Prelude.rnf textInput
