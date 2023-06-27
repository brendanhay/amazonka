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
-- Module      : Amazonka.ChimeSdkVoice.Types.SpeakerSearchResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SpeakerSearchResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of a speaker search analysis.
--
-- /See:/ 'newSpeakerSearchResult' smart constructor.
data SpeakerSearchResult = SpeakerSearchResult'
  { -- | The confidence score in the speaker search analysis.
    confidenceScore :: Prelude.Maybe Prelude.Double,
    -- | The voice profile ID.
    voiceProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpeakerSearchResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidenceScore', 'speakerSearchResult_confidenceScore' - The confidence score in the speaker search analysis.
--
-- 'voiceProfileId', 'speakerSearchResult_voiceProfileId' - The voice profile ID.
newSpeakerSearchResult ::
  SpeakerSearchResult
newSpeakerSearchResult =
  SpeakerSearchResult'
    { confidenceScore =
        Prelude.Nothing,
      voiceProfileId = Prelude.Nothing
    }

-- | The confidence score in the speaker search analysis.
speakerSearchResult_confidenceScore :: Lens.Lens' SpeakerSearchResult (Prelude.Maybe Prelude.Double)
speakerSearchResult_confidenceScore = Lens.lens (\SpeakerSearchResult' {confidenceScore} -> confidenceScore) (\s@SpeakerSearchResult' {} a -> s {confidenceScore = a} :: SpeakerSearchResult)

-- | The voice profile ID.
speakerSearchResult_voiceProfileId :: Lens.Lens' SpeakerSearchResult (Prelude.Maybe Prelude.Text)
speakerSearchResult_voiceProfileId = Lens.lens (\SpeakerSearchResult' {voiceProfileId} -> voiceProfileId) (\s@SpeakerSearchResult' {} a -> s {voiceProfileId = a} :: SpeakerSearchResult)

instance Data.FromJSON SpeakerSearchResult where
  parseJSON =
    Data.withObject
      "SpeakerSearchResult"
      ( \x ->
          SpeakerSearchResult'
            Prelude.<$> (x Data..:? "ConfidenceScore")
            Prelude.<*> (x Data..:? "VoiceProfileId")
      )

instance Prelude.Hashable SpeakerSearchResult where
  hashWithSalt _salt SpeakerSearchResult' {..} =
    _salt
      `Prelude.hashWithSalt` confidenceScore
      `Prelude.hashWithSalt` voiceProfileId

instance Prelude.NFData SpeakerSearchResult where
  rnf SpeakerSearchResult' {..} =
    Prelude.rnf confidenceScore
      `Prelude.seq` Prelude.rnf voiceProfileId
