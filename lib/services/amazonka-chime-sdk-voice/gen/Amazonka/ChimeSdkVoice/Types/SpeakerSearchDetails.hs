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
-- Module      : Amazonka.ChimeSdkVoice.Types.SpeakerSearchDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SpeakerSearchDetails where

import Amazonka.ChimeSdkVoice.Types.SpeakerSearchResult
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a speaker search task.
--
-- /See:/ 'newSpeakerSearchDetails' smart constructor.
data SpeakerSearchDetails = SpeakerSearchDetails'
  { -- | The result value in the speaker search details.
    results :: Prelude.Maybe [SpeakerSearchResult],
    -- | The status of a voice print generation operation,
    -- @VoiceprintGenerationSuccess@ or @VoiceprintGenerationFailure@..
    voiceprintGenerationStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpeakerSearchDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'speakerSearchDetails_results' - The result value in the speaker search details.
--
-- 'voiceprintGenerationStatus', 'speakerSearchDetails_voiceprintGenerationStatus' - The status of a voice print generation operation,
-- @VoiceprintGenerationSuccess@ or @VoiceprintGenerationFailure@..
newSpeakerSearchDetails ::
  SpeakerSearchDetails
newSpeakerSearchDetails =
  SpeakerSearchDetails'
    { results = Prelude.Nothing,
      voiceprintGenerationStatus = Prelude.Nothing
    }

-- | The result value in the speaker search details.
speakerSearchDetails_results :: Lens.Lens' SpeakerSearchDetails (Prelude.Maybe [SpeakerSearchResult])
speakerSearchDetails_results = Lens.lens (\SpeakerSearchDetails' {results} -> results) (\s@SpeakerSearchDetails' {} a -> s {results = a} :: SpeakerSearchDetails) Prelude.. Lens.mapping Lens.coerced

-- | The status of a voice print generation operation,
-- @VoiceprintGenerationSuccess@ or @VoiceprintGenerationFailure@..
speakerSearchDetails_voiceprintGenerationStatus :: Lens.Lens' SpeakerSearchDetails (Prelude.Maybe Prelude.Text)
speakerSearchDetails_voiceprintGenerationStatus = Lens.lens (\SpeakerSearchDetails' {voiceprintGenerationStatus} -> voiceprintGenerationStatus) (\s@SpeakerSearchDetails' {} a -> s {voiceprintGenerationStatus = a} :: SpeakerSearchDetails)

instance Data.FromJSON SpeakerSearchDetails where
  parseJSON =
    Data.withObject
      "SpeakerSearchDetails"
      ( \x ->
          SpeakerSearchDetails'
            Prelude.<$> (x Data..:? "Results" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VoiceprintGenerationStatus")
      )

instance Prelude.Hashable SpeakerSearchDetails where
  hashWithSalt _salt SpeakerSearchDetails' {..} =
    _salt
      `Prelude.hashWithSalt` results
      `Prelude.hashWithSalt` voiceprintGenerationStatus

instance Prelude.NFData SpeakerSearchDetails where
  rnf SpeakerSearchDetails' {..} =
    Prelude.rnf results
      `Prelude.seq` Prelude.rnf voiceprintGenerationStatus
