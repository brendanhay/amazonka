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
-- Module      : Network.AWS.Transcribe.Types.MedicalTranscript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MedicalTranscript where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies the location of a medical transcript.
--
-- /See:/ 'newMedicalTranscript' smart constructor.
data MedicalTranscript = MedicalTranscript'
  { -- | The S3 object location of the medical transcript.
    --
    -- Use this URI to access the medical transcript. This URI points to the S3
    -- bucket you created to store the medical transcript.
    transcriptFileUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MedicalTranscript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptFileUri', 'medicalTranscript_transcriptFileUri' - The S3 object location of the medical transcript.
--
-- Use this URI to access the medical transcript. This URI points to the S3
-- bucket you created to store the medical transcript.
newMedicalTranscript ::
  MedicalTranscript
newMedicalTranscript =
  MedicalTranscript'
    { transcriptFileUri =
        Prelude.Nothing
    }

-- | The S3 object location of the medical transcript.
--
-- Use this URI to access the medical transcript. This URI points to the S3
-- bucket you created to store the medical transcript.
medicalTranscript_transcriptFileUri :: Lens.Lens' MedicalTranscript (Prelude.Maybe Prelude.Text)
medicalTranscript_transcriptFileUri = Lens.lens (\MedicalTranscript' {transcriptFileUri} -> transcriptFileUri) (\s@MedicalTranscript' {} a -> s {transcriptFileUri = a} :: MedicalTranscript)

instance Prelude.FromJSON MedicalTranscript where
  parseJSON =
    Prelude.withObject
      "MedicalTranscript"
      ( \x ->
          MedicalTranscript'
            Prelude.<$> (x Prelude..:? "TranscriptFileUri")
      )

instance Prelude.Hashable MedicalTranscript

instance Prelude.NFData MedicalTranscript
