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
-- Module      : Amazonka.Translate.Types.JobDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.JobDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The number of documents successfully and unsuccessfully processed during
-- a translation job.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | The number of documents that could not be processed during a translation
    -- job.
    documentsWithErrorsCount :: Prelude.Maybe Prelude.Int,
    -- | The number of documents successfully processed during a translation job.
    translatedDocumentsCount :: Prelude.Maybe Prelude.Int,
    -- | The number of documents used as input in a translation job.
    inputDocumentsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentsWithErrorsCount', 'jobDetails_documentsWithErrorsCount' - The number of documents that could not be processed during a translation
-- job.
--
-- 'translatedDocumentsCount', 'jobDetails_translatedDocumentsCount' - The number of documents successfully processed during a translation job.
--
-- 'inputDocumentsCount', 'jobDetails_inputDocumentsCount' - The number of documents used as input in a translation job.
newJobDetails ::
  JobDetails
newJobDetails =
  JobDetails'
    { documentsWithErrorsCount =
        Prelude.Nothing,
      translatedDocumentsCount = Prelude.Nothing,
      inputDocumentsCount = Prelude.Nothing
    }

-- | The number of documents that could not be processed during a translation
-- job.
jobDetails_documentsWithErrorsCount :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Int)
jobDetails_documentsWithErrorsCount = Lens.lens (\JobDetails' {documentsWithErrorsCount} -> documentsWithErrorsCount) (\s@JobDetails' {} a -> s {documentsWithErrorsCount = a} :: JobDetails)

-- | The number of documents successfully processed during a translation job.
jobDetails_translatedDocumentsCount :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Int)
jobDetails_translatedDocumentsCount = Lens.lens (\JobDetails' {translatedDocumentsCount} -> translatedDocumentsCount) (\s@JobDetails' {} a -> s {translatedDocumentsCount = a} :: JobDetails)

-- | The number of documents used as input in a translation job.
jobDetails_inputDocumentsCount :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Int)
jobDetails_inputDocumentsCount = Lens.lens (\JobDetails' {inputDocumentsCount} -> inputDocumentsCount) (\s@JobDetails' {} a -> s {inputDocumentsCount = a} :: JobDetails)

instance Core.FromJSON JobDetails where
  parseJSON =
    Core.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Prelude.<$> (x Core..:? "DocumentsWithErrorsCount")
            Prelude.<*> (x Core..:? "TranslatedDocumentsCount")
            Prelude.<*> (x Core..:? "InputDocumentsCount")
      )

instance Prelude.Hashable JobDetails where
  hashWithSalt _salt JobDetails' {..} =
    _salt
      `Prelude.hashWithSalt` documentsWithErrorsCount
      `Prelude.hashWithSalt` translatedDocumentsCount
      `Prelude.hashWithSalt` inputDocumentsCount

instance Prelude.NFData JobDetails where
  rnf JobDetails' {..} =
    Prelude.rnf documentsWithErrorsCount
      `Prelude.seq` Prelude.rnf translatedDocumentsCount
      `Prelude.seq` Prelude.rnf inputDocumentsCount
