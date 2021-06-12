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
-- Module      : Network.AWS.Translate.Types.JobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.JobDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The number of documents successfully and unsuccessfully processed during
-- a translation job.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | The number of documents used as input in a translation job.
    inputDocumentsCount :: Core.Maybe Core.Int,
    -- | The number of documents that could not be processed during a translation
    -- job.
    documentsWithErrorsCount :: Core.Maybe Core.Int,
    -- | The number of documents successfully processed during a translation job.
    translatedDocumentsCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDocumentsCount', 'jobDetails_inputDocumentsCount' - The number of documents used as input in a translation job.
--
-- 'documentsWithErrorsCount', 'jobDetails_documentsWithErrorsCount' - The number of documents that could not be processed during a translation
-- job.
--
-- 'translatedDocumentsCount', 'jobDetails_translatedDocumentsCount' - The number of documents successfully processed during a translation job.
newJobDetails ::
  JobDetails
newJobDetails =
  JobDetails'
    { inputDocumentsCount = Core.Nothing,
      documentsWithErrorsCount = Core.Nothing,
      translatedDocumentsCount = Core.Nothing
    }

-- | The number of documents used as input in a translation job.
jobDetails_inputDocumentsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jobDetails_inputDocumentsCount = Lens.lens (\JobDetails' {inputDocumentsCount} -> inputDocumentsCount) (\s@JobDetails' {} a -> s {inputDocumentsCount = a} :: JobDetails)

-- | The number of documents that could not be processed during a translation
-- job.
jobDetails_documentsWithErrorsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jobDetails_documentsWithErrorsCount = Lens.lens (\JobDetails' {documentsWithErrorsCount} -> documentsWithErrorsCount) (\s@JobDetails' {} a -> s {documentsWithErrorsCount = a} :: JobDetails)

-- | The number of documents successfully processed during a translation job.
jobDetails_translatedDocumentsCount :: Lens.Lens' JobDetails (Core.Maybe Core.Int)
jobDetails_translatedDocumentsCount = Lens.lens (\JobDetails' {translatedDocumentsCount} -> translatedDocumentsCount) (\s@JobDetails' {} a -> s {translatedDocumentsCount = a} :: JobDetails)

instance Core.FromJSON JobDetails where
  parseJSON =
    Core.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Core.<$> (x Core..:? "InputDocumentsCount")
            Core.<*> (x Core..:? "DocumentsWithErrorsCount")
            Core.<*> (x Core..:? "TranslatedDocumentsCount")
      )

instance Core.Hashable JobDetails

instance Core.NFData JobDetails
