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
-- Module      : Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.PostLaunchActionExecutionStatus
import Amazonka.MGN.Types.SsmDocument
import Amazonka.MGN.Types.SsmDocumentType
import qualified Amazonka.Prelude as Prelude

-- | Job type.
--
-- /See:/ 'newJobPostLaunchActionsLaunchStatus' smart constructor.
data JobPostLaunchActionsLaunchStatus = JobPostLaunchActionsLaunchStatus'
  { -- | Job type.
    executionID :: Prelude.Maybe Prelude.Text,
    -- | Job type.
    ssmDocument :: Prelude.Maybe SsmDocument,
    -- | Job type.
    ssmDocumentType :: Prelude.Maybe SsmDocumentType,
    -- | Job type.
    executionStatus :: Prelude.Maybe PostLaunchActionExecutionStatus,
    -- | Job type.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobPostLaunchActionsLaunchStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionID', 'jobPostLaunchActionsLaunchStatus_executionID' - Job type.
--
-- 'ssmDocument', 'jobPostLaunchActionsLaunchStatus_ssmDocument' - Job type.
--
-- 'ssmDocumentType', 'jobPostLaunchActionsLaunchStatus_ssmDocumentType' - Job type.
--
-- 'executionStatus', 'jobPostLaunchActionsLaunchStatus_executionStatus' - Job type.
--
-- 'failureReason', 'jobPostLaunchActionsLaunchStatus_failureReason' - Job type.
newJobPostLaunchActionsLaunchStatus ::
  JobPostLaunchActionsLaunchStatus
newJobPostLaunchActionsLaunchStatus =
  JobPostLaunchActionsLaunchStatus'
    { executionID =
        Prelude.Nothing,
      ssmDocument = Prelude.Nothing,
      ssmDocumentType = Prelude.Nothing,
      executionStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | Job type.
jobPostLaunchActionsLaunchStatus_executionID :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe Prelude.Text)
jobPostLaunchActionsLaunchStatus_executionID = Lens.lens (\JobPostLaunchActionsLaunchStatus' {executionID} -> executionID) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {executionID = a} :: JobPostLaunchActionsLaunchStatus)

-- | Job type.
jobPostLaunchActionsLaunchStatus_ssmDocument :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe SsmDocument)
jobPostLaunchActionsLaunchStatus_ssmDocument = Lens.lens (\JobPostLaunchActionsLaunchStatus' {ssmDocument} -> ssmDocument) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {ssmDocument = a} :: JobPostLaunchActionsLaunchStatus)

-- | Job type.
jobPostLaunchActionsLaunchStatus_ssmDocumentType :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe SsmDocumentType)
jobPostLaunchActionsLaunchStatus_ssmDocumentType = Lens.lens (\JobPostLaunchActionsLaunchStatus' {ssmDocumentType} -> ssmDocumentType) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {ssmDocumentType = a} :: JobPostLaunchActionsLaunchStatus)

-- | Job type.
jobPostLaunchActionsLaunchStatus_executionStatus :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe PostLaunchActionExecutionStatus)
jobPostLaunchActionsLaunchStatus_executionStatus = Lens.lens (\JobPostLaunchActionsLaunchStatus' {executionStatus} -> executionStatus) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {executionStatus = a} :: JobPostLaunchActionsLaunchStatus)

-- | Job type.
jobPostLaunchActionsLaunchStatus_failureReason :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe Prelude.Text)
jobPostLaunchActionsLaunchStatus_failureReason = Lens.lens (\JobPostLaunchActionsLaunchStatus' {failureReason} -> failureReason) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {failureReason = a} :: JobPostLaunchActionsLaunchStatus)

instance
  Core.FromJSON
    JobPostLaunchActionsLaunchStatus
  where
  parseJSON =
    Core.withObject
      "JobPostLaunchActionsLaunchStatus"
      ( \x ->
          JobPostLaunchActionsLaunchStatus'
            Prelude.<$> (x Core..:? "executionID")
            Prelude.<*> (x Core..:? "ssmDocument")
            Prelude.<*> (x Core..:? "ssmDocumentType")
            Prelude.<*> (x Core..:? "executionStatus")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance
  Prelude.Hashable
    JobPostLaunchActionsLaunchStatus
  where
  hashWithSalt
    _salt
    JobPostLaunchActionsLaunchStatus' {..} =
      _salt `Prelude.hashWithSalt` executionID
        `Prelude.hashWithSalt` ssmDocument
        `Prelude.hashWithSalt` ssmDocumentType
        `Prelude.hashWithSalt` executionStatus
        `Prelude.hashWithSalt` failureReason

instance
  Prelude.NFData
    JobPostLaunchActionsLaunchStatus
  where
  rnf JobPostLaunchActionsLaunchStatus' {..} =
    Prelude.rnf executionID
      `Prelude.seq` Prelude.rnf ssmDocument
      `Prelude.seq` Prelude.rnf ssmDocumentType
      `Prelude.seq` Prelude.rnf executionStatus
      `Prelude.seq` Prelude.rnf failureReason
