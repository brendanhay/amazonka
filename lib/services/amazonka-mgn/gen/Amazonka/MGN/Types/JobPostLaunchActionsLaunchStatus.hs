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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.PostLaunchActionExecutionStatus
import Amazonka.MGN.Types.SsmDocument
import Amazonka.MGN.Types.SsmDocumentType
import qualified Amazonka.Prelude as Prelude

-- | Launch Status of the Job Post Launch Actions.
--
-- /See:/ 'newJobPostLaunchActionsLaunchStatus' smart constructor.
data JobPostLaunchActionsLaunchStatus = JobPostLaunchActionsLaunchStatus'
  { -- | AWS Systems Manager Document\'s execution ID of the of the Job Post
    -- Launch Actions.
    executionID :: Prelude.Maybe Prelude.Text,
    -- | AWS Systems Manager Document\'s execution status.
    executionStatus :: Prelude.Maybe PostLaunchActionExecutionStatus,
    -- | AWS Systems Manager Document\'s failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | AWS Systems Manager\'s Document of the of the Job Post Launch Actions.
    ssmDocument :: Prelude.Maybe SsmDocument,
    -- | AWS Systems Manager Document type.
    ssmDocumentType :: Prelude.Maybe SsmDocumentType
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
-- 'executionID', 'jobPostLaunchActionsLaunchStatus_executionID' - AWS Systems Manager Document\'s execution ID of the of the Job Post
-- Launch Actions.
--
-- 'executionStatus', 'jobPostLaunchActionsLaunchStatus_executionStatus' - AWS Systems Manager Document\'s execution status.
--
-- 'failureReason', 'jobPostLaunchActionsLaunchStatus_failureReason' - AWS Systems Manager Document\'s failure reason.
--
-- 'ssmDocument', 'jobPostLaunchActionsLaunchStatus_ssmDocument' - AWS Systems Manager\'s Document of the of the Job Post Launch Actions.
--
-- 'ssmDocumentType', 'jobPostLaunchActionsLaunchStatus_ssmDocumentType' - AWS Systems Manager Document type.
newJobPostLaunchActionsLaunchStatus ::
  JobPostLaunchActionsLaunchStatus
newJobPostLaunchActionsLaunchStatus =
  JobPostLaunchActionsLaunchStatus'
    { executionID =
        Prelude.Nothing,
      executionStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      ssmDocument = Prelude.Nothing,
      ssmDocumentType = Prelude.Nothing
    }

-- | AWS Systems Manager Document\'s execution ID of the of the Job Post
-- Launch Actions.
jobPostLaunchActionsLaunchStatus_executionID :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe Prelude.Text)
jobPostLaunchActionsLaunchStatus_executionID = Lens.lens (\JobPostLaunchActionsLaunchStatus' {executionID} -> executionID) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {executionID = a} :: JobPostLaunchActionsLaunchStatus)

-- | AWS Systems Manager Document\'s execution status.
jobPostLaunchActionsLaunchStatus_executionStatus :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe PostLaunchActionExecutionStatus)
jobPostLaunchActionsLaunchStatus_executionStatus = Lens.lens (\JobPostLaunchActionsLaunchStatus' {executionStatus} -> executionStatus) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {executionStatus = a} :: JobPostLaunchActionsLaunchStatus)

-- | AWS Systems Manager Document\'s failure reason.
jobPostLaunchActionsLaunchStatus_failureReason :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe Prelude.Text)
jobPostLaunchActionsLaunchStatus_failureReason = Lens.lens (\JobPostLaunchActionsLaunchStatus' {failureReason} -> failureReason) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {failureReason = a} :: JobPostLaunchActionsLaunchStatus)

-- | AWS Systems Manager\'s Document of the of the Job Post Launch Actions.
jobPostLaunchActionsLaunchStatus_ssmDocument :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe SsmDocument)
jobPostLaunchActionsLaunchStatus_ssmDocument = Lens.lens (\JobPostLaunchActionsLaunchStatus' {ssmDocument} -> ssmDocument) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {ssmDocument = a} :: JobPostLaunchActionsLaunchStatus)

-- | AWS Systems Manager Document type.
jobPostLaunchActionsLaunchStatus_ssmDocumentType :: Lens.Lens' JobPostLaunchActionsLaunchStatus (Prelude.Maybe SsmDocumentType)
jobPostLaunchActionsLaunchStatus_ssmDocumentType = Lens.lens (\JobPostLaunchActionsLaunchStatus' {ssmDocumentType} -> ssmDocumentType) (\s@JobPostLaunchActionsLaunchStatus' {} a -> s {ssmDocumentType = a} :: JobPostLaunchActionsLaunchStatus)

instance
  Data.FromJSON
    JobPostLaunchActionsLaunchStatus
  where
  parseJSON =
    Data.withObject
      "JobPostLaunchActionsLaunchStatus"
      ( \x ->
          JobPostLaunchActionsLaunchStatus'
            Prelude.<$> (x Data..:? "executionID")
            Prelude.<*> (x Data..:? "executionStatus")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "ssmDocument")
            Prelude.<*> (x Data..:? "ssmDocumentType")
      )

instance
  Prelude.Hashable
    JobPostLaunchActionsLaunchStatus
  where
  hashWithSalt
    _salt
    JobPostLaunchActionsLaunchStatus' {..} =
      _salt
        `Prelude.hashWithSalt` executionID
        `Prelude.hashWithSalt` executionStatus
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` ssmDocument
        `Prelude.hashWithSalt` ssmDocumentType

instance
  Prelude.NFData
    JobPostLaunchActionsLaunchStatus
  where
  rnf JobPostLaunchActionsLaunchStatus' {..} =
    Prelude.rnf executionID `Prelude.seq`
      Prelude.rnf executionStatus `Prelude.seq`
        Prelude.rnf failureReason `Prelude.seq`
          Prelude.rnf ssmDocument `Prelude.seq`
            Prelude.rnf ssmDocumentType
