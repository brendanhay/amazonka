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
-- Module      : Amazonka.EMRServerless.Types.JobRunSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.JobRunSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.JobRunState
import qualified Amazonka.Prelude as Prelude

-- | The summary of attributes associated with a job run.
--
-- /See:/ 'newJobRunSummary' smart constructor.
data JobRunSummary = JobRunSummary'
  { -- | The optional job run name. This doesn\'t have to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of job run, such as Spark or Hive.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application the job is running on.
    applicationId :: Prelude.Text,
    -- | The ID of the job run.
    id :: Prelude.Text,
    -- | The ARN of the job run.
    arn :: Prelude.Text,
    -- | The user who created the job run.
    createdBy :: Prelude.Text,
    -- | The date and time when the job run was created.
    createdAt :: Data.POSIX,
    -- | The date and time when the job run was last updated.
    updatedAt :: Data.POSIX,
    -- | The execution role ARN of the job run.
    executionRole :: Prelude.Text,
    -- | The state of the job run.
    state :: JobRunState,
    -- | The state details of the job run.
    stateDetails :: Prelude.Text,
    -- | The EMR release associated with the application your job is running on.
    releaseLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobRunSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'jobRunSummary_name' - The optional job run name. This doesn\'t have to be unique.
--
-- 'type'', 'jobRunSummary_type' - The type of job run, such as Spark or Hive.
--
-- 'applicationId', 'jobRunSummary_applicationId' - The ID of the application the job is running on.
--
-- 'id', 'jobRunSummary_id' - The ID of the job run.
--
-- 'arn', 'jobRunSummary_arn' - The ARN of the job run.
--
-- 'createdBy', 'jobRunSummary_createdBy' - The user who created the job run.
--
-- 'createdAt', 'jobRunSummary_createdAt' - The date and time when the job run was created.
--
-- 'updatedAt', 'jobRunSummary_updatedAt' - The date and time when the job run was last updated.
--
-- 'executionRole', 'jobRunSummary_executionRole' - The execution role ARN of the job run.
--
-- 'state', 'jobRunSummary_state' - The state of the job run.
--
-- 'stateDetails', 'jobRunSummary_stateDetails' - The state details of the job run.
--
-- 'releaseLabel', 'jobRunSummary_releaseLabel' - The EMR release associated with the application your job is running on.
newJobRunSummary ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'executionRole'
  Prelude.Text ->
  -- | 'state'
  JobRunState ->
  -- | 'stateDetails'
  Prelude.Text ->
  -- | 'releaseLabel'
  Prelude.Text ->
  JobRunSummary
newJobRunSummary
  pApplicationId_
  pId_
  pArn_
  pCreatedBy_
  pCreatedAt_
  pUpdatedAt_
  pExecutionRole_
  pState_
  pStateDetails_
  pReleaseLabel_ =
    JobRunSummary'
      { name = Prelude.Nothing,
        type' = Prelude.Nothing,
        applicationId = pApplicationId_,
        id = pId_,
        arn = pArn_,
        createdBy = pCreatedBy_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        executionRole = pExecutionRole_,
        state = pState_,
        stateDetails = pStateDetails_,
        releaseLabel = pReleaseLabel_
      }

-- | The optional job run name. This doesn\'t have to be unique.
jobRunSummary_name :: Lens.Lens' JobRunSummary (Prelude.Maybe Prelude.Text)
jobRunSummary_name = Lens.lens (\JobRunSummary' {name} -> name) (\s@JobRunSummary' {} a -> s {name = a} :: JobRunSummary)

-- | The type of job run, such as Spark or Hive.
jobRunSummary_type :: Lens.Lens' JobRunSummary (Prelude.Maybe Prelude.Text)
jobRunSummary_type = Lens.lens (\JobRunSummary' {type'} -> type') (\s@JobRunSummary' {} a -> s {type' = a} :: JobRunSummary)

-- | The ID of the application the job is running on.
jobRunSummary_applicationId :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_applicationId = Lens.lens (\JobRunSummary' {applicationId} -> applicationId) (\s@JobRunSummary' {} a -> s {applicationId = a} :: JobRunSummary)

-- | The ID of the job run.
jobRunSummary_id :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_id = Lens.lens (\JobRunSummary' {id} -> id) (\s@JobRunSummary' {} a -> s {id = a} :: JobRunSummary)

-- | The ARN of the job run.
jobRunSummary_arn :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_arn = Lens.lens (\JobRunSummary' {arn} -> arn) (\s@JobRunSummary' {} a -> s {arn = a} :: JobRunSummary)

-- | The user who created the job run.
jobRunSummary_createdBy :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_createdBy = Lens.lens (\JobRunSummary' {createdBy} -> createdBy) (\s@JobRunSummary' {} a -> s {createdBy = a} :: JobRunSummary)

-- | The date and time when the job run was created.
jobRunSummary_createdAt :: Lens.Lens' JobRunSummary Prelude.UTCTime
jobRunSummary_createdAt = Lens.lens (\JobRunSummary' {createdAt} -> createdAt) (\s@JobRunSummary' {} a -> s {createdAt = a} :: JobRunSummary) Prelude.. Data._Time

-- | The date and time when the job run was last updated.
jobRunSummary_updatedAt :: Lens.Lens' JobRunSummary Prelude.UTCTime
jobRunSummary_updatedAt = Lens.lens (\JobRunSummary' {updatedAt} -> updatedAt) (\s@JobRunSummary' {} a -> s {updatedAt = a} :: JobRunSummary) Prelude.. Data._Time

-- | The execution role ARN of the job run.
jobRunSummary_executionRole :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_executionRole = Lens.lens (\JobRunSummary' {executionRole} -> executionRole) (\s@JobRunSummary' {} a -> s {executionRole = a} :: JobRunSummary)

-- | The state of the job run.
jobRunSummary_state :: Lens.Lens' JobRunSummary JobRunState
jobRunSummary_state = Lens.lens (\JobRunSummary' {state} -> state) (\s@JobRunSummary' {} a -> s {state = a} :: JobRunSummary)

-- | The state details of the job run.
jobRunSummary_stateDetails :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_stateDetails = Lens.lens (\JobRunSummary' {stateDetails} -> stateDetails) (\s@JobRunSummary' {} a -> s {stateDetails = a} :: JobRunSummary)

-- | The EMR release associated with the application your job is running on.
jobRunSummary_releaseLabel :: Lens.Lens' JobRunSummary Prelude.Text
jobRunSummary_releaseLabel = Lens.lens (\JobRunSummary' {releaseLabel} -> releaseLabel) (\s@JobRunSummary' {} a -> s {releaseLabel = a} :: JobRunSummary)

instance Data.FromJSON JobRunSummary where
  parseJSON =
    Data.withObject
      "JobRunSummary"
      ( \x ->
          JobRunSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdBy")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "executionRole")
            Prelude.<*> (x Data..: "state")
            Prelude.<*> (x Data..: "stateDetails")
            Prelude.<*> (x Data..: "releaseLabel")
      )

instance Prelude.Hashable JobRunSummary where
  hashWithSalt _salt JobRunSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateDetails
      `Prelude.hashWithSalt` releaseLabel

instance Prelude.NFData JobRunSummary where
  rnf JobRunSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateDetails
      `Prelude.seq` Prelude.rnf releaseLabel
