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
-- Module      : Amazonka.EMRContainers.Types.JobRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.JobRun where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.ConfigurationOverrides
import Amazonka.EMRContainers.Types.FailureReason
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.JobRunState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This entity describes a job run. A job run is a unit of work, such as a
-- Spark jar, PySpark script, or SparkSQL query, that you submit to Amazon
-- EMR on EKS.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | The reasons why the job run has failed.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The state of the job run.
    state :: Prelude.Maybe JobRunState,
    -- | The client token used to start a job run.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of job run.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the job run was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Additional details of the job run state.
    stateDetails :: Prelude.Maybe Prelude.Text,
    -- | The user who created the job run.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The execution role ARN of the job run.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Parameters of job driver for the job run.
    jobDriver :: Prelude.Maybe JobDriver,
    -- | The configuration settings that are used to override default
    -- configuration.
    configurationOverrides :: Prelude.Maybe ConfigurationOverrides,
    -- | The date and time when the job run has finished.
    finishedAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the job run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The release version of Amazon EMR.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job run.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job run\'s virtual cluster.
    virtualClusterId :: Prelude.Maybe Prelude.Text,
    -- | The assigned tags of the job run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'jobRun_failureReason' - The reasons why the job run has failed.
--
-- 'state', 'jobRun_state' - The state of the job run.
--
-- 'clientToken', 'jobRun_clientToken' - The client token used to start a job run.
--
-- 'arn', 'jobRun_arn' - The ARN of job run.
--
-- 'createdAt', 'jobRun_createdAt' - The date and time when the job run was created.
--
-- 'stateDetails', 'jobRun_stateDetails' - Additional details of the job run state.
--
-- 'createdBy', 'jobRun_createdBy' - The user who created the job run.
--
-- 'executionRoleArn', 'jobRun_executionRoleArn' - The execution role ARN of the job run.
--
-- 'jobDriver', 'jobRun_jobDriver' - Parameters of job driver for the job run.
--
-- 'configurationOverrides', 'jobRun_configurationOverrides' - The configuration settings that are used to override default
-- configuration.
--
-- 'finishedAt', 'jobRun_finishedAt' - The date and time when the job run has finished.
--
-- 'name', 'jobRun_name' - The name of the job run.
--
-- 'releaseLabel', 'jobRun_releaseLabel' - The release version of Amazon EMR.
--
-- 'id', 'jobRun_id' - The ID of the job run.
--
-- 'virtualClusterId', 'jobRun_virtualClusterId' - The ID of the job run\'s virtual cluster.
--
-- 'tags', 'jobRun_tags' - The assigned tags of the job run.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { failureReason = Prelude.Nothing,
      state = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      stateDetails = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      jobDriver = Prelude.Nothing,
      configurationOverrides = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      id = Prelude.Nothing,
      virtualClusterId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The reasons why the job run has failed.
jobRun_failureReason :: Lens.Lens' JobRun (Prelude.Maybe FailureReason)
jobRun_failureReason = Lens.lens (\JobRun' {failureReason} -> failureReason) (\s@JobRun' {} a -> s {failureReason = a} :: JobRun)

-- | The state of the job run.
jobRun_state :: Lens.Lens' JobRun (Prelude.Maybe JobRunState)
jobRun_state = Lens.lens (\JobRun' {state} -> state) (\s@JobRun' {} a -> s {state = a} :: JobRun)

-- | The client token used to start a job run.
jobRun_clientToken :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_clientToken = Lens.lens (\JobRun' {clientToken} -> clientToken) (\s@JobRun' {} a -> s {clientToken = a} :: JobRun)

-- | The ARN of job run.
jobRun_arn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_arn = Lens.lens (\JobRun' {arn} -> arn) (\s@JobRun' {} a -> s {arn = a} :: JobRun)

-- | The date and time when the job run was created.
jobRun_createdAt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_createdAt = Lens.lens (\JobRun' {createdAt} -> createdAt) (\s@JobRun' {} a -> s {createdAt = a} :: JobRun) Prelude.. Lens.mapping Core._Time

-- | Additional details of the job run state.
jobRun_stateDetails :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_stateDetails = Lens.lens (\JobRun' {stateDetails} -> stateDetails) (\s@JobRun' {} a -> s {stateDetails = a} :: JobRun)

-- | The user who created the job run.
jobRun_createdBy :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_createdBy = Lens.lens (\JobRun' {createdBy} -> createdBy) (\s@JobRun' {} a -> s {createdBy = a} :: JobRun)

-- | The execution role ARN of the job run.
jobRun_executionRoleArn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_executionRoleArn = Lens.lens (\JobRun' {executionRoleArn} -> executionRoleArn) (\s@JobRun' {} a -> s {executionRoleArn = a} :: JobRun)

-- | Parameters of job driver for the job run.
jobRun_jobDriver :: Lens.Lens' JobRun (Prelude.Maybe JobDriver)
jobRun_jobDriver = Lens.lens (\JobRun' {jobDriver} -> jobDriver) (\s@JobRun' {} a -> s {jobDriver = a} :: JobRun)

-- | The configuration settings that are used to override default
-- configuration.
jobRun_configurationOverrides :: Lens.Lens' JobRun (Prelude.Maybe ConfigurationOverrides)
jobRun_configurationOverrides = Lens.lens (\JobRun' {configurationOverrides} -> configurationOverrides) (\s@JobRun' {} a -> s {configurationOverrides = a} :: JobRun)

-- | The date and time when the job run has finished.
jobRun_finishedAt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_finishedAt = Lens.lens (\JobRun' {finishedAt} -> finishedAt) (\s@JobRun' {} a -> s {finishedAt = a} :: JobRun) Prelude.. Lens.mapping Core._Time

-- | The name of the job run.
jobRun_name :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_name = Lens.lens (\JobRun' {name} -> name) (\s@JobRun' {} a -> s {name = a} :: JobRun)

-- | The release version of Amazon EMR.
jobRun_releaseLabel :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_releaseLabel = Lens.lens (\JobRun' {releaseLabel} -> releaseLabel) (\s@JobRun' {} a -> s {releaseLabel = a} :: JobRun)

-- | The ID of the job run.
jobRun_id :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_id = Lens.lens (\JobRun' {id} -> id) (\s@JobRun' {} a -> s {id = a} :: JobRun)

-- | The ID of the job run\'s virtual cluster.
jobRun_virtualClusterId :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_virtualClusterId = Lens.lens (\JobRun' {virtualClusterId} -> virtualClusterId) (\s@JobRun' {} a -> s {virtualClusterId = a} :: JobRun)

-- | The assigned tags of the job run.
jobRun_tags :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobRun_tags = Lens.lens (\JobRun' {tags} -> tags) (\s@JobRun' {} a -> s {tags = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON JobRun where
  parseJSON =
    Core.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "clientToken")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "stateDetails")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "executionRoleArn")
            Prelude.<*> (x Core..:? "jobDriver")
            Prelude.<*> (x Core..:? "configurationOverrides")
            Prelude.<*> (x Core..:? "finishedAt")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "releaseLabel")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "virtualClusterId")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable JobRun

instance Prelude.NFData JobRun
