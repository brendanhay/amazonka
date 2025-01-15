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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.JobRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.ConfigurationOverrides
import Amazonka.EMRContainers.Types.FailureReason
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.JobRunState
import qualified Amazonka.Prelude as Prelude

-- | This entity describes a job run. A job run is a unit of work, such as a
-- Spark jar, PySpark script, or SparkSQL query, that you submit to Amazon
-- EMR on EKS.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | The ARN of job run.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The client token used to start a job run.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings that are used to override default
    -- configuration.
    configurationOverrides :: Prelude.Maybe ConfigurationOverrides,
    -- | The date and time when the job run was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The user who created the job run.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The execution role ARN of the job run.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The reasons why the job run has failed.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The date and time when the job run has finished.
    finishedAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the job run.
    id :: Prelude.Maybe Prelude.Text,
    -- | Parameters of job driver for the job run.
    jobDriver :: Prelude.Maybe JobDriver,
    -- | The name of the job run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The release version of Amazon EMR.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The state of the job run.
    state :: Prelude.Maybe JobRunState,
    -- | Additional details of the job run state.
    stateDetails :: Prelude.Maybe Prelude.Text,
    -- | The assigned tags of the job run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the job run\'s virtual cluster.
    virtualClusterId :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'jobRun_arn' - The ARN of job run.
--
-- 'clientToken', 'jobRun_clientToken' - The client token used to start a job run.
--
-- 'configurationOverrides', 'jobRun_configurationOverrides' - The configuration settings that are used to override default
-- configuration.
--
-- 'createdAt', 'jobRun_createdAt' - The date and time when the job run was created.
--
-- 'createdBy', 'jobRun_createdBy' - The user who created the job run.
--
-- 'executionRoleArn', 'jobRun_executionRoleArn' - The execution role ARN of the job run.
--
-- 'failureReason', 'jobRun_failureReason' - The reasons why the job run has failed.
--
-- 'finishedAt', 'jobRun_finishedAt' - The date and time when the job run has finished.
--
-- 'id', 'jobRun_id' - The ID of the job run.
--
-- 'jobDriver', 'jobRun_jobDriver' - Parameters of job driver for the job run.
--
-- 'name', 'jobRun_name' - The name of the job run.
--
-- 'releaseLabel', 'jobRun_releaseLabel' - The release version of Amazon EMR.
--
-- 'state', 'jobRun_state' - The state of the job run.
--
-- 'stateDetails', 'jobRun_stateDetails' - Additional details of the job run state.
--
-- 'tags', 'jobRun_tags' - The assigned tags of the job run.
--
-- 'virtualClusterId', 'jobRun_virtualClusterId' - The ID of the job run\'s virtual cluster.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { arn = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      configurationOverrides = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      id = Prelude.Nothing,
      jobDriver = Prelude.Nothing,
      name = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      state = Prelude.Nothing,
      stateDetails = Prelude.Nothing,
      tags = Prelude.Nothing,
      virtualClusterId = Prelude.Nothing
    }

-- | The ARN of job run.
jobRun_arn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_arn = Lens.lens (\JobRun' {arn} -> arn) (\s@JobRun' {} a -> s {arn = a} :: JobRun)

-- | The client token used to start a job run.
jobRun_clientToken :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_clientToken = Lens.lens (\JobRun' {clientToken} -> clientToken) (\s@JobRun' {} a -> s {clientToken = a} :: JobRun)

-- | The configuration settings that are used to override default
-- configuration.
jobRun_configurationOverrides :: Lens.Lens' JobRun (Prelude.Maybe ConfigurationOverrides)
jobRun_configurationOverrides = Lens.lens (\JobRun' {configurationOverrides} -> configurationOverrides) (\s@JobRun' {} a -> s {configurationOverrides = a} :: JobRun)

-- | The date and time when the job run was created.
jobRun_createdAt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_createdAt = Lens.lens (\JobRun' {createdAt} -> createdAt) (\s@JobRun' {} a -> s {createdAt = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | The user who created the job run.
jobRun_createdBy :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_createdBy = Lens.lens (\JobRun' {createdBy} -> createdBy) (\s@JobRun' {} a -> s {createdBy = a} :: JobRun)

-- | The execution role ARN of the job run.
jobRun_executionRoleArn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_executionRoleArn = Lens.lens (\JobRun' {executionRoleArn} -> executionRoleArn) (\s@JobRun' {} a -> s {executionRoleArn = a} :: JobRun)

-- | The reasons why the job run has failed.
jobRun_failureReason :: Lens.Lens' JobRun (Prelude.Maybe FailureReason)
jobRun_failureReason = Lens.lens (\JobRun' {failureReason} -> failureReason) (\s@JobRun' {} a -> s {failureReason = a} :: JobRun)

-- | The date and time when the job run has finished.
jobRun_finishedAt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_finishedAt = Lens.lens (\JobRun' {finishedAt} -> finishedAt) (\s@JobRun' {} a -> s {finishedAt = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | The ID of the job run.
jobRun_id :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_id = Lens.lens (\JobRun' {id} -> id) (\s@JobRun' {} a -> s {id = a} :: JobRun)

-- | Parameters of job driver for the job run.
jobRun_jobDriver :: Lens.Lens' JobRun (Prelude.Maybe JobDriver)
jobRun_jobDriver = Lens.lens (\JobRun' {jobDriver} -> jobDriver) (\s@JobRun' {} a -> s {jobDriver = a} :: JobRun)

-- | The name of the job run.
jobRun_name :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_name = Lens.lens (\JobRun' {name} -> name) (\s@JobRun' {} a -> s {name = a} :: JobRun)

-- | The release version of Amazon EMR.
jobRun_releaseLabel :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_releaseLabel = Lens.lens (\JobRun' {releaseLabel} -> releaseLabel) (\s@JobRun' {} a -> s {releaseLabel = a} :: JobRun)

-- | The state of the job run.
jobRun_state :: Lens.Lens' JobRun (Prelude.Maybe JobRunState)
jobRun_state = Lens.lens (\JobRun' {state} -> state) (\s@JobRun' {} a -> s {state = a} :: JobRun)

-- | Additional details of the job run state.
jobRun_stateDetails :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_stateDetails = Lens.lens (\JobRun' {stateDetails} -> stateDetails) (\s@JobRun' {} a -> s {stateDetails = a} :: JobRun)

-- | The assigned tags of the job run.
jobRun_tags :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobRun_tags = Lens.lens (\JobRun' {tags} -> tags) (\s@JobRun' {} a -> s {tags = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the job run\'s virtual cluster.
jobRun_virtualClusterId :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_virtualClusterId = Lens.lens (\JobRun' {virtualClusterId} -> virtualClusterId) (\s@JobRun' {} a -> s {virtualClusterId = a} :: JobRun)

instance Data.FromJSON JobRun where
  parseJSON =
    Data.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "clientToken")
            Prelude.<*> (x Data..:? "configurationOverrides")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "executionRoleArn")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "finishedAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "jobDriver")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "releaseLabel")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "stateDetails")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "virtualClusterId")
      )

instance Prelude.Hashable JobRun where
  hashWithSalt _salt JobRun' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` configurationOverrides
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` finishedAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` jobDriver
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateDetails
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData JobRun where
  rnf JobRun' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf configurationOverrides `Prelude.seq`
          Prelude.rnf createdAt `Prelude.seq`
            Prelude.rnf createdBy `Prelude.seq`
              Prelude.rnf executionRoleArn `Prelude.seq`
                Prelude.rnf failureReason `Prelude.seq`
                  Prelude.rnf finishedAt `Prelude.seq`
                    Prelude.rnf id `Prelude.seq`
                      Prelude.rnf jobDriver `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf releaseLabel `Prelude.seq`
                            Prelude.rnf state `Prelude.seq`
                              Prelude.rnf stateDetails `Prelude.seq`
                                Prelude.rnf tags `Prelude.seq`
                                  Prelude.rnf virtualClusterId
