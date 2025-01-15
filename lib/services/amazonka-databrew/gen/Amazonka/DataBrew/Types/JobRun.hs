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
-- Module      : Amazonka.DataBrew.Types.JobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.JobRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.DataCatalogOutput
import Amazonka.DataBrew.Types.DatabaseOutput
import Amazonka.DataBrew.Types.JobRunState
import Amazonka.DataBrew.Types.JobSample
import Amazonka.DataBrew.Types.LogSubscription
import Amazonka.DataBrew.Types.Output
import Amazonka.DataBrew.Types.RecipeReference
import Amazonka.DataBrew.Types.ValidationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Represents one run of a DataBrew job.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | The number of times that DataBrew has attempted to run the job.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the job completed processing.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The name of the dataset for the job to process.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | A message indicating an error (if any) that was encountered when the job
    -- ran.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, during which a job run consumed
    -- resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | The name of the job being processed during this run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | A sample configuration for profile jobs only, which determines the
    -- number of rows on which the profile job is run. If a @JobSample@ value
    -- isn\'t provided, the default is used. The default value is CUSTOM_ROWS
    -- for the mode parameter and 20,000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | The name of an Amazon CloudWatch log group, where the job writes
    -- diagnostic messages when it runs.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The current status of Amazon CloudWatch logging for the job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | One or more output artifacts from a job run.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | The set of steps processed by the job.
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | The unique identifier of the job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who initiated the job run.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the job run began.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The current state of the job run entity itself.
    state :: Prelude.Maybe JobRunState,
    -- | List of validation configurations that are applied to the profile job
    -- run.
    validationConfigurations :: Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attempt', 'jobRun_attempt' - The number of times that DataBrew has attempted to run the job.
--
-- 'completedOn', 'jobRun_completedOn' - The date and time when the job completed processing.
--
-- 'dataCatalogOutputs', 'jobRun_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'databaseOutputs', 'jobRun_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'datasetName', 'jobRun_datasetName' - The name of the dataset for the job to process.
--
-- 'errorMessage', 'jobRun_errorMessage' - A message indicating an error (if any) that was encountered when the job
-- ran.
--
-- 'executionTime', 'jobRun_executionTime' - The amount of time, in seconds, during which a job run consumed
-- resources.
--
-- 'jobName', 'jobRun_jobName' - The name of the job being processed during this run.
--
-- 'jobSample', 'jobRun_jobSample' - A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default is used. The default value is CUSTOM_ROWS
-- for the mode parameter and 20,000 for the size parameter.
--
-- 'logGroupName', 'jobRun_logGroupName' - The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
--
-- 'logSubscription', 'jobRun_logSubscription' - The current status of Amazon CloudWatch logging for the job run.
--
-- 'outputs', 'jobRun_outputs' - One or more output artifacts from a job run.
--
-- 'recipeReference', 'jobRun_recipeReference' - The set of steps processed by the job.
--
-- 'runId', 'jobRun_runId' - The unique identifier of the job run.
--
-- 'startedBy', 'jobRun_startedBy' - The Amazon Resource Name (ARN) of the user who initiated the job run.
--
-- 'startedOn', 'jobRun_startedOn' - The date and time when the job run began.
--
-- 'state', 'jobRun_state' - The current state of the job run entity itself.
--
-- 'validationConfigurations', 'jobRun_validationConfigurations' - List of validation configurations that are applied to the profile job
-- run.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { attempt = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      dataCatalogOutputs = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobSample = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      outputs = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      runId = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      state = Prelude.Nothing,
      validationConfigurations = Prelude.Nothing
    }

-- | The number of times that DataBrew has attempted to run the job.
jobRun_attempt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_attempt = Lens.lens (\JobRun' {attempt} -> attempt) (\s@JobRun' {} a -> s {attempt = a} :: JobRun)

-- | The date and time when the job completed processing.
jobRun_completedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_completedOn = Lens.lens (\JobRun' {completedOn} -> completedOn) (\s@JobRun' {} a -> s {completedOn = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
jobRun_dataCatalogOutputs :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
jobRun_dataCatalogOutputs = Lens.lens (\JobRun' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@JobRun' {} a -> s {dataCatalogOutputs = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
jobRun_databaseOutputs :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
jobRun_databaseOutputs = Lens.lens (\JobRun' {databaseOutputs} -> databaseOutputs) (\s@JobRun' {} a -> s {databaseOutputs = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset for the job to process.
jobRun_datasetName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_datasetName = Lens.lens (\JobRun' {datasetName} -> datasetName) (\s@JobRun' {} a -> s {datasetName = a} :: JobRun)

-- | A message indicating an error (if any) that was encountered when the job
-- ran.
jobRun_errorMessage :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_errorMessage = Lens.lens (\JobRun' {errorMessage} -> errorMessage) (\s@JobRun' {} a -> s {errorMessage = a} :: JobRun)

-- | The amount of time, in seconds, during which a job run consumed
-- resources.
jobRun_executionTime :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_executionTime = Lens.lens (\JobRun' {executionTime} -> executionTime) (\s@JobRun' {} a -> s {executionTime = a} :: JobRun)

-- | The name of the job being processed during this run.
jobRun_jobName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_jobName = Lens.lens (\JobRun' {jobName} -> jobName) (\s@JobRun' {} a -> s {jobName = a} :: JobRun)

-- | A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default is used. The default value is CUSTOM_ROWS
-- for the mode parameter and 20,000 for the size parameter.
jobRun_jobSample :: Lens.Lens' JobRun (Prelude.Maybe JobSample)
jobRun_jobSample = Lens.lens (\JobRun' {jobSample} -> jobSample) (\s@JobRun' {} a -> s {jobSample = a} :: JobRun)

-- | The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
jobRun_logGroupName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_logGroupName = Lens.lens (\JobRun' {logGroupName} -> logGroupName) (\s@JobRun' {} a -> s {logGroupName = a} :: JobRun)

-- | The current status of Amazon CloudWatch logging for the job run.
jobRun_logSubscription :: Lens.Lens' JobRun (Prelude.Maybe LogSubscription)
jobRun_logSubscription = Lens.lens (\JobRun' {logSubscription} -> logSubscription) (\s@JobRun' {} a -> s {logSubscription = a} :: JobRun)

-- | One or more output artifacts from a job run.
jobRun_outputs :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.NonEmpty Output))
jobRun_outputs = Lens.lens (\JobRun' {outputs} -> outputs) (\s@JobRun' {} a -> s {outputs = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The set of steps processed by the job.
jobRun_recipeReference :: Lens.Lens' JobRun (Prelude.Maybe RecipeReference)
jobRun_recipeReference = Lens.lens (\JobRun' {recipeReference} -> recipeReference) (\s@JobRun' {} a -> s {recipeReference = a} :: JobRun)

-- | The unique identifier of the job run.
jobRun_runId :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_runId = Lens.lens (\JobRun' {runId} -> runId) (\s@JobRun' {} a -> s {runId = a} :: JobRun)

-- | The Amazon Resource Name (ARN) of the user who initiated the job run.
jobRun_startedBy :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_startedBy = Lens.lens (\JobRun' {startedBy} -> startedBy) (\s@JobRun' {} a -> s {startedBy = a} :: JobRun)

-- | The date and time when the job run began.
jobRun_startedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_startedOn = Lens.lens (\JobRun' {startedOn} -> startedOn) (\s@JobRun' {} a -> s {startedOn = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | The current state of the job run entity itself.
jobRun_state :: Lens.Lens' JobRun (Prelude.Maybe JobRunState)
jobRun_state = Lens.lens (\JobRun' {state} -> state) (\s@JobRun' {} a -> s {state = a} :: JobRun)

-- | List of validation configurations that are applied to the profile job
-- run.
jobRun_validationConfigurations :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration))
jobRun_validationConfigurations = Lens.lens (\JobRun' {validationConfigurations} -> validationConfigurations) (\s@JobRun' {} a -> s {validationConfigurations = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobRun where
  parseJSON =
    Data.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Data..:? "Attempt")
            Prelude.<*> (x Data..:? "CompletedOn")
            Prelude.<*> (x Data..:? "DataCatalogOutputs")
            Prelude.<*> (x Data..:? "DatabaseOutputs")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ExecutionTime")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobSample")
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "LogSubscription")
            Prelude.<*> (x Data..:? "Outputs")
            Prelude.<*> (x Data..:? "RecipeReference")
            Prelude.<*> (x Data..:? "RunId")
            Prelude.<*> (x Data..:? "StartedBy")
            Prelude.<*> (x Data..:? "StartedOn")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "ValidationConfigurations")
      )

instance Prelude.Hashable JobRun where
  hashWithSalt _salt JobRun' {..} =
    _salt
      `Prelude.hashWithSalt` attempt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` dataCatalogOutputs
      `Prelude.hashWithSalt` databaseOutputs
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` executionTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobSample
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logSubscription
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` recipeReference
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` validationConfigurations

instance Prelude.NFData JobRun where
  rnf JobRun' {..} =
    Prelude.rnf attempt `Prelude.seq`
      Prelude.rnf completedOn `Prelude.seq`
        Prelude.rnf dataCatalogOutputs `Prelude.seq`
          Prelude.rnf databaseOutputs `Prelude.seq`
            Prelude.rnf datasetName `Prelude.seq`
              Prelude.rnf errorMessage `Prelude.seq`
                Prelude.rnf executionTime `Prelude.seq`
                  Prelude.rnf jobName `Prelude.seq`
                    Prelude.rnf jobSample `Prelude.seq`
                      Prelude.rnf logGroupName `Prelude.seq`
                        Prelude.rnf logSubscription `Prelude.seq`
                          Prelude.rnf outputs `Prelude.seq`
                            Prelude.rnf recipeReference `Prelude.seq`
                              Prelude.rnf runId `Prelude.seq`
                                Prelude.rnf startedBy `Prelude.seq`
                                  Prelude.rnf startedOn `Prelude.seq`
                                    Prelude.rnf state `Prelude.seq`
                                      Prelude.rnf
                                        validationConfigurations
