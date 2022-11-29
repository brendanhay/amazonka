{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataBrew.DescribeJobRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents one run of a DataBrew job.
module Amazonka.DataBrew.DescribeJobRun
  ( -- * Creating a Request
    DescribeJobRun (..),
    newDescribeJobRun,

    -- * Request Lenses
    describeJobRun_name,
    describeJobRun_runId,

    -- * Destructuring the Response
    DescribeJobRunResponse (..),
    newDescribeJobRunResponse,

    -- * Response Lenses
    describeJobRunResponse_jobSample,
    describeJobRunResponse_databaseOutputs,
    describeJobRunResponse_startedOn,
    describeJobRunResponse_errorMessage,
    describeJobRunResponse_attempt,
    describeJobRunResponse_dataCatalogOutputs,
    describeJobRunResponse_datasetName,
    describeJobRunResponse_state,
    describeJobRunResponse_executionTime,
    describeJobRunResponse_logSubscription,
    describeJobRunResponse_completedOn,
    describeJobRunResponse_startedBy,
    describeJobRunResponse_recipeReference,
    describeJobRunResponse_outputs,
    describeJobRunResponse_profileConfiguration,
    describeJobRunResponse_runId,
    describeJobRunResponse_logGroupName,
    describeJobRunResponse_validationConfigurations,
    describeJobRunResponse_httpStatus,
    describeJobRunResponse_jobName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobRun' smart constructor.
data DescribeJobRun = DescribeJobRun'
  { -- | The name of the job being processed during this run.
    name :: Prelude.Text,
    -- | The unique identifier of the job run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeJobRun_name' - The name of the job being processed during this run.
--
-- 'runId', 'describeJobRun_runId' - The unique identifier of the job run.
newDescribeJobRun ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  DescribeJobRun
newDescribeJobRun pName_ pRunId_ =
  DescribeJobRun' {name = pName_, runId = pRunId_}

-- | The name of the job being processed during this run.
describeJobRun_name :: Lens.Lens' DescribeJobRun Prelude.Text
describeJobRun_name = Lens.lens (\DescribeJobRun' {name} -> name) (\s@DescribeJobRun' {} a -> s {name = a} :: DescribeJobRun)

-- | The unique identifier of the job run.
describeJobRun_runId :: Lens.Lens' DescribeJobRun Prelude.Text
describeJobRun_runId = Lens.lens (\DescribeJobRun' {runId} -> runId) (\s@DescribeJobRun' {} a -> s {runId = a} :: DescribeJobRun)

instance Core.AWSRequest DescribeJobRun where
  type
    AWSResponse DescribeJobRun =
      DescribeJobRunResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobRunResponse'
            Prelude.<$> (x Core..?> "JobSample")
            Prelude.<*> (x Core..?> "DatabaseOutputs")
            Prelude.<*> (x Core..?> "StartedOn")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "Attempt")
            Prelude.<*> (x Core..?> "DataCatalogOutputs")
            Prelude.<*> (x Core..?> "DatasetName")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "ExecutionTime")
            Prelude.<*> (x Core..?> "LogSubscription")
            Prelude.<*> (x Core..?> "CompletedOn")
            Prelude.<*> (x Core..?> "StartedBy")
            Prelude.<*> (x Core..?> "RecipeReference")
            Prelude.<*> (x Core..?> "Outputs")
            Prelude.<*> (x Core..?> "ProfileConfiguration")
            Prelude.<*> (x Core..?> "RunId")
            Prelude.<*> (x Core..?> "LogGroupName")
            Prelude.<*> (x Core..?> "ValidationConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "JobName")
      )

instance Prelude.Hashable DescribeJobRun where
  hashWithSalt _salt DescribeJobRun' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData DescribeJobRun where
  rnf DescribeJobRun' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf runId

instance Core.ToHeaders DescribeJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeJobRun where
  toPath DescribeJobRun' {..} =
    Prelude.mconcat
      [ "/jobs/",
        Core.toBS name,
        "/jobRun/",
        Core.toBS runId
      ]

instance Core.ToQuery DescribeJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobRunResponse' smart constructor.
data DescribeJobRunResponse = DescribeJobRunResponse'
  { -- | Sample configuration for profile jobs only. Determines the number of
    -- rows on which the profile job will be executed. If a JobSample value is
    -- not provided, the default value will be used. The default value is
    -- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The date and time when the job run began.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | A message indicating an error (if any) that was encountered when the job
    -- ran.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The number of times that DataBrew has attempted to run the job.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | The name of the dataset for the job to process.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the job run entity itself.
    state :: Prelude.Maybe JobRunState,
    -- | The amount of time, in seconds, during which the job run consumed
    -- resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | The current status of Amazon CloudWatch logging for the job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The date and time when the job completed processing.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who started the job run.
    startedBy :: Prelude.Maybe Prelude.Text,
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | One or more output artifacts from a job run.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | Configuration for profile jobs. Used to select columns, do evaluations,
    -- and override default parameters of evaluations. When configuration is
    -- null, the profile job will run with default settings.
    profileConfiguration :: Prelude.Maybe ProfileConfiguration,
    -- | The unique identifier of the job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of an Amazon CloudWatch log group, where the job writes
    -- diagnostic messages when it runs.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | List of validation configurations that are applied to the profile job.
    validationConfigurations :: Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job being processed during this run.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobSample', 'describeJobRunResponse_jobSample' - Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
--
-- 'databaseOutputs', 'describeJobRunResponse_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'startedOn', 'describeJobRunResponse_startedOn' - The date and time when the job run began.
--
-- 'errorMessage', 'describeJobRunResponse_errorMessage' - A message indicating an error (if any) that was encountered when the job
-- ran.
--
-- 'attempt', 'describeJobRunResponse_attempt' - The number of times that DataBrew has attempted to run the job.
--
-- 'dataCatalogOutputs', 'describeJobRunResponse_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'datasetName', 'describeJobRunResponse_datasetName' - The name of the dataset for the job to process.
--
-- 'state', 'describeJobRunResponse_state' - The current state of the job run entity itself.
--
-- 'executionTime', 'describeJobRunResponse_executionTime' - The amount of time, in seconds, during which the job run consumed
-- resources.
--
-- 'logSubscription', 'describeJobRunResponse_logSubscription' - The current status of Amazon CloudWatch logging for the job run.
--
-- 'completedOn', 'describeJobRunResponse_completedOn' - The date and time when the job completed processing.
--
-- 'startedBy', 'describeJobRunResponse_startedBy' - The Amazon Resource Name (ARN) of the user who started the job run.
--
-- 'recipeReference', 'describeJobRunResponse_recipeReference' - Undocumented member.
--
-- 'outputs', 'describeJobRunResponse_outputs' - One or more output artifacts from a job run.
--
-- 'profileConfiguration', 'describeJobRunResponse_profileConfiguration' - Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
--
-- 'runId', 'describeJobRunResponse_runId' - The unique identifier of the job run.
--
-- 'logGroupName', 'describeJobRunResponse_logGroupName' - The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
--
-- 'validationConfigurations', 'describeJobRunResponse_validationConfigurations' - List of validation configurations that are applied to the profile job.
--
-- 'httpStatus', 'describeJobRunResponse_httpStatus' - The response's http status code.
--
-- 'jobName', 'describeJobRunResponse_jobName' - The name of the job being processed during this run.
newDescribeJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobName'
  Prelude.Text ->
  DescribeJobRunResponse
newDescribeJobRunResponse pHttpStatus_ pJobName_ =
  DescribeJobRunResponse'
    { jobSample =
        Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      attempt = Prelude.Nothing,
      dataCatalogOutputs = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      state = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      outputs = Prelude.Nothing,
      profileConfiguration = Prelude.Nothing,
      runId = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      validationConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobName = pJobName_
    }

-- | Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
describeJobRunResponse_jobSample :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe JobSample)
describeJobRunResponse_jobSample = Lens.lens (\DescribeJobRunResponse' {jobSample} -> jobSample) (\s@DescribeJobRunResponse' {} a -> s {jobSample = a} :: DescribeJobRunResponse)

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
describeJobRunResponse_databaseOutputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
describeJobRunResponse_databaseOutputs = Lens.lens (\DescribeJobRunResponse' {databaseOutputs} -> databaseOutputs) (\s@DescribeJobRunResponse' {} a -> s {databaseOutputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the job run began.
describeJobRunResponse_startedOn :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.UTCTime)
describeJobRunResponse_startedOn = Lens.lens (\DescribeJobRunResponse' {startedOn} -> startedOn) (\s@DescribeJobRunResponse' {} a -> s {startedOn = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Core._Time

-- | A message indicating an error (if any) that was encountered when the job
-- ran.
describeJobRunResponse_errorMessage :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_errorMessage = Lens.lens (\DescribeJobRunResponse' {errorMessage} -> errorMessage) (\s@DescribeJobRunResponse' {} a -> s {errorMessage = a} :: DescribeJobRunResponse)

-- | The number of times that DataBrew has attempted to run the job.
describeJobRunResponse_attempt :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Int)
describeJobRunResponse_attempt = Lens.lens (\DescribeJobRunResponse' {attempt} -> attempt) (\s@DescribeJobRunResponse' {} a -> s {attempt = a} :: DescribeJobRunResponse)

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
describeJobRunResponse_dataCatalogOutputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
describeJobRunResponse_dataCatalogOutputs = Lens.lens (\DescribeJobRunResponse' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@DescribeJobRunResponse' {} a -> s {dataCatalogOutputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset for the job to process.
describeJobRunResponse_datasetName :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_datasetName = Lens.lens (\DescribeJobRunResponse' {datasetName} -> datasetName) (\s@DescribeJobRunResponse' {} a -> s {datasetName = a} :: DescribeJobRunResponse)

-- | The current state of the job run entity itself.
describeJobRunResponse_state :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe JobRunState)
describeJobRunResponse_state = Lens.lens (\DescribeJobRunResponse' {state} -> state) (\s@DescribeJobRunResponse' {} a -> s {state = a} :: DescribeJobRunResponse)

-- | The amount of time, in seconds, during which the job run consumed
-- resources.
describeJobRunResponse_executionTime :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Int)
describeJobRunResponse_executionTime = Lens.lens (\DescribeJobRunResponse' {executionTime} -> executionTime) (\s@DescribeJobRunResponse' {} a -> s {executionTime = a} :: DescribeJobRunResponse)

-- | The current status of Amazon CloudWatch logging for the job run.
describeJobRunResponse_logSubscription :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe LogSubscription)
describeJobRunResponse_logSubscription = Lens.lens (\DescribeJobRunResponse' {logSubscription} -> logSubscription) (\s@DescribeJobRunResponse' {} a -> s {logSubscription = a} :: DescribeJobRunResponse)

-- | The date and time when the job completed processing.
describeJobRunResponse_completedOn :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.UTCTime)
describeJobRunResponse_completedOn = Lens.lens (\DescribeJobRunResponse' {completedOn} -> completedOn) (\s@DescribeJobRunResponse' {} a -> s {completedOn = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the user who started the job run.
describeJobRunResponse_startedBy :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_startedBy = Lens.lens (\DescribeJobRunResponse' {startedBy} -> startedBy) (\s@DescribeJobRunResponse' {} a -> s {startedBy = a} :: DescribeJobRunResponse)

-- | Undocumented member.
describeJobRunResponse_recipeReference :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe RecipeReference)
describeJobRunResponse_recipeReference = Lens.lens (\DescribeJobRunResponse' {recipeReference} -> recipeReference) (\s@DescribeJobRunResponse' {} a -> s {recipeReference = a} :: DescribeJobRunResponse)

-- | One or more output artifacts from a job run.
describeJobRunResponse_outputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty Output))
describeJobRunResponse_outputs = Lens.lens (\DescribeJobRunResponse' {outputs} -> outputs) (\s@DescribeJobRunResponse' {} a -> s {outputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
describeJobRunResponse_profileConfiguration :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe ProfileConfiguration)
describeJobRunResponse_profileConfiguration = Lens.lens (\DescribeJobRunResponse' {profileConfiguration} -> profileConfiguration) (\s@DescribeJobRunResponse' {} a -> s {profileConfiguration = a} :: DescribeJobRunResponse)

-- | The unique identifier of the job run.
describeJobRunResponse_runId :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_runId = Lens.lens (\DescribeJobRunResponse' {runId} -> runId) (\s@DescribeJobRunResponse' {} a -> s {runId = a} :: DescribeJobRunResponse)

-- | The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
describeJobRunResponse_logGroupName :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_logGroupName = Lens.lens (\DescribeJobRunResponse' {logGroupName} -> logGroupName) (\s@DescribeJobRunResponse' {} a -> s {logGroupName = a} :: DescribeJobRunResponse)

-- | List of validation configurations that are applied to the profile job.
describeJobRunResponse_validationConfigurations :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration))
describeJobRunResponse_validationConfigurations = Lens.lens (\DescribeJobRunResponse' {validationConfigurations} -> validationConfigurations) (\s@DescribeJobRunResponse' {} a -> s {validationConfigurations = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeJobRunResponse_httpStatus :: Lens.Lens' DescribeJobRunResponse Prelude.Int
describeJobRunResponse_httpStatus = Lens.lens (\DescribeJobRunResponse' {httpStatus} -> httpStatus) (\s@DescribeJobRunResponse' {} a -> s {httpStatus = a} :: DescribeJobRunResponse)

-- | The name of the job being processed during this run.
describeJobRunResponse_jobName :: Lens.Lens' DescribeJobRunResponse Prelude.Text
describeJobRunResponse_jobName = Lens.lens (\DescribeJobRunResponse' {jobName} -> jobName) (\s@DescribeJobRunResponse' {} a -> s {jobName = a} :: DescribeJobRunResponse)

instance Prelude.NFData DescribeJobRunResponse where
  rnf DescribeJobRunResponse' {..} =
    Prelude.rnf jobSample
      `Prelude.seq` Prelude.rnf databaseOutputs
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf attempt
      `Prelude.seq` Prelude.rnf dataCatalogOutputs
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf recipeReference
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf profileConfiguration
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf
        validationConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobName
