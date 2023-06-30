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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeJobRunResponse_attempt,
    describeJobRunResponse_completedOn,
    describeJobRunResponse_dataCatalogOutputs,
    describeJobRunResponse_databaseOutputs,
    describeJobRunResponse_datasetName,
    describeJobRunResponse_errorMessage,
    describeJobRunResponse_executionTime,
    describeJobRunResponse_jobSample,
    describeJobRunResponse_logGroupName,
    describeJobRunResponse_logSubscription,
    describeJobRunResponse_outputs,
    describeJobRunResponse_profileConfiguration,
    describeJobRunResponse_recipeReference,
    describeJobRunResponse_runId,
    describeJobRunResponse_startedBy,
    describeJobRunResponse_startedOn,
    describeJobRunResponse_state,
    describeJobRunResponse_validationConfigurations,
    describeJobRunResponse_httpStatus,
    describeJobRunResponse_jobName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "Attempt")
            Prelude.<*> (x Data..?> "CompletedOn")
            Prelude.<*> (x Data..?> "DataCatalogOutputs")
            Prelude.<*> (x Data..?> "DatabaseOutputs")
            Prelude.<*> (x Data..?> "DatasetName")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "ExecutionTime")
            Prelude.<*> (x Data..?> "JobSample")
            Prelude.<*> (x Data..?> "LogGroupName")
            Prelude.<*> (x Data..?> "LogSubscription")
            Prelude.<*> (x Data..?> "Outputs")
            Prelude.<*> (x Data..?> "ProfileConfiguration")
            Prelude.<*> (x Data..?> "RecipeReference")
            Prelude.<*> (x Data..?> "RunId")
            Prelude.<*> (x Data..?> "StartedBy")
            Prelude.<*> (x Data..?> "StartedOn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "ValidationConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobName")
      )

instance Prelude.Hashable DescribeJobRun where
  hashWithSalt _salt DescribeJobRun' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData DescribeJobRun where
  rnf DescribeJobRun' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders DescribeJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeJobRun where
  toPath DescribeJobRun' {..} =
    Prelude.mconcat
      [ "/jobs/",
        Data.toBS name,
        "/jobRun/",
        Data.toBS runId
      ]

instance Data.ToQuery DescribeJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobRunResponse' smart constructor.
data DescribeJobRunResponse = DescribeJobRunResponse'
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
    -- | The amount of time, in seconds, during which the job run consumed
    -- resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | Sample configuration for profile jobs only. Determines the number of
    -- rows on which the profile job will be executed. If a JobSample value is
    -- not provided, the default value will be used. The default value is
    -- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | The name of an Amazon CloudWatch log group, where the job writes
    -- diagnostic messages when it runs.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The current status of Amazon CloudWatch logging for the job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | One or more output artifacts from a job run.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | Configuration for profile jobs. Used to select columns, do evaluations,
    -- and override default parameters of evaluations. When configuration is
    -- null, the profile job will run with default settings.
    profileConfiguration :: Prelude.Maybe ProfileConfiguration,
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | The unique identifier of the job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who started the job run.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the job run began.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The current state of the job run entity itself.
    state :: Prelude.Maybe JobRunState,
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
-- 'attempt', 'describeJobRunResponse_attempt' - The number of times that DataBrew has attempted to run the job.
--
-- 'completedOn', 'describeJobRunResponse_completedOn' - The date and time when the job completed processing.
--
-- 'dataCatalogOutputs', 'describeJobRunResponse_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'databaseOutputs', 'describeJobRunResponse_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'datasetName', 'describeJobRunResponse_datasetName' - The name of the dataset for the job to process.
--
-- 'errorMessage', 'describeJobRunResponse_errorMessage' - A message indicating an error (if any) that was encountered when the job
-- ran.
--
-- 'executionTime', 'describeJobRunResponse_executionTime' - The amount of time, in seconds, during which the job run consumed
-- resources.
--
-- 'jobSample', 'describeJobRunResponse_jobSample' - Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
--
-- 'logGroupName', 'describeJobRunResponse_logGroupName' - The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
--
-- 'logSubscription', 'describeJobRunResponse_logSubscription' - The current status of Amazon CloudWatch logging for the job run.
--
-- 'outputs', 'describeJobRunResponse_outputs' - One or more output artifacts from a job run.
--
-- 'profileConfiguration', 'describeJobRunResponse_profileConfiguration' - Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
--
-- 'recipeReference', 'describeJobRunResponse_recipeReference' - Undocumented member.
--
-- 'runId', 'describeJobRunResponse_runId' - The unique identifier of the job run.
--
-- 'startedBy', 'describeJobRunResponse_startedBy' - The Amazon Resource Name (ARN) of the user who started the job run.
--
-- 'startedOn', 'describeJobRunResponse_startedOn' - The date and time when the job run began.
--
-- 'state', 'describeJobRunResponse_state' - The current state of the job run entity itself.
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
    { attempt = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      dataCatalogOutputs = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      jobSample = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      outputs = Prelude.Nothing,
      profileConfiguration = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      runId = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      state = Prelude.Nothing,
      validationConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobName = pJobName_
    }

-- | The number of times that DataBrew has attempted to run the job.
describeJobRunResponse_attempt :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Int)
describeJobRunResponse_attempt = Lens.lens (\DescribeJobRunResponse' {attempt} -> attempt) (\s@DescribeJobRunResponse' {} a -> s {attempt = a} :: DescribeJobRunResponse)

-- | The date and time when the job completed processing.
describeJobRunResponse_completedOn :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.UTCTime)
describeJobRunResponse_completedOn = Lens.lens (\DescribeJobRunResponse' {completedOn} -> completedOn) (\s@DescribeJobRunResponse' {} a -> s {completedOn = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Data._Time

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
describeJobRunResponse_dataCatalogOutputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
describeJobRunResponse_dataCatalogOutputs = Lens.lens (\DescribeJobRunResponse' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@DescribeJobRunResponse' {} a -> s {dataCatalogOutputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
describeJobRunResponse_databaseOutputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
describeJobRunResponse_databaseOutputs = Lens.lens (\DescribeJobRunResponse' {databaseOutputs} -> databaseOutputs) (\s@DescribeJobRunResponse' {} a -> s {databaseOutputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset for the job to process.
describeJobRunResponse_datasetName :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_datasetName = Lens.lens (\DescribeJobRunResponse' {datasetName} -> datasetName) (\s@DescribeJobRunResponse' {} a -> s {datasetName = a} :: DescribeJobRunResponse)

-- | A message indicating an error (if any) that was encountered when the job
-- ran.
describeJobRunResponse_errorMessage :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_errorMessage = Lens.lens (\DescribeJobRunResponse' {errorMessage} -> errorMessage) (\s@DescribeJobRunResponse' {} a -> s {errorMessage = a} :: DescribeJobRunResponse)

-- | The amount of time, in seconds, during which the job run consumed
-- resources.
describeJobRunResponse_executionTime :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Int)
describeJobRunResponse_executionTime = Lens.lens (\DescribeJobRunResponse' {executionTime} -> executionTime) (\s@DescribeJobRunResponse' {} a -> s {executionTime = a} :: DescribeJobRunResponse)

-- | Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
describeJobRunResponse_jobSample :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe JobSample)
describeJobRunResponse_jobSample = Lens.lens (\DescribeJobRunResponse' {jobSample} -> jobSample) (\s@DescribeJobRunResponse' {} a -> s {jobSample = a} :: DescribeJobRunResponse)

-- | The name of an Amazon CloudWatch log group, where the job writes
-- diagnostic messages when it runs.
describeJobRunResponse_logGroupName :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_logGroupName = Lens.lens (\DescribeJobRunResponse' {logGroupName} -> logGroupName) (\s@DescribeJobRunResponse' {} a -> s {logGroupName = a} :: DescribeJobRunResponse)

-- | The current status of Amazon CloudWatch logging for the job run.
describeJobRunResponse_logSubscription :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe LogSubscription)
describeJobRunResponse_logSubscription = Lens.lens (\DescribeJobRunResponse' {logSubscription} -> logSubscription) (\s@DescribeJobRunResponse' {} a -> s {logSubscription = a} :: DescribeJobRunResponse)

-- | One or more output artifacts from a job run.
describeJobRunResponse_outputs :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe (Prelude.NonEmpty Output))
describeJobRunResponse_outputs = Lens.lens (\DescribeJobRunResponse' {outputs} -> outputs) (\s@DescribeJobRunResponse' {} a -> s {outputs = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
describeJobRunResponse_profileConfiguration :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe ProfileConfiguration)
describeJobRunResponse_profileConfiguration = Lens.lens (\DescribeJobRunResponse' {profileConfiguration} -> profileConfiguration) (\s@DescribeJobRunResponse' {} a -> s {profileConfiguration = a} :: DescribeJobRunResponse)

-- | Undocumented member.
describeJobRunResponse_recipeReference :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe RecipeReference)
describeJobRunResponse_recipeReference = Lens.lens (\DescribeJobRunResponse' {recipeReference} -> recipeReference) (\s@DescribeJobRunResponse' {} a -> s {recipeReference = a} :: DescribeJobRunResponse)

-- | The unique identifier of the job run.
describeJobRunResponse_runId :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_runId = Lens.lens (\DescribeJobRunResponse' {runId} -> runId) (\s@DescribeJobRunResponse' {} a -> s {runId = a} :: DescribeJobRunResponse)

-- | The Amazon Resource Name (ARN) of the user who started the job run.
describeJobRunResponse_startedBy :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.Text)
describeJobRunResponse_startedBy = Lens.lens (\DescribeJobRunResponse' {startedBy} -> startedBy) (\s@DescribeJobRunResponse' {} a -> s {startedBy = a} :: DescribeJobRunResponse)

-- | The date and time when the job run began.
describeJobRunResponse_startedOn :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe Prelude.UTCTime)
describeJobRunResponse_startedOn = Lens.lens (\DescribeJobRunResponse' {startedOn} -> startedOn) (\s@DescribeJobRunResponse' {} a -> s {startedOn = a} :: DescribeJobRunResponse) Prelude.. Lens.mapping Data._Time

-- | The current state of the job run entity itself.
describeJobRunResponse_state :: Lens.Lens' DescribeJobRunResponse (Prelude.Maybe JobRunState)
describeJobRunResponse_state = Lens.lens (\DescribeJobRunResponse' {state} -> state) (\s@DescribeJobRunResponse' {} a -> s {state = a} :: DescribeJobRunResponse)

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
    Prelude.rnf attempt
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf dataCatalogOutputs
      `Prelude.seq` Prelude.rnf databaseOutputs
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf jobSample
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf profileConfiguration
      `Prelude.seq` Prelude.rnf recipeReference
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf
        validationConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobName
