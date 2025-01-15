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
-- Module      : Amazonka.MigrationHubOrchestrator.GetWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get migration workflow.
module Amazonka.MigrationHubOrchestrator.GetWorkflow
  ( -- * Creating a Request
    GetWorkflow (..),
    newGetWorkflow,

    -- * Request Lenses
    getWorkflow_id,

    -- * Destructuring the Response
    GetWorkflowResponse (..),
    newGetWorkflowResponse,

    -- * Response Lenses
    getWorkflowResponse_adsApplicationConfigurationId,
    getWorkflowResponse_adsApplicationName,
    getWorkflowResponse_arn,
    getWorkflowResponse_completedSteps,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_description,
    getWorkflowResponse_endTime,
    getWorkflowResponse_id,
    getWorkflowResponse_lastModifiedTime,
    getWorkflowResponse_lastStartTime,
    getWorkflowResponse_lastStopTime,
    getWorkflowResponse_name,
    getWorkflowResponse_status,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_tags,
    getWorkflowResponse_templateId,
    getWorkflowResponse_tools,
    getWorkflowResponse_totalSteps,
    getWorkflowResponse_workflowBucket,
    getWorkflowResponse_workflowInputs,
    getWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | The ID of the migration workflow.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWorkflow_id' - The ID of the migration workflow.
newGetWorkflow ::
  -- | 'id'
  Prelude.Text ->
  GetWorkflow
newGetWorkflow pId_ = GetWorkflow' {id = pId_}

-- | The ID of the migration workflow.
getWorkflow_id :: Lens.Lens' GetWorkflow Prelude.Text
getWorkflow_id = Lens.lens (\GetWorkflow' {id} -> id) (\s@GetWorkflow' {} a -> s {id = a} :: GetWorkflow)

instance Core.AWSRequest GetWorkflow where
  type AWSResponse GetWorkflow = GetWorkflowResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Prelude.<$> (x Data..?> "adsApplicationConfigurationId")
            Prelude.<*> (x Data..?> "adsApplicationName")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "completedSteps")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "lastStartTime")
            Prelude.<*> (x Data..?> "lastStopTime")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "templateId")
            Prelude.<*> (x Data..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "totalSteps")
            Prelude.<*> (x Data..?> "workflowBucket")
            Prelude.<*> (x Data..?> "workflowInputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflow where
  hashWithSalt _salt GetWorkflow' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorkflow where
  rnf GetWorkflow' {..} = Prelude.rnf id

instance Data.ToHeaders GetWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflow where
  toPath GetWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Data.toBS id]

instance Data.ToQuery GetWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The configuration ID of the application configured in Application
    -- Discovery Service.
    adsApplicationConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the application configured in Application Discovery Service.
    adsApplicationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Get a list of completed steps in the migration workflow.
    completedSteps :: Prelude.Maybe Prelude.Int,
    -- | The time at which the migration workflow was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the migration workflow was last started.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the migration workflow was last stopped.
    lastStopTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The tags added to the migration workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The total number of steps in the migration workflow.
    totalSteps :: Prelude.Maybe Prelude.Int,
    -- | The Amazon S3 bucket where the migration logs are stored.
    workflowBucket :: Prelude.Maybe Prelude.Text,
    -- | The inputs required for creating the migration workflow.
    workflowInputs :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text StepInput)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsApplicationConfigurationId', 'getWorkflowResponse_adsApplicationConfigurationId' - The configuration ID of the application configured in Application
-- Discovery Service.
--
-- 'adsApplicationName', 'getWorkflowResponse_adsApplicationName' - The name of the application configured in Application Discovery Service.
--
-- 'arn', 'getWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'completedSteps', 'getWorkflowResponse_completedSteps' - Get a list of completed steps in the migration workflow.
--
-- 'creationTime', 'getWorkflowResponse_creationTime' - The time at which the migration workflow was created.
--
-- 'description', 'getWorkflowResponse_description' - The description of the migration workflow.
--
-- 'endTime', 'getWorkflowResponse_endTime' - The time at which the migration workflow ended.
--
-- 'id', 'getWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'lastModifiedTime', 'getWorkflowResponse_lastModifiedTime' - The time at which the migration workflow was last modified.
--
-- 'lastStartTime', 'getWorkflowResponse_lastStartTime' - The time at which the migration workflow was last started.
--
-- 'lastStopTime', 'getWorkflowResponse_lastStopTime' - The time at which the migration workflow was last stopped.
--
-- 'name', 'getWorkflowResponse_name' - The name of the migration workflow.
--
-- 'status', 'getWorkflowResponse_status' - The status of the migration workflow.
--
-- 'statusMessage', 'getWorkflowResponse_statusMessage' - The status message of the migration workflow.
--
-- 'tags', 'getWorkflowResponse_tags' - The tags added to the migration workflow.
--
-- 'templateId', 'getWorkflowResponse_templateId' - The ID of the template.
--
-- 'tools', 'getWorkflowResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'totalSteps', 'getWorkflowResponse_totalSteps' - The total number of steps in the migration workflow.
--
-- 'workflowBucket', 'getWorkflowResponse_workflowBucket' - The Amazon S3 bucket where the migration logs are stored.
--
-- 'workflowInputs', 'getWorkflowResponse_workflowInputs' - The inputs required for creating the migration workflow.
--
-- 'httpStatus', 'getWorkflowResponse_httpStatus' - The response's http status code.
newGetWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowResponse
newGetWorkflowResponse pHttpStatus_ =
  GetWorkflowResponse'
    { adsApplicationConfigurationId =
        Prelude.Nothing,
      adsApplicationName = Prelude.Nothing,
      arn = Prelude.Nothing,
      completedSteps = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      lastStopTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateId = Prelude.Nothing,
      tools = Prelude.Nothing,
      totalSteps = Prelude.Nothing,
      workflowBucket = Prelude.Nothing,
      workflowInputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration ID of the application configured in Application
-- Discovery Service.
getWorkflowResponse_adsApplicationConfigurationId :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_adsApplicationConfigurationId = Lens.lens (\GetWorkflowResponse' {adsApplicationConfigurationId} -> adsApplicationConfigurationId) (\s@GetWorkflowResponse' {} a -> s {adsApplicationConfigurationId = a} :: GetWorkflowResponse)

-- | The name of the application configured in Application Discovery Service.
getWorkflowResponse_adsApplicationName :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_adsApplicationName = Lens.lens (\GetWorkflowResponse' {adsApplicationName} -> adsApplicationName) (\s@GetWorkflowResponse' {} a -> s {adsApplicationName = a} :: GetWorkflowResponse)

-- | The Amazon Resource Name (ARN) of the migration workflow.
getWorkflowResponse_arn :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_arn = Lens.lens (\GetWorkflowResponse' {arn} -> arn) (\s@GetWorkflowResponse' {} a -> s {arn = a} :: GetWorkflowResponse)

-- | Get a list of completed steps in the migration workflow.
getWorkflowResponse_completedSteps :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Int)
getWorkflowResponse_completedSteps = Lens.lens (\GetWorkflowResponse' {completedSteps} -> completedSteps) (\s@GetWorkflowResponse' {} a -> s {completedSteps = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow was created.
getWorkflowResponse_creationTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_creationTime = Lens.lens (\GetWorkflowResponse' {creationTime} -> creationTime) (\s@GetWorkflowResponse' {} a -> s {creationTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the migration workflow.
getWorkflowResponse_description :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_description = Lens.lens (\GetWorkflowResponse' {description} -> description) (\s@GetWorkflowResponse' {} a -> s {description = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow ended.
getWorkflowResponse_endTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_endTime = Lens.lens (\GetWorkflowResponse' {endTime} -> endTime) (\s@GetWorkflowResponse' {} a -> s {endTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the migration workflow.
getWorkflowResponse_id :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_id = Lens.lens (\GetWorkflowResponse' {id} -> id) (\s@GetWorkflowResponse' {} a -> s {id = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow was last modified.
getWorkflowResponse_lastModifiedTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastModifiedTime = Lens.lens (\GetWorkflowResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetWorkflowResponse' {} a -> s {lastModifiedTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The time at which the migration workflow was last started.
getWorkflowResponse_lastStartTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastStartTime = Lens.lens (\GetWorkflowResponse' {lastStartTime} -> lastStartTime) (\s@GetWorkflowResponse' {} a -> s {lastStartTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The time at which the migration workflow was last stopped.
getWorkflowResponse_lastStopTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastStopTime = Lens.lens (\GetWorkflowResponse' {lastStopTime} -> lastStopTime) (\s@GetWorkflowResponse' {} a -> s {lastStopTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the migration workflow.
getWorkflowResponse_name :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_name = Lens.lens (\GetWorkflowResponse' {name} -> name) (\s@GetWorkflowResponse' {} a -> s {name = a} :: GetWorkflowResponse)

-- | The status of the migration workflow.
getWorkflowResponse_status :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
getWorkflowResponse_status = Lens.lens (\GetWorkflowResponse' {status} -> status) (\s@GetWorkflowResponse' {} a -> s {status = a} :: GetWorkflowResponse)

-- | The status message of the migration workflow.
getWorkflowResponse_statusMessage :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_statusMessage = Lens.lens (\GetWorkflowResponse' {statusMessage} -> statusMessage) (\s@GetWorkflowResponse' {} a -> s {statusMessage = a} :: GetWorkflowResponse)

-- | The tags added to the migration workflow.
getWorkflowResponse_tags :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getWorkflowResponse_tags = Lens.lens (\GetWorkflowResponse' {tags} -> tags) (\s@GetWorkflowResponse' {} a -> s {tags = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the template.
getWorkflowResponse_templateId :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_templateId = Lens.lens (\GetWorkflowResponse' {templateId} -> templateId) (\s@GetWorkflowResponse' {} a -> s {templateId = a} :: GetWorkflowResponse)

-- | List of AWS services utilized in a migration workflow.
getWorkflowResponse_tools :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe [Tool])
getWorkflowResponse_tools = Lens.lens (\GetWorkflowResponse' {tools} -> tools) (\s@GetWorkflowResponse' {} a -> s {tools = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of steps in the migration workflow.
getWorkflowResponse_totalSteps :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Int)
getWorkflowResponse_totalSteps = Lens.lens (\GetWorkflowResponse' {totalSteps} -> totalSteps) (\s@GetWorkflowResponse' {} a -> s {totalSteps = a} :: GetWorkflowResponse)

-- | The Amazon S3 bucket where the migration logs are stored.
getWorkflowResponse_workflowBucket :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_workflowBucket = Lens.lens (\GetWorkflowResponse' {workflowBucket} -> workflowBucket) (\s@GetWorkflowResponse' {} a -> s {workflowBucket = a} :: GetWorkflowResponse)

-- | The inputs required for creating the migration workflow.
getWorkflowResponse_workflowInputs :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text StepInput))
getWorkflowResponse_workflowInputs = Lens.lens (\GetWorkflowResponse' {workflowInputs} -> workflowInputs) (\s@GetWorkflowResponse' {} a -> s {workflowInputs = a} :: GetWorkflowResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Prelude.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Prelude.NFData GetWorkflowResponse where
  rnf GetWorkflowResponse' {..} =
    Prelude.rnf adsApplicationConfigurationId `Prelude.seq`
      Prelude.rnf adsApplicationName `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf completedSteps `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf endTime `Prelude.seq`
                  Prelude.rnf id `Prelude.seq`
                    Prelude.rnf lastModifiedTime `Prelude.seq`
                      Prelude.rnf lastStartTime `Prelude.seq`
                        Prelude.rnf lastStopTime `Prelude.seq`
                          Prelude.rnf name `Prelude.seq`
                            Prelude.rnf status `Prelude.seq`
                              Prelude.rnf statusMessage `Prelude.seq`
                                Prelude.rnf tags `Prelude.seq`
                                  Prelude.rnf templateId `Prelude.seq`
                                    Prelude.rnf tools `Prelude.seq`
                                      Prelude.rnf totalSteps `Prelude.seq`
                                        Prelude.rnf workflowBucket `Prelude.seq`
                                          Prelude.rnf workflowInputs `Prelude.seq`
                                            Prelude.rnf httpStatus
