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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getWorkflowResponse_tags,
    getWorkflowResponse_name,
    getWorkflowResponse_workflowBucket,
    getWorkflowResponse_adsApplicationName,
    getWorkflowResponse_adsApplicationConfigurationId,
    getWorkflowResponse_tools,
    getWorkflowResponse_lastStartTime,
    getWorkflowResponse_arn,
    getWorkflowResponse_status,
    getWorkflowResponse_description,
    getWorkflowResponse_templateId,
    getWorkflowResponse_endTime,
    getWorkflowResponse_id,
    getWorkflowResponse_lastModifiedTime,
    getWorkflowResponse_lastStopTime,
    getWorkflowResponse_completedSteps,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_totalSteps,
    getWorkflowResponse_workflowInputs,
    getWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "workflowBucket")
            Prelude.<*> (x Core..?> "adsApplicationName")
            Prelude.<*> (x Core..?> "adsApplicationConfigurationId")
            Prelude.<*> (x Core..?> "tools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "lastStartTime")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "templateId")
            Prelude.<*> (x Core..?> "endTime")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "lastModifiedTime")
            Prelude.<*> (x Core..?> "lastStopTime")
            Prelude.<*> (x Core..?> "completedSteps")
            Prelude.<*> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "statusMessage")
            Prelude.<*> (x Core..?> "totalSteps")
            Prelude.<*> (x Core..?> "workflowInputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflow where
  hashWithSalt _salt GetWorkflow' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorkflow where
  rnf GetWorkflow' {..} = Prelude.rnf id

instance Core.ToHeaders GetWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWorkflow where
  toPath GetWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Core.toBS id]

instance Core.ToQuery GetWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The tags added to the migration workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket where the migration logs are stored.
    workflowBucket :: Prelude.Maybe Prelude.Text,
    -- | The name of the application configured in Application Discovery Service.
    adsApplicationName :: Prelude.Maybe Prelude.Text,
    -- | The configuration ID of the application configured in Application
    -- Discovery Service.
    adsApplicationConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | List of AWS services utilized in a migration workflow.
    tools :: Prelude.Maybe [Tool],
    -- | The time at which the migration workflow was last started.
    lastStartTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The time at which the migration workflow was last stopped.
    lastStopTime :: Prelude.Maybe Core.POSIX,
    -- | Get a list of completed steps in the migration workflow.
    completedSteps :: Prelude.Maybe Prelude.Int,
    -- | The time at which the migration workflow was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The total number of steps in the migration workflow.
    totalSteps :: Prelude.Maybe Prelude.Int,
    -- | The inputs required for creating the migration workflow.
    workflowInputs :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text StepInput)),
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
-- 'tags', 'getWorkflowResponse_tags' - The tags added to the migration workflow.
--
-- 'name', 'getWorkflowResponse_name' - The name of the migration workflow.
--
-- 'workflowBucket', 'getWorkflowResponse_workflowBucket' - The Amazon S3 bucket where the migration logs are stored.
--
-- 'adsApplicationName', 'getWorkflowResponse_adsApplicationName' - The name of the application configured in Application Discovery Service.
--
-- 'adsApplicationConfigurationId', 'getWorkflowResponse_adsApplicationConfigurationId' - The configuration ID of the application configured in Application
-- Discovery Service.
--
-- 'tools', 'getWorkflowResponse_tools' - List of AWS services utilized in a migration workflow.
--
-- 'lastStartTime', 'getWorkflowResponse_lastStartTime' - The time at which the migration workflow was last started.
--
-- 'arn', 'getWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'status', 'getWorkflowResponse_status' - The status of the migration workflow.
--
-- 'description', 'getWorkflowResponse_description' - The description of the migration workflow.
--
-- 'templateId', 'getWorkflowResponse_templateId' - The ID of the template.
--
-- 'endTime', 'getWorkflowResponse_endTime' - The time at which the migration workflow ended.
--
-- 'id', 'getWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'lastModifiedTime', 'getWorkflowResponse_lastModifiedTime' - The time at which the migration workflow was last modified.
--
-- 'lastStopTime', 'getWorkflowResponse_lastStopTime' - The time at which the migration workflow was last stopped.
--
-- 'completedSteps', 'getWorkflowResponse_completedSteps' - Get a list of completed steps in the migration workflow.
--
-- 'creationTime', 'getWorkflowResponse_creationTime' - The time at which the migration workflow was created.
--
-- 'statusMessage', 'getWorkflowResponse_statusMessage' - The status message of the migration workflow.
--
-- 'totalSteps', 'getWorkflowResponse_totalSteps' - The total number of steps in the migration workflow.
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
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      workflowBucket = Prelude.Nothing,
      adsApplicationName = Prelude.Nothing,
      adsApplicationConfigurationId = Prelude.Nothing,
      tools = Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      templateId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastStopTime = Prelude.Nothing,
      completedSteps = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      totalSteps = Prelude.Nothing,
      workflowInputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags added to the migration workflow.
getWorkflowResponse_tags :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getWorkflowResponse_tags = Lens.lens (\GetWorkflowResponse' {tags} -> tags) (\s@GetWorkflowResponse' {} a -> s {tags = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the migration workflow.
getWorkflowResponse_name :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_name = Lens.lens (\GetWorkflowResponse' {name} -> name) (\s@GetWorkflowResponse' {} a -> s {name = a} :: GetWorkflowResponse)

-- | The Amazon S3 bucket where the migration logs are stored.
getWorkflowResponse_workflowBucket :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_workflowBucket = Lens.lens (\GetWorkflowResponse' {workflowBucket} -> workflowBucket) (\s@GetWorkflowResponse' {} a -> s {workflowBucket = a} :: GetWorkflowResponse)

-- | The name of the application configured in Application Discovery Service.
getWorkflowResponse_adsApplicationName :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_adsApplicationName = Lens.lens (\GetWorkflowResponse' {adsApplicationName} -> adsApplicationName) (\s@GetWorkflowResponse' {} a -> s {adsApplicationName = a} :: GetWorkflowResponse)

-- | The configuration ID of the application configured in Application
-- Discovery Service.
getWorkflowResponse_adsApplicationConfigurationId :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_adsApplicationConfigurationId = Lens.lens (\GetWorkflowResponse' {adsApplicationConfigurationId} -> adsApplicationConfigurationId) (\s@GetWorkflowResponse' {} a -> s {adsApplicationConfigurationId = a} :: GetWorkflowResponse)

-- | List of AWS services utilized in a migration workflow.
getWorkflowResponse_tools :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe [Tool])
getWorkflowResponse_tools = Lens.lens (\GetWorkflowResponse' {tools} -> tools) (\s@GetWorkflowResponse' {} a -> s {tools = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the migration workflow was last started.
getWorkflowResponse_lastStartTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastStartTime = Lens.lens (\GetWorkflowResponse' {lastStartTime} -> lastStartTime) (\s@GetWorkflowResponse' {} a -> s {lastStartTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the migration workflow.
getWorkflowResponse_arn :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_arn = Lens.lens (\GetWorkflowResponse' {arn} -> arn) (\s@GetWorkflowResponse' {} a -> s {arn = a} :: GetWorkflowResponse)

-- | The status of the migration workflow.
getWorkflowResponse_status :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
getWorkflowResponse_status = Lens.lens (\GetWorkflowResponse' {status} -> status) (\s@GetWorkflowResponse' {} a -> s {status = a} :: GetWorkflowResponse)

-- | The description of the migration workflow.
getWorkflowResponse_description :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_description = Lens.lens (\GetWorkflowResponse' {description} -> description) (\s@GetWorkflowResponse' {} a -> s {description = a} :: GetWorkflowResponse)

-- | The ID of the template.
getWorkflowResponse_templateId :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_templateId = Lens.lens (\GetWorkflowResponse' {templateId} -> templateId) (\s@GetWorkflowResponse' {} a -> s {templateId = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow ended.
getWorkflowResponse_endTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_endTime = Lens.lens (\GetWorkflowResponse' {endTime} -> endTime) (\s@GetWorkflowResponse' {} a -> s {endTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the migration workflow.
getWorkflowResponse_id :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_id = Lens.lens (\GetWorkflowResponse' {id} -> id) (\s@GetWorkflowResponse' {} a -> s {id = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow was last modified.
getWorkflowResponse_lastModifiedTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastModifiedTime = Lens.lens (\GetWorkflowResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetWorkflowResponse' {} a -> s {lastModifiedTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Core._Time

-- | The time at which the migration workflow was last stopped.
getWorkflowResponse_lastStopTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_lastStopTime = Lens.lens (\GetWorkflowResponse' {lastStopTime} -> lastStopTime) (\s@GetWorkflowResponse' {} a -> s {lastStopTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Core._Time

-- | Get a list of completed steps in the migration workflow.
getWorkflowResponse_completedSteps :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Int)
getWorkflowResponse_completedSteps = Lens.lens (\GetWorkflowResponse' {completedSteps} -> completedSteps) (\s@GetWorkflowResponse' {} a -> s {completedSteps = a} :: GetWorkflowResponse)

-- | The time at which the migration workflow was created.
getWorkflowResponse_creationTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_creationTime = Lens.lens (\GetWorkflowResponse' {creationTime} -> creationTime) (\s@GetWorkflowResponse' {} a -> s {creationTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Core._Time

-- | The status message of the migration workflow.
getWorkflowResponse_statusMessage :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_statusMessage = Lens.lens (\GetWorkflowResponse' {statusMessage} -> statusMessage) (\s@GetWorkflowResponse' {} a -> s {statusMessage = a} :: GetWorkflowResponse)

-- | The total number of steps in the migration workflow.
getWorkflowResponse_totalSteps :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Int)
getWorkflowResponse_totalSteps = Lens.lens (\GetWorkflowResponse' {totalSteps} -> totalSteps) (\s@GetWorkflowResponse' {} a -> s {totalSteps = a} :: GetWorkflowResponse)

-- | The inputs required for creating the migration workflow.
getWorkflowResponse_workflowInputs :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text StepInput))
getWorkflowResponse_workflowInputs = Lens.lens (\GetWorkflowResponse' {workflowInputs} -> workflowInputs) (\s@GetWorkflowResponse' {} a -> s {workflowInputs = a} :: GetWorkflowResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Prelude.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Prelude.NFData GetWorkflowResponse where
  rnf GetWorkflowResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowBucket
      `Prelude.seq` Prelude.rnf adsApplicationName
      `Prelude.seq` Prelude.rnf adsApplicationConfigurationId
      `Prelude.seq` Prelude.rnf tools
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastStopTime
      `Prelude.seq` Prelude.rnf completedSteps
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf totalSteps
      `Prelude.seq` Prelude.rnf workflowInputs
      `Prelude.seq` Prelude.rnf httpStatus
