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
-- Module      : Amazonka.Transfer.CreateWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to create a workflow with specified steps and step details
-- the workflow invokes after file transfer completes. After creating a
-- workflow, you can associate the workflow created with any transfer
-- servers by specifying the @workflow-details@ field in @CreateServer@ and
-- @UpdateServer@ operations.
module Amazonka.Transfer.CreateWorkflow
  ( -- * Creating a Request
    CreateWorkflow (..),
    newCreateWorkflow,

    -- * Request Lenses
    createWorkflow_description,
    createWorkflow_onExceptionSteps,
    createWorkflow_tags,
    createWorkflow_steps,

    -- * Destructuring the Response
    CreateWorkflowResponse (..),
    newCreateWorkflowResponse,

    -- * Response Lenses
    createWorkflowResponse_httpStatus,
    createWorkflowResponse_workflowId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { -- | A textual description for the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the steps (actions) to take if errors are encountered during
    -- execution of the workflow.
    --
    -- For custom steps, the Lambda function needs to send @FAILURE@ to the
    -- call back API to kick off the exception steps. Additionally, if the
    -- Lambda does not send @SUCCESS@ before it times out, the exception steps
    -- are executed.
    onExceptionSteps :: Prelude.Maybe [WorkflowStep],
    -- | Key-value pairs that can be used to group and search for workflows. Tags
    -- are metadata attached to workflows for any purpose.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the details for the steps that are in the specified workflow.
    --
    -- The @TYPE@ specifies which of the following actions is being taken for
    -- this step.
    --
    -- -   __@COPY@__ - Copy the file to another location.
    --
    -- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
    --
    -- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
    --     uploaded.
    --
    -- -   __@DELETE@__ - Delete the file.
    --
    -- -   __@TAG@__ - Add a tag to the file.
    --
    -- Currently, copying and tagging are supported only on S3.
    --
    -- For file location, you specify either the Amazon S3 bucket and key, or
    -- the Amazon EFS file system ID and path.
    steps :: [WorkflowStep]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createWorkflow_description' - A textual description for the workflow.
--
-- 'onExceptionSteps', 'createWorkflow_onExceptionSteps' - Specifies the steps (actions) to take if errors are encountered during
-- execution of the workflow.
--
-- For custom steps, the Lambda function needs to send @FAILURE@ to the
-- call back API to kick off the exception steps. Additionally, if the
-- Lambda does not send @SUCCESS@ before it times out, the exception steps
-- are executed.
--
-- 'tags', 'createWorkflow_tags' - Key-value pairs that can be used to group and search for workflows. Tags
-- are metadata attached to workflows for any purpose.
--
-- 'steps', 'createWorkflow_steps' - Specifies the details for the steps that are in the specified workflow.
--
-- The @TYPE@ specifies which of the following actions is being taken for
-- this step.
--
-- -   __@COPY@__ - Copy the file to another location.
--
-- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
--
-- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
--     uploaded.
--
-- -   __@DELETE@__ - Delete the file.
--
-- -   __@TAG@__ - Add a tag to the file.
--
-- Currently, copying and tagging are supported only on S3.
--
-- For file location, you specify either the Amazon S3 bucket and key, or
-- the Amazon EFS file system ID and path.
newCreateWorkflow ::
  CreateWorkflow
newCreateWorkflow =
  CreateWorkflow'
    { description = Prelude.Nothing,
      onExceptionSteps = Prelude.Nothing,
      tags = Prelude.Nothing,
      steps = Prelude.mempty
    }

-- | A textual description for the workflow.
createWorkflow_description :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_description = Lens.lens (\CreateWorkflow' {description} -> description) (\s@CreateWorkflow' {} a -> s {description = a} :: CreateWorkflow)

-- | Specifies the steps (actions) to take if errors are encountered during
-- execution of the workflow.
--
-- For custom steps, the Lambda function needs to send @FAILURE@ to the
-- call back API to kick off the exception steps. Additionally, if the
-- Lambda does not send @SUCCESS@ before it times out, the exception steps
-- are executed.
createWorkflow_onExceptionSteps :: Lens.Lens' CreateWorkflow (Prelude.Maybe [WorkflowStep])
createWorkflow_onExceptionSteps = Lens.lens (\CreateWorkflow' {onExceptionSteps} -> onExceptionSteps) (\s@CreateWorkflow' {} a -> s {onExceptionSteps = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | Key-value pairs that can be used to group and search for workflows. Tags
-- are metadata attached to workflows for any purpose.
createWorkflow_tags :: Lens.Lens' CreateWorkflow (Prelude.Maybe (Prelude.NonEmpty Tag))
createWorkflow_tags = Lens.lens (\CreateWorkflow' {tags} -> tags) (\s@CreateWorkflow' {} a -> s {tags = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the details for the steps that are in the specified workflow.
--
-- The @TYPE@ specifies which of the following actions is being taken for
-- this step.
--
-- -   __@COPY@__ - Copy the file to another location.
--
-- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
--
-- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
--     uploaded.
--
-- -   __@DELETE@__ - Delete the file.
--
-- -   __@TAG@__ - Add a tag to the file.
--
-- Currently, copying and tagging are supported only on S3.
--
-- For file location, you specify either the Amazon S3 bucket and key, or
-- the Amazon EFS file system ID and path.
createWorkflow_steps :: Lens.Lens' CreateWorkflow [WorkflowStep]
createWorkflow_steps = Lens.lens (\CreateWorkflow' {steps} -> steps) (\s@CreateWorkflow' {} a -> s {steps = a} :: CreateWorkflow) Prelude.. Lens.coerced

instance Core.AWSRequest CreateWorkflow where
  type
    AWSResponse CreateWorkflow =
      CreateWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "WorkflowId")
      )

instance Prelude.Hashable CreateWorkflow where
  hashWithSalt _salt CreateWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` onExceptionSteps
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` steps

instance Prelude.NFData CreateWorkflow where
  rnf CreateWorkflow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf onExceptionSteps
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf steps

instance Data.ToHeaders CreateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.CreateWorkflow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("OnExceptionSteps" Data..=)
              Prelude.<$> onExceptionSteps,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Steps" Data..= steps)
          ]
      )

instance Data.ToPath CreateWorkflow where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkflowResponse_httpStatus' - The response's http status code.
--
-- 'workflowId', 'createWorkflowResponse_workflowId' - A unique identifier for the workflow.
newCreateWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workflowId'
  Prelude.Text ->
  CreateWorkflowResponse
newCreateWorkflowResponse pHttpStatus_ pWorkflowId_ =
  CreateWorkflowResponse'
    { httpStatus = pHttpStatus_,
      workflowId = pWorkflowId_
    }

-- | The response's http status code.
createWorkflowResponse_httpStatus :: Lens.Lens' CreateWorkflowResponse Prelude.Int
createWorkflowResponse_httpStatus = Lens.lens (\CreateWorkflowResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowResponse' {} a -> s {httpStatus = a} :: CreateWorkflowResponse)

-- | A unique identifier for the workflow.
createWorkflowResponse_workflowId :: Lens.Lens' CreateWorkflowResponse Prelude.Text
createWorkflowResponse_workflowId = Lens.lens (\CreateWorkflowResponse' {workflowId} -> workflowId) (\s@CreateWorkflowResponse' {} a -> s {workflowId = a} :: CreateWorkflowResponse)

instance Prelude.NFData CreateWorkflowResponse where
  rnf CreateWorkflowResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workflowId
