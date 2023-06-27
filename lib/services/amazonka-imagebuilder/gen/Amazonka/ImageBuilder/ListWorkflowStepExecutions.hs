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
-- Module      : Amazonka.ImageBuilder.ListWorkflowStepExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows runtime data for each step in a runtime instance of the workflow
-- that you specify in the request.
module Amazonka.ImageBuilder.ListWorkflowStepExecutions
  ( -- * Creating a Request
    ListWorkflowStepExecutions (..),
    newListWorkflowStepExecutions,

    -- * Request Lenses
    listWorkflowStepExecutions_maxResults,
    listWorkflowStepExecutions_nextToken,
    listWorkflowStepExecutions_workflowExecutionId,

    -- * Destructuring the Response
    ListWorkflowStepExecutionsResponse (..),
    newListWorkflowStepExecutionsResponse,

    -- * Response Lenses
    listWorkflowStepExecutionsResponse_imageBuildVersionArn,
    listWorkflowStepExecutionsResponse_message,
    listWorkflowStepExecutionsResponse_nextToken,
    listWorkflowStepExecutionsResponse_requestId,
    listWorkflowStepExecutionsResponse_steps,
    listWorkflowStepExecutionsResponse_workflowBuildVersionArn,
    listWorkflowStepExecutionsResponse_workflowExecutionId,
    listWorkflowStepExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflowStepExecutions' smart constructor.
data ListWorkflowStepExecutions = ListWorkflowStepExecutions'
  { -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Image Builder assigned to keep track of
    -- runtime details when it ran the workflow.
    workflowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowStepExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkflowStepExecutions_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listWorkflowStepExecutions_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'workflowExecutionId', 'listWorkflowStepExecutions_workflowExecutionId' - The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
newListWorkflowStepExecutions ::
  -- | 'workflowExecutionId'
  Prelude.Text ->
  ListWorkflowStepExecutions
newListWorkflowStepExecutions pWorkflowExecutionId_ =
  ListWorkflowStepExecutions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workflowExecutionId = pWorkflowExecutionId_
    }

-- | The maximum items to return in a request.
listWorkflowStepExecutions_maxResults :: Lens.Lens' ListWorkflowStepExecutions (Prelude.Maybe Prelude.Natural)
listWorkflowStepExecutions_maxResults = Lens.lens (\ListWorkflowStepExecutions' {maxResults} -> maxResults) (\s@ListWorkflowStepExecutions' {} a -> s {maxResults = a} :: ListWorkflowStepExecutions)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listWorkflowStepExecutions_nextToken :: Lens.Lens' ListWorkflowStepExecutions (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutions_nextToken = Lens.lens (\ListWorkflowStepExecutions' {nextToken} -> nextToken) (\s@ListWorkflowStepExecutions' {} a -> s {nextToken = a} :: ListWorkflowStepExecutions)

-- | The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
listWorkflowStepExecutions_workflowExecutionId :: Lens.Lens' ListWorkflowStepExecutions Prelude.Text
listWorkflowStepExecutions_workflowExecutionId = Lens.lens (\ListWorkflowStepExecutions' {workflowExecutionId} -> workflowExecutionId) (\s@ListWorkflowStepExecutions' {} a -> s {workflowExecutionId = a} :: ListWorkflowStepExecutions)

instance Core.AWSRequest ListWorkflowStepExecutions where
  type
    AWSResponse ListWorkflowStepExecutions =
      ListWorkflowStepExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowStepExecutionsResponse'
            Prelude.<$> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (x Data..?> "message")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "steps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "workflowBuildVersionArn")
            Prelude.<*> (x Data..?> "workflowExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkflowStepExecutions where
  hashWithSalt _salt ListWorkflowStepExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workflowExecutionId

instance Prelude.NFData ListWorkflowStepExecutions where
  rnf ListWorkflowStepExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workflowExecutionId

instance Data.ToHeaders ListWorkflowStepExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorkflowStepExecutions where
  toJSON ListWorkflowStepExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("workflowExecutionId" Data..= workflowExecutionId)
          ]
      )

instance Data.ToPath ListWorkflowStepExecutions where
  toPath = Prelude.const "/ListWorkflowStepExecutions"

instance Data.ToQuery ListWorkflowStepExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkflowStepExecutionsResponse' smart constructor.
data ListWorkflowStepExecutionsResponse = ListWorkflowStepExecutionsResponse'
  { -- | The image build version resource ARN that\'s associated with the
    -- specified runtime instance of the workflow.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The output message from the list action, if applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The next token used for paginated responses. When this field isn\'t
    -- empty, there are additional elements that the service has\'ot included
    -- in this request. Use this token with the next request to retrieve
    -- additional objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Contains an array of runtime details that represents each step in this
    -- runtime instance of the workflow.
    steps :: Prelude.Maybe [WorkflowStepMetadata],
    -- | The build version ARN for the Image Builder workflow resource that
    -- defines the steps for this runtime instance of the workflow.
    workflowBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Image Builder assigned to keep track of
    -- runtime details when it ran the workflow.
    workflowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowStepExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'listWorkflowStepExecutionsResponse_imageBuildVersionArn' - The image build version resource ARN that\'s associated with the
-- specified runtime instance of the workflow.
--
-- 'message', 'listWorkflowStepExecutionsResponse_message' - The output message from the list action, if applicable.
--
-- 'nextToken', 'listWorkflowStepExecutionsResponse_nextToken' - The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
--
-- 'requestId', 'listWorkflowStepExecutionsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'steps', 'listWorkflowStepExecutionsResponse_steps' - Contains an array of runtime details that represents each step in this
-- runtime instance of the workflow.
--
-- 'workflowBuildVersionArn', 'listWorkflowStepExecutionsResponse_workflowBuildVersionArn' - The build version ARN for the Image Builder workflow resource that
-- defines the steps for this runtime instance of the workflow.
--
-- 'workflowExecutionId', 'listWorkflowStepExecutionsResponse_workflowExecutionId' - The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
--
-- 'httpStatus', 'listWorkflowStepExecutionsResponse_httpStatus' - The response's http status code.
newListWorkflowStepExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowStepExecutionsResponse
newListWorkflowStepExecutionsResponse pHttpStatus_ =
  ListWorkflowStepExecutionsResponse'
    { imageBuildVersionArn =
        Prelude.Nothing,
      message = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      steps = Prelude.Nothing,
      workflowBuildVersionArn =
        Prelude.Nothing,
      workflowExecutionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image build version resource ARN that\'s associated with the
-- specified runtime instance of the workflow.
listWorkflowStepExecutionsResponse_imageBuildVersionArn :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_imageBuildVersionArn = Lens.lens (\ListWorkflowStepExecutionsResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {imageBuildVersionArn = a} :: ListWorkflowStepExecutionsResponse)

-- | The output message from the list action, if applicable.
listWorkflowStepExecutionsResponse_message :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_message = Lens.lens (\ListWorkflowStepExecutionsResponse' {message} -> message) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {message = a} :: ListWorkflowStepExecutionsResponse)

-- | The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
listWorkflowStepExecutionsResponse_nextToken :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_nextToken = Lens.lens (\ListWorkflowStepExecutionsResponse' {nextToken} -> nextToken) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {nextToken = a} :: ListWorkflowStepExecutionsResponse)

-- | The request ID that uniquely identifies this request.
listWorkflowStepExecutionsResponse_requestId :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_requestId = Lens.lens (\ListWorkflowStepExecutionsResponse' {requestId} -> requestId) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {requestId = a} :: ListWorkflowStepExecutionsResponse)

-- | Contains an array of runtime details that represents each step in this
-- runtime instance of the workflow.
listWorkflowStepExecutionsResponse_steps :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe [WorkflowStepMetadata])
listWorkflowStepExecutionsResponse_steps = Lens.lens (\ListWorkflowStepExecutionsResponse' {steps} -> steps) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {steps = a} :: ListWorkflowStepExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The build version ARN for the Image Builder workflow resource that
-- defines the steps for this runtime instance of the workflow.
listWorkflowStepExecutionsResponse_workflowBuildVersionArn :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_workflowBuildVersionArn = Lens.lens (\ListWorkflowStepExecutionsResponse' {workflowBuildVersionArn} -> workflowBuildVersionArn) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {workflowBuildVersionArn = a} :: ListWorkflowStepExecutionsResponse)

-- | The unique identifier that Image Builder assigned to keep track of
-- runtime details when it ran the workflow.
listWorkflowStepExecutionsResponse_workflowExecutionId :: Lens.Lens' ListWorkflowStepExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepExecutionsResponse_workflowExecutionId = Lens.lens (\ListWorkflowStepExecutionsResponse' {workflowExecutionId} -> workflowExecutionId) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {workflowExecutionId = a} :: ListWorkflowStepExecutionsResponse)

-- | The response's http status code.
listWorkflowStepExecutionsResponse_httpStatus :: Lens.Lens' ListWorkflowStepExecutionsResponse Prelude.Int
listWorkflowStepExecutionsResponse_httpStatus = Lens.lens (\ListWorkflowStepExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowStepExecutionsResponse' {} a -> s {httpStatus = a} :: ListWorkflowStepExecutionsResponse)

instance
  Prelude.NFData
    ListWorkflowStepExecutionsResponse
  where
  rnf ListWorkflowStepExecutionsResponse' {..} =
    Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf steps
      `Prelude.seq` Prelude.rnf workflowBuildVersionArn
      `Prelude.seq` Prelude.rnf workflowExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
