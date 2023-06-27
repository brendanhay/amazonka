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
-- Module      : Amazonka.ImageBuilder.ListWorkflowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of workflow runtime instance metadata objects for a
-- specific image build version.
module Amazonka.ImageBuilder.ListWorkflowExecutions
  ( -- * Creating a Request
    ListWorkflowExecutions (..),
    newListWorkflowExecutions,

    -- * Request Lenses
    listWorkflowExecutions_maxResults,
    listWorkflowExecutions_nextToken,
    listWorkflowExecutions_imageBuildVersionArn,

    -- * Destructuring the Response
    ListWorkflowExecutionsResponse (..),
    newListWorkflowExecutionsResponse,

    -- * Response Lenses
    listWorkflowExecutionsResponse_imageBuildVersionArn,
    listWorkflowExecutionsResponse_message,
    listWorkflowExecutionsResponse_nextToken,
    listWorkflowExecutionsResponse_requestId,
    listWorkflowExecutionsResponse_workflowExecutions,
    listWorkflowExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflowExecutions' smart constructor.
data ListWorkflowExecutions = ListWorkflowExecutions'
  { -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List all workflow runtime instances for the specified image build
    -- version resource ARN.
    imageBuildVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkflowExecutions_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listWorkflowExecutions_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'imageBuildVersionArn', 'listWorkflowExecutions_imageBuildVersionArn' - List all workflow runtime instances for the specified image build
-- version resource ARN.
newListWorkflowExecutions ::
  -- | 'imageBuildVersionArn'
  Prelude.Text ->
  ListWorkflowExecutions
newListWorkflowExecutions pImageBuildVersionArn_ =
  ListWorkflowExecutions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imageBuildVersionArn = pImageBuildVersionArn_
    }

-- | The maximum items to return in a request.
listWorkflowExecutions_maxResults :: Lens.Lens' ListWorkflowExecutions (Prelude.Maybe Prelude.Natural)
listWorkflowExecutions_maxResults = Lens.lens (\ListWorkflowExecutions' {maxResults} -> maxResults) (\s@ListWorkflowExecutions' {} a -> s {maxResults = a} :: ListWorkflowExecutions)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listWorkflowExecutions_nextToken :: Lens.Lens' ListWorkflowExecutions (Prelude.Maybe Prelude.Text)
listWorkflowExecutions_nextToken = Lens.lens (\ListWorkflowExecutions' {nextToken} -> nextToken) (\s@ListWorkflowExecutions' {} a -> s {nextToken = a} :: ListWorkflowExecutions)

-- | List all workflow runtime instances for the specified image build
-- version resource ARN.
listWorkflowExecutions_imageBuildVersionArn :: Lens.Lens' ListWorkflowExecutions Prelude.Text
listWorkflowExecutions_imageBuildVersionArn = Lens.lens (\ListWorkflowExecutions' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ListWorkflowExecutions' {} a -> s {imageBuildVersionArn = a} :: ListWorkflowExecutions)

instance Core.AWSRequest ListWorkflowExecutions where
  type
    AWSResponse ListWorkflowExecutions =
      ListWorkflowExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowExecutionsResponse'
            Prelude.<$> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (x Data..?> "message")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> ( x
                            Data..?> "workflowExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkflowExecutions where
  hashWithSalt _salt ListWorkflowExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` imageBuildVersionArn

instance Prelude.NFData ListWorkflowExecutions where
  rnf ListWorkflowExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageBuildVersionArn

instance Data.ToHeaders ListWorkflowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorkflowExecutions where
  toJSON ListWorkflowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "imageBuildVersionArn"
                  Data..= imageBuildVersionArn
              )
          ]
      )

instance Data.ToPath ListWorkflowExecutions where
  toPath = Prelude.const "/ListWorkflowExecutions"

instance Data.ToQuery ListWorkflowExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkflowExecutionsResponse' smart constructor.
data ListWorkflowExecutionsResponse = ListWorkflowExecutionsResponse'
  { -- | The resource ARN of the image build version for which you requested a
    -- list of workflow runtime details.
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
    -- | Contains an array of runtime details that represents each time a
    -- workflow ran for the requested image build version.
    workflowExecutions :: Prelude.Maybe [WorkflowExecutionMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'listWorkflowExecutionsResponse_imageBuildVersionArn' - The resource ARN of the image build version for which you requested a
-- list of workflow runtime details.
--
-- 'message', 'listWorkflowExecutionsResponse_message' - The output message from the list action, if applicable.
--
-- 'nextToken', 'listWorkflowExecutionsResponse_nextToken' - The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
--
-- 'requestId', 'listWorkflowExecutionsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'workflowExecutions', 'listWorkflowExecutionsResponse_workflowExecutions' - Contains an array of runtime details that represents each time a
-- workflow ran for the requested image build version.
--
-- 'httpStatus', 'listWorkflowExecutionsResponse_httpStatus' - The response's http status code.
newListWorkflowExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowExecutionsResponse
newListWorkflowExecutionsResponse pHttpStatus_ =
  ListWorkflowExecutionsResponse'
    { imageBuildVersionArn =
        Prelude.Nothing,
      message = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      workflowExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource ARN of the image build version for which you requested a
-- list of workflow runtime details.
listWorkflowExecutionsResponse_imageBuildVersionArn :: Lens.Lens' ListWorkflowExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowExecutionsResponse_imageBuildVersionArn = Lens.lens (\ListWorkflowExecutionsResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@ListWorkflowExecutionsResponse' {} a -> s {imageBuildVersionArn = a} :: ListWorkflowExecutionsResponse)

-- | The output message from the list action, if applicable.
listWorkflowExecutionsResponse_message :: Lens.Lens' ListWorkflowExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowExecutionsResponse_message = Lens.lens (\ListWorkflowExecutionsResponse' {message} -> message) (\s@ListWorkflowExecutionsResponse' {} a -> s {message = a} :: ListWorkflowExecutionsResponse)

-- | The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
listWorkflowExecutionsResponse_nextToken :: Lens.Lens' ListWorkflowExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowExecutionsResponse_nextToken = Lens.lens (\ListWorkflowExecutionsResponse' {nextToken} -> nextToken) (\s@ListWorkflowExecutionsResponse' {} a -> s {nextToken = a} :: ListWorkflowExecutionsResponse)

-- | The request ID that uniquely identifies this request.
listWorkflowExecutionsResponse_requestId :: Lens.Lens' ListWorkflowExecutionsResponse (Prelude.Maybe Prelude.Text)
listWorkflowExecutionsResponse_requestId = Lens.lens (\ListWorkflowExecutionsResponse' {requestId} -> requestId) (\s@ListWorkflowExecutionsResponse' {} a -> s {requestId = a} :: ListWorkflowExecutionsResponse)

-- | Contains an array of runtime details that represents each time a
-- workflow ran for the requested image build version.
listWorkflowExecutionsResponse_workflowExecutions :: Lens.Lens' ListWorkflowExecutionsResponse (Prelude.Maybe [WorkflowExecutionMetadata])
listWorkflowExecutionsResponse_workflowExecutions = Lens.lens (\ListWorkflowExecutionsResponse' {workflowExecutions} -> workflowExecutions) (\s@ListWorkflowExecutionsResponse' {} a -> s {workflowExecutions = a} :: ListWorkflowExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkflowExecutionsResponse_httpStatus :: Lens.Lens' ListWorkflowExecutionsResponse Prelude.Int
listWorkflowExecutionsResponse_httpStatus = Lens.lens (\ListWorkflowExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowExecutionsResponse' {} a -> s {httpStatus = a} :: ListWorkflowExecutionsResponse)

instance
  Prelude.NFData
    ListWorkflowExecutionsResponse
  where
  rnf ListWorkflowExecutionsResponse' {..} =
    Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf workflowExecutions
      `Prelude.seq` Prelude.rnf httpStatus
