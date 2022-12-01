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
-- Module      : Amazonka.Connect.ListTaskTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists task templates for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListTaskTemplates
  ( -- * Creating a Request
    ListTaskTemplates (..),
    newListTaskTemplates,

    -- * Request Lenses
    listTaskTemplates_name,
    listTaskTemplates_nextToken,
    listTaskTemplates_status,
    listTaskTemplates_maxResults,
    listTaskTemplates_instanceId,

    -- * Destructuring the Response
    ListTaskTemplatesResponse (..),
    newListTaskTemplatesResponse,

    -- * Response Lenses
    listTaskTemplatesResponse_nextToken,
    listTaskTemplatesResponse_taskTemplates,
    listTaskTemplatesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTaskTemplates' smart constructor.
data ListTaskTemplates = ListTaskTemplates'
  { -- | The name of the task template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    --
    -- It is not expected that you set this because the value returned in the
    -- previous response is always null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
    -- Tasks can only be created from @ACTIVE@ templates. If a template is
    -- marked as @INACTIVE@, then a task that refers to this template cannot be
    -- created.
    status :: Prelude.Maybe TaskTemplateStatus,
    -- | The maximum number of results to return per page.
    --
    -- It is not expected that you set this.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listTaskTemplates_name' - The name of the task template.
--
-- 'nextToken', 'listTaskTemplates_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- It is not expected that you set this because the value returned in the
-- previous response is always null.
--
-- 'status', 'listTaskTemplates_status' - Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
--
-- 'maxResults', 'listTaskTemplates_maxResults' - The maximum number of results to return per page.
--
-- It is not expected that you set this.
--
-- 'instanceId', 'listTaskTemplates_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListTaskTemplates ::
  -- | 'instanceId'
  Prelude.Text ->
  ListTaskTemplates
newListTaskTemplates pInstanceId_ =
  ListTaskTemplates'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The name of the task template.
listTaskTemplates_name :: Lens.Lens' ListTaskTemplates (Prelude.Maybe Prelude.Text)
listTaskTemplates_name = Lens.lens (\ListTaskTemplates' {name} -> name) (\s@ListTaskTemplates' {} a -> s {name = a} :: ListTaskTemplates)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- It is not expected that you set this because the value returned in the
-- previous response is always null.
listTaskTemplates_nextToken :: Lens.Lens' ListTaskTemplates (Prelude.Maybe Prelude.Text)
listTaskTemplates_nextToken = Lens.lens (\ListTaskTemplates' {nextToken} -> nextToken) (\s@ListTaskTemplates' {} a -> s {nextToken = a} :: ListTaskTemplates)

-- | Marks a template as @ACTIVE@ or @INACTIVE@ for a task to refer to it.
-- Tasks can only be created from @ACTIVE@ templates. If a template is
-- marked as @INACTIVE@, then a task that refers to this template cannot be
-- created.
listTaskTemplates_status :: Lens.Lens' ListTaskTemplates (Prelude.Maybe TaskTemplateStatus)
listTaskTemplates_status = Lens.lens (\ListTaskTemplates' {status} -> status) (\s@ListTaskTemplates' {} a -> s {status = a} :: ListTaskTemplates)

-- | The maximum number of results to return per page.
--
-- It is not expected that you set this.
listTaskTemplates_maxResults :: Lens.Lens' ListTaskTemplates (Prelude.Maybe Prelude.Natural)
listTaskTemplates_maxResults = Lens.lens (\ListTaskTemplates' {maxResults} -> maxResults) (\s@ListTaskTemplates' {} a -> s {maxResults = a} :: ListTaskTemplates)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listTaskTemplates_instanceId :: Lens.Lens' ListTaskTemplates Prelude.Text
listTaskTemplates_instanceId = Lens.lens (\ListTaskTemplates' {instanceId} -> instanceId) (\s@ListTaskTemplates' {} a -> s {instanceId = a} :: ListTaskTemplates)

instance Core.AWSPager ListTaskTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTaskTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTaskTemplatesResponse_taskTemplates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTaskTemplates_nextToken
          Lens..~ rs
          Lens.^? listTaskTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTaskTemplates where
  type
    AWSResponse ListTaskTemplates =
      ListTaskTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskTemplatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TaskTemplates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTaskTemplates where
  hashWithSalt _salt ListTaskTemplates' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListTaskTemplates where
  rnf ListTaskTemplates' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders ListTaskTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTaskTemplates where
  toPath ListTaskTemplates' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/task/template"
      ]

instance Core.ToQuery ListTaskTemplates where
  toQuery ListTaskTemplates' {..} =
    Prelude.mconcat
      [ "name" Core.=: name,
        "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTaskTemplatesResponse' smart constructor.
data ListTaskTemplatesResponse = ListTaskTemplatesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- This is always returned as a null in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides details about a list of task templates belonging to an
    -- instance.
    taskTemplates :: Prelude.Maybe [TaskTemplateMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTaskTemplatesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as a null in the response.
--
-- 'taskTemplates', 'listTaskTemplatesResponse_taskTemplates' - Provides details about a list of task templates belonging to an
-- instance.
--
-- 'httpStatus', 'listTaskTemplatesResponse_httpStatus' - The response's http status code.
newListTaskTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTaskTemplatesResponse
newListTaskTemplatesResponse pHttpStatus_ =
  ListTaskTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      taskTemplates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as a null in the response.
listTaskTemplatesResponse_nextToken :: Lens.Lens' ListTaskTemplatesResponse (Prelude.Maybe Prelude.Text)
listTaskTemplatesResponse_nextToken = Lens.lens (\ListTaskTemplatesResponse' {nextToken} -> nextToken) (\s@ListTaskTemplatesResponse' {} a -> s {nextToken = a} :: ListTaskTemplatesResponse)

-- | Provides details about a list of task templates belonging to an
-- instance.
listTaskTemplatesResponse_taskTemplates :: Lens.Lens' ListTaskTemplatesResponse (Prelude.Maybe [TaskTemplateMetadata])
listTaskTemplatesResponse_taskTemplates = Lens.lens (\ListTaskTemplatesResponse' {taskTemplates} -> taskTemplates) (\s@ListTaskTemplatesResponse' {} a -> s {taskTemplates = a} :: ListTaskTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTaskTemplatesResponse_httpStatus :: Lens.Lens' ListTaskTemplatesResponse Prelude.Int
listTaskTemplatesResponse_httpStatus = Lens.lens (\ListTaskTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListTaskTemplatesResponse' {} a -> s {httpStatus = a} :: ListTaskTemplatesResponse)

instance Prelude.NFData ListTaskTemplatesResponse where
  rnf ListTaskTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskTemplates
      `Prelude.seq` Prelude.rnf httpStatus
