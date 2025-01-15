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
-- Module      : Amazonka.IoT.ListAuditMitigationActionsExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of audit mitigation action tasks that were executed.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAuditMitigationActionsExecutions>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListAuditMitigationActionsExecutions
  ( -- * Creating a Request
    ListAuditMitigationActionsExecutions (..),
    newListAuditMitigationActionsExecutions,

    -- * Request Lenses
    listAuditMitigationActionsExecutions_actionStatus,
    listAuditMitigationActionsExecutions_maxResults,
    listAuditMitigationActionsExecutions_nextToken,
    listAuditMitigationActionsExecutions_taskId,
    listAuditMitigationActionsExecutions_findingId,

    -- * Destructuring the Response
    ListAuditMitigationActionsExecutionsResponse (..),
    newListAuditMitigationActionsExecutionsResponse,

    -- * Response Lenses
    listAuditMitigationActionsExecutionsResponse_actionsExecutions,
    listAuditMitigationActionsExecutionsResponse_nextToken,
    listAuditMitigationActionsExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAuditMitigationActionsExecutions' smart constructor.
data ListAuditMitigationActionsExecutions = ListAuditMitigationActionsExecutions'
  { -- | Specify this filter to limit results to those with a specific status.
    actionStatus :: Prelude.Maybe AuditMitigationActionsExecutionStatus,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify this filter to limit results to actions for a specific audit
    -- mitigation actions task.
    taskId :: Prelude.Text,
    -- | Specify this filter to limit results to those that were applied to a
    -- specific audit finding.
    findingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditMitigationActionsExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionStatus', 'listAuditMitigationActionsExecutions_actionStatus' - Specify this filter to limit results to those with a specific status.
--
-- 'maxResults', 'listAuditMitigationActionsExecutions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'nextToken', 'listAuditMitigationActionsExecutions_nextToken' - The token for the next set of results.
--
-- 'taskId', 'listAuditMitigationActionsExecutions_taskId' - Specify this filter to limit results to actions for a specific audit
-- mitigation actions task.
--
-- 'findingId', 'listAuditMitigationActionsExecutions_findingId' - Specify this filter to limit results to those that were applied to a
-- specific audit finding.
newListAuditMitigationActionsExecutions ::
  -- | 'taskId'
  Prelude.Text ->
  -- | 'findingId'
  Prelude.Text ->
  ListAuditMitigationActionsExecutions
newListAuditMitigationActionsExecutions
  pTaskId_
  pFindingId_ =
    ListAuditMitigationActionsExecutions'
      { actionStatus =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        taskId = pTaskId_,
        findingId = pFindingId_
      }

-- | Specify this filter to limit results to those with a specific status.
listAuditMitigationActionsExecutions_actionStatus :: Lens.Lens' ListAuditMitigationActionsExecutions (Prelude.Maybe AuditMitigationActionsExecutionStatus)
listAuditMitigationActionsExecutions_actionStatus = Lens.lens (\ListAuditMitigationActionsExecutions' {actionStatus} -> actionStatus) (\s@ListAuditMitigationActionsExecutions' {} a -> s {actionStatus = a} :: ListAuditMitigationActionsExecutions)

-- | The maximum number of results to return at one time. The default is 25.
listAuditMitigationActionsExecutions_maxResults :: Lens.Lens' ListAuditMitigationActionsExecutions (Prelude.Maybe Prelude.Natural)
listAuditMitigationActionsExecutions_maxResults = Lens.lens (\ListAuditMitigationActionsExecutions' {maxResults} -> maxResults) (\s@ListAuditMitigationActionsExecutions' {} a -> s {maxResults = a} :: ListAuditMitigationActionsExecutions)

-- | The token for the next set of results.
listAuditMitigationActionsExecutions_nextToken :: Lens.Lens' ListAuditMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsExecutions_nextToken = Lens.lens (\ListAuditMitigationActionsExecutions' {nextToken} -> nextToken) (\s@ListAuditMitigationActionsExecutions' {} a -> s {nextToken = a} :: ListAuditMitigationActionsExecutions)

-- | Specify this filter to limit results to actions for a specific audit
-- mitigation actions task.
listAuditMitigationActionsExecutions_taskId :: Lens.Lens' ListAuditMitigationActionsExecutions Prelude.Text
listAuditMitigationActionsExecutions_taskId = Lens.lens (\ListAuditMitigationActionsExecutions' {taskId} -> taskId) (\s@ListAuditMitigationActionsExecutions' {} a -> s {taskId = a} :: ListAuditMitigationActionsExecutions)

-- | Specify this filter to limit results to those that were applied to a
-- specific audit finding.
listAuditMitigationActionsExecutions_findingId :: Lens.Lens' ListAuditMitigationActionsExecutions Prelude.Text
listAuditMitigationActionsExecutions_findingId = Lens.lens (\ListAuditMitigationActionsExecutions' {findingId} -> findingId) (\s@ListAuditMitigationActionsExecutions' {} a -> s {findingId = a} :: ListAuditMitigationActionsExecutions)

instance
  Core.AWSPager
    ListAuditMitigationActionsExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditMitigationActionsExecutionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditMitigationActionsExecutionsResponse_actionsExecutions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAuditMitigationActionsExecutions_nextToken
              Lens..~ rs
              Lens.^? listAuditMitigationActionsExecutionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAuditMitigationActionsExecutions
  where
  type
    AWSResponse ListAuditMitigationActionsExecutions =
      ListAuditMitigationActionsExecutionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsExecutionsResponse'
            Prelude.<$> ( x
                            Data..?> "actionsExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAuditMitigationActionsExecutions
  where
  hashWithSalt
    _salt
    ListAuditMitigationActionsExecutions' {..} =
      _salt
        `Prelude.hashWithSalt` actionStatus
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` findingId

instance
  Prelude.NFData
    ListAuditMitigationActionsExecutions
  where
  rnf ListAuditMitigationActionsExecutions' {..} =
    Prelude.rnf actionStatus `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf taskId `Prelude.seq`
            Prelude.rnf findingId

instance
  Data.ToHeaders
    ListAuditMitigationActionsExecutions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListAuditMitigationActionsExecutions
  where
  toPath =
    Prelude.const "/audit/mitigationactions/executions"

instance
  Data.ToQuery
    ListAuditMitigationActionsExecutions
  where
  toQuery ListAuditMitigationActionsExecutions' {..} =
    Prelude.mconcat
      [ "actionStatus" Data.=: actionStatus,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "taskId" Data.=: taskId,
        "findingId" Data.=: findingId
      ]

-- | /See:/ 'newListAuditMitigationActionsExecutionsResponse' smart constructor.
data ListAuditMitigationActionsExecutionsResponse = ListAuditMitigationActionsExecutionsResponse'
  { -- | A set of task execution results based on the input parameters. Details
    -- include the mitigation action applied, start time, and task status.
    actionsExecutions :: Prelude.Maybe [AuditMitigationActionExecutionMetadata],
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditMitigationActionsExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionsExecutions', 'listAuditMitigationActionsExecutionsResponse_actionsExecutions' - A set of task execution results based on the input parameters. Details
-- include the mitigation action applied, start time, and task status.
--
-- 'nextToken', 'listAuditMitigationActionsExecutionsResponse_nextToken' - The token for the next set of results.
--
-- 'httpStatus', 'listAuditMitigationActionsExecutionsResponse_httpStatus' - The response's http status code.
newListAuditMitigationActionsExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAuditMitigationActionsExecutionsResponse
newListAuditMitigationActionsExecutionsResponse
  pHttpStatus_ =
    ListAuditMitigationActionsExecutionsResponse'
      { actionsExecutions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A set of task execution results based on the input parameters. Details
-- include the mitigation action applied, start time, and task status.
listAuditMitigationActionsExecutionsResponse_actionsExecutions :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Prelude.Maybe [AuditMitigationActionExecutionMetadata])
listAuditMitigationActionsExecutionsResponse_actionsExecutions = Lens.lens (\ListAuditMitigationActionsExecutionsResponse' {actionsExecutions} -> actionsExecutions) (\s@ListAuditMitigationActionsExecutionsResponse' {} a -> s {actionsExecutions = a} :: ListAuditMitigationActionsExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results.
listAuditMitigationActionsExecutionsResponse_nextToken :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsExecutionsResponse_nextToken = Lens.lens (\ListAuditMitigationActionsExecutionsResponse' {nextToken} -> nextToken) (\s@ListAuditMitigationActionsExecutionsResponse' {} a -> s {nextToken = a} :: ListAuditMitigationActionsExecutionsResponse)

-- | The response's http status code.
listAuditMitigationActionsExecutionsResponse_httpStatus :: Lens.Lens' ListAuditMitigationActionsExecutionsResponse Prelude.Int
listAuditMitigationActionsExecutionsResponse_httpStatus = Lens.lens (\ListAuditMitigationActionsExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListAuditMitigationActionsExecutionsResponse' {} a -> s {httpStatus = a} :: ListAuditMitigationActionsExecutionsResponse)

instance
  Prelude.NFData
    ListAuditMitigationActionsExecutionsResponse
  where
  rnf ListAuditMitigationActionsExecutionsResponse' {..} =
    Prelude.rnf actionsExecutions `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
