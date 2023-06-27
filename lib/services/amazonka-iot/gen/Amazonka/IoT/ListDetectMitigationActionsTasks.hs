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
-- Module      : Amazonka.IoT.ListDetectMitigationActionsTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List of Device Defender ML Detect mitigation actions tasks.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListDetectMitigationActionsTasks>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListDetectMitigationActionsTasks
  ( -- * Creating a Request
    ListDetectMitigationActionsTasks (..),
    newListDetectMitigationActionsTasks,

    -- * Request Lenses
    listDetectMitigationActionsTasks_maxResults,
    listDetectMitigationActionsTasks_nextToken,
    listDetectMitigationActionsTasks_startTime,
    listDetectMitigationActionsTasks_endTime,

    -- * Destructuring the Response
    ListDetectMitigationActionsTasksResponse (..),
    newListDetectMitigationActionsTasksResponse,

    -- * Response Lenses
    listDetectMitigationActionsTasksResponse_nextToken,
    listDetectMitigationActionsTasksResponse_tasks,
    listDetectMitigationActionsTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectMitigationActionsTasks' smart constructor.
data ListDetectMitigationActionsTasks = ListDetectMitigationActionsTasks'
  { -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Data.POSIX,
    -- | The end of the time period for which ML Detect mitigation actions tasks
    -- are returned.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectMitigationActionsTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDetectMitigationActionsTasks_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'nextToken', 'listDetectMitigationActionsTasks_nextToken' - The token for the next set of results.
--
-- 'startTime', 'listDetectMitigationActionsTasks_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'endTime', 'listDetectMitigationActionsTasks_endTime' - The end of the time period for which ML Detect mitigation actions tasks
-- are returned.
newListDetectMitigationActionsTasks ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ListDetectMitigationActionsTasks
newListDetectMitigationActionsTasks
  pStartTime_
  pEndTime_ =
    ListDetectMitigationActionsTasks'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | The maximum number of results to return at one time. The default is 25.
listDetectMitigationActionsTasks_maxResults :: Lens.Lens' ListDetectMitigationActionsTasks (Prelude.Maybe Prelude.Natural)
listDetectMitigationActionsTasks_maxResults = Lens.lens (\ListDetectMitigationActionsTasks' {maxResults} -> maxResults) (\s@ListDetectMitigationActionsTasks' {} a -> s {maxResults = a} :: ListDetectMitigationActionsTasks)

-- | The token for the next set of results.
listDetectMitigationActionsTasks_nextToken :: Lens.Lens' ListDetectMitigationActionsTasks (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsTasks_nextToken = Lens.lens (\ListDetectMitigationActionsTasks' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsTasks' {} a -> s {nextToken = a} :: ListDetectMitigationActionsTasks)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listDetectMitigationActionsTasks_startTime :: Lens.Lens' ListDetectMitigationActionsTasks Prelude.UTCTime
listDetectMitigationActionsTasks_startTime = Lens.lens (\ListDetectMitigationActionsTasks' {startTime} -> startTime) (\s@ListDetectMitigationActionsTasks' {} a -> s {startTime = a} :: ListDetectMitigationActionsTasks) Prelude.. Data._Time

-- | The end of the time period for which ML Detect mitigation actions tasks
-- are returned.
listDetectMitigationActionsTasks_endTime :: Lens.Lens' ListDetectMitigationActionsTasks Prelude.UTCTime
listDetectMitigationActionsTasks_endTime = Lens.lens (\ListDetectMitigationActionsTasks' {endTime} -> endTime) (\s@ListDetectMitigationActionsTasks' {} a -> s {endTime = a} :: ListDetectMitigationActionsTasks) Prelude.. Data._Time

instance
  Core.AWSPager
    ListDetectMitigationActionsTasks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsTasksResponse_tasks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDetectMitigationActionsTasks_nextToken
          Lens..~ rs
          Lens.^? listDetectMitigationActionsTasksResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDetectMitigationActionsTasks
  where
  type
    AWSResponse ListDetectMitigationActionsTasks =
      ListDetectMitigationActionsTasksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectMitigationActionsTasksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDetectMitigationActionsTasks
  where
  hashWithSalt
    _salt
    ListDetectMitigationActionsTasks' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` endTime

instance
  Prelude.NFData
    ListDetectMitigationActionsTasks
  where
  rnf ListDetectMitigationActionsTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance
  Data.ToHeaders
    ListDetectMitigationActionsTasks
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDetectMitigationActionsTasks where
  toPath =
    Prelude.const "/detect/mitigationactions/tasks"

instance
  Data.ToQuery
    ListDetectMitigationActionsTasks
  where
  toQuery ListDetectMitigationActionsTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "startTime" Data.=: startTime,
        "endTime" Data.=: endTime
      ]

-- | /See:/ 'newListDetectMitigationActionsTasksResponse' smart constructor.
data ListDetectMitigationActionsTasksResponse = ListDetectMitigationActionsTasksResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of ML Detect mitigation tasks that matched the filter
    -- criteria.
    tasks :: Prelude.Maybe [DetectMitigationActionsTaskSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectMitigationActionsTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectMitigationActionsTasksResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'tasks', 'listDetectMitigationActionsTasksResponse_tasks' - The collection of ML Detect mitigation tasks that matched the filter
-- criteria.
--
-- 'httpStatus', 'listDetectMitigationActionsTasksResponse_httpStatus' - The response's http status code.
newListDetectMitigationActionsTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectMitigationActionsTasksResponse
newListDetectMitigationActionsTasksResponse
  pHttpStatus_ =
    ListDetectMitigationActionsTasksResponse'
      { nextToken =
          Prelude.Nothing,
        tasks = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listDetectMitigationActionsTasksResponse_nextToken :: Lens.Lens' ListDetectMitigationActionsTasksResponse (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsTasksResponse_nextToken = Lens.lens (\ListDetectMitigationActionsTasksResponse' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsTasksResponse' {} a -> s {nextToken = a} :: ListDetectMitigationActionsTasksResponse)

-- | The collection of ML Detect mitigation tasks that matched the filter
-- criteria.
listDetectMitigationActionsTasksResponse_tasks :: Lens.Lens' ListDetectMitigationActionsTasksResponse (Prelude.Maybe [DetectMitigationActionsTaskSummary])
listDetectMitigationActionsTasksResponse_tasks = Lens.lens (\ListDetectMitigationActionsTasksResponse' {tasks} -> tasks) (\s@ListDetectMitigationActionsTasksResponse' {} a -> s {tasks = a} :: ListDetectMitigationActionsTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDetectMitigationActionsTasksResponse_httpStatus :: Lens.Lens' ListDetectMitigationActionsTasksResponse Prelude.Int
listDetectMitigationActionsTasksResponse_httpStatus = Lens.lens (\ListDetectMitigationActionsTasksResponse' {httpStatus} -> httpStatus) (\s@ListDetectMitigationActionsTasksResponse' {} a -> s {httpStatus = a} :: ListDetectMitigationActionsTasksResponse)

instance
  Prelude.NFData
    ListDetectMitigationActionsTasksResponse
  where
  rnf ListDetectMitigationActionsTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
