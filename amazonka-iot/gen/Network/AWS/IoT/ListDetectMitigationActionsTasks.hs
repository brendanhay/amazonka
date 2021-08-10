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
-- Module      : Network.AWS.IoT.ListDetectMitigationActionsTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List of Device Defender ML Detect mitigation actions tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDetectMitigationActionsTasks
  ( -- * Creating a Request
    ListDetectMitigationActionsTasks (..),
    newListDetectMitigationActionsTasks,

    -- * Request Lenses
    listDetectMitigationActionsTasks_nextToken,
    listDetectMitigationActionsTasks_maxResults,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDetectMitigationActionsTasks' smart constructor.
data ListDetectMitigationActionsTasks = ListDetectMitigationActionsTasks'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Core.POSIX,
    -- | The end of the time period for which ML Detect mitigation actions tasks
    -- are returned.
    endTime :: Core.POSIX
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
-- 'nextToken', 'listDetectMitigationActionsTasks_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listDetectMitigationActionsTasks_maxResults' - The maximum number of results to return at one time. The default is 25.
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
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The token for the next set of results.
listDetectMitigationActionsTasks_nextToken :: Lens.Lens' ListDetectMitigationActionsTasks (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsTasks_nextToken = Lens.lens (\ListDetectMitigationActionsTasks' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsTasks' {} a -> s {nextToken = a} :: ListDetectMitigationActionsTasks)

-- | The maximum number of results to return at one time. The default is 25.
listDetectMitigationActionsTasks_maxResults :: Lens.Lens' ListDetectMitigationActionsTasks (Prelude.Maybe Prelude.Natural)
listDetectMitigationActionsTasks_maxResults = Lens.lens (\ListDetectMitigationActionsTasks' {maxResults} -> maxResults) (\s@ListDetectMitigationActionsTasks' {} a -> s {maxResults = a} :: ListDetectMitigationActionsTasks)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listDetectMitigationActionsTasks_startTime :: Lens.Lens' ListDetectMitigationActionsTasks Prelude.UTCTime
listDetectMitigationActionsTasks_startTime = Lens.lens (\ListDetectMitigationActionsTasks' {startTime} -> startTime) (\s@ListDetectMitigationActionsTasks' {} a -> s {startTime = a} :: ListDetectMitigationActionsTasks) Prelude.. Core._Time

-- | The end of the time period for which ML Detect mitigation actions tasks
-- are returned.
listDetectMitigationActionsTasks_endTime :: Lens.Lens' ListDetectMitigationActionsTasks Prelude.UTCTime
listDetectMitigationActionsTasks_endTime = Lens.lens (\ListDetectMitigationActionsTasks' {endTime} -> endTime) (\s@ListDetectMitigationActionsTasks' {} a -> s {endTime = a} :: ListDetectMitigationActionsTasks) Prelude.. Core._Time

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
      Prelude.Just Prelude.$
        rq
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectMitigationActionsTasksResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDetectMitigationActionsTasks

instance
  Prelude.NFData
    ListDetectMitigationActionsTasks

instance
  Core.ToHeaders
    ListDetectMitigationActionsTasks
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDetectMitigationActionsTasks where
  toPath =
    Prelude.const "/detect/mitigationactions/tasks"

instance
  Core.ToQuery
    ListDetectMitigationActionsTasks
  where
  toQuery ListDetectMitigationActionsTasks' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "startTime" Core.=: startTime,
        "endTime" Core.=: endTime
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
listDetectMitigationActionsTasksResponse_tasks = Lens.lens (\ListDetectMitigationActionsTasksResponse' {tasks} -> tasks) (\s@ListDetectMitigationActionsTasksResponse' {} a -> s {tasks = a} :: ListDetectMitigationActionsTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDetectMitigationActionsTasksResponse_httpStatus :: Lens.Lens' ListDetectMitigationActionsTasksResponse Prelude.Int
listDetectMitigationActionsTasksResponse_httpStatus = Lens.lens (\ListDetectMitigationActionsTasksResponse' {httpStatus} -> httpStatus) (\s@ListDetectMitigationActionsTasksResponse' {} a -> s {httpStatus = a} :: ListDetectMitigationActionsTasksResponse)

instance
  Prelude.NFData
    ListDetectMitigationActionsTasksResponse
