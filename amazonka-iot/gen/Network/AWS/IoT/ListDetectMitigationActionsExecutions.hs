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
-- Module      : Network.AWS.IoT.ListDetectMitigationActionsExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists mitigation actions executions for a Device Defender ML Detect
-- Security Profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDetectMitigationActionsExecutions
  ( -- * Creating a Request
    ListDetectMitigationActionsExecutions (..),
    newListDetectMitigationActionsExecutions,

    -- * Request Lenses
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_violationId,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_startTime,
    listDetectMitigationActionsExecutions_endTime,

    -- * Destructuring the Response
    ListDetectMitigationActionsExecutionsResponse (..),
    newListDetectMitigationActionsExecutionsResponse,

    -- * Response Lenses
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDetectMitigationActionsExecutions' smart constructor.
data ListDetectMitigationActionsExecutions = ListDetectMitigationActionsExecutions'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The unique identifier of the violation.
    violationId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the thing whose mitigation actions are listed.
    thingName :: Core.Maybe Core.Text,
    -- | The unique identifier of the task.
    taskId :: Core.Maybe Core.Text,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end of the time period for which ML Detect mitigation actions
    -- executions are returned.
    endTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDetectMitigationActionsExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectMitigationActionsExecutions_nextToken' - The token for the next set of results.
--
-- 'violationId', 'listDetectMitigationActionsExecutions_violationId' - The unique identifier of the violation.
--
-- 'maxResults', 'listDetectMitigationActionsExecutions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'thingName', 'listDetectMitigationActionsExecutions_thingName' - The name of the thing whose mitigation actions are listed.
--
-- 'taskId', 'listDetectMitigationActionsExecutions_taskId' - The unique identifier of the task.
--
-- 'startTime', 'listDetectMitigationActionsExecutions_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'endTime', 'listDetectMitigationActionsExecutions_endTime' - The end of the time period for which ML Detect mitigation actions
-- executions are returned.
newListDetectMitigationActionsExecutions ::
  ListDetectMitigationActionsExecutions
newListDetectMitigationActionsExecutions =
  ListDetectMitigationActionsExecutions'
    { nextToken =
        Core.Nothing,
      violationId = Core.Nothing,
      maxResults = Core.Nothing,
      thingName = Core.Nothing,
      taskId = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing
    }

-- | The token for the next set of results.
listDetectMitigationActionsExecutions_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.Text)
listDetectMitigationActionsExecutions_nextToken = Lens.lens (\ListDetectMitigationActionsExecutions' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutions' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutions)

-- | The unique identifier of the violation.
listDetectMitigationActionsExecutions_violationId :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.Text)
listDetectMitigationActionsExecutions_violationId = Lens.lens (\ListDetectMitigationActionsExecutions' {violationId} -> violationId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {violationId = a} :: ListDetectMitigationActionsExecutions)

-- | The maximum number of results to return at one time. The default is 25.
listDetectMitigationActionsExecutions_maxResults :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.Natural)
listDetectMitigationActionsExecutions_maxResults = Lens.lens (\ListDetectMitigationActionsExecutions' {maxResults} -> maxResults) (\s@ListDetectMitigationActionsExecutions' {} a -> s {maxResults = a} :: ListDetectMitigationActionsExecutions)

-- | The name of the thing whose mitigation actions are listed.
listDetectMitigationActionsExecutions_thingName :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.Text)
listDetectMitigationActionsExecutions_thingName = Lens.lens (\ListDetectMitigationActionsExecutions' {thingName} -> thingName) (\s@ListDetectMitigationActionsExecutions' {} a -> s {thingName = a} :: ListDetectMitigationActionsExecutions)

-- | The unique identifier of the task.
listDetectMitigationActionsExecutions_taskId :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.Text)
listDetectMitigationActionsExecutions_taskId = Lens.lens (\ListDetectMitigationActionsExecutions' {taskId} -> taskId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {taskId = a} :: ListDetectMitigationActionsExecutions)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listDetectMitigationActionsExecutions_startTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.UTCTime)
listDetectMitigationActionsExecutions_startTime = Lens.lens (\ListDetectMitigationActionsExecutions' {startTime} -> startTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {startTime = a} :: ListDetectMitigationActionsExecutions) Core.. Lens.mapping Core._Time

-- | The end of the time period for which ML Detect mitigation actions
-- executions are returned.
listDetectMitigationActionsExecutions_endTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Core.Maybe Core.UTCTime)
listDetectMitigationActionsExecutions_endTime = Lens.lens (\ListDetectMitigationActionsExecutions' {endTime} -> endTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {endTime = a} :: ListDetectMitigationActionsExecutions) Core.. Lens.mapping Core._Time

instance
  Core.AWSPager
    ListDetectMitigationActionsExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_actionsExecutions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDetectMitigationActionsExecutions_nextToken
          Lens..~ rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    ListDetectMitigationActionsExecutions
  where
  type
    AWSResponse
      ListDetectMitigationActionsExecutions =
      ListDetectMitigationActionsExecutionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectMitigationActionsExecutionsResponse'
            Core.<$> (x Core..?> "nextToken")
              Core.<*> (x Core..?> "actionsExecutions" Core..!@ Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListDetectMitigationActionsExecutions

instance
  Core.NFData
    ListDetectMitigationActionsExecutions

instance
  Core.ToHeaders
    ListDetectMitigationActionsExecutions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ListDetectMitigationActionsExecutions
  where
  toPath =
    Core.const "/detect/mitigationactions/executions"

instance
  Core.ToQuery
    ListDetectMitigationActionsExecutions
  where
  toQuery ListDetectMitigationActionsExecutions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "violationId" Core.=: violationId,
        "maxResults" Core.=: maxResults,
        "thingName" Core.=: thingName,
        "taskId" Core.=: taskId,
        "startTime" Core.=: startTime,
        "endTime" Core.=: endTime
      ]

-- | /See:/ 'newListDetectMitigationActionsExecutionsResponse' smart constructor.
data ListDetectMitigationActionsExecutionsResponse = ListDetectMitigationActionsExecutionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | List of actions executions.
    actionsExecutions :: Core.Maybe [DetectMitigationActionExecution],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDetectMitigationActionsExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectMitigationActionsExecutionsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'actionsExecutions', 'listDetectMitigationActionsExecutionsResponse_actionsExecutions' - List of actions executions.
--
-- 'httpStatus', 'listDetectMitigationActionsExecutionsResponse_httpStatus' - The response's http status code.
newListDetectMitigationActionsExecutionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDetectMitigationActionsExecutionsResponse
newListDetectMitigationActionsExecutionsResponse
  pHttpStatus_ =
    ListDetectMitigationActionsExecutionsResponse'
      { nextToken =
          Core.Nothing,
        actionsExecutions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listDetectMitigationActionsExecutionsResponse_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Core.Maybe Core.Text)
listDetectMitigationActionsExecutionsResponse_nextToken = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutionsResponse)

-- | List of actions executions.
listDetectMitigationActionsExecutionsResponse_actionsExecutions :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Core.Maybe [DetectMitigationActionExecution])
listDetectMitigationActionsExecutionsResponse_actionsExecutions = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {actionsExecutions} -> actionsExecutions) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {actionsExecutions = a} :: ListDetectMitigationActionsExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDetectMitigationActionsExecutionsResponse_httpStatus :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse Core.Int
listDetectMitigationActionsExecutionsResponse_httpStatus = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {httpStatus = a} :: ListDetectMitigationActionsExecutionsResponse)

instance
  Core.NFData
    ListDetectMitigationActionsExecutionsResponse
