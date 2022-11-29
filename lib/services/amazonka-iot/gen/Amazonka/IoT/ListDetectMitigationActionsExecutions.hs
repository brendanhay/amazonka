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
-- Module      : Amazonka.IoT.ListDetectMitigationActionsExecutions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists mitigation actions executions for a Device Defender ML Detect
-- Security Profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListDetectMitigationActionsExecutions>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListDetectMitigationActionsExecutions
  ( -- * Creating a Request
    ListDetectMitigationActionsExecutions (..),
    newListDetectMitigationActionsExecutions,

    -- * Request Lenses
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_violationId,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_endTime,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutions_startTime,

    -- * Destructuring the Response
    ListDetectMitigationActionsExecutionsResponse (..),
    newListDetectMitigationActionsExecutionsResponse,

    -- * Response Lenses
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectMitigationActionsExecutions' smart constructor.
data ListDetectMitigationActionsExecutions = ListDetectMitigationActionsExecutions'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing whose mitigation actions are listed.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the violation.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The end of the time period for which ML Detect mitigation actions
    -- executions are returned.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'thingName', 'listDetectMitigationActionsExecutions_thingName' - The name of the thing whose mitigation actions are listed.
--
-- 'violationId', 'listDetectMitigationActionsExecutions_violationId' - The unique identifier of the violation.
--
-- 'taskId', 'listDetectMitigationActionsExecutions_taskId' - The unique identifier of the task.
--
-- 'endTime', 'listDetectMitigationActionsExecutions_endTime' - The end of the time period for which ML Detect mitigation actions
-- executions are returned.
--
-- 'maxResults', 'listDetectMitigationActionsExecutions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'startTime', 'listDetectMitigationActionsExecutions_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
newListDetectMitigationActionsExecutions ::
  ListDetectMitigationActionsExecutions
newListDetectMitigationActionsExecutions =
  ListDetectMitigationActionsExecutions'
    { nextToken =
        Prelude.Nothing,
      thingName = Prelude.Nothing,
      violationId = Prelude.Nothing,
      taskId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The token for the next set of results.
listDetectMitigationActionsExecutions_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_nextToken = Lens.lens (\ListDetectMitigationActionsExecutions' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutions' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutions)

-- | The name of the thing whose mitigation actions are listed.
listDetectMitigationActionsExecutions_thingName :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_thingName = Lens.lens (\ListDetectMitigationActionsExecutions' {thingName} -> thingName) (\s@ListDetectMitigationActionsExecutions' {} a -> s {thingName = a} :: ListDetectMitigationActionsExecutions)

-- | The unique identifier of the violation.
listDetectMitigationActionsExecutions_violationId :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_violationId = Lens.lens (\ListDetectMitigationActionsExecutions' {violationId} -> violationId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {violationId = a} :: ListDetectMitigationActionsExecutions)

-- | The unique identifier of the task.
listDetectMitigationActionsExecutions_taskId :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_taskId = Lens.lens (\ListDetectMitigationActionsExecutions' {taskId} -> taskId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {taskId = a} :: ListDetectMitigationActionsExecutions)

-- | The end of the time period for which ML Detect mitigation actions
-- executions are returned.
listDetectMitigationActionsExecutions_endTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.UTCTime)
listDetectMitigationActionsExecutions_endTime = Lens.lens (\ListDetectMitigationActionsExecutions' {endTime} -> endTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {endTime = a} :: ListDetectMitigationActionsExecutions) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return at one time. The default is 25.
listDetectMitigationActionsExecutions_maxResults :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Natural)
listDetectMitigationActionsExecutions_maxResults = Lens.lens (\ListDetectMitigationActionsExecutions' {maxResults} -> maxResults) (\s@ListDetectMitigationActionsExecutions' {} a -> s {maxResults = a} :: ListDetectMitigationActionsExecutions)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listDetectMitigationActionsExecutions_startTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.UTCTime)
listDetectMitigationActionsExecutions_startTime = Lens.lens (\ListDetectMitigationActionsExecutions' {startTime} -> startTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {startTime = a} :: ListDetectMitigationActionsExecutions) Prelude.. Lens.mapping Core._Time

instance
  Core.AWSPager
    ListDetectMitigationActionsExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_actionsExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDetectMitigationActionsExecutions_nextToken
          Lens..~ rs
            Lens.^? listDetectMitigationActionsExecutionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDetectMitigationActionsExecutions
  where
  type
    AWSResponse
      ListDetectMitigationActionsExecutions =
      ListDetectMitigationActionsExecutionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectMitigationActionsExecutionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> ( x Core..?> "actionsExecutions"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDetectMitigationActionsExecutions
  where
  hashWithSalt
    _salt
    ListDetectMitigationActionsExecutions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` thingName
        `Prelude.hashWithSalt` violationId
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` startTime

instance
  Prelude.NFData
    ListDetectMitigationActionsExecutions
  where
  rnf ListDetectMitigationActionsExecutions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startTime

instance
  Core.ToHeaders
    ListDetectMitigationActionsExecutions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListDetectMitigationActionsExecutions
  where
  toPath =
    Prelude.const
      "/detect/mitigationactions/executions"

instance
  Core.ToQuery
    ListDetectMitigationActionsExecutions
  where
  toQuery ListDetectMitigationActionsExecutions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "thingName" Core.=: thingName,
        "violationId" Core.=: violationId,
        "taskId" Core.=: taskId,
        "endTime" Core.=: endTime,
        "maxResults" Core.=: maxResults,
        "startTime" Core.=: startTime
      ]

-- | /See:/ 'newListDetectMitigationActionsExecutionsResponse' smart constructor.
data ListDetectMitigationActionsExecutionsResponse = ListDetectMitigationActionsExecutionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of actions executions.
    actionsExecutions :: Prelude.Maybe [DetectMitigationActionExecution],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDetectMitigationActionsExecutionsResponse
newListDetectMitigationActionsExecutionsResponse
  pHttpStatus_ =
    ListDetectMitigationActionsExecutionsResponse'
      { nextToken =
          Prelude.Nothing,
        actionsExecutions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listDetectMitigationActionsExecutionsResponse_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutionsResponse_nextToken = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutionsResponse)

-- | List of actions executions.
listDetectMitigationActionsExecutionsResponse_actionsExecutions :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Prelude.Maybe [DetectMitigationActionExecution])
listDetectMitigationActionsExecutionsResponse_actionsExecutions = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {actionsExecutions} -> actionsExecutions) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {actionsExecutions = a} :: ListDetectMitigationActionsExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDetectMitigationActionsExecutionsResponse_httpStatus :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse Prelude.Int
listDetectMitigationActionsExecutionsResponse_httpStatus = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {httpStatus = a} :: ListDetectMitigationActionsExecutionsResponse)

instance
  Prelude.NFData
    ListDetectMitigationActionsExecutionsResponse
  where
  rnf
    ListDetectMitigationActionsExecutionsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf actionsExecutions
        `Prelude.seq` Prelude.rnf httpStatus
