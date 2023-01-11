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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listDetectMitigationActionsExecutions_endTime,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_startTime,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_violationId,

    -- * Destructuring the Response
    ListDetectMitigationActionsExecutionsResponse (..),
    newListDetectMitigationActionsExecutionsResponse,

    -- * Response Lenses
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectMitigationActionsExecutions' smart constructor.
data ListDetectMitigationActionsExecutions = ListDetectMitigationActionsExecutions'
  { -- | The end of the time period for which ML Detect mitigation actions
    -- executions are returned.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing whose mitigation actions are listed.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the violation.
    violationId :: Prelude.Maybe Prelude.Text
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
-- 'endTime', 'listDetectMitigationActionsExecutions_endTime' - The end of the time period for which ML Detect mitigation actions
-- executions are returned.
--
-- 'maxResults', 'listDetectMitigationActionsExecutions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'nextToken', 'listDetectMitigationActionsExecutions_nextToken' - The token for the next set of results.
--
-- 'startTime', 'listDetectMitigationActionsExecutions_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'taskId', 'listDetectMitigationActionsExecutions_taskId' - The unique identifier of the task.
--
-- 'thingName', 'listDetectMitigationActionsExecutions_thingName' - The name of the thing whose mitigation actions are listed.
--
-- 'violationId', 'listDetectMitigationActionsExecutions_violationId' - The unique identifier of the violation.
newListDetectMitigationActionsExecutions ::
  ListDetectMitigationActionsExecutions
newListDetectMitigationActionsExecutions =
  ListDetectMitigationActionsExecutions'
    { endTime =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      taskId = Prelude.Nothing,
      thingName = Prelude.Nothing,
      violationId = Prelude.Nothing
    }

-- | The end of the time period for which ML Detect mitigation actions
-- executions are returned.
listDetectMitigationActionsExecutions_endTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.UTCTime)
listDetectMitigationActionsExecutions_endTime = Lens.lens (\ListDetectMitigationActionsExecutions' {endTime} -> endTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {endTime = a} :: ListDetectMitigationActionsExecutions) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to return at one time. The default is 25.
listDetectMitigationActionsExecutions_maxResults :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Natural)
listDetectMitigationActionsExecutions_maxResults = Lens.lens (\ListDetectMitigationActionsExecutions' {maxResults} -> maxResults) (\s@ListDetectMitigationActionsExecutions' {} a -> s {maxResults = a} :: ListDetectMitigationActionsExecutions)

-- | The token for the next set of results.
listDetectMitigationActionsExecutions_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_nextToken = Lens.lens (\ListDetectMitigationActionsExecutions' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutions' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutions)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listDetectMitigationActionsExecutions_startTime :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.UTCTime)
listDetectMitigationActionsExecutions_startTime = Lens.lens (\ListDetectMitigationActionsExecutions' {startTime} -> startTime) (\s@ListDetectMitigationActionsExecutions' {} a -> s {startTime = a} :: ListDetectMitigationActionsExecutions) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the task.
listDetectMitigationActionsExecutions_taskId :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_taskId = Lens.lens (\ListDetectMitigationActionsExecutions' {taskId} -> taskId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {taskId = a} :: ListDetectMitigationActionsExecutions)

-- | The name of the thing whose mitigation actions are listed.
listDetectMitigationActionsExecutions_thingName :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_thingName = Lens.lens (\ListDetectMitigationActionsExecutions' {thingName} -> thingName) (\s@ListDetectMitigationActionsExecutions' {} a -> s {thingName = a} :: ListDetectMitigationActionsExecutions)

-- | The unique identifier of the violation.
listDetectMitigationActionsExecutions_violationId :: Lens.Lens' ListDetectMitigationActionsExecutions (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutions_violationId = Lens.lens (\ListDetectMitigationActionsExecutions' {violationId} -> violationId) (\s@ListDetectMitigationActionsExecutions' {} a -> s {violationId = a} :: ListDetectMitigationActionsExecutions)

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
            Prelude.<$> ( x Data..?> "actionsExecutions"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDetectMitigationActionsExecutions
  where
  hashWithSalt
    _salt
    ListDetectMitigationActionsExecutions' {..} =
      _salt `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` thingName
        `Prelude.hashWithSalt` violationId

instance
  Prelude.NFData
    ListDetectMitigationActionsExecutions
  where
  rnf ListDetectMitigationActionsExecutions' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf violationId

instance
  Data.ToHeaders
    ListDetectMitigationActionsExecutions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListDetectMitigationActionsExecutions
  where
  toPath =
    Prelude.const
      "/detect/mitigationactions/executions"

instance
  Data.ToQuery
    ListDetectMitigationActionsExecutions
  where
  toQuery ListDetectMitigationActionsExecutions' {..} =
    Prelude.mconcat
      [ "endTime" Data.=: endTime,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "startTime" Data.=: startTime,
        "taskId" Data.=: taskId,
        "thingName" Data.=: thingName,
        "violationId" Data.=: violationId
      ]

-- | /See:/ 'newListDetectMitigationActionsExecutionsResponse' smart constructor.
data ListDetectMitigationActionsExecutionsResponse = ListDetectMitigationActionsExecutionsResponse'
  { -- | List of actions executions.
    actionsExecutions :: Prelude.Maybe [DetectMitigationActionExecution],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'actionsExecutions', 'listDetectMitigationActionsExecutionsResponse_actionsExecutions' - List of actions executions.
--
-- 'nextToken', 'listDetectMitigationActionsExecutionsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'httpStatus', 'listDetectMitigationActionsExecutionsResponse_httpStatus' - The response's http status code.
newListDetectMitigationActionsExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectMitigationActionsExecutionsResponse
newListDetectMitigationActionsExecutionsResponse
  pHttpStatus_ =
    ListDetectMitigationActionsExecutionsResponse'
      { actionsExecutions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | List of actions executions.
listDetectMitigationActionsExecutionsResponse_actionsExecutions :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Prelude.Maybe [DetectMitigationActionExecution])
listDetectMitigationActionsExecutionsResponse_actionsExecutions = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {actionsExecutions} -> actionsExecutions) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {actionsExecutions = a} :: ListDetectMitigationActionsExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listDetectMitigationActionsExecutionsResponse_nextToken :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse (Prelude.Maybe Prelude.Text)
listDetectMitigationActionsExecutionsResponse_nextToken = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {nextToken} -> nextToken) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {nextToken = a} :: ListDetectMitigationActionsExecutionsResponse)

-- | The response's http status code.
listDetectMitigationActionsExecutionsResponse_httpStatus :: Lens.Lens' ListDetectMitigationActionsExecutionsResponse Prelude.Int
listDetectMitigationActionsExecutionsResponse_httpStatus = Lens.lens (\ListDetectMitigationActionsExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListDetectMitigationActionsExecutionsResponse' {} a -> s {httpStatus = a} :: ListDetectMitigationActionsExecutionsResponse)

instance
  Prelude.NFData
    ListDetectMitigationActionsExecutionsResponse
  where
  rnf
    ListDetectMitigationActionsExecutionsResponse' {..} =
      Prelude.rnf actionsExecutions
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
