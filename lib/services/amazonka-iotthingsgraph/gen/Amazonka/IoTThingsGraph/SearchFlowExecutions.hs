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
-- Module      : Amazonka.IoTThingsGraph.SearchFlowExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for AWS IoT Things Graph workflow execution instances.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchFlowExecutions
  ( -- * Creating a Request
    SearchFlowExecutions (..),
    newSearchFlowExecutions,

    -- * Request Lenses
    searchFlowExecutions_nextToken,
    searchFlowExecutions_endTime,
    searchFlowExecutions_maxResults,
    searchFlowExecutions_flowExecutionId,
    searchFlowExecutions_startTime,
    searchFlowExecutions_systemInstanceId,

    -- * Destructuring the Response
    SearchFlowExecutionsResponse (..),
    newSearchFlowExecutionsResponse,

    -- * Response Lenses
    searchFlowExecutionsResponse_nextToken,
    searchFlowExecutionsResponse_summaries,
    searchFlowExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchFlowExecutions' smart constructor.
data SearchFlowExecutions = SearchFlowExecutions'
  { -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the latest flow execution to return.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of a flow execution.
    flowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the earliest flow execution to return.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the system instance that contains the flow.
    systemInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFlowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFlowExecutions_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'endTime', 'searchFlowExecutions_endTime' - The date and time of the latest flow execution to return.
--
-- 'maxResults', 'searchFlowExecutions_maxResults' - The maximum number of results to return in the response.
--
-- 'flowExecutionId', 'searchFlowExecutions_flowExecutionId' - The ID of a flow execution.
--
-- 'startTime', 'searchFlowExecutions_startTime' - The date and time of the earliest flow execution to return.
--
-- 'systemInstanceId', 'searchFlowExecutions_systemInstanceId' - The ID of the system instance that contains the flow.
newSearchFlowExecutions ::
  -- | 'systemInstanceId'
  Prelude.Text ->
  SearchFlowExecutions
newSearchFlowExecutions pSystemInstanceId_ =
  SearchFlowExecutions'
    { nextToken = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      flowExecutionId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      systemInstanceId = pSystemInstanceId_
    }

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchFlowExecutions_nextToken :: Lens.Lens' SearchFlowExecutions (Prelude.Maybe Prelude.Text)
searchFlowExecutions_nextToken = Lens.lens (\SearchFlowExecutions' {nextToken} -> nextToken) (\s@SearchFlowExecutions' {} a -> s {nextToken = a} :: SearchFlowExecutions)

-- | The date and time of the latest flow execution to return.
searchFlowExecutions_endTime :: Lens.Lens' SearchFlowExecutions (Prelude.Maybe Prelude.UTCTime)
searchFlowExecutions_endTime = Lens.lens (\SearchFlowExecutions' {endTime} -> endTime) (\s@SearchFlowExecutions' {} a -> s {endTime = a} :: SearchFlowExecutions) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return in the response.
searchFlowExecutions_maxResults :: Lens.Lens' SearchFlowExecutions (Prelude.Maybe Prelude.Natural)
searchFlowExecutions_maxResults = Lens.lens (\SearchFlowExecutions' {maxResults} -> maxResults) (\s@SearchFlowExecutions' {} a -> s {maxResults = a} :: SearchFlowExecutions)

-- | The ID of a flow execution.
searchFlowExecutions_flowExecutionId :: Lens.Lens' SearchFlowExecutions (Prelude.Maybe Prelude.Text)
searchFlowExecutions_flowExecutionId = Lens.lens (\SearchFlowExecutions' {flowExecutionId} -> flowExecutionId) (\s@SearchFlowExecutions' {} a -> s {flowExecutionId = a} :: SearchFlowExecutions)

-- | The date and time of the earliest flow execution to return.
searchFlowExecutions_startTime :: Lens.Lens' SearchFlowExecutions (Prelude.Maybe Prelude.UTCTime)
searchFlowExecutions_startTime = Lens.lens (\SearchFlowExecutions' {startTime} -> startTime) (\s@SearchFlowExecutions' {} a -> s {startTime = a} :: SearchFlowExecutions) Prelude.. Lens.mapping Core._Time

-- | The ID of the system instance that contains the flow.
searchFlowExecutions_systemInstanceId :: Lens.Lens' SearchFlowExecutions Prelude.Text
searchFlowExecutions_systemInstanceId = Lens.lens (\SearchFlowExecutions' {systemInstanceId} -> systemInstanceId) (\s@SearchFlowExecutions' {} a -> s {systemInstanceId = a} :: SearchFlowExecutions)

instance Core.AWSPager SearchFlowExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchFlowExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchFlowExecutionsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchFlowExecutions_nextToken
          Lens..~ rs
          Lens.^? searchFlowExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchFlowExecutions where
  type
    AWSResponse SearchFlowExecutions =
      SearchFlowExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFlowExecutionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchFlowExecutions where
  hashWithSalt _salt SearchFlowExecutions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` flowExecutionId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` systemInstanceId

instance Prelude.NFData SearchFlowExecutions where
  rnf SearchFlowExecutions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf flowExecutionId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf systemInstanceId

instance Core.ToHeaders SearchFlowExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchFlowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchFlowExecutions where
  toJSON SearchFlowExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("flowExecutionId" Core..=)
              Prelude.<$> flowExecutionId,
            ("startTime" Core..=) Prelude.<$> startTime,
            Prelude.Just
              ("systemInstanceId" Core..= systemInstanceId)
          ]
      )

instance Core.ToPath SearchFlowExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchFlowExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchFlowExecutionsResponse' smart constructor.
data SearchFlowExecutionsResponse = SearchFlowExecutionsResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain summary information about each workflow
    -- execution in the result set.
    summaries :: Prelude.Maybe [FlowExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFlowExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFlowExecutionsResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'searchFlowExecutionsResponse_summaries' - An array of objects that contain summary information about each workflow
-- execution in the result set.
--
-- 'httpStatus', 'searchFlowExecutionsResponse_httpStatus' - The response's http status code.
newSearchFlowExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchFlowExecutionsResponse
newSearchFlowExecutionsResponse pHttpStatus_ =
  SearchFlowExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchFlowExecutionsResponse_nextToken :: Lens.Lens' SearchFlowExecutionsResponse (Prelude.Maybe Prelude.Text)
searchFlowExecutionsResponse_nextToken = Lens.lens (\SearchFlowExecutionsResponse' {nextToken} -> nextToken) (\s@SearchFlowExecutionsResponse' {} a -> s {nextToken = a} :: SearchFlowExecutionsResponse)

-- | An array of objects that contain summary information about each workflow
-- execution in the result set.
searchFlowExecutionsResponse_summaries :: Lens.Lens' SearchFlowExecutionsResponse (Prelude.Maybe [FlowExecutionSummary])
searchFlowExecutionsResponse_summaries = Lens.lens (\SearchFlowExecutionsResponse' {summaries} -> summaries) (\s@SearchFlowExecutionsResponse' {} a -> s {summaries = a} :: SearchFlowExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchFlowExecutionsResponse_httpStatus :: Lens.Lens' SearchFlowExecutionsResponse Prelude.Int
searchFlowExecutionsResponse_httpStatus = Lens.lens (\SearchFlowExecutionsResponse' {httpStatus} -> httpStatus) (\s@SearchFlowExecutionsResponse' {} a -> s {httpStatus = a} :: SearchFlowExecutionsResponse)

instance Prelude.NFData SearchFlowExecutionsResponse where
  rnf SearchFlowExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus
