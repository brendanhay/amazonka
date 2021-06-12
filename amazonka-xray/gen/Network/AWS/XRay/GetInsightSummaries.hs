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
-- Module      : Network.AWS.XRay.GetInsightSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summaries of all insights in the specified group matching
-- the provided filter values.
module Network.AWS.XRay.GetInsightSummaries
  ( -- * Creating a Request
    GetInsightSummaries (..),
    newGetInsightSummaries,

    -- * Request Lenses
    getInsightSummaries_nextToken,
    getInsightSummaries_states,
    getInsightSummaries_maxResults,
    getInsightSummaries_groupName,
    getInsightSummaries_groupARN,
    getInsightSummaries_startTime,
    getInsightSummaries_endTime,

    -- * Destructuring the Response
    GetInsightSummariesResponse (..),
    newGetInsightSummariesResponse,

    -- * Response Lenses
    getInsightSummariesResponse_insightSummaries,
    getInsightSummariesResponse_nextToken,
    getInsightSummariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetInsightSummaries' smart constructor.
data GetInsightSummaries = GetInsightSummaries'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of insight states.
    states :: Core.Maybe [InsightState],
    -- | The maximum number of results to display.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the group. Required if the GroupARN isn\'t provided.
    groupName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the group. Required if the GroupName
    -- isn\'t provided.
    groupARN :: Core.Maybe Core.Text,
    -- | The beginning of the time frame in which the insights started. The start
    -- time can\'t be more than 30 days old.
    startTime :: Core.POSIX,
    -- | The end of the time frame in which the insights ended. The end time
    -- can\'t be more than 30 days old.
    endTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInsightSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInsightSummaries_nextToken' - Pagination token.
--
-- 'states', 'getInsightSummaries_states' - The list of insight states.
--
-- 'maxResults', 'getInsightSummaries_maxResults' - The maximum number of results to display.
--
-- 'groupName', 'getInsightSummaries_groupName' - The name of the group. Required if the GroupARN isn\'t provided.
--
-- 'groupARN', 'getInsightSummaries_groupARN' - The Amazon Resource Name (ARN) of the group. Required if the GroupName
-- isn\'t provided.
--
-- 'startTime', 'getInsightSummaries_startTime' - The beginning of the time frame in which the insights started. The start
-- time can\'t be more than 30 days old.
--
-- 'endTime', 'getInsightSummaries_endTime' - The end of the time frame in which the insights ended. The end time
-- can\'t be more than 30 days old.
newGetInsightSummaries ::
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  GetInsightSummaries
newGetInsightSummaries pStartTime_ pEndTime_ =
  GetInsightSummaries'
    { nextToken = Core.Nothing,
      states = Core.Nothing,
      maxResults = Core.Nothing,
      groupName = Core.Nothing,
      groupARN = Core.Nothing,
      startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | Pagination token.
getInsightSummaries_nextToken :: Lens.Lens' GetInsightSummaries (Core.Maybe Core.Text)
getInsightSummaries_nextToken = Lens.lens (\GetInsightSummaries' {nextToken} -> nextToken) (\s@GetInsightSummaries' {} a -> s {nextToken = a} :: GetInsightSummaries)

-- | The list of insight states.
getInsightSummaries_states :: Lens.Lens' GetInsightSummaries (Core.Maybe [InsightState])
getInsightSummaries_states = Lens.lens (\GetInsightSummaries' {states} -> states) (\s@GetInsightSummaries' {} a -> s {states = a} :: GetInsightSummaries) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to display.
getInsightSummaries_maxResults :: Lens.Lens' GetInsightSummaries (Core.Maybe Core.Natural)
getInsightSummaries_maxResults = Lens.lens (\GetInsightSummaries' {maxResults} -> maxResults) (\s@GetInsightSummaries' {} a -> s {maxResults = a} :: GetInsightSummaries)

-- | The name of the group. Required if the GroupARN isn\'t provided.
getInsightSummaries_groupName :: Lens.Lens' GetInsightSummaries (Core.Maybe Core.Text)
getInsightSummaries_groupName = Lens.lens (\GetInsightSummaries' {groupName} -> groupName) (\s@GetInsightSummaries' {} a -> s {groupName = a} :: GetInsightSummaries)

-- | The Amazon Resource Name (ARN) of the group. Required if the GroupName
-- isn\'t provided.
getInsightSummaries_groupARN :: Lens.Lens' GetInsightSummaries (Core.Maybe Core.Text)
getInsightSummaries_groupARN = Lens.lens (\GetInsightSummaries' {groupARN} -> groupARN) (\s@GetInsightSummaries' {} a -> s {groupARN = a} :: GetInsightSummaries)

-- | The beginning of the time frame in which the insights started. The start
-- time can\'t be more than 30 days old.
getInsightSummaries_startTime :: Lens.Lens' GetInsightSummaries Core.UTCTime
getInsightSummaries_startTime = Lens.lens (\GetInsightSummaries' {startTime} -> startTime) (\s@GetInsightSummaries' {} a -> s {startTime = a} :: GetInsightSummaries) Core.. Core._Time

-- | The end of the time frame in which the insights ended. The end time
-- can\'t be more than 30 days old.
getInsightSummaries_endTime :: Lens.Lens' GetInsightSummaries Core.UTCTime
getInsightSummaries_endTime = Lens.lens (\GetInsightSummaries' {endTime} -> endTime) (\s@GetInsightSummaries' {} a -> s {endTime = a} :: GetInsightSummaries) Core.. Core._Time

instance Core.AWSRequest GetInsightSummaries where
  type
    AWSResponse GetInsightSummaries =
      GetInsightSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSummariesResponse'
            Core.<$> (x Core..?> "InsightSummaries" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInsightSummaries

instance Core.NFData GetInsightSummaries

instance Core.ToHeaders GetInsightSummaries where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetInsightSummaries where
  toJSON GetInsightSummaries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("States" Core..=) Core.<$> states,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("GroupName" Core..=) Core.<$> groupName,
            ("GroupARN" Core..=) Core.<$> groupARN,
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetInsightSummaries where
  toPath = Core.const "/InsightSummaries"

instance Core.ToQuery GetInsightSummaries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInsightSummariesResponse' smart constructor.
data GetInsightSummariesResponse = GetInsightSummariesResponse'
  { -- | The summary of each insight within the group matching the provided
    -- filters. The summary contains the InsightID, start and end time, the
    -- root cause service, the root cause and client impact statistics, the top
    -- anomalous services, and the status of the insight.
    insightSummaries :: Core.Maybe [InsightSummary],
    -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInsightSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightSummaries', 'getInsightSummariesResponse_insightSummaries' - The summary of each insight within the group matching the provided
-- filters. The summary contains the InsightID, start and end time, the
-- root cause service, the root cause and client impact statistics, the top
-- anomalous services, and the status of the insight.
--
-- 'nextToken', 'getInsightSummariesResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'getInsightSummariesResponse_httpStatus' - The response's http status code.
newGetInsightSummariesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInsightSummariesResponse
newGetInsightSummariesResponse pHttpStatus_ =
  GetInsightSummariesResponse'
    { insightSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary of each insight within the group matching the provided
-- filters. The summary contains the InsightID, start and end time, the
-- root cause service, the root cause and client impact statistics, the top
-- anomalous services, and the status of the insight.
getInsightSummariesResponse_insightSummaries :: Lens.Lens' GetInsightSummariesResponse (Core.Maybe [InsightSummary])
getInsightSummariesResponse_insightSummaries = Lens.lens (\GetInsightSummariesResponse' {insightSummaries} -> insightSummaries) (\s@GetInsightSummariesResponse' {} a -> s {insightSummaries = a} :: GetInsightSummariesResponse) Core.. Lens.mapping Lens._Coerce

-- | Pagination token.
getInsightSummariesResponse_nextToken :: Lens.Lens' GetInsightSummariesResponse (Core.Maybe Core.Text)
getInsightSummariesResponse_nextToken = Lens.lens (\GetInsightSummariesResponse' {nextToken} -> nextToken) (\s@GetInsightSummariesResponse' {} a -> s {nextToken = a} :: GetInsightSummariesResponse)

-- | The response's http status code.
getInsightSummariesResponse_httpStatus :: Lens.Lens' GetInsightSummariesResponse Core.Int
getInsightSummariesResponse_httpStatus = Lens.lens (\GetInsightSummariesResponse' {httpStatus} -> httpStatus) (\s@GetInsightSummariesResponse' {} a -> s {httpStatus = a} :: GetInsightSummariesResponse)

instance Core.NFData GetInsightSummariesResponse
