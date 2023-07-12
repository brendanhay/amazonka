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
-- Module      : Amazonka.XRay.GetInsightSummaries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summaries of all insights in the specified group matching
-- the provided filter values.
module Amazonka.XRay.GetInsightSummaries
  ( -- * Creating a Request
    GetInsightSummaries (..),
    newGetInsightSummaries,

    -- * Request Lenses
    getInsightSummaries_groupARN,
    getInsightSummaries_groupName,
    getInsightSummaries_maxResults,
    getInsightSummaries_nextToken,
    getInsightSummaries_states,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetInsightSummaries' smart constructor.
data GetInsightSummaries = GetInsightSummaries'
  { -- | The Amazon Resource Name (ARN) of the group. Required if the GroupName
    -- isn\'t provided.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the group. Required if the GroupARN isn\'t provided.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to display.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of insight states.
    states :: Prelude.Maybe [InsightState],
    -- | The beginning of the time frame in which the insights started. The start
    -- time can\'t be more than 30 days old.
    startTime :: Data.POSIX,
    -- | The end of the time frame in which the insights ended. The end time
    -- can\'t be more than 30 days old.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupARN', 'getInsightSummaries_groupARN' - The Amazon Resource Name (ARN) of the group. Required if the GroupName
-- isn\'t provided.
--
-- 'groupName', 'getInsightSummaries_groupName' - The name of the group. Required if the GroupARN isn\'t provided.
--
-- 'maxResults', 'getInsightSummaries_maxResults' - The maximum number of results to display.
--
-- 'nextToken', 'getInsightSummaries_nextToken' - Pagination token.
--
-- 'states', 'getInsightSummaries_states' - The list of insight states.
--
-- 'startTime', 'getInsightSummaries_startTime' - The beginning of the time frame in which the insights started. The start
-- time can\'t be more than 30 days old.
--
-- 'endTime', 'getInsightSummaries_endTime' - The end of the time frame in which the insights ended. The end time
-- can\'t be more than 30 days old.
newGetInsightSummaries ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetInsightSummaries
newGetInsightSummaries pStartTime_ pEndTime_ =
  GetInsightSummaries'
    { groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      states = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The Amazon Resource Name (ARN) of the group. Required if the GroupName
-- isn\'t provided.
getInsightSummaries_groupARN :: Lens.Lens' GetInsightSummaries (Prelude.Maybe Prelude.Text)
getInsightSummaries_groupARN = Lens.lens (\GetInsightSummaries' {groupARN} -> groupARN) (\s@GetInsightSummaries' {} a -> s {groupARN = a} :: GetInsightSummaries)

-- | The name of the group. Required if the GroupARN isn\'t provided.
getInsightSummaries_groupName :: Lens.Lens' GetInsightSummaries (Prelude.Maybe Prelude.Text)
getInsightSummaries_groupName = Lens.lens (\GetInsightSummaries' {groupName} -> groupName) (\s@GetInsightSummaries' {} a -> s {groupName = a} :: GetInsightSummaries)

-- | The maximum number of results to display.
getInsightSummaries_maxResults :: Lens.Lens' GetInsightSummaries (Prelude.Maybe Prelude.Natural)
getInsightSummaries_maxResults = Lens.lens (\GetInsightSummaries' {maxResults} -> maxResults) (\s@GetInsightSummaries' {} a -> s {maxResults = a} :: GetInsightSummaries)

-- | Pagination token.
getInsightSummaries_nextToken :: Lens.Lens' GetInsightSummaries (Prelude.Maybe Prelude.Text)
getInsightSummaries_nextToken = Lens.lens (\GetInsightSummaries' {nextToken} -> nextToken) (\s@GetInsightSummaries' {} a -> s {nextToken = a} :: GetInsightSummaries)

-- | The list of insight states.
getInsightSummaries_states :: Lens.Lens' GetInsightSummaries (Prelude.Maybe [InsightState])
getInsightSummaries_states = Lens.lens (\GetInsightSummaries' {states} -> states) (\s@GetInsightSummaries' {} a -> s {states = a} :: GetInsightSummaries) Prelude.. Lens.mapping Lens.coerced

-- | The beginning of the time frame in which the insights started. The start
-- time can\'t be more than 30 days old.
getInsightSummaries_startTime :: Lens.Lens' GetInsightSummaries Prelude.UTCTime
getInsightSummaries_startTime = Lens.lens (\GetInsightSummaries' {startTime} -> startTime) (\s@GetInsightSummaries' {} a -> s {startTime = a} :: GetInsightSummaries) Prelude.. Data._Time

-- | The end of the time frame in which the insights ended. The end time
-- can\'t be more than 30 days old.
getInsightSummaries_endTime :: Lens.Lens' GetInsightSummaries Prelude.UTCTime
getInsightSummaries_endTime = Lens.lens (\GetInsightSummaries' {endTime} -> endTime) (\s@GetInsightSummaries' {} a -> s {endTime = a} :: GetInsightSummaries) Prelude.. Data._Time

instance Core.AWSRequest GetInsightSummaries where
  type
    AWSResponse GetInsightSummaries =
      GetInsightSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSummariesResponse'
            Prelude.<$> ( x
                            Data..?> "InsightSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightSummaries where
  hashWithSalt _salt GetInsightSummaries' {..} =
    _salt
      `Prelude.hashWithSalt` groupARN
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetInsightSummaries where
  rnf GetInsightSummaries' {..} =
    Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf states
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetInsightSummaries where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetInsightSummaries where
  toJSON GetInsightSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupARN" Data..=) Prelude.<$> groupARN,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("States" Data..=) Prelude.<$> states,
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetInsightSummaries where
  toPath = Prelude.const "/InsightSummaries"

instance Data.ToQuery GetInsightSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightSummariesResponse' smart constructor.
data GetInsightSummariesResponse = GetInsightSummariesResponse'
  { -- | The summary of each insight within the group matching the provided
    -- filters. The summary contains the InsightID, start and end time, the
    -- root cause service, the root cause and client impact statistics, the top
    -- anomalous services, and the status of the insight.
    insightSummaries :: Prelude.Maybe [InsightSummary],
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetInsightSummariesResponse
newGetInsightSummariesResponse pHttpStatus_ =
  GetInsightSummariesResponse'
    { insightSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary of each insight within the group matching the provided
-- filters. The summary contains the InsightID, start and end time, the
-- root cause service, the root cause and client impact statistics, the top
-- anomalous services, and the status of the insight.
getInsightSummariesResponse_insightSummaries :: Lens.Lens' GetInsightSummariesResponse (Prelude.Maybe [InsightSummary])
getInsightSummariesResponse_insightSummaries = Lens.lens (\GetInsightSummariesResponse' {insightSummaries} -> insightSummaries) (\s@GetInsightSummariesResponse' {} a -> s {insightSummaries = a} :: GetInsightSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token.
getInsightSummariesResponse_nextToken :: Lens.Lens' GetInsightSummariesResponse (Prelude.Maybe Prelude.Text)
getInsightSummariesResponse_nextToken = Lens.lens (\GetInsightSummariesResponse' {nextToken} -> nextToken) (\s@GetInsightSummariesResponse' {} a -> s {nextToken = a} :: GetInsightSummariesResponse)

-- | The response's http status code.
getInsightSummariesResponse_httpStatus :: Lens.Lens' GetInsightSummariesResponse Prelude.Int
getInsightSummariesResponse_httpStatus = Lens.lens (\GetInsightSummariesResponse' {httpStatus} -> httpStatus) (\s@GetInsightSummariesResponse' {} a -> s {httpStatus = a} :: GetInsightSummariesResponse)

instance Prelude.NFData GetInsightSummariesResponse where
  rnf GetInsightSummariesResponse' {..} =
    Prelude.rnf insightSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
