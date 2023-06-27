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
-- Module      : Amazonka.SecurityHub.GetFindingHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns history for a Security Hub finding in the last 90 days. The
-- history includes changes made to any fields in the Amazon Web Services
-- Security Finding Format (ASFF).
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.GetFindingHistory
  ( -- * Creating a Request
    GetFindingHistory (..),
    newGetFindingHistory,

    -- * Request Lenses
    getFindingHistory_endTime,
    getFindingHistory_maxResults,
    getFindingHistory_nextToken,
    getFindingHistory_startTime,
    getFindingHistory_findingIdentifier,

    -- * Destructuring the Response
    GetFindingHistoryResponse (..),
    newGetFindingHistoryResponse,

    -- * Response Lenses
    getFindingHistoryResponse_nextToken,
    getFindingHistoryResponse_records,
    getFindingHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetFindingHistory' smart constructor.
data GetFindingHistory = GetFindingHistory'
  { -- | An ISO 8601-formatted timestamp that indicates the end time of the
    -- requested finding history. A correctly formatted example is
    -- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
    -- and time should be separated by @T@. For more information, see
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    --
    -- If you provide values for both @StartTime@ and @EndTime@, Security Hub
    -- returns finding history for the specified time period. If you provide a
    -- value for @StartTime@ but not for @EndTime@, Security Hub returns
    -- finding history from the @StartTime@ to the time at which the API is
    -- called. If you provide a value for @EndTime@ but not for @StartTime@,
    -- Security Hub returns finding history from the
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
    -- timestamp of the finding to the @EndTime@. If you provide neither
    -- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
    -- CreatedAt timestamp of the finding to the time at which the API is
    -- called. In all of these scenarios, the response is limited to 100
    -- results, and the maximum time period is limited to 90 days.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum number of results to be returned. If you don’t provide it,
    -- Security Hub returns up to 100 results of finding history.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token for pagination purposes. Provide @NULL@ as the initial value. In
    -- subsequent requests, provide the token included in the response to get
    -- up to an additional 100 results of finding history. If you don’t provide
    -- @NextToken@, Security Hub returns up to 100 results of finding history
    -- for each request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An ISO 8601-formatted timestamp that indicates the start time of the
    -- requested finding history. A correctly formatted example is
    -- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
    -- and time should be separated by @T@. For more information, see
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    --
    -- If you provide values for both @StartTime@ and @EndTime@, Security Hub
    -- returns finding history for the specified time period. If you provide a
    -- value for @StartTime@ but not for @EndTime@, Security Hub returns
    -- finding history from the @StartTime@ to the time at which the API is
    -- called. If you provide a value for @EndTime@ but not for @StartTime@,
    -- Security Hub returns finding history from the
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
    -- timestamp of the finding to the @EndTime@. If you provide neither
    -- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
    -- CreatedAt timestamp of the finding to the time at which the API is
    -- called. In all of these scenarios, the response is limited to 100
    -- results, and the maximum time period is limited to 90 days.
    startTime :: Prelude.Maybe Data.ISO8601,
    findingIdentifier :: AwsSecurityFindingIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getFindingHistory_endTime' - An ISO 8601-formatted timestamp that indicates the end time of the
-- requested finding history. A correctly formatted example is
-- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
-- and time should be separated by @T@. For more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- If you provide values for both @StartTime@ and @EndTime@, Security Hub
-- returns finding history for the specified time period. If you provide a
-- value for @StartTime@ but not for @EndTime@, Security Hub returns
-- finding history from the @StartTime@ to the time at which the API is
-- called. If you provide a value for @EndTime@ but not for @StartTime@,
-- Security Hub returns finding history from the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
-- timestamp of the finding to the @EndTime@. If you provide neither
-- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
-- CreatedAt timestamp of the finding to the time at which the API is
-- called. In all of these scenarios, the response is limited to 100
-- results, and the maximum time period is limited to 90 days.
--
-- 'maxResults', 'getFindingHistory_maxResults' - The maximum number of results to be returned. If you don’t provide it,
-- Security Hub returns up to 100 results of finding history.
--
-- 'nextToken', 'getFindingHistory_nextToken' - A token for pagination purposes. Provide @NULL@ as the initial value. In
-- subsequent requests, provide the token included in the response to get
-- up to an additional 100 results of finding history. If you don’t provide
-- @NextToken@, Security Hub returns up to 100 results of finding history
-- for each request.
--
-- 'startTime', 'getFindingHistory_startTime' - An ISO 8601-formatted timestamp that indicates the start time of the
-- requested finding history. A correctly formatted example is
-- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
-- and time should be separated by @T@. For more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- If you provide values for both @StartTime@ and @EndTime@, Security Hub
-- returns finding history for the specified time period. If you provide a
-- value for @StartTime@ but not for @EndTime@, Security Hub returns
-- finding history from the @StartTime@ to the time at which the API is
-- called. If you provide a value for @EndTime@ but not for @StartTime@,
-- Security Hub returns finding history from the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
-- timestamp of the finding to the @EndTime@. If you provide neither
-- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
-- CreatedAt timestamp of the finding to the time at which the API is
-- called. In all of these scenarios, the response is limited to 100
-- results, and the maximum time period is limited to 90 days.
--
-- 'findingIdentifier', 'getFindingHistory_findingIdentifier' - Undocumented member.
newGetFindingHistory ::
  -- | 'findingIdentifier'
  AwsSecurityFindingIdentifier ->
  GetFindingHistory
newGetFindingHistory pFindingIdentifier_ =
  GetFindingHistory'
    { endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      findingIdentifier = pFindingIdentifier_
    }

-- | An ISO 8601-formatted timestamp that indicates the end time of the
-- requested finding history. A correctly formatted example is
-- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
-- and time should be separated by @T@. For more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- If you provide values for both @StartTime@ and @EndTime@, Security Hub
-- returns finding history for the specified time period. If you provide a
-- value for @StartTime@ but not for @EndTime@, Security Hub returns
-- finding history from the @StartTime@ to the time at which the API is
-- called. If you provide a value for @EndTime@ but not for @StartTime@,
-- Security Hub returns finding history from the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
-- timestamp of the finding to the @EndTime@. If you provide neither
-- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
-- CreatedAt timestamp of the finding to the time at which the API is
-- called. In all of these scenarios, the response is limited to 100
-- results, and the maximum time period is limited to 90 days.
getFindingHistory_endTime :: Lens.Lens' GetFindingHistory (Prelude.Maybe Prelude.UTCTime)
getFindingHistory_endTime = Lens.lens (\GetFindingHistory' {endTime} -> endTime) (\s@GetFindingHistory' {} a -> s {endTime = a} :: GetFindingHistory) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to be returned. If you don’t provide it,
-- Security Hub returns up to 100 results of finding history.
getFindingHistory_maxResults :: Lens.Lens' GetFindingHistory (Prelude.Maybe Prelude.Natural)
getFindingHistory_maxResults = Lens.lens (\GetFindingHistory' {maxResults} -> maxResults) (\s@GetFindingHistory' {} a -> s {maxResults = a} :: GetFindingHistory)

-- | A token for pagination purposes. Provide @NULL@ as the initial value. In
-- subsequent requests, provide the token included in the response to get
-- up to an additional 100 results of finding history. If you don’t provide
-- @NextToken@, Security Hub returns up to 100 results of finding history
-- for each request.
getFindingHistory_nextToken :: Lens.Lens' GetFindingHistory (Prelude.Maybe Prelude.Text)
getFindingHistory_nextToken = Lens.lens (\GetFindingHistory' {nextToken} -> nextToken) (\s@GetFindingHistory' {} a -> s {nextToken = a} :: GetFindingHistory)

-- | An ISO 8601-formatted timestamp that indicates the start time of the
-- requested finding history. A correctly formatted example is
-- @2020-05-21T20:16:34.724Z@. The value cannot contain spaces, and date
-- and time should be separated by @T@. For more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- If you provide values for both @StartTime@ and @EndTime@, Security Hub
-- returns finding history for the specified time period. If you provide a
-- value for @StartTime@ but not for @EndTime@, Security Hub returns
-- finding history from the @StartTime@ to the time at which the API is
-- called. If you provide a value for @EndTime@ but not for @StartTime@,
-- Security Hub returns finding history from the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_AwsSecurityFindingFilters.html#securityhub-Type-AwsSecurityFindingFilters-CreatedAt CreatedAt>
-- timestamp of the finding to the @EndTime@. If you provide neither
-- @StartTime@ nor @EndTime@, Security Hub returns finding history from the
-- CreatedAt timestamp of the finding to the time at which the API is
-- called. In all of these scenarios, the response is limited to 100
-- results, and the maximum time period is limited to 90 days.
getFindingHistory_startTime :: Lens.Lens' GetFindingHistory (Prelude.Maybe Prelude.UTCTime)
getFindingHistory_startTime = Lens.lens (\GetFindingHistory' {startTime} -> startTime) (\s@GetFindingHistory' {} a -> s {startTime = a} :: GetFindingHistory) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
getFindingHistory_findingIdentifier :: Lens.Lens' GetFindingHistory AwsSecurityFindingIdentifier
getFindingHistory_findingIdentifier = Lens.lens (\GetFindingHistory' {findingIdentifier} -> findingIdentifier) (\s@GetFindingHistory' {} a -> s {findingIdentifier = a} :: GetFindingHistory)

instance Core.AWSPager GetFindingHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getFindingHistoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getFindingHistoryResponse_records
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getFindingHistory_nextToken
          Lens..~ rs
          Lens.^? getFindingHistoryResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetFindingHistory where
  type
    AWSResponse GetFindingHistory =
      GetFindingHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingHistoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Records" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFindingHistory where
  hashWithSalt _salt GetFindingHistory' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` findingIdentifier

instance Prelude.NFData GetFindingHistory where
  rnf GetFindingHistory' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf findingIdentifier

instance Data.ToHeaders GetFindingHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFindingHistory where
  toJSON GetFindingHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTime" Data..=) Prelude.<$> startTime,
            Prelude.Just
              ("FindingIdentifier" Data..= findingIdentifier)
          ]
      )

instance Data.ToPath GetFindingHistory where
  toPath = Prelude.const "/findingHistory/get"

instance Data.ToQuery GetFindingHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingHistoryResponse' smart constructor.
data GetFindingHistoryResponse = GetFindingHistoryResponse'
  { -- | A token for pagination purposes. Provide this token in the subsequent
    -- request to @GetFindingsHistory@ to get up to an additional 100 results
    -- of history for the same finding that you specified in your initial
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of events that altered the specified finding during the specified
    -- time period.
    records :: Prelude.Maybe [FindingHistoryRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFindingHistoryResponse_nextToken' - A token for pagination purposes. Provide this token in the subsequent
-- request to @GetFindingsHistory@ to get up to an additional 100 results
-- of history for the same finding that you specified in your initial
-- request.
--
-- 'records', 'getFindingHistoryResponse_records' - A list of events that altered the specified finding during the specified
-- time period.
--
-- 'httpStatus', 'getFindingHistoryResponse_httpStatus' - The response's http status code.
newGetFindingHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingHistoryResponse
newGetFindingHistoryResponse pHttpStatus_ =
  GetFindingHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      records = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for pagination purposes. Provide this token in the subsequent
-- request to @GetFindingsHistory@ to get up to an additional 100 results
-- of history for the same finding that you specified in your initial
-- request.
getFindingHistoryResponse_nextToken :: Lens.Lens' GetFindingHistoryResponse (Prelude.Maybe Prelude.Text)
getFindingHistoryResponse_nextToken = Lens.lens (\GetFindingHistoryResponse' {nextToken} -> nextToken) (\s@GetFindingHistoryResponse' {} a -> s {nextToken = a} :: GetFindingHistoryResponse)

-- | A list of events that altered the specified finding during the specified
-- time period.
getFindingHistoryResponse_records :: Lens.Lens' GetFindingHistoryResponse (Prelude.Maybe [FindingHistoryRecord])
getFindingHistoryResponse_records = Lens.lens (\GetFindingHistoryResponse' {records} -> records) (\s@GetFindingHistoryResponse' {} a -> s {records = a} :: GetFindingHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getFindingHistoryResponse_httpStatus :: Lens.Lens' GetFindingHistoryResponse Prelude.Int
getFindingHistoryResponse_httpStatus = Lens.lens (\GetFindingHistoryResponse' {httpStatus} -> httpStatus) (\s@GetFindingHistoryResponse' {} a -> s {httpStatus = a} :: GetFindingHistoryResponse)

instance Prelude.NFData GetFindingHistoryResponse where
  rnf GetFindingHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf httpStatus
