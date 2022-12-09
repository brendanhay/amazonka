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
-- Module      : Amazonka.Location.GetDevicePositionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the device position history from a tracker resource within a
-- specified range of time.
--
-- Device positions are deleted after 30 days.
--
-- This operation returns paginated results.
module Amazonka.Location.GetDevicePositionHistory
  ( -- * Creating a Request
    GetDevicePositionHistory (..),
    newGetDevicePositionHistory,

    -- * Request Lenses
    getDevicePositionHistory_endTimeExclusive,
    getDevicePositionHistory_maxResults,
    getDevicePositionHistory_nextToken,
    getDevicePositionHistory_startTimeInclusive,
    getDevicePositionHistory_deviceId,
    getDevicePositionHistory_trackerName,

    -- * Destructuring the Response
    GetDevicePositionHistoryResponse (..),
    newGetDevicePositionHistoryResponse,

    -- * Response Lenses
    getDevicePositionHistoryResponse_nextToken,
    getDevicePositionHistoryResponse_httpStatus,
    getDevicePositionHistoryResponse_devicePositions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevicePositionHistory' smart constructor.
data GetDevicePositionHistory = GetDevicePositionHistory'
  { -- | Specify the end time for the position history in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be the
    -- time that the request is made.
    --
    -- Requirement:
    --
    -- -   The time specified for @EndTimeExclusive@ must be after the time for
    --     @StartTimeInclusive@.
    endTimeExclusive :: Prelude.Maybe Data.POSIX,
    -- | An optional limit for the number of device positions returned in a
    -- single call.
    --
    -- Default value: @100@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token specifying which page of results to return in the
    -- response. If no token is provided, the default page is the first page.
    --
    -- Default value: @null@
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify the start time for the position history in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be 24
    -- hours prior to the time that the request is made.
    --
    -- Requirement:
    --
    -- -   The time specified for @StartTimeInclusive@ must be before
    --     @EndTimeExclusive@.
    startTimeInclusive :: Prelude.Maybe Data.POSIX,
    -- | The device whose position history you want to retrieve.
    deviceId :: Prelude.Text,
    -- | The tracker resource receiving the request for the device position
    -- history.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePositionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTimeExclusive', 'getDevicePositionHistory_endTimeExclusive' - Specify the end time for the position history in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be the
-- time that the request is made.
--
-- Requirement:
--
-- -   The time specified for @EndTimeExclusive@ must be after the time for
--     @StartTimeInclusive@.
--
-- 'maxResults', 'getDevicePositionHistory_maxResults' - An optional limit for the number of device positions returned in a
-- single call.
--
-- Default value: @100@
--
-- 'nextToken', 'getDevicePositionHistory_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
--
-- 'startTimeInclusive', 'getDevicePositionHistory_startTimeInclusive' - Specify the start time for the position history in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be 24
-- hours prior to the time that the request is made.
--
-- Requirement:
--
-- -   The time specified for @StartTimeInclusive@ must be before
--     @EndTimeExclusive@.
--
-- 'deviceId', 'getDevicePositionHistory_deviceId' - The device whose position history you want to retrieve.
--
-- 'trackerName', 'getDevicePositionHistory_trackerName' - The tracker resource receiving the request for the device position
-- history.
newGetDevicePositionHistory ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  GetDevicePositionHistory
newGetDevicePositionHistory pDeviceId_ pTrackerName_ =
  GetDevicePositionHistory'
    { endTimeExclusive =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTimeInclusive = Prelude.Nothing,
      deviceId = pDeviceId_,
      trackerName = pTrackerName_
    }

-- | Specify the end time for the position history in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be the
-- time that the request is made.
--
-- Requirement:
--
-- -   The time specified for @EndTimeExclusive@ must be after the time for
--     @StartTimeInclusive@.
getDevicePositionHistory_endTimeExclusive :: Lens.Lens' GetDevicePositionHistory (Prelude.Maybe Prelude.UTCTime)
getDevicePositionHistory_endTimeExclusive = Lens.lens (\GetDevicePositionHistory' {endTimeExclusive} -> endTimeExclusive) (\s@GetDevicePositionHistory' {} a -> s {endTimeExclusive = a} :: GetDevicePositionHistory) Prelude.. Lens.mapping Data._Time

-- | An optional limit for the number of device positions returned in a
-- single call.
--
-- Default value: @100@
getDevicePositionHistory_maxResults :: Lens.Lens' GetDevicePositionHistory (Prelude.Maybe Prelude.Natural)
getDevicePositionHistory_maxResults = Lens.lens (\GetDevicePositionHistory' {maxResults} -> maxResults) (\s@GetDevicePositionHistory' {} a -> s {maxResults = a} :: GetDevicePositionHistory)

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
getDevicePositionHistory_nextToken :: Lens.Lens' GetDevicePositionHistory (Prelude.Maybe Prelude.Text)
getDevicePositionHistory_nextToken = Lens.lens (\GetDevicePositionHistory' {nextToken} -> nextToken) (\s@GetDevicePositionHistory' {} a -> s {nextToken = a} :: GetDevicePositionHistory)

-- | Specify the start time for the position history in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. By default, the value will be 24
-- hours prior to the time that the request is made.
--
-- Requirement:
--
-- -   The time specified for @StartTimeInclusive@ must be before
--     @EndTimeExclusive@.
getDevicePositionHistory_startTimeInclusive :: Lens.Lens' GetDevicePositionHistory (Prelude.Maybe Prelude.UTCTime)
getDevicePositionHistory_startTimeInclusive = Lens.lens (\GetDevicePositionHistory' {startTimeInclusive} -> startTimeInclusive) (\s@GetDevicePositionHistory' {} a -> s {startTimeInclusive = a} :: GetDevicePositionHistory) Prelude.. Lens.mapping Data._Time

-- | The device whose position history you want to retrieve.
getDevicePositionHistory_deviceId :: Lens.Lens' GetDevicePositionHistory Prelude.Text
getDevicePositionHistory_deviceId = Lens.lens (\GetDevicePositionHistory' {deviceId} -> deviceId) (\s@GetDevicePositionHistory' {} a -> s {deviceId = a} :: GetDevicePositionHistory)

-- | The tracker resource receiving the request for the device position
-- history.
getDevicePositionHistory_trackerName :: Lens.Lens' GetDevicePositionHistory Prelude.Text
getDevicePositionHistory_trackerName = Lens.lens (\GetDevicePositionHistory' {trackerName} -> trackerName) (\s@GetDevicePositionHistory' {} a -> s {trackerName = a} :: GetDevicePositionHistory)

instance Core.AWSPager GetDevicePositionHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDevicePositionHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getDevicePositionHistoryResponse_devicePositions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDevicePositionHistory_nextToken
          Lens..~ rs
          Lens.^? getDevicePositionHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetDevicePositionHistory where
  type
    AWSResponse GetDevicePositionHistory =
      GetDevicePositionHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePositionHistoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "DevicePositions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetDevicePositionHistory where
  hashWithSalt _salt GetDevicePositionHistory' {..} =
    _salt `Prelude.hashWithSalt` endTimeExclusive
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTimeInclusive
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData GetDevicePositionHistory where
  rnf GetDevicePositionHistory' {..} =
    Prelude.rnf endTimeExclusive
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTimeInclusive
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf trackerName

instance Data.ToHeaders GetDevicePositionHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDevicePositionHistory where
  toJSON GetDevicePositionHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTimeExclusive" Data..=)
              Prelude.<$> endTimeExclusive,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTimeInclusive" Data..=)
              Prelude.<$> startTimeInclusive
          ]
      )

instance Data.ToPath GetDevicePositionHistory where
  toPath GetDevicePositionHistory' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Data.toBS trackerName,
        "/devices/",
        Data.toBS deviceId,
        "/list-positions"
      ]

instance Data.ToQuery GetDevicePositionHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevicePositionHistoryResponse' smart constructor.
data GetDevicePositionHistoryResponse = GetDevicePositionHistoryResponse'
  { -- | A pagination token indicating there are additional pages available. You
    -- can use the token in a following request to fetch the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains the position history details for the requested device.
    devicePositions :: [DevicePosition]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePositionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDevicePositionHistoryResponse_nextToken' - A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
--
-- 'httpStatus', 'getDevicePositionHistoryResponse_httpStatus' - The response's http status code.
--
-- 'devicePositions', 'getDevicePositionHistoryResponse_devicePositions' - Contains the position history details for the requested device.
newGetDevicePositionHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevicePositionHistoryResponse
newGetDevicePositionHistoryResponse pHttpStatus_ =
  GetDevicePositionHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      devicePositions = Prelude.mempty
    }

-- | A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
getDevicePositionHistoryResponse_nextToken :: Lens.Lens' GetDevicePositionHistoryResponse (Prelude.Maybe Prelude.Text)
getDevicePositionHistoryResponse_nextToken = Lens.lens (\GetDevicePositionHistoryResponse' {nextToken} -> nextToken) (\s@GetDevicePositionHistoryResponse' {} a -> s {nextToken = a} :: GetDevicePositionHistoryResponse)

-- | The response's http status code.
getDevicePositionHistoryResponse_httpStatus :: Lens.Lens' GetDevicePositionHistoryResponse Prelude.Int
getDevicePositionHistoryResponse_httpStatus = Lens.lens (\GetDevicePositionHistoryResponse' {httpStatus} -> httpStatus) (\s@GetDevicePositionHistoryResponse' {} a -> s {httpStatus = a} :: GetDevicePositionHistoryResponse)

-- | Contains the position history details for the requested device.
getDevicePositionHistoryResponse_devicePositions :: Lens.Lens' GetDevicePositionHistoryResponse [DevicePosition]
getDevicePositionHistoryResponse_devicePositions = Lens.lens (\GetDevicePositionHistoryResponse' {devicePositions} -> devicePositions) (\s@GetDevicePositionHistoryResponse' {} a -> s {devicePositions = a} :: GetDevicePositionHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetDevicePositionHistoryResponse
  where
  rnf GetDevicePositionHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devicePositions
