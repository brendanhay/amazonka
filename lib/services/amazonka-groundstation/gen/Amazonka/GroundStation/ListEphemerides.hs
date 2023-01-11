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
-- Module      : Amazonka.GroundStation.ListEphemerides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List existing ephemerides.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListEphemerides
  ( -- * Creating a Request
    ListEphemerides (..),
    newListEphemerides,

    -- * Request Lenses
    listEphemerides_maxResults,
    listEphemerides_nextToken,
    listEphemerides_statusList,
    listEphemerides_endTime,
    listEphemerides_satelliteId,
    listEphemerides_startTime,

    -- * Destructuring the Response
    ListEphemeridesResponse (..),
    newListEphemeridesResponse,

    -- * Response Lenses
    listEphemeridesResponse_ephemerides,
    listEphemeridesResponse_nextToken,
    listEphemeridesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEphemerides' smart constructor.
data ListEphemerides = ListEphemerides'
  { -- | Maximum number of ephemerides to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ephemeris status to return.
    statusList :: Prelude.Maybe [EphemerisStatus],
    -- | The end time to list in UTC. The operation will return an ephemeris if
    -- its expiration time is within the time range defined by the @startTime@
    -- and @endTime@.
    endTime :: Data.POSIX,
    -- | The AWS Ground Station satellite ID to list ephemeris for.
    satelliteId :: Prelude.Text,
    -- | The start time to list in UTC. The operation will return an ephemeris if
    -- its expiration time is within the time range defined by the @startTime@
    -- and @endTime@.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEphemerides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEphemerides_maxResults' - Maximum number of ephemerides to return.
--
-- 'nextToken', 'listEphemerides_nextToken' - Pagination token.
--
-- 'statusList', 'listEphemerides_statusList' - The list of ephemeris status to return.
--
-- 'endTime', 'listEphemerides_endTime' - The end time to list in UTC. The operation will return an ephemeris if
-- its expiration time is within the time range defined by the @startTime@
-- and @endTime@.
--
-- 'satelliteId', 'listEphemerides_satelliteId' - The AWS Ground Station satellite ID to list ephemeris for.
--
-- 'startTime', 'listEphemerides_startTime' - The start time to list in UTC. The operation will return an ephemeris if
-- its expiration time is within the time range defined by the @startTime@
-- and @endTime@.
newListEphemerides ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'satelliteId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  ListEphemerides
newListEphemerides
  pEndTime_
  pSatelliteId_
  pStartTime_ =
    ListEphemerides'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        statusList = Prelude.Nothing,
        endTime = Data._Time Lens.# pEndTime_,
        satelliteId = pSatelliteId_,
        startTime = Data._Time Lens.# pStartTime_
      }

-- | Maximum number of ephemerides to return.
listEphemerides_maxResults :: Lens.Lens' ListEphemerides (Prelude.Maybe Prelude.Natural)
listEphemerides_maxResults = Lens.lens (\ListEphemerides' {maxResults} -> maxResults) (\s@ListEphemerides' {} a -> s {maxResults = a} :: ListEphemerides)

-- | Pagination token.
listEphemerides_nextToken :: Lens.Lens' ListEphemerides (Prelude.Maybe Prelude.Text)
listEphemerides_nextToken = Lens.lens (\ListEphemerides' {nextToken} -> nextToken) (\s@ListEphemerides' {} a -> s {nextToken = a} :: ListEphemerides)

-- | The list of ephemeris status to return.
listEphemerides_statusList :: Lens.Lens' ListEphemerides (Prelude.Maybe [EphemerisStatus])
listEphemerides_statusList = Lens.lens (\ListEphemerides' {statusList} -> statusList) (\s@ListEphemerides' {} a -> s {statusList = a} :: ListEphemerides) Prelude.. Lens.mapping Lens.coerced

-- | The end time to list in UTC. The operation will return an ephemeris if
-- its expiration time is within the time range defined by the @startTime@
-- and @endTime@.
listEphemerides_endTime :: Lens.Lens' ListEphemerides Prelude.UTCTime
listEphemerides_endTime = Lens.lens (\ListEphemerides' {endTime} -> endTime) (\s@ListEphemerides' {} a -> s {endTime = a} :: ListEphemerides) Prelude.. Data._Time

-- | The AWS Ground Station satellite ID to list ephemeris for.
listEphemerides_satelliteId :: Lens.Lens' ListEphemerides Prelude.Text
listEphemerides_satelliteId = Lens.lens (\ListEphemerides' {satelliteId} -> satelliteId) (\s@ListEphemerides' {} a -> s {satelliteId = a} :: ListEphemerides)

-- | The start time to list in UTC. The operation will return an ephemeris if
-- its expiration time is within the time range defined by the @startTime@
-- and @endTime@.
listEphemerides_startTime :: Lens.Lens' ListEphemerides Prelude.UTCTime
listEphemerides_startTime = Lens.lens (\ListEphemerides' {startTime} -> startTime) (\s@ListEphemerides' {} a -> s {startTime = a} :: ListEphemerides) Prelude.. Data._Time

instance Core.AWSPager ListEphemerides where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEphemeridesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEphemeridesResponse_ephemerides
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEphemerides_nextToken
          Lens..~ rs
          Lens.^? listEphemeridesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEphemerides where
  type
    AWSResponse ListEphemerides =
      ListEphemeridesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEphemeridesResponse'
            Prelude.<$> (x Data..?> "ephemerides")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEphemerides where
  hashWithSalt _salt ListEphemerides' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusList
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` satelliteId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListEphemerides where
  rnf ListEphemerides' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusList
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf satelliteId
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders ListEphemerides where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEphemerides where
  toJSON ListEphemerides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("statusList" Data..=) Prelude.<$> statusList,
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("satelliteId" Data..= satelliteId),
            Prelude.Just ("startTime" Data..= startTime)
          ]
      )

instance Data.ToPath ListEphemerides where
  toPath = Prelude.const "/ephemerides"

instance Data.ToQuery ListEphemerides where
  toQuery ListEphemerides' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEphemeridesResponse' smart constructor.
data ListEphemeridesResponse = ListEphemeridesResponse'
  { -- | List of ephemerides.
    ephemerides :: Prelude.Maybe (Prelude.NonEmpty EphemerisItem),
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEphemeridesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ephemerides', 'listEphemeridesResponse_ephemerides' - List of ephemerides.
--
-- 'nextToken', 'listEphemeridesResponse_nextToken' - Pagination token.
--
-- 'httpStatus', 'listEphemeridesResponse_httpStatus' - The response's http status code.
newListEphemeridesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEphemeridesResponse
newListEphemeridesResponse pHttpStatus_ =
  ListEphemeridesResponse'
    { ephemerides =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of ephemerides.
listEphemeridesResponse_ephemerides :: Lens.Lens' ListEphemeridesResponse (Prelude.Maybe (Prelude.NonEmpty EphemerisItem))
listEphemeridesResponse_ephemerides = Lens.lens (\ListEphemeridesResponse' {ephemerides} -> ephemerides) (\s@ListEphemeridesResponse' {} a -> s {ephemerides = a} :: ListEphemeridesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token.
listEphemeridesResponse_nextToken :: Lens.Lens' ListEphemeridesResponse (Prelude.Maybe Prelude.Text)
listEphemeridesResponse_nextToken = Lens.lens (\ListEphemeridesResponse' {nextToken} -> nextToken) (\s@ListEphemeridesResponse' {} a -> s {nextToken = a} :: ListEphemeridesResponse)

-- | The response's http status code.
listEphemeridesResponse_httpStatus :: Lens.Lens' ListEphemeridesResponse Prelude.Int
listEphemeridesResponse_httpStatus = Lens.lens (\ListEphemeridesResponse' {httpStatus} -> httpStatus) (\s@ListEphemeridesResponse' {} a -> s {httpStatus = a} :: ListEphemeridesResponse)

instance Prelude.NFData ListEphemeridesResponse where
  rnf ListEphemeridesResponse' {..} =
    Prelude.rnf ephemerides
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
