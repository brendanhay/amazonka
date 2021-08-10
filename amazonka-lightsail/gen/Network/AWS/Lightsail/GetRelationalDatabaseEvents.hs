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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of events for a specific database in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseEvents
  ( -- * Creating a Request
    GetRelationalDatabaseEvents (..),
    newGetRelationalDatabaseEvents,

    -- * Request Lenses
    getRelationalDatabaseEvents_durationInMinutes,
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_relationalDatabaseName,

    -- * Destructuring the Response
    GetRelationalDatabaseEventsResponse (..),
    newGetRelationalDatabaseEventsResponse,

    -- * Response Lenses
    getRelationalDatabaseEventsResponse_nextPageToken,
    getRelationalDatabaseEventsResponse_relationalDatabaseEvents,
    getRelationalDatabaseEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseEvents' smart constructor.
data GetRelationalDatabaseEvents = GetRelationalDatabaseEvents'
  { -- | The number of minutes in the past from which to retrieve events. For
    -- example, to get all events from the past 2 hours, enter 120.
    --
    -- Default: @60@
    --
    -- The minimum is 1 and the maximum is 14 days (20160 minutes).
    durationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseEvents@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the database from which to get events.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'getRelationalDatabaseEvents_durationInMinutes' - The number of minutes in the past from which to retrieve events. For
-- example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
--
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
--
-- 'pageToken', 'getRelationalDatabaseEvents_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
--
-- 'relationalDatabaseName', 'getRelationalDatabaseEvents_relationalDatabaseName' - The name of the database from which to get events.
newGetRelationalDatabaseEvents ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  GetRelationalDatabaseEvents
newGetRelationalDatabaseEvents
  pRelationalDatabaseName_ =
    GetRelationalDatabaseEvents'
      { durationInMinutes =
          Prelude.Nothing,
        pageToken = Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The number of minutes in the past from which to retrieve events. For
-- example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
--
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
getRelationalDatabaseEvents_durationInMinutes :: Lens.Lens' GetRelationalDatabaseEvents (Prelude.Maybe Prelude.Int)
getRelationalDatabaseEvents_durationInMinutes = Lens.lens (\GetRelationalDatabaseEvents' {durationInMinutes} -> durationInMinutes) (\s@GetRelationalDatabaseEvents' {} a -> s {durationInMinutes = a} :: GetRelationalDatabaseEvents)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseEvents_pageToken :: Lens.Lens' GetRelationalDatabaseEvents (Prelude.Maybe Prelude.Text)
getRelationalDatabaseEvents_pageToken = Lens.lens (\GetRelationalDatabaseEvents' {pageToken} -> pageToken) (\s@GetRelationalDatabaseEvents' {} a -> s {pageToken = a} :: GetRelationalDatabaseEvents)

-- | The name of the database from which to get events.
getRelationalDatabaseEvents_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseEvents Prelude.Text
getRelationalDatabaseEvents_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseEvents' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseEvents' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseEvents)

instance Core.AWSPager GetRelationalDatabaseEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseEventsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseEventsResponse_relationalDatabaseEvents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRelationalDatabaseEvents_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseEventsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetRelationalDatabaseEvents where
  type
    AWSResponse GetRelationalDatabaseEvents =
      GetRelationalDatabaseEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseEventsResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> ( x Core..?> "relationalDatabaseEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRelationalDatabaseEvents

instance Prelude.NFData GetRelationalDatabaseEvents

instance Core.ToHeaders GetRelationalDatabaseEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabaseEvents where
  toJSON GetRelationalDatabaseEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("durationInMinutes" Core..=)
              Prelude.<$> durationInMinutes,
            ("pageToken" Core..=) Prelude.<$> pageToken,
            Prelude.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath GetRelationalDatabaseEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabaseEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseEventsResponse' smart constructor.
data GetRelationalDatabaseEventsResponse = GetRelationalDatabaseEventsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseEvents@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object describing the result of your get relational database events
    -- request.
    relationalDatabaseEvents :: Prelude.Maybe [RelationalDatabaseEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getRelationalDatabaseEventsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseEvents@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'relationalDatabaseEvents', 'getRelationalDatabaseEventsResponse_relationalDatabaseEvents' - An object describing the result of your get relational database events
-- request.
--
-- 'httpStatus', 'getRelationalDatabaseEventsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseEventsResponse
newGetRelationalDatabaseEventsResponse pHttpStatus_ =
  GetRelationalDatabaseEventsResponse'
    { nextPageToken =
        Prelude.Nothing,
      relationalDatabaseEvents =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseEvents@ request and specify the next page token
-- using the @pageToken@ parameter.
getRelationalDatabaseEventsResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseEventsResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseEventsResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseEventsResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseEventsResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseEventsResponse)

-- | An object describing the result of your get relational database events
-- request.
getRelationalDatabaseEventsResponse_relationalDatabaseEvents :: Lens.Lens' GetRelationalDatabaseEventsResponse (Prelude.Maybe [RelationalDatabaseEvent])
getRelationalDatabaseEventsResponse_relationalDatabaseEvents = Lens.lens (\GetRelationalDatabaseEventsResponse' {relationalDatabaseEvents} -> relationalDatabaseEvents) (\s@GetRelationalDatabaseEventsResponse' {} a -> s {relationalDatabaseEvents = a} :: GetRelationalDatabaseEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRelationalDatabaseEventsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseEventsResponse Prelude.Int
getRelationalDatabaseEventsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseEventsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseEventsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseEventsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseEventsResponse
