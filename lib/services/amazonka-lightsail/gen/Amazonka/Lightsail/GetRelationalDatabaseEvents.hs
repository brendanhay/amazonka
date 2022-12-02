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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of events for a specific database in Amazon Lightsail.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetRelationalDatabaseEvents
  ( -- * Creating a Request
    GetRelationalDatabaseEvents (..),
    newGetRelationalDatabaseEvents,

    -- * Request Lenses
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_durationInMinutes,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabaseEvents' smart constructor.
data GetRelationalDatabaseEvents = GetRelationalDatabaseEvents'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseEvents@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes in the past from which to retrieve events. For
    -- example, to get all events from the past 2 hours, enter 120.
    --
    -- Default: @60@
    --
    -- The minimum is 1 and the maximum is 14 days (20160 minutes).
    durationInMinutes :: Prelude.Maybe Prelude.Int,
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
-- 'pageToken', 'getRelationalDatabaseEvents_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
--
-- 'durationInMinutes', 'getRelationalDatabaseEvents_durationInMinutes' - The number of minutes in the past from which to retrieve events. For
-- example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
--
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
--
-- 'relationalDatabaseName', 'getRelationalDatabaseEvents_relationalDatabaseName' - The name of the database from which to get events.
newGetRelationalDatabaseEvents ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  GetRelationalDatabaseEvents
newGetRelationalDatabaseEvents
  pRelationalDatabaseName_ =
    GetRelationalDatabaseEvents'
      { pageToken =
          Prelude.Nothing,
        durationInMinutes = Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseEvents_pageToken :: Lens.Lens' GetRelationalDatabaseEvents (Prelude.Maybe Prelude.Text)
getRelationalDatabaseEvents_pageToken = Lens.lens (\GetRelationalDatabaseEvents' {pageToken} -> pageToken) (\s@GetRelationalDatabaseEvents' {} a -> s {pageToken = a} :: GetRelationalDatabaseEvents)

-- | The number of minutes in the past from which to retrieve events. For
-- example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
--
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
getRelationalDatabaseEvents_durationInMinutes :: Lens.Lens' GetRelationalDatabaseEvents (Prelude.Maybe Prelude.Int)
getRelationalDatabaseEvents_durationInMinutes = Lens.lens (\GetRelationalDatabaseEvents' {durationInMinutes} -> durationInMinutes) (\s@GetRelationalDatabaseEvents' {} a -> s {durationInMinutes = a} :: GetRelationalDatabaseEvents)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseEventsResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> ( x Data..?> "relationalDatabaseEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRelationalDatabaseEvents where
  hashWithSalt _salt GetRelationalDatabaseEvents' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` durationInMinutes
      `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData GetRelationalDatabaseEvents where
  rnf GetRelationalDatabaseEvents' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf durationInMinutes
      `Prelude.seq` Prelude.rnf relationalDatabaseName

instance Data.ToHeaders GetRelationalDatabaseEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseEvents where
  toJSON GetRelationalDatabaseEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pageToken" Data..=) Prelude.<$> pageToken,
            ("durationInMinutes" Data..=)
              Prelude.<$> durationInMinutes,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath GetRelationalDatabaseEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseEvents where
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
getRelationalDatabaseEventsResponse_relationalDatabaseEvents = Lens.lens (\GetRelationalDatabaseEventsResponse' {relationalDatabaseEvents} -> relationalDatabaseEvents) (\s@GetRelationalDatabaseEventsResponse' {} a -> s {relationalDatabaseEvents = a} :: GetRelationalDatabaseEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabaseEventsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseEventsResponse Prelude.Int
getRelationalDatabaseEventsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseEventsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseEventsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseEventsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseEventsResponse
  where
  rnf GetRelationalDatabaseEventsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf relationalDatabaseEvents
      `Prelude.seq` Prelude.rnf httpStatus
