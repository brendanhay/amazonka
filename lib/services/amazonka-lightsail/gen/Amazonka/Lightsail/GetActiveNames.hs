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
-- Module      : Amazonka.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetActiveNames
  ( -- * Creating a Request
    GetActiveNames (..),
    newGetActiveNames,

    -- * Request Lenses
    getActiveNames_pageToken,

    -- * Destructuring the Response
    GetActiveNamesResponse (..),
    newGetActiveNamesResponse,

    -- * Response Lenses
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetActiveNames' smart constructor.
data GetActiveNames = GetActiveNames'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetActiveNames@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActiveNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getActiveNames_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
newGetActiveNames ::
  GetActiveNames
newGetActiveNames =
  GetActiveNames' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getActiveNames_pageToken :: Lens.Lens' GetActiveNames (Prelude.Maybe Prelude.Text)
getActiveNames_pageToken = Lens.lens (\GetActiveNames' {pageToken} -> pageToken) (\s@GetActiveNames' {} a -> s {pageToken = a} :: GetActiveNames)

instance Core.AWSPager GetActiveNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getActiveNamesResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getActiveNamesResponse_activeNames
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getActiveNames_pageToken
              Lens..~ rs
              Lens.^? getActiveNamesResponse_nextPageToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetActiveNames where
  type
    AWSResponse GetActiveNames =
      GetActiveNamesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActiveNamesResponse'
            Prelude.<$> (x Data..?> "activeNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetActiveNames where
  hashWithSalt _salt GetActiveNames' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetActiveNames where
  rnf GetActiveNames' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetActiveNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetActiveNames" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetActiveNames where
  toJSON GetActiveNames' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetActiveNames where
  toPath = Prelude.const "/"

instance Data.ToQuery GetActiveNames where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { -- | The list of active names returned by the get active names request.
    activeNames :: Prelude.Maybe [Prelude.Text],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetActiveNames@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActiveNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeNames', 'getActiveNamesResponse_activeNames' - The list of active names returned by the get active names request.
--
-- 'nextPageToken', 'getActiveNamesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetActiveNames@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getActiveNamesResponse_httpStatus' - The response's http status code.
newGetActiveNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetActiveNamesResponse
newGetActiveNamesResponse pHttpStatus_ =
  GetActiveNamesResponse'
    { activeNames =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of active names returned by the get active names request.
getActiveNamesResponse_activeNames :: Lens.Lens' GetActiveNamesResponse (Prelude.Maybe [Prelude.Text])
getActiveNamesResponse_activeNames = Lens.lens (\GetActiveNamesResponse' {activeNames} -> activeNames) (\s@GetActiveNamesResponse' {} a -> s {activeNames = a} :: GetActiveNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetActiveNames@
-- request and specify the next page token using the @pageToken@ parameter.
getActiveNamesResponse_nextPageToken :: Lens.Lens' GetActiveNamesResponse (Prelude.Maybe Prelude.Text)
getActiveNamesResponse_nextPageToken = Lens.lens (\GetActiveNamesResponse' {nextPageToken} -> nextPageToken) (\s@GetActiveNamesResponse' {} a -> s {nextPageToken = a} :: GetActiveNamesResponse)

-- | The response's http status code.
getActiveNamesResponse_httpStatus :: Lens.Lens' GetActiveNamesResponse Prelude.Int
getActiveNamesResponse_httpStatus = Lens.lens (\GetActiveNamesResponse' {httpStatus} -> httpStatus) (\s@GetActiveNamesResponse' {} a -> s {httpStatus = a} :: GetActiveNamesResponse)

instance Prelude.NFData GetActiveNamesResponse where
  rnf GetActiveNamesResponse' {..} =
    Prelude.rnf activeNames `Prelude.seq`
      Prelude.rnf nextPageToken `Prelude.seq`
        Prelude.rnf httpStatus
