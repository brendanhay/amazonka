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
-- Module      : Amazonka.Lightsail.GetDisks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disks in your AWS account
-- and region.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetDisks
  ( -- * Creating a Request
    GetDisks (..),
    newGetDisks,

    -- * Request Lenses
    getDisks_pageToken,

    -- * Destructuring the Response
    GetDisksResponse (..),
    newGetDisksResponse,

    -- * Response Lenses
    getDisksResponse_disks,
    getDisksResponse_nextPageToken,
    getDisksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDisks' smart constructor.
data GetDisks = GetDisks'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDisks@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDisks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getDisks_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetDisks ::
  GetDisks
newGetDisks = GetDisks' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getDisks_pageToken :: Lens.Lens' GetDisks (Prelude.Maybe Prelude.Text)
getDisks_pageToken = Lens.lens (\GetDisks' {pageToken} -> pageToken) (\s@GetDisks' {} a -> s {pageToken = a} :: GetDisks)

instance Core.AWSPager GetDisks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDisksResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDisksResponse_disks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getDisks_pageToken
              Lens..~ rs
              Lens.^? getDisksResponse_nextPageToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetDisks where
  type AWSResponse GetDisks = GetDisksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDisksResponse'
            Prelude.<$> (x Data..?> "disks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDisks where
  hashWithSalt _salt GetDisks' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetDisks where
  rnf GetDisks' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetDisks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDisks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDisks where
  toJSON GetDisks' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetDisks where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDisks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDisksResponse' smart constructor.
data GetDisksResponse = GetDisksResponse'
  { -- | An array of objects containing information about all block storage
    -- disks.
    disks :: Prelude.Maybe [Disk],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDisks@ request and
    -- specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDisksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disks', 'getDisksResponse_disks' - An array of objects containing information about all block storage
-- disks.
--
-- 'nextPageToken', 'getDisksResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDisks@ request and
-- specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getDisksResponse_httpStatus' - The response's http status code.
newGetDisksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDisksResponse
newGetDisksResponse pHttpStatus_ =
  GetDisksResponse'
    { disks = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects containing information about all block storage
-- disks.
getDisksResponse_disks :: Lens.Lens' GetDisksResponse (Prelude.Maybe [Disk])
getDisksResponse_disks = Lens.lens (\GetDisksResponse' {disks} -> disks) (\s@GetDisksResponse' {} a -> s {disks = a} :: GetDisksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDisks@ request and
-- specify the next page token using the @pageToken@ parameter.
getDisksResponse_nextPageToken :: Lens.Lens' GetDisksResponse (Prelude.Maybe Prelude.Text)
getDisksResponse_nextPageToken = Lens.lens (\GetDisksResponse' {nextPageToken} -> nextPageToken) (\s@GetDisksResponse' {} a -> s {nextPageToken = a} :: GetDisksResponse)

-- | The response's http status code.
getDisksResponse_httpStatus :: Lens.Lens' GetDisksResponse Prelude.Int
getDisksResponse_httpStatus = Lens.lens (\GetDisksResponse' {httpStatus} -> httpStatus) (\s@GetDisksResponse' {} a -> s {httpStatus = a} :: GetDisksResponse)

instance Prelude.NFData GetDisksResponse where
  rnf GetDisksResponse' {..} =
    Prelude.rnf disks `Prelude.seq`
      Prelude.rnf nextPageToken `Prelude.seq`
        Prelude.rnf httpStatus
