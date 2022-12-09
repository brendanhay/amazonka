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
-- Module      : Amazonka.Lightsail.GetBlueprints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available instance images, or /blueprints/. You can
-- use a blueprint to create a new instance already running a specific
-- operating system, as well as a preinstalled app or development stack.
-- The software each instance is running depends on the blueprint image you
-- choose.
--
-- Use active blueprints when creating new instances. Inactive blueprints
-- are listed to support customers with existing instances and are not
-- necessarily available to create new instances. Blueprints are marked
-- inactive when they become outdated due to operating system updates or
-- new application releases.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetBlueprints
  ( -- * Creating a Request
    GetBlueprints (..),
    newGetBlueprints,

    -- * Request Lenses
    getBlueprints_includeInactive,
    getBlueprints_pageToken,

    -- * Destructuring the Response
    GetBlueprintsResponse (..),
    newGetBlueprintsResponse,

    -- * Response Lenses
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_nextPageToken,
    getBlueprintsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBlueprints' smart constructor.
data GetBlueprints = GetBlueprints'
  { -- | A Boolean value that indicates whether to include inactive (unavailable)
    -- blueprints in the response of your request.
    includeInactive :: Prelude.Maybe Prelude.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBlueprints@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeInactive', 'getBlueprints_includeInactive' - A Boolean value that indicates whether to include inactive (unavailable)
-- blueprints in the response of your request.
--
-- 'pageToken', 'getBlueprints_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetBlueprints ::
  GetBlueprints
newGetBlueprints =
  GetBlueprints'
    { includeInactive = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | A Boolean value that indicates whether to include inactive (unavailable)
-- blueprints in the response of your request.
getBlueprints_includeInactive :: Lens.Lens' GetBlueprints (Prelude.Maybe Prelude.Bool)
getBlueprints_includeInactive = Lens.lens (\GetBlueprints' {includeInactive} -> includeInactive) (\s@GetBlueprints' {} a -> s {includeInactive = a} :: GetBlueprints)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getBlueprints_pageToken :: Lens.Lens' GetBlueprints (Prelude.Maybe Prelude.Text)
getBlueprints_pageToken = Lens.lens (\GetBlueprints' {pageToken} -> pageToken) (\s@GetBlueprints' {} a -> s {pageToken = a} :: GetBlueprints)

instance Core.AWSPager GetBlueprints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBlueprintsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBlueprintsResponse_blueprints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getBlueprints_pageToken
          Lens..~ rs
          Lens.^? getBlueprintsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetBlueprints where
  type
    AWSResponse GetBlueprints =
      GetBlueprintsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintsResponse'
            Prelude.<$> (x Data..?> "blueprints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBlueprints where
  hashWithSalt _salt GetBlueprints' {..} =
    _salt `Prelude.hashWithSalt` includeInactive
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetBlueprints where
  rnf GetBlueprints' {..} =
    Prelude.rnf includeInactive
      `Prelude.seq` Prelude.rnf pageToken

instance Data.ToHeaders GetBlueprints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetBlueprints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBlueprints where
  toJSON GetBlueprints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includeInactive" Data..=)
              Prelude.<$> includeInactive,
            ("pageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath GetBlueprints where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBlueprints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlueprintsResponse' smart constructor.
data GetBlueprintsResponse = GetBlueprintsResponse'
  { -- | An array of key-value pairs that contains information about the
    -- available blueprints.
    blueprints :: Prelude.Maybe [Blueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetBlueprints@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprints', 'getBlueprintsResponse_blueprints' - An array of key-value pairs that contains information about the
-- available blueprints.
--
-- 'nextPageToken', 'getBlueprintsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBlueprints@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getBlueprintsResponse_httpStatus' - The response's http status code.
newGetBlueprintsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBlueprintsResponse
newGetBlueprintsResponse pHttpStatus_ =
  GetBlueprintsResponse'
    { blueprints =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs that contains information about the
-- available blueprints.
getBlueprintsResponse_blueprints :: Lens.Lens' GetBlueprintsResponse (Prelude.Maybe [Blueprint])
getBlueprintsResponse_blueprints = Lens.lens (\GetBlueprintsResponse' {blueprints} -> blueprints) (\s@GetBlueprintsResponse' {} a -> s {blueprints = a} :: GetBlueprintsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBlueprints@ request
-- and specify the next page token using the @pageToken@ parameter.
getBlueprintsResponse_nextPageToken :: Lens.Lens' GetBlueprintsResponse (Prelude.Maybe Prelude.Text)
getBlueprintsResponse_nextPageToken = Lens.lens (\GetBlueprintsResponse' {nextPageToken} -> nextPageToken) (\s@GetBlueprintsResponse' {} a -> s {nextPageToken = a} :: GetBlueprintsResponse)

-- | The response's http status code.
getBlueprintsResponse_httpStatus :: Lens.Lens' GetBlueprintsResponse Prelude.Int
getBlueprintsResponse_httpStatus = Lens.lens (\GetBlueprintsResponse' {httpStatus} -> httpStatus) (\s@GetBlueprintsResponse' {} a -> s {httpStatus = a} :: GetBlueprintsResponse)

instance Prelude.NFData GetBlueprintsResponse where
  rnf GetBlueprintsResponse' {..} =
    Prelude.rnf blueprints
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
