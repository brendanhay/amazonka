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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available database blueprints in Amazon Lightsail. A
-- blueprint describes the major engine version of a database.
--
-- You can use a blueprint ID to create a new database that runs a specific
-- database engine.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
  ( -- * Creating a Request
    GetRelationalDatabaseBlueprints (..),
    newGetRelationalDatabaseBlueprints,

    -- * Request Lenses
    getRelationalDatabaseBlueprints_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabaseBlueprintsResponse (..),
    newGetRelationalDatabaseBlueprintsResponse,

    -- * Response Lenses
    getRelationalDatabaseBlueprintsResponse_blueprints,
    getRelationalDatabaseBlueprintsResponse_nextPageToken,
    getRelationalDatabaseBlueprintsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseBlueprints' smart constructor.
data GetRelationalDatabaseBlueprints = GetRelationalDatabaseBlueprints'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial
    -- @GetRelationalDatabaseBlueprints@ request. If your results are
    -- paginated, the response will return a next page token that you can
    -- specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getRelationalDatabaseBlueprints_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial
-- @GetRelationalDatabaseBlueprints@ request. If your results are
-- paginated, the response will return a next page token that you can
-- specify as the page token in a subsequent request.
newGetRelationalDatabaseBlueprints ::
  GetRelationalDatabaseBlueprints
newGetRelationalDatabaseBlueprints =
  GetRelationalDatabaseBlueprints'
    { pageToken =
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial
-- @GetRelationalDatabaseBlueprints@ request. If your results are
-- paginated, the response will return a next page token that you can
-- specify as the page token in a subsequent request.
getRelationalDatabaseBlueprints_pageToken :: Lens.Lens' GetRelationalDatabaseBlueprints (Prelude.Maybe Prelude.Text)
getRelationalDatabaseBlueprints_pageToken = Lens.lens (\GetRelationalDatabaseBlueprints' {pageToken} -> pageToken) (\s@GetRelationalDatabaseBlueprints' {} a -> s {pageToken = a} :: GetRelationalDatabaseBlueprints)

instance
  Core.AWSPager
    GetRelationalDatabaseBlueprints
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBlueprintsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBlueprintsResponse_blueprints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRelationalDatabaseBlueprints_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseBlueprintsResponse_nextPageToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetRelationalDatabaseBlueprints
  where
  type
    AWSResponse GetRelationalDatabaseBlueprints =
      GetRelationalDatabaseBlueprintsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBlueprintsResponse'
            Prelude.<$> (x Core..?> "blueprints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseBlueprints

instance
  Prelude.NFData
    GetRelationalDatabaseBlueprints

instance
  Core.ToHeaders
    GetRelationalDatabaseBlueprints
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseBlueprints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabaseBlueprints where
  toJSON GetRelationalDatabaseBlueprints' {..} =
    Core.object
      ( Prelude.catMaybes
          [("pageToken" Core..=) Prelude.<$> pageToken]
      )

instance Core.ToPath GetRelationalDatabaseBlueprints where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabaseBlueprints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseBlueprintsResponse' smart constructor.
data GetRelationalDatabaseBlueprintsResponse = GetRelationalDatabaseBlueprintsResponse'
  { -- | An object describing the result of your get relational database
    -- blueprints request.
    blueprints :: Prelude.Maybe [RelationalDatabaseBlueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseBlueprints@ request and specify the next page
    -- token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBlueprintsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprints', 'getRelationalDatabaseBlueprintsResponse_blueprints' - An object describing the result of your get relational database
-- blueprints request.
--
-- 'nextPageToken', 'getRelationalDatabaseBlueprintsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBlueprints@ request and specify the next page
-- token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getRelationalDatabaseBlueprintsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseBlueprintsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseBlueprintsResponse
newGetRelationalDatabaseBlueprintsResponse
  pHttpStatus_ =
    GetRelationalDatabaseBlueprintsResponse'
      { blueprints =
          Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object describing the result of your get relational database
-- blueprints request.
getRelationalDatabaseBlueprintsResponse_blueprints :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Prelude.Maybe [RelationalDatabaseBlueprint])
getRelationalDatabaseBlueprintsResponse_blueprints = Lens.lens (\GetRelationalDatabaseBlueprintsResponse' {blueprints} -> blueprints) (\s@GetRelationalDatabaseBlueprintsResponse' {} a -> s {blueprints = a} :: GetRelationalDatabaseBlueprintsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBlueprints@ request and specify the next page
-- token using the @pageToken@ parameter.
getRelationalDatabaseBlueprintsResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseBlueprintsResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseBlueprintsResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseBlueprintsResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseBlueprintsResponse)

-- | The response's http status code.
getRelationalDatabaseBlueprintsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse Prelude.Int
getRelationalDatabaseBlueprintsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseBlueprintsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseBlueprintsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseBlueprintsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseBlueprintsResponse
