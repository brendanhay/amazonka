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
-- Module      : Network.AWS.Lightsail.GetBlueprints
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Lightsail.GetBlueprints
  ( -- * Creating a Request
    GetBlueprints (..),
    newGetBlueprints,

    -- * Request Lenses
    getBlueprints_pageToken,
    getBlueprints_includeInactive,

    -- * Destructuring the Response
    GetBlueprintsResponse (..),
    newGetBlueprintsResponse,

    -- * Response Lenses
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_nextPageToken,
    getBlueprintsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBlueprints' smart constructor.
data GetBlueprints = GetBlueprints'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBlueprints@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether to include inactive results in your
    -- request.
    includeInactive :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getBlueprints_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
--
-- 'includeInactive', 'getBlueprints_includeInactive' - A Boolean value indicating whether to include inactive results in your
-- request.
newGetBlueprints ::
  GetBlueprints
newGetBlueprints =
  GetBlueprints'
    { pageToken = Core.Nothing,
      includeInactive = Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getBlueprints_pageToken :: Lens.Lens' GetBlueprints (Core.Maybe Core.Text)
getBlueprints_pageToken = Lens.lens (\GetBlueprints' {pageToken} -> pageToken) (\s@GetBlueprints' {} a -> s {pageToken = a} :: GetBlueprints)

-- | A Boolean value indicating whether to include inactive results in your
-- request.
getBlueprints_includeInactive :: Lens.Lens' GetBlueprints (Core.Maybe Core.Bool)
getBlueprints_includeInactive = Lens.lens (\GetBlueprints' {includeInactive} -> includeInactive) (\s@GetBlueprints' {} a -> s {includeInactive = a} :: GetBlueprints)

instance Core.AWSPager GetBlueprints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBlueprintsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getBlueprintsResponse_blueprints Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getBlueprints_pageToken
          Lens..~ rs
          Lens.^? getBlueprintsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetBlueprints where
  type
    AWSResponse GetBlueprints =
      GetBlueprintsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintsResponse'
            Core.<$> (x Core..?> "blueprints" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBlueprints

instance Core.NFData GetBlueprints

instance Core.ToHeaders GetBlueprints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetBlueprints" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetBlueprints where
  toJSON GetBlueprints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("pageToken" Core..=) Core.<$> pageToken,
            ("includeInactive" Core..=)
              Core.<$> includeInactive
          ]
      )

instance Core.ToPath GetBlueprints where
  toPath = Core.const "/"

instance Core.ToQuery GetBlueprints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetBlueprintsResponse' smart constructor.
data GetBlueprintsResponse = GetBlueprintsResponse'
  { -- | An array of key-value pairs that contains information about the
    -- available blueprints.
    blueprints :: Core.Maybe [Blueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetBlueprints@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetBlueprintsResponse
newGetBlueprintsResponse pHttpStatus_ =
  GetBlueprintsResponse'
    { blueprints = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs that contains information about the
-- available blueprints.
getBlueprintsResponse_blueprints :: Lens.Lens' GetBlueprintsResponse (Core.Maybe [Blueprint])
getBlueprintsResponse_blueprints = Lens.lens (\GetBlueprintsResponse' {blueprints} -> blueprints) (\s@GetBlueprintsResponse' {} a -> s {blueprints = a} :: GetBlueprintsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBlueprints@ request
-- and specify the next page token using the @pageToken@ parameter.
getBlueprintsResponse_nextPageToken :: Lens.Lens' GetBlueprintsResponse (Core.Maybe Core.Text)
getBlueprintsResponse_nextPageToken = Lens.lens (\GetBlueprintsResponse' {nextPageToken} -> nextPageToken) (\s@GetBlueprintsResponse' {} a -> s {nextPageToken = a} :: GetBlueprintsResponse)

-- | The response's http status code.
getBlueprintsResponse_httpStatus :: Lens.Lens' GetBlueprintsResponse Core.Int
getBlueprintsResponse_httpStatus = Lens.lens (\GetBlueprintsResponse' {httpStatus} -> httpStatus) (\s@GetBlueprintsResponse' {} a -> s {httpStatus = a} :: GetBlueprintsResponse)

instance Core.NFData GetBlueprintsResponse
