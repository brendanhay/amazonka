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
-- Module      : Network.AWS.GuardDuty.ListPublishingDestinations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of publishing destinations associated with the specified
-- @dectectorId@.
module Network.AWS.GuardDuty.ListPublishingDestinations
  ( -- * Creating a Request
    ListPublishingDestinations (..),
    newListPublishingDestinations,

    -- * Request Lenses
    listPublishingDestinations_nextToken,
    listPublishingDestinations_maxResults,
    listPublishingDestinations_detectorId,

    -- * Destructuring the Response
    ListPublishingDestinationsResponse (..),
    newListPublishingDestinationsResponse,

    -- * Response Lenses
    listPublishingDestinationsResponse_nextToken,
    listPublishingDestinationsResponse_httpStatus,
    listPublishingDestinationsResponse_destinations,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPublishingDestinations' smart constructor.
data ListPublishingDestinations = ListPublishingDestinations'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the detector to retrieve publishing destinations for.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPublishingDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPublishingDestinations_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'maxResults', 'listPublishingDestinations_maxResults' - The maximum number of results to return in the response.
--
-- 'detectorId', 'listPublishingDestinations_detectorId' - The ID of the detector to retrieve publishing destinations for.
newListPublishingDestinations ::
  -- | 'detectorId'
  Core.Text ->
  ListPublishingDestinations
newListPublishingDestinations pDetectorId_ =
  ListPublishingDestinations'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listPublishingDestinations_nextToken :: Lens.Lens' ListPublishingDestinations (Core.Maybe Core.Text)
listPublishingDestinations_nextToken = Lens.lens (\ListPublishingDestinations' {nextToken} -> nextToken) (\s@ListPublishingDestinations' {} a -> s {nextToken = a} :: ListPublishingDestinations)

-- | The maximum number of results to return in the response.
listPublishingDestinations_maxResults :: Lens.Lens' ListPublishingDestinations (Core.Maybe Core.Natural)
listPublishingDestinations_maxResults = Lens.lens (\ListPublishingDestinations' {maxResults} -> maxResults) (\s@ListPublishingDestinations' {} a -> s {maxResults = a} :: ListPublishingDestinations)

-- | The ID of the detector to retrieve publishing destinations for.
listPublishingDestinations_detectorId :: Lens.Lens' ListPublishingDestinations Core.Text
listPublishingDestinations_detectorId = Lens.lens (\ListPublishingDestinations' {detectorId} -> detectorId) (\s@ListPublishingDestinations' {} a -> s {detectorId = a} :: ListPublishingDestinations)

instance Core.AWSRequest ListPublishingDestinations where
  type
    AWSResponse ListPublishingDestinations =
      ListPublishingDestinationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublishingDestinationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "destinations" Core..!@ Core.mempty)
      )

instance Core.Hashable ListPublishingDestinations

instance Core.NFData ListPublishingDestinations

instance Core.ToHeaders ListPublishingDestinations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListPublishingDestinations where
  toPath ListPublishingDestinations' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/publishingDestination"
      ]

instance Core.ToQuery ListPublishingDestinations where
  toQuery ListPublishingDestinations' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPublishingDestinationsResponse' smart constructor.
data ListPublishingDestinationsResponse = ListPublishingDestinationsResponse'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A @Destinations@ object that includes information about each publishing
    -- destination returned.
    destinations :: [Destination]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPublishingDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPublishingDestinationsResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listPublishingDestinationsResponse_httpStatus' - The response's http status code.
--
-- 'destinations', 'listPublishingDestinationsResponse_destinations' - A @Destinations@ object that includes information about each publishing
-- destination returned.
newListPublishingDestinationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPublishingDestinationsResponse
newListPublishingDestinationsResponse pHttpStatus_ =
  ListPublishingDestinationsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      destinations = Core.mempty
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listPublishingDestinationsResponse_nextToken :: Lens.Lens' ListPublishingDestinationsResponse (Core.Maybe Core.Text)
listPublishingDestinationsResponse_nextToken = Lens.lens (\ListPublishingDestinationsResponse' {nextToken} -> nextToken) (\s@ListPublishingDestinationsResponse' {} a -> s {nextToken = a} :: ListPublishingDestinationsResponse)

-- | The response's http status code.
listPublishingDestinationsResponse_httpStatus :: Lens.Lens' ListPublishingDestinationsResponse Core.Int
listPublishingDestinationsResponse_httpStatus = Lens.lens (\ListPublishingDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListPublishingDestinationsResponse' {} a -> s {httpStatus = a} :: ListPublishingDestinationsResponse)

-- | A @Destinations@ object that includes information about each publishing
-- destination returned.
listPublishingDestinationsResponse_destinations :: Lens.Lens' ListPublishingDestinationsResponse [Destination]
listPublishingDestinationsResponse_destinations = Lens.lens (\ListPublishingDestinationsResponse' {destinations} -> destinations) (\s@ListPublishingDestinationsResponse' {} a -> s {destinations = a} :: ListPublishingDestinationsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListPublishingDestinationsResponse
