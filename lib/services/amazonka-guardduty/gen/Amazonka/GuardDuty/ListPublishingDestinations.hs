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
-- Module      : Amazonka.GuardDuty.ListPublishingDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of publishing destinations associated with the specified
-- @detectorId@.
module Amazonka.GuardDuty.ListPublishingDestinations
  ( -- * Creating a Request
    ListPublishingDestinations (..),
    newListPublishingDestinations,

    -- * Request Lenses
    listPublishingDestinations_maxResults,
    listPublishingDestinations_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPublishingDestinations' smart constructor.
data ListPublishingDestinations = ListPublishingDestinations'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector to retrieve publishing destinations for.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublishingDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPublishingDestinations_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listPublishingDestinations_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'detectorId', 'listPublishingDestinations_detectorId' - The ID of the detector to retrieve publishing destinations for.
newListPublishingDestinations ::
  -- | 'detectorId'
  Prelude.Text ->
  ListPublishingDestinations
newListPublishingDestinations pDetectorId_ =
  ListPublishingDestinations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | The maximum number of results to return in the response.
listPublishingDestinations_maxResults :: Lens.Lens' ListPublishingDestinations (Prelude.Maybe Prelude.Natural)
listPublishingDestinations_maxResults = Lens.lens (\ListPublishingDestinations' {maxResults} -> maxResults) (\s@ListPublishingDestinations' {} a -> s {maxResults = a} :: ListPublishingDestinations)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listPublishingDestinations_nextToken :: Lens.Lens' ListPublishingDestinations (Prelude.Maybe Prelude.Text)
listPublishingDestinations_nextToken = Lens.lens (\ListPublishingDestinations' {nextToken} -> nextToken) (\s@ListPublishingDestinations' {} a -> s {nextToken = a} :: ListPublishingDestinations)

-- | The ID of the detector to retrieve publishing destinations for.
listPublishingDestinations_detectorId :: Lens.Lens' ListPublishingDestinations Prelude.Text
listPublishingDestinations_detectorId = Lens.lens (\ListPublishingDestinations' {detectorId} -> detectorId) (\s@ListPublishingDestinations' {} a -> s {detectorId = a} :: ListPublishingDestinations)

instance Core.AWSRequest ListPublishingDestinations where
  type
    AWSResponse ListPublishingDestinations =
      ListPublishingDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublishingDestinationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "destinations" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPublishingDestinations where
  hashWithSalt _salt ListPublishingDestinations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData ListPublishingDestinations where
  rnf ListPublishingDestinations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders ListPublishingDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPublishingDestinations where
  toPath ListPublishingDestinations' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/publishingDestination"
      ]

instance Data.ToQuery ListPublishingDestinations where
  toQuery ListPublishingDestinations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPublishingDestinationsResponse' smart constructor.
data ListPublishingDestinationsResponse = ListPublishingDestinationsResponse'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @Destinations@ object that includes information about each publishing
    -- destination returned.
    destinations :: [Destination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPublishingDestinationsResponse
newListPublishingDestinationsResponse pHttpStatus_ =
  ListPublishingDestinationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      destinations = Prelude.mempty
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listPublishingDestinationsResponse_nextToken :: Lens.Lens' ListPublishingDestinationsResponse (Prelude.Maybe Prelude.Text)
listPublishingDestinationsResponse_nextToken = Lens.lens (\ListPublishingDestinationsResponse' {nextToken} -> nextToken) (\s@ListPublishingDestinationsResponse' {} a -> s {nextToken = a} :: ListPublishingDestinationsResponse)

-- | The response's http status code.
listPublishingDestinationsResponse_httpStatus :: Lens.Lens' ListPublishingDestinationsResponse Prelude.Int
listPublishingDestinationsResponse_httpStatus = Lens.lens (\ListPublishingDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListPublishingDestinationsResponse' {} a -> s {httpStatus = a} :: ListPublishingDestinationsResponse)

-- | A @Destinations@ object that includes information about each publishing
-- destination returned.
listPublishingDestinationsResponse_destinations :: Lens.Lens' ListPublishingDestinationsResponse [Destination]
listPublishingDestinationsResponse_destinations = Lens.lens (\ListPublishingDestinationsResponse' {destinations} -> destinations) (\s@ListPublishingDestinationsResponse' {} a -> s {destinations = a} :: ListPublishingDestinationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListPublishingDestinationsResponse
  where
  rnf ListPublishingDestinationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf destinations
