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
-- Module      : Amazonka.DataSync.ListLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of source and destination locations.
--
-- If you have more locations than are returned in a response (that is, the
-- response returns only a truncated list of your agents), the response
-- contains a token that you can specify in your next request to fetch the
-- next page of locations.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListLocations
  ( -- * Creating a Request
    ListLocations (..),
    newListLocations,

    -- * Request Lenses
    listLocations_filters,
    listLocations_maxResults,
    listLocations_nextToken,

    -- * Destructuring the Response
    ListLocationsResponse (..),
    newListLocationsResponse,

    -- * Response Lenses
    listLocationsResponse_locations,
    listLocationsResponse_nextToken,
    listLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListLocationsRequest
--
-- /See:/ 'newListLocations' smart constructor.
data ListLocations = ListLocations'
  { -- | You can use API filters to narrow down the list of resources returned by
    -- @ListLocations@. For example, to retrieve all tasks on a specific source
    -- location, you can use @ListLocations@ with filter name @LocationType S3@
    -- and @Operator Equals@.
    filters :: Prelude.Maybe [LocationFilter],
    -- | The maximum number of locations to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque string that indicates the position at which to begin the next
    -- list of locations.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listLocations_filters' - You can use API filters to narrow down the list of resources returned by
-- @ListLocations@. For example, to retrieve all tasks on a specific source
-- location, you can use @ListLocations@ with filter name @LocationType S3@
-- and @Operator Equals@.
--
-- 'maxResults', 'listLocations_maxResults' - The maximum number of locations to return.
--
-- 'nextToken', 'listLocations_nextToken' - An opaque string that indicates the position at which to begin the next
-- list of locations.
newListLocations ::
  ListLocations
newListLocations =
  ListLocations'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | You can use API filters to narrow down the list of resources returned by
-- @ListLocations@. For example, to retrieve all tasks on a specific source
-- location, you can use @ListLocations@ with filter name @LocationType S3@
-- and @Operator Equals@.
listLocations_filters :: Lens.Lens' ListLocations (Prelude.Maybe [LocationFilter])
listLocations_filters = Lens.lens (\ListLocations' {filters} -> filters) (\s@ListLocations' {} a -> s {filters = a} :: ListLocations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of locations to return.
listLocations_maxResults :: Lens.Lens' ListLocations (Prelude.Maybe Prelude.Natural)
listLocations_maxResults = Lens.lens (\ListLocations' {maxResults} -> maxResults) (\s@ListLocations' {} a -> s {maxResults = a} :: ListLocations)

-- | An opaque string that indicates the position at which to begin the next
-- list of locations.
listLocations_nextToken :: Lens.Lens' ListLocations (Prelude.Maybe Prelude.Text)
listLocations_nextToken = Lens.lens (\ListLocations' {nextToken} -> nextToken) (\s@ListLocations' {} a -> s {nextToken = a} :: ListLocations)

instance Core.AWSPager ListLocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLocationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLocationsResponse_locations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listLocations_nextToken
              Lens..~ rs
              Lens.^? listLocationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListLocations where
  type
    AWSResponse ListLocations =
      ListLocationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLocationsResponse'
            Prelude.<$> (x Data..?> "Locations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLocations where
  hashWithSalt _salt ListLocations' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLocations where
  rnf ListLocations' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.ListLocations" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLocations where
  toJSON ListLocations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLocations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLocations where
  toQuery = Prelude.const Prelude.mempty

-- | ListLocationsResponse
--
-- /See:/ 'newListLocationsResponse' smart constructor.
data ListLocationsResponse = ListLocationsResponse'
  { -- | An array that contains a list of locations.
    locations :: Prelude.Maybe [LocationListEntry],
    -- | An opaque string that indicates the position at which to begin returning
    -- the next list of locations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locations', 'listLocationsResponse_locations' - An array that contains a list of locations.
--
-- 'nextToken', 'listLocationsResponse_nextToken' - An opaque string that indicates the position at which to begin returning
-- the next list of locations.
--
-- 'httpStatus', 'listLocationsResponse_httpStatus' - The response's http status code.
newListLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLocationsResponse
newListLocationsResponse pHttpStatus_ =
  ListLocationsResponse'
    { locations = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains a list of locations.
listLocationsResponse_locations :: Lens.Lens' ListLocationsResponse (Prelude.Maybe [LocationListEntry])
listLocationsResponse_locations = Lens.lens (\ListLocationsResponse' {locations} -> locations) (\s@ListLocationsResponse' {} a -> s {locations = a} :: ListLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque string that indicates the position at which to begin returning
-- the next list of locations.
listLocationsResponse_nextToken :: Lens.Lens' ListLocationsResponse (Prelude.Maybe Prelude.Text)
listLocationsResponse_nextToken = Lens.lens (\ListLocationsResponse' {nextToken} -> nextToken) (\s@ListLocationsResponse' {} a -> s {nextToken = a} :: ListLocationsResponse)

-- | The response's http status code.
listLocationsResponse_httpStatus :: Lens.Lens' ListLocationsResponse Prelude.Int
listLocationsResponse_httpStatus = Lens.lens (\ListLocationsResponse' {httpStatus} -> httpStatus) (\s@ListLocationsResponse' {} a -> s {httpStatus = a} :: ListLocationsResponse)

instance Prelude.NFData ListLocationsResponse where
  rnf ListLocationsResponse' {..} =
    Prelude.rnf locations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
