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
-- Module      : Amazonka.GameLift.ListLocations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all custom and Amazon Web Services locations.
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListLocations
  ( -- * Creating a Request
    ListLocations (..),
    newListLocations,

    -- * Request Lenses
    listLocations_filters,
    listLocations_limit,
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
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLocations' smart constructor.
data ListLocations = ListLocations'
  { -- | Filters the list for @AWS@ or @CUSTOM@ locations.
    filters :: Prelude.Maybe (Prelude.NonEmpty LocationFilter),
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
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
-- 'filters', 'listLocations_filters' - Filters the list for @AWS@ or @CUSTOM@ locations.
--
-- 'limit', 'listLocations_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'nextToken', 'listLocations_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
newListLocations ::
  ListLocations
newListLocations =
  ListLocations'
    { filters = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the list for @AWS@ or @CUSTOM@ locations.
listLocations_filters :: Lens.Lens' ListLocations (Prelude.Maybe (Prelude.NonEmpty LocationFilter))
listLocations_filters = Lens.lens (\ListLocations' {filters} -> filters) (\s@ListLocations' {} a -> s {filters = a} :: ListLocations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listLocations_limit :: Lens.Lens' ListLocations (Prelude.Maybe Prelude.Natural)
listLocations_limit = Lens.lens (\ListLocations' {limit} -> limit) (\s@ListLocations' {} a -> s {limit = a} :: ListLocations)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listLocations_nextToken :: Lens.Lens' ListLocations (Prelude.Maybe Prelude.Text)
listLocations_nextToken = Lens.lens (\ListLocations' {nextToken} -> nextToken) (\s@ListLocations' {} a -> s {nextToken = a} :: ListLocations)

instance Core.AWSPager ListLocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLocationsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLocationsResponse_locations Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLocations_nextToken
          Lens..~ rs
          Lens.^? listLocationsResponse_nextToken Prelude.. Lens._Just

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
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLocations where
  rnf ListLocations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ListLocations" :: Prelude.ByteString),
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
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLocations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLocations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLocationsResponse' smart constructor.
data ListLocationsResponse = ListLocationsResponse'
  { -- | A collection of locations.
    locations :: Prelude.Maybe [LocationModel],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
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
-- 'locations', 'listLocationsResponse_locations' - A collection of locations.
--
-- 'nextToken', 'listLocationsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
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

-- | A collection of locations.
listLocationsResponse_locations :: Lens.Lens' ListLocationsResponse (Prelude.Maybe [LocationModel])
listLocationsResponse_locations = Lens.lens (\ListLocationsResponse' {locations} -> locations) (\s@ListLocationsResponse' {} a -> s {locations = a} :: ListLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listLocationsResponse_nextToken :: Lens.Lens' ListLocationsResponse (Prelude.Maybe Prelude.Text)
listLocationsResponse_nextToken = Lens.lens (\ListLocationsResponse' {nextToken} -> nextToken) (\s@ListLocationsResponse' {} a -> s {nextToken = a} :: ListLocationsResponse)

-- | The response's http status code.
listLocationsResponse_httpStatus :: Lens.Lens' ListLocationsResponse Prelude.Int
listLocationsResponse_httpStatus = Lens.lens (\ListLocationsResponse' {httpStatus} -> httpStatus) (\s@ListLocationsResponse' {} a -> s {httpStatus = a} :: ListLocationsResponse)

instance Prelude.NFData ListLocationsResponse where
  rnf ListLocationsResponse' {..} =
    Prelude.rnf locations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
