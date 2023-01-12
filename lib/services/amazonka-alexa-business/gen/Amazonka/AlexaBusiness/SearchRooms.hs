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
-- Module      : Amazonka.AlexaBusiness.SearchRooms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches rooms and lists the ones that meet a set of filter and sort
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.SearchRooms
  ( -- * Creating a Request
    SearchRooms (..),
    newSearchRooms,

    -- * Request Lenses
    searchRooms_filters,
    searchRooms_maxResults,
    searchRooms_nextToken,
    searchRooms_sortCriteria,

    -- * Destructuring the Response
    SearchRoomsResponse (..),
    newSearchRoomsResponse,

    -- * Response Lenses
    searchRoomsResponse_nextToken,
    searchRoomsResponse_rooms,
    searchRoomsResponse_totalCount,
    searchRoomsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { -- | The filters to use to list a specified set of rooms. The supported
    -- filter keys are RoomName and ProfileName.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of rooms. The
    -- supported sort keys are RoomName and ProfileName.
    sortCriteria :: Prelude.Maybe [Sort]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRooms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchRooms_filters' - The filters to use to list a specified set of rooms. The supported
-- filter keys are RoomName and ProfileName.
--
-- 'maxResults', 'searchRooms_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'searchRooms_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'sortCriteria', 'searchRooms_sortCriteria' - The sort order to use in listing the specified set of rooms. The
-- supported sort keys are RoomName and ProfileName.
newSearchRooms ::
  SearchRooms
newSearchRooms =
  SearchRooms'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | The filters to use to list a specified set of rooms. The supported
-- filter keys are RoomName and ProfileName.
searchRooms_filters :: Lens.Lens' SearchRooms (Prelude.Maybe [Filter])
searchRooms_filters = Lens.lens (\SearchRooms' {filters} -> filters) (\s@SearchRooms' {} a -> s {filters = a} :: SearchRooms) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchRooms_maxResults :: Lens.Lens' SearchRooms (Prelude.Maybe Prelude.Natural)
searchRooms_maxResults = Lens.lens (\SearchRooms' {maxResults} -> maxResults) (\s@SearchRooms' {} a -> s {maxResults = a} :: SearchRooms)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchRooms_nextToken :: Lens.Lens' SearchRooms (Prelude.Maybe Prelude.Text)
searchRooms_nextToken = Lens.lens (\SearchRooms' {nextToken} -> nextToken) (\s@SearchRooms' {} a -> s {nextToken = a} :: SearchRooms)

-- | The sort order to use in listing the specified set of rooms. The
-- supported sort keys are RoomName and ProfileName.
searchRooms_sortCriteria :: Lens.Lens' SearchRooms (Prelude.Maybe [Sort])
searchRooms_sortCriteria = Lens.lens (\SearchRooms' {sortCriteria} -> sortCriteria) (\s@SearchRooms' {} a -> s {sortCriteria = a} :: SearchRooms) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager SearchRooms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchRoomsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchRoomsResponse_rooms Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchRooms_nextToken
          Lens..~ rs
          Lens.^? searchRoomsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchRooms where
  type AWSResponse SearchRooms = SearchRoomsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRoomsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Rooms" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchRooms where
  hashWithSalt _salt SearchRooms' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchRooms where
  rnf SearchRooms' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria

instance Data.ToHeaders SearchRooms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchRooms" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchRooms where
  toJSON SearchRooms' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchRooms where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchRooms where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchRoomsResponse' smart constructor.
data SearchRoomsResponse = SearchRoomsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The rooms that meet the specified set of filter criteria, in sort order.
    rooms :: Prelude.Maybe [RoomData],
    -- | The total number of rooms returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRoomsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchRoomsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'rooms', 'searchRoomsResponse_rooms' - The rooms that meet the specified set of filter criteria, in sort order.
--
-- 'totalCount', 'searchRoomsResponse_totalCount' - The total number of rooms returned.
--
-- 'httpStatus', 'searchRoomsResponse_httpStatus' - The response's http status code.
newSearchRoomsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchRoomsResponse
newSearchRoomsResponse pHttpStatus_ =
  SearchRoomsResponse'
    { nextToken = Prelude.Nothing,
      rooms = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchRoomsResponse_nextToken :: Lens.Lens' SearchRoomsResponse (Prelude.Maybe Prelude.Text)
searchRoomsResponse_nextToken = Lens.lens (\SearchRoomsResponse' {nextToken} -> nextToken) (\s@SearchRoomsResponse' {} a -> s {nextToken = a} :: SearchRoomsResponse)

-- | The rooms that meet the specified set of filter criteria, in sort order.
searchRoomsResponse_rooms :: Lens.Lens' SearchRoomsResponse (Prelude.Maybe [RoomData])
searchRoomsResponse_rooms = Lens.lens (\SearchRoomsResponse' {rooms} -> rooms) (\s@SearchRoomsResponse' {} a -> s {rooms = a} :: SearchRoomsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of rooms returned.
searchRoomsResponse_totalCount :: Lens.Lens' SearchRoomsResponse (Prelude.Maybe Prelude.Int)
searchRoomsResponse_totalCount = Lens.lens (\SearchRoomsResponse' {totalCount} -> totalCount) (\s@SearchRoomsResponse' {} a -> s {totalCount = a} :: SearchRoomsResponse)

-- | The response's http status code.
searchRoomsResponse_httpStatus :: Lens.Lens' SearchRoomsResponse Prelude.Int
searchRoomsResponse_httpStatus = Lens.lens (\SearchRoomsResponse' {httpStatus} -> httpStatus) (\s@SearchRoomsResponse' {} a -> s {httpStatus = a} :: SearchRoomsResponse)

instance Prelude.NFData SearchRoomsResponse where
  rnf SearchRoomsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rooms
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf httpStatus
