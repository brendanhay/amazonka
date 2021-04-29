{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.SearchRooms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches rooms and lists the ones that meet a set of filter and sort
-- criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchRooms
  ( -- * Creating a Request
    SearchRooms (..),
    newSearchRooms,

    -- * Request Lenses
    searchRooms_nextToken,
    searchRooms_sortCriteria,
    searchRooms_maxResults,
    searchRooms_filters,

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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of rooms. The
    -- supported sort keys are RoomName and ProfileName.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filters to use to list a specified set of rooms. The supported
    -- filter keys are RoomName and ProfileName.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SearchRooms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchRooms_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'sortCriteria', 'searchRooms_sortCriteria' - The sort order to use in listing the specified set of rooms. The
-- supported sort keys are RoomName and ProfileName.
--
-- 'maxResults', 'searchRooms_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchRooms_filters' - The filters to use to list a specified set of rooms. The supported
-- filter keys are RoomName and ProfileName.
newSearchRooms ::
  SearchRooms
newSearchRooms =
  SearchRooms'
    { nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchRooms_nextToken :: Lens.Lens' SearchRooms (Prelude.Maybe Prelude.Text)
searchRooms_nextToken = Lens.lens (\SearchRooms' {nextToken} -> nextToken) (\s@SearchRooms' {} a -> s {nextToken = a} :: SearchRooms)

-- | The sort order to use in listing the specified set of rooms. The
-- supported sort keys are RoomName and ProfileName.
searchRooms_sortCriteria :: Lens.Lens' SearchRooms (Prelude.Maybe [Sort])
searchRooms_sortCriteria = Lens.lens (\SearchRooms' {sortCriteria} -> sortCriteria) (\s@SearchRooms' {} a -> s {sortCriteria = a} :: SearchRooms) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchRooms_maxResults :: Lens.Lens' SearchRooms (Prelude.Maybe Prelude.Natural)
searchRooms_maxResults = Lens.lens (\SearchRooms' {maxResults} -> maxResults) (\s@SearchRooms' {} a -> s {maxResults = a} :: SearchRooms)

-- | The filters to use to list a specified set of rooms. The supported
-- filter keys are RoomName and ProfileName.
searchRooms_filters :: Lens.Lens' SearchRooms (Prelude.Maybe [Filter])
searchRooms_filters = Lens.lens (\SearchRooms' {filters} -> filters) (\s@SearchRooms' {} a -> s {filters = a} :: SearchRooms) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager SearchRooms where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? searchRoomsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? searchRoomsResponse_rooms Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& searchRooms_nextToken
          Lens..~ rs
          Lens.^? searchRoomsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest SearchRooms where
  type Rs SearchRooms = SearchRoomsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRoomsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Rooms" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchRooms

instance Prelude.NFData SearchRooms

instance Prelude.ToHeaders SearchRooms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.SearchRooms" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SearchRooms where
  toJSON SearchRooms' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("SortCriteria" Prelude..=) Prelude.<$> sortCriteria,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath SearchRooms where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SearchRooms where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
searchRoomsResponse_rooms = Lens.lens (\SearchRoomsResponse' {rooms} -> rooms) (\s@SearchRoomsResponse' {} a -> s {rooms = a} :: SearchRoomsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The total number of rooms returned.
searchRoomsResponse_totalCount :: Lens.Lens' SearchRoomsResponse (Prelude.Maybe Prelude.Int)
searchRoomsResponse_totalCount = Lens.lens (\SearchRoomsResponse' {totalCount} -> totalCount) (\s@SearchRoomsResponse' {} a -> s {totalCount = a} :: SearchRoomsResponse)

-- | The response's http status code.
searchRoomsResponse_httpStatus :: Lens.Lens' SearchRoomsResponse Prelude.Int
searchRoomsResponse_httpStatus = Lens.lens (\SearchRoomsResponse' {httpStatus} -> httpStatus) (\s@SearchRoomsResponse' {} a -> s {httpStatus = a} :: SearchRoomsResponse)

instance Prelude.NFData SearchRoomsResponse
