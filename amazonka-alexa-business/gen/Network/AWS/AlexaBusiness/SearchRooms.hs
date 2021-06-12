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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use in listing the specified set of rooms. The
    -- supported sort keys are RoomName and ProfileName.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use to list a specified set of rooms. The supported
    -- filter keys are RoomName and ProfileName.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchRooms_nextToken :: Lens.Lens' SearchRooms (Core.Maybe Core.Text)
searchRooms_nextToken = Lens.lens (\SearchRooms' {nextToken} -> nextToken) (\s@SearchRooms' {} a -> s {nextToken = a} :: SearchRooms)

-- | The sort order to use in listing the specified set of rooms. The
-- supported sort keys are RoomName and ProfileName.
searchRooms_sortCriteria :: Lens.Lens' SearchRooms (Core.Maybe [Sort])
searchRooms_sortCriteria = Lens.lens (\SearchRooms' {sortCriteria} -> sortCriteria) (\s@SearchRooms' {} a -> s {sortCriteria = a} :: SearchRooms) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchRooms_maxResults :: Lens.Lens' SearchRooms (Core.Maybe Core.Natural)
searchRooms_maxResults = Lens.lens (\SearchRooms' {maxResults} -> maxResults) (\s@SearchRooms' {} a -> s {maxResults = a} :: SearchRooms)

-- | The filters to use to list a specified set of rooms. The supported
-- filter keys are RoomName and ProfileName.
searchRooms_filters :: Lens.Lens' SearchRooms (Core.Maybe [Filter])
searchRooms_filters = Lens.lens (\SearchRooms' {filters} -> filters) (\s@SearchRooms' {} a -> s {filters = a} :: SearchRooms) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager SearchRooms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchRoomsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchRoomsResponse_rooms Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchRooms_nextToken
          Lens..~ rs
          Lens.^? searchRoomsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest SearchRooms where
  type AWSResponse SearchRooms = SearchRoomsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRoomsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Rooms" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchRooms

instance Core.NFData SearchRooms

instance Core.ToHeaders SearchRooms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.SearchRooms" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchRooms where
  toJSON SearchRooms' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchRooms where
  toPath = Core.const "/"

instance Core.ToQuery SearchRooms where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchRoomsResponse' smart constructor.
data SearchRoomsResponse = SearchRoomsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The rooms that meet the specified set of filter criteria, in sort order.
    rooms :: Core.Maybe [RoomData],
    -- | The total number of rooms returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SearchRoomsResponse
newSearchRoomsResponse pHttpStatus_ =
  SearchRoomsResponse'
    { nextToken = Core.Nothing,
      rooms = Core.Nothing,
      totalCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchRoomsResponse_nextToken :: Lens.Lens' SearchRoomsResponse (Core.Maybe Core.Text)
searchRoomsResponse_nextToken = Lens.lens (\SearchRoomsResponse' {nextToken} -> nextToken) (\s@SearchRoomsResponse' {} a -> s {nextToken = a} :: SearchRoomsResponse)

-- | The rooms that meet the specified set of filter criteria, in sort order.
searchRoomsResponse_rooms :: Lens.Lens' SearchRoomsResponse (Core.Maybe [RoomData])
searchRoomsResponse_rooms = Lens.lens (\SearchRoomsResponse' {rooms} -> rooms) (\s@SearchRoomsResponse' {} a -> s {rooms = a} :: SearchRoomsResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of rooms returned.
searchRoomsResponse_totalCount :: Lens.Lens' SearchRoomsResponse (Core.Maybe Core.Int)
searchRoomsResponse_totalCount = Lens.lens (\SearchRoomsResponse' {totalCount} -> totalCount) (\s@SearchRoomsResponse' {} a -> s {totalCount = a} :: SearchRoomsResponse)

-- | The response's http status code.
searchRoomsResponse_httpStatus :: Lens.Lens' SearchRoomsResponse Core.Int
searchRoomsResponse_httpStatus = Lens.lens (\SearchRoomsResponse' {httpStatus} -> httpStatus) (\s@SearchRoomsResponse' {} a -> s {httpStatus = a} :: SearchRoomsResponse)

instance Core.NFData SearchRoomsResponse
