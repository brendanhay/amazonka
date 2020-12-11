{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchRooms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches rooms and lists the ones that meet a set of filter and sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchRooms
  ( -- * Creating a request
    SearchRooms (..),
    mkSearchRooms,

    -- ** Request lenses
    srFilters,
    srSortCriteria,
    srNextToken,
    srMaxResults,

    -- * Destructuring the response
    SearchRoomsResponse (..),
    mkSearchRoomsResponse,

    -- ** Response lenses
    srrsRooms,
    srrsNextToken,
    srrsTotalCount,
    srrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { filters :: Lude.Maybe [Filter],
    sortCriteria :: Lude.Maybe [Sort],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchRooms' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'sortCriteria' - The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
mkSearchRooms ::
  SearchRooms
mkSearchRooms =
  SearchRooms'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srFilters :: Lens.Lens' SearchRooms (Lude.Maybe [Filter])
srFilters = Lens.lens (filters :: SearchRooms -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchRooms)
{-# DEPRECATED srFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSortCriteria :: Lens.Lens' SearchRooms (Lude.Maybe [Sort])
srSortCriteria = Lens.lens (sortCriteria :: SearchRooms -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchRooms)
{-# DEPRECATED srSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNextToken :: Lens.Lens' SearchRooms (Lude.Maybe Lude.Text)
srNextToken = Lens.lens (nextToken :: SearchRooms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchRooms)
{-# DEPRECATED srNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srMaxResults :: Lens.Lens' SearchRooms (Lude.Maybe Lude.Natural)
srMaxResults = Lens.lens (maxResults :: SearchRooms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchRooms)
{-# DEPRECATED srMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchRooms where
  page rq rs
    | Page.stop (rs Lens.^. srrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. srrsRooms) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& srNextToken Lens..~ rs Lens.^. srrsNextToken

instance Lude.AWSRequest SearchRooms where
  type Rs SearchRooms = SearchRoomsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchRoomsResponse'
            Lude.<$> (x Lude..?> "Rooms" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchRooms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchRooms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchRooms where
  toJSON SearchRooms' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchRooms where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchRooms where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchRoomsResponse' smart constructor.
data SearchRoomsResponse = SearchRoomsResponse'
  { rooms ::
      Lude.Maybe [RoomData],
    nextToken :: Lude.Maybe Lude.Text,
    totalCount :: Lude.Maybe Lude.Int,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchRoomsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'responseStatus' - The response status code.
-- * 'rooms' - The rooms that meet the specified set of filter criteria, in sort order.
-- * 'totalCount' - The total number of rooms returned.
mkSearchRoomsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchRoomsResponse
mkSearchRoomsResponse pResponseStatus_ =
  SearchRoomsResponse'
    { rooms = Lude.Nothing,
      nextToken = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rooms that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'rooms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsRooms :: Lens.Lens' SearchRoomsResponse (Lude.Maybe [RoomData])
srrsRooms = Lens.lens (rooms :: SearchRoomsResponse -> Lude.Maybe [RoomData]) (\s a -> s {rooms = a} :: SearchRoomsResponse)
{-# DEPRECATED srrsRooms "Use generic-lens or generic-optics with 'rooms' instead." #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsNextToken :: Lens.Lens' SearchRoomsResponse (Lude.Maybe Lude.Text)
srrsNextToken = Lens.lens (nextToken :: SearchRoomsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchRoomsResponse)
{-# DEPRECATED srrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of rooms returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsTotalCount :: Lens.Lens' SearchRoomsResponse (Lude.Maybe Lude.Int)
srrsTotalCount = Lens.lens (totalCount :: SearchRoomsResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchRoomsResponse)
{-# DEPRECATED srrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SearchRoomsResponse Lude.Int
srrsResponseStatus = Lens.lens (responseStatus :: SearchRoomsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchRoomsResponse)
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
