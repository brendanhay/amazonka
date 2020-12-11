{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUsers
  ( -- * Creating a request
    DescribeUsers (..),
    mkDescribeUsers,

    -- ** Request lenses
    duFilters,
    duEngine,
    duUserId,
    duMarker,
    duMaxRecords,

    -- * Destructuring the response
    DescribeUsersResponse (..),
    mkDescribeUsersResponse,

    -- ** Response lenses
    dursUsers,
    dursMarker,
    dursResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { filters :: Lude.Maybe [Filter],
    engine :: Lude.Maybe Lude.Text,
    userId :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsers' with the minimum fields required to make a request.
--
-- * 'engine' - The Redis engine.
-- * 'filters' - Filter to determine the list of User IDs to return.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
-- * 'userId' - The ID of the user.
mkDescribeUsers ::
  DescribeUsers
mkDescribeUsers =
  DescribeUsers'
    { filters = Lude.Nothing,
      engine = Lude.Nothing,
      userId = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filter to determine the list of User IDs to return.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFilters :: Lens.Lens' DescribeUsers (Lude.Maybe [Filter])
duFilters = Lens.lens (filters :: DescribeUsers -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeUsers)
{-# DEPRECATED duFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The Redis engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duEngine :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duEngine = Lens.lens (engine :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DescribeUsers)
{-# DEPRECATED duEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duUserId = Lens.lens (userId :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DescribeUsers)
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duMarker = Lens.lens (marker :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUsers)
{-# DEPRECATED duMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMaxRecords :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Int)
duMaxRecords = Lens.lens (maxRecords :: DescribeUsers -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeUsers)
{-# DEPRECATED duMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeUsers where
  page rq rs
    | Page.stop (rs Lens.^. dursMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& duMarker Lens..~ rs Lens.^. dursMarker

instance Lude.AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeUsersResult"
      ( \s h x ->
          DescribeUsersResponse'
            Lude.<$> ( x Lude..@? "Users" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUsers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeUsers" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filters),
        "Engine" Lude.=: engine,
        "UserId" Lude.=: userId,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { users ::
      Lude.Maybe [User],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeUsersResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
-- * 'responseStatus' - The response status code.
-- * 'users' - A list of users.
mkDescribeUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUsersResponse
mkDescribeUsersResponse pResponseStatus_ =
  DescribeUsersResponse'
    { users = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUsers :: Lens.Lens' DescribeUsersResponse (Lude.Maybe [User])
dursUsers = Lens.lens (users :: DescribeUsersResponse -> Lude.Maybe [User]) (\s a -> s {users = a} :: DescribeUsersResponse)
{-# DEPRECATED dursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursMarker :: Lens.Lens' DescribeUsersResponse (Lude.Maybe Lude.Text)
dursMarker = Lens.lens (marker :: DescribeUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUsersResponse)
{-# DEPRECATED dursMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DescribeUsersResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DescribeUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUsersResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
