{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeUserGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of user groups.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUserGroups
  ( -- * Creating a request
    DescribeUserGroups (..),
    mkDescribeUserGroups,

    -- ** Request lenses
    dugUserGroupId,
    dugMarker,
    dugMaxRecords,

    -- * Destructuring the response
    DescribeUserGroupsResponse (..),
    mkDescribeUserGroupsResponse,

    -- ** Response lenses
    dugrsUserGroups,
    dugrsMarker,
    dugrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserGroups' smart constructor.
data DescribeUserGroups = DescribeUserGroups'
  { -- | The ID of the user group.
    userGroupId :: Lude.Maybe Lude.Text,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserGroups' with the minimum fields required to make a request.
--
-- * 'userGroupId' - The ID of the user group.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
mkDescribeUserGroups ::
  DescribeUserGroups
mkDescribeUserGroups =
  DescribeUserGroups'
    { userGroupId = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugUserGroupId :: Lens.Lens' DescribeUserGroups (Lude.Maybe Lude.Text)
dugUserGroupId = Lens.lens (userGroupId :: DescribeUserGroups -> Lude.Maybe Lude.Text) (\s a -> s {userGroupId = a} :: DescribeUserGroups)
{-# DEPRECATED dugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugMarker :: Lens.Lens' DescribeUserGroups (Lude.Maybe Lude.Text)
dugMarker = Lens.lens (marker :: DescribeUserGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUserGroups)
{-# DEPRECATED dugMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugMaxRecords :: Lens.Lens' DescribeUserGroups (Lude.Maybe Lude.Int)
dugMaxRecords = Lens.lens (maxRecords :: DescribeUserGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeUserGroups)
{-# DEPRECATED dugMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeUserGroups where
  page rq rs
    | Page.stop (rs Lens.^. dugrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dugrsUserGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dugMarker Lens..~ rs Lens.^. dugrsMarker

instance Lude.AWSRequest DescribeUserGroups where
  type Rs DescribeUserGroups = DescribeUserGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeUserGroupsResult"
      ( \s h x ->
          DescribeUserGroupsResponse'
            Lude.<$> ( x Lude..@? "UserGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeUserGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserGroups where
  toQuery DescribeUserGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeUserGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "UserGroupId" Lude.=: userGroupId,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeUserGroupsResponse' smart constructor.
data DescribeUserGroupsResponse = DescribeUserGroupsResponse'
  { -- | Returns a list of user groups.
    userGroups :: Lude.Maybe [UserGroup],
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserGroupsResponse' with the minimum fields required to make a request.
--
-- * 'userGroups' - Returns a list of user groups.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
-- * 'responseStatus' - The response status code.
mkDescribeUserGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserGroupsResponse
mkDescribeUserGroupsResponse pResponseStatus_ =
  DescribeUserGroupsResponse'
    { userGroups = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of user groups.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrsUserGroups :: Lens.Lens' DescribeUserGroupsResponse (Lude.Maybe [UserGroup])
dugrsUserGroups = Lens.lens (userGroups :: DescribeUserGroupsResponse -> Lude.Maybe [UserGroup]) (\s a -> s {userGroups = a} :: DescribeUserGroupsResponse)
{-# DEPRECATED dugrsUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrsMarker :: Lens.Lens' DescribeUserGroupsResponse (Lude.Maybe Lude.Text)
dugrsMarker = Lens.lens (marker :: DescribeUserGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUserGroupsResponse)
{-# DEPRECATED dugrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrsResponseStatus :: Lens.Lens' DescribeUserGroupsResponse Lude.Int
dugrsResponseStatus = Lens.lens (responseStatus :: DescribeUserGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserGroupsResponse)
{-# DEPRECATED dugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
