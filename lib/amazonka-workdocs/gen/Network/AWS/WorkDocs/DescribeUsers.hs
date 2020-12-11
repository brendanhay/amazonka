{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified users. You can describe all users or filter the results (for example, by status or organization).
--
-- By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeUsers
  ( -- * Creating a request
    DescribeUsers (..),
    mkDescribeUsers,

    -- ** Request lenses
    duInclude,
    duUserIds,
    duAuthenticationToken,
    duSort,
    duMarker,
    duQuery,
    duLimit,
    duOrder,
    duOrganizationId,
    duFields,

    -- * Destructuring the response
    DescribeUsersResponse (..),
    mkDescribeUsersResponse,

    -- ** Response lenses
    dursUsers,
    dursTotalNumberOfUsers,
    dursMarker,
    dursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { include ::
      Lude.Maybe UserFilterType,
    userIds :: Lude.Maybe Lude.Text,
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sort :: Lude.Maybe UserSortType,
    marker :: Lude.Maybe Lude.Text,
    query :: Lude.Maybe (Lude.Sensitive Lude.Text),
    limit :: Lude.Maybe Lude.Natural,
    order :: Lude.Maybe OrderType,
    organizationId :: Lude.Maybe Lude.Text,
    fields :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsers' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'fields' - A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
-- * 'include' - The state of the users. Specify "ALL" to include inactive users.
-- * 'limit' - The maximum number of items to return.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'order' - The order for the results.
-- * 'organizationId' - The ID of the organization.
-- * 'query' - A query to filter users by user name.
-- * 'sort' - The sorting criteria.
-- * 'userIds' - The IDs of the users.
mkDescribeUsers ::
  DescribeUsers
mkDescribeUsers =
  DescribeUsers'
    { include = Lude.Nothing,
      userIds = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      sort = Lude.Nothing,
      marker = Lude.Nothing,
      query = Lude.Nothing,
      limit = Lude.Nothing,
      order = Lude.Nothing,
      organizationId = Lude.Nothing,
      fields = Lude.Nothing
    }

-- | The state of the users. Specify "ALL" to include inactive users.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duInclude :: Lens.Lens' DescribeUsers (Lude.Maybe UserFilterType)
duInclude = Lens.lens (include :: DescribeUsers -> Lude.Maybe UserFilterType) (\s a -> s {include = a} :: DescribeUsers)
{-# DEPRECATED duInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The IDs of the users.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserIds :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duUserIds = Lens.lens (userIds :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {userIds = a} :: DescribeUsers)
{-# DEPRECATED duUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duAuthenticationToken :: Lens.Lens' DescribeUsers (Lude.Maybe (Lude.Sensitive Lude.Text))
duAuthenticationToken = Lens.lens (authenticationToken :: DescribeUsers -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeUsers)
{-# DEPRECATED duAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duSort :: Lens.Lens' DescribeUsers (Lude.Maybe UserSortType)
duSort = Lens.lens (sort :: DescribeUsers -> Lude.Maybe UserSortType) (\s a -> s {sort = a} :: DescribeUsers)
{-# DEPRECATED duSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duMarker = Lens.lens (marker :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeUsers)
{-# DEPRECATED duMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A query to filter users by user name.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duQuery :: Lens.Lens' DescribeUsers (Lude.Maybe (Lude.Sensitive Lude.Text))
duQuery = Lens.lens (query :: DescribeUsers -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {query = a} :: DescribeUsers)
{-# DEPRECATED duQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duLimit :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Natural)
duLimit = Lens.lens (limit :: DescribeUsers -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeUsers)
{-# DEPRECATED duLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The order for the results.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrder :: Lens.Lens' DescribeUsers (Lude.Maybe OrderType)
duOrder = Lens.lens (order :: DescribeUsers -> Lude.Maybe OrderType) (\s a -> s {order = a} :: DescribeUsers)
{-# DEPRECATED duOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duOrganizationId = Lens.lens (organizationId :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DescribeUsers)
{-# DEPRECATED duOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFields :: Lens.Lens' DescribeUsers (Lude.Maybe Lude.Text)
duFields = Lens.lens (fields :: DescribeUsers -> Lude.Maybe Lude.Text) (\s a -> s {fields = a} :: DescribeUsers)
{-# DEPRECATED duFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Page.AWSPager DescribeUsers where
  page rq rs
    | Page.stop (rs Lens.^. dursMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& duMarker Lens..~ rs Lens.^. dursMarker

instance Lude.AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Lude.<$> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalNumberOfUsers")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUsers where
  toHeaders DescribeUsers' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeUsers where
  toPath = Lude.const "/api/v1/users"

instance Lude.ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    Lude.mconcat
      [ "include" Lude.=: include,
        "userIds" Lude.=: userIds,
        "sort" Lude.=: sort,
        "marker" Lude.=: marker,
        "query" Lude.=: query,
        "limit" Lude.=: limit,
        "order" Lude.=: order,
        "organizationId" Lude.=: organizationId,
        "fields" Lude.=: fields
      ]

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { users ::
      Lude.Maybe [User],
    totalNumberOfUsers :: Lude.Maybe Lude.Integer,
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
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'totalNumberOfUsers' - The total number of users included in the results.
-- * 'users' - The users.
mkDescribeUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUsersResponse
mkDescribeUsersResponse pResponseStatus_ =
  DescribeUsersResponse'
    { users = Lude.Nothing,
      totalNumberOfUsers = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursUsers :: Lens.Lens' DescribeUsersResponse (Lude.Maybe [User])
dursUsers = Lens.lens (users :: DescribeUsersResponse -> Lude.Maybe [User]) (\s a -> s {users = a} :: DescribeUsersResponse)
{-# DEPRECATED dursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The total number of users included in the results.
--
-- /Note:/ Consider using 'totalNumberOfUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursTotalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Lude.Maybe Lude.Integer)
dursTotalNumberOfUsers = Lens.lens (totalNumberOfUsers :: DescribeUsersResponse -> Lude.Maybe Lude.Integer) (\s a -> s {totalNumberOfUsers = a} :: DescribeUsersResponse)
{-# DEPRECATED dursTotalNumberOfUsers "Use generic-lens or generic-optics with 'totalNumberOfUsers' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
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
