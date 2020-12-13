{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the groups specified by the query. Groups are defined by the underlying Active Directory.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeGroups
  ( -- * Creating a request
    DescribeGroups (..),
    mkDescribeGroups,

    -- ** Request lenses
    dgAuthenticationToken,
    dgSearchQuery,
    dgMarker,
    dgLimit,
    dgOrganizationId,

    -- * Destructuring the response
    DescribeGroupsResponse (..),
    mkDescribeGroupsResponse,

    -- ** Response lenses
    dgrsGroups,
    dgrsMarker,
    dgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | A query to describe groups by group name.
    searchQuery :: Lude.Sensitive Lude.Text,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    limit :: Lude.Maybe Lude.Natural,
    -- | The ID of the organization.
    organizationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGroups' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'searchQuery' - A query to describe groups by group name.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'limit' - The maximum number of items to return with this call.
-- * 'organizationId' - The ID of the organization.
mkDescribeGroups ::
  -- | 'searchQuery'
  Lude.Sensitive Lude.Text ->
  DescribeGroups
mkDescribeGroups pSearchQuery_ =
  DescribeGroups'
    { authenticationToken = Lude.Nothing,
      searchQuery = pSearchQuery_,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      organizationId = Lude.Nothing
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgAuthenticationToken :: Lens.Lens' DescribeGroups (Lude.Maybe (Lude.Sensitive Lude.Text))
dgAuthenticationToken = Lens.lens (authenticationToken :: DescribeGroups -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeGroups)
{-# DEPRECATED dgAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | A query to describe groups by group name.
--
-- /Note:/ Consider using 'searchQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgSearchQuery :: Lens.Lens' DescribeGroups (Lude.Sensitive Lude.Text)
dgSearchQuery = Lens.lens (searchQuery :: DescribeGroups -> Lude.Sensitive Lude.Text) (\s a -> s {searchQuery = a} :: DescribeGroups)
{-# DEPRECATED dgSearchQuery "Use generic-lens or generic-optics with 'searchQuery' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgMarker :: Lens.Lens' DescribeGroups (Lude.Maybe Lude.Text)
dgMarker = Lens.lens (marker :: DescribeGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGroups)
{-# DEPRECATED dgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgLimit :: Lens.Lens' DescribeGroups (Lude.Maybe Lude.Natural)
dgLimit = Lens.lens (limit :: DescribeGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeGroups)
{-# DEPRECATED dgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgOrganizationId :: Lens.Lens' DescribeGroups (Lude.Maybe Lude.Text)
dgOrganizationId = Lens.lens (organizationId :: DescribeGroups -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DescribeGroups)
{-# DEPRECATED dgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager DescribeGroups where
  page rq rs
    | Page.stop (rs Lens.^. dgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dgrsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dgMarker Lens..~ rs Lens.^. dgrsMarker

instance Lude.AWSRequest DescribeGroups where
  type Rs DescribeGroups = DescribeGroupsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGroups where
  toHeaders DescribeGroups' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeGroups where
  toPath = Lude.const "/api/v1/groups"

instance Lude.ToQuery DescribeGroups where
  toQuery DescribeGroups' {..} =
    Lude.mconcat
      [ "searchQuery" Lude.=: searchQuery,
        "marker" Lude.=: marker,
        "limit" Lude.=: limit,
        "organizationId" Lude.=: organizationId
      ]

-- | /See:/ 'mkDescribeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { -- | The list of groups.
    groups :: Lude.Maybe [GroupMetadata],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - The list of groups.
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGroupsResponse
mkDescribeGroupsResponse pResponseStatus_ =
  DescribeGroupsResponse'
    { groups = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsGroups :: Lens.Lens' DescribeGroupsResponse (Lude.Maybe [GroupMetadata])
dgrsGroups = Lens.lens (groups :: DescribeGroupsResponse -> Lude.Maybe [GroupMetadata]) (\s a -> s {groups = a} :: DescribeGroupsResponse)
{-# DEPRECATED dgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsMarker :: Lens.Lens' DescribeGroupsResponse (Lude.Maybe Lude.Text)
dgrsMarker = Lens.lens (marker :: DescribeGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGroupsResponse)
{-# DEPRECATED dgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsResponseStatus :: Lens.Lens' DescribeGroupsResponse Lude.Int
dgrsResponseStatus = Lens.lens (responseStatus :: DescribeGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGroupsResponse)
{-# DEPRECATED dgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
