{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a specified resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeResourcePermissions
  ( -- * Creating a request
    DescribeResourcePermissions (..),
    mkDescribeResourcePermissions,

    -- ** Request lenses
    drpPrincipalId,
    drpAuthenticationToken,
    drpMarker,
    drpLimit,
    drpResourceId,

    -- * Destructuring the response
    DescribeResourcePermissionsResponse (..),
    mkDescribeResourcePermissionsResponse,

    -- ** Response lenses
    drprsPrincipals,
    drprsMarker,
    drprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeResourcePermissions' smart constructor.
data DescribeResourcePermissions = DescribeResourcePermissions'
  { principalId ::
      Lude.Maybe Lude.Text,
    authenticationToken ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    resourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourcePermissions' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'limit' - The maximum number of items to return with this call.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call)
-- * 'principalId' - The ID of the principal to filter permissions by.
-- * 'resourceId' - The ID of the resource.
mkDescribeResourcePermissions ::
  -- | 'resourceId'
  Lude.Text ->
  DescribeResourcePermissions
mkDescribeResourcePermissions pResourceId_ =
  DescribeResourcePermissions'
    { principalId = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the principal to filter permissions by.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPrincipalId :: Lens.Lens' DescribeResourcePermissions (Lude.Maybe Lude.Text)
drpPrincipalId = Lens.lens (principalId :: DescribeResourcePermissions -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: DescribeResourcePermissions)
{-# DEPRECATED drpPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpAuthenticationToken :: Lens.Lens' DescribeResourcePermissions (Lude.Maybe (Lude.Sensitive Lude.Text))
drpAuthenticationToken = Lens.lens (authenticationToken :: DescribeResourcePermissions -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeResourcePermissions)
{-# DEPRECATED drpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpMarker :: Lens.Lens' DescribeResourcePermissions (Lude.Maybe Lude.Text)
drpMarker = Lens.lens (marker :: DescribeResourcePermissions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeResourcePermissions)
{-# DEPRECATED drpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLimit :: Lens.Lens' DescribeResourcePermissions (Lude.Maybe Lude.Natural)
drpLimit = Lens.lens (limit :: DescribeResourcePermissions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeResourcePermissions)
{-# DEPRECATED drpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpResourceId :: Lens.Lens' DescribeResourcePermissions Lude.Text
drpResourceId = Lens.lens (resourceId :: DescribeResourcePermissions -> Lude.Text) (\s a -> s {resourceId = a} :: DescribeResourcePermissions)
{-# DEPRECATED drpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Page.AWSPager DescribeResourcePermissions where
  page rq rs
    | Page.stop (rs Lens.^. drprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drprsPrincipals) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& drpMarker Lens..~ rs Lens.^. drprsMarker

instance Lude.AWSRequest DescribeResourcePermissions where
  type
    Rs DescribeResourcePermissions =
      DescribeResourcePermissionsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourcePermissionsResponse'
            Lude.<$> (x Lude..?> "Principals" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeResourcePermissions where
  toHeaders DescribeResourcePermissions' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeResourcePermissions where
  toPath DescribeResourcePermissions' {..} =
    Lude.mconcat
      ["/api/v1/resources/", Lude.toBS resourceId, "/permissions"]

instance Lude.ToQuery DescribeResourcePermissions where
  toQuery DescribeResourcePermissions' {..} =
    Lude.mconcat
      [ "principalId" Lude.=: principalId,
        "marker" Lude.=: marker,
        "limit" Lude.=: limit
      ]

-- | /See:/ 'mkDescribeResourcePermissionsResponse' smart constructor.
data DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse'
  { principals ::
      Lude.Maybe
        [Principal],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourcePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'principals' - The principals.
-- * 'responseStatus' - The response status code.
mkDescribeResourcePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeResourcePermissionsResponse
mkDescribeResourcePermissionsResponse pResponseStatus_ =
  DescribeResourcePermissionsResponse'
    { principals = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The principals.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsPrincipals :: Lens.Lens' DescribeResourcePermissionsResponse (Lude.Maybe [Principal])
drprsPrincipals = Lens.lens (principals :: DescribeResourcePermissionsResponse -> Lude.Maybe [Principal]) (\s a -> s {principals = a} :: DescribeResourcePermissionsResponse)
{-# DEPRECATED drprsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsMarker :: Lens.Lens' DescribeResourcePermissionsResponse (Lude.Maybe Lude.Text)
drprsMarker = Lens.lens (marker :: DescribeResourcePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeResourcePermissionsResponse)
{-# DEPRECATED drprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DescribeResourcePermissionsResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DescribeResourcePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourcePermissionsResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
