{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user activities in a specified time period.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeActivities
  ( -- * Creating a request
    DescribeActivities (..),
    mkDescribeActivities,

    -- ** Request lenses
    daResourceId,
    daIncludeIndirectActivities,
    daStartTime,
    daAuthenticationToken,
    daUserId,
    daMarker,
    daEndTime,
    daLimit,
    daActivityTypes,
    daOrganizationId,

    -- * Destructuring the response
    DescribeActivitiesResponse (..),
    mkDescribeActivitiesResponse,

    -- ** Response lenses
    darsUserActivities,
    darsMarker,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeActivities' smart constructor.
data DescribeActivities = DescribeActivities'
  { -- | The document or folder ID for which to describe activity types.
    resourceId :: Lude.Maybe Lude.Text,
    -- | Includes indirect activities. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
    includeIndirectActivities :: Lude.Maybe Lude.Bool,
    -- | The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
    userId :: Lude.Maybe Lude.Text,
    -- | The marker for the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of items to return.
    limit :: Lude.Maybe Lude.Natural,
    -- | Specifies which activity types to include in the response. If this field is left empty, all activity types are returned.
    activityTypes :: Lude.Maybe Lude.Text,
    -- | The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
    organizationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivities' with the minimum fields required to make a request.
--
-- * 'resourceId' - The document or folder ID for which to describe activity types.
-- * 'includeIndirectActivities' - Includes indirect activities. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
-- * 'startTime' - The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'userId' - The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
-- * 'marker' - The marker for the next set of results.
-- * 'endTime' - The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
-- * 'limit' - The maximum number of items to return.
-- * 'activityTypes' - Specifies which activity types to include in the response. If this field is left empty, all activity types are returned.
-- * 'organizationId' - The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
mkDescribeActivities ::
  DescribeActivities
mkDescribeActivities =
  DescribeActivities'
    { resourceId = Lude.Nothing,
      includeIndirectActivities = Lude.Nothing,
      startTime = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      userId = Lude.Nothing,
      marker = Lude.Nothing,
      endTime = Lude.Nothing,
      limit = Lude.Nothing,
      activityTypes = Lude.Nothing,
      organizationId = Lude.Nothing
    }

-- | The document or folder ID for which to describe activity types.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daResourceId :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Text)
daResourceId = Lens.lens (resourceId :: DescribeActivities -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeActivities)
{-# DEPRECATED daResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Includes indirect activities. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
--
-- /Note:/ Consider using 'includeIndirectActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daIncludeIndirectActivities :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Bool)
daIncludeIndirectActivities = Lens.lens (includeIndirectActivities :: DescribeActivities -> Lude.Maybe Lude.Bool) (\s a -> s {includeIndirectActivities = a} :: DescribeActivities)
{-# DEPRECATED daIncludeIndirectActivities "Use generic-lens or generic-optics with 'includeIndirectActivities' instead." #-}

-- | The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStartTime :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Timestamp)
daStartTime = Lens.lens (startTime :: DescribeActivities -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeActivities)
{-# DEPRECATED daStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthenticationToken :: Lens.Lens' DescribeActivities (Lude.Maybe (Lude.Sensitive Lude.Text))
daAuthenticationToken = Lens.lens (authenticationToken :: DescribeActivities -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DescribeActivities)
{-# DEPRECATED daAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daUserId :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Text)
daUserId = Lens.lens (userId :: DescribeActivities -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DescribeActivities)
{-# DEPRECATED daUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMarker :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Text)
daMarker = Lens.lens (marker :: DescribeActivities -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeActivities)
{-# DEPRECATED daMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEndTime :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Timestamp)
daEndTime = Lens.lens (endTime :: DescribeActivities -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeActivities)
{-# DEPRECATED daEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daLimit :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Natural)
daLimit = Lens.lens (limit :: DescribeActivities -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeActivities)
{-# DEPRECATED daLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Specifies which activity types to include in the response. If this field is left empty, all activity types are returned.
--
-- /Note:/ Consider using 'activityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivityTypes :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Text)
daActivityTypes = Lens.lens (activityTypes :: DescribeActivities -> Lude.Maybe Lude.Text) (\s a -> s {activityTypes = a} :: DescribeActivities)
{-# DEPRECATED daActivityTypes "Use generic-lens or generic-optics with 'activityTypes' instead." #-}

-- | The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daOrganizationId :: Lens.Lens' DescribeActivities (Lude.Maybe Lude.Text)
daOrganizationId = Lens.lens (organizationId :: DescribeActivities -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DescribeActivities)
{-# DEPRECATED daOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager DescribeActivities where
  page rq rs
    | Page.stop (rs Lens.^. darsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. darsUserActivities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& daMarker Lens..~ rs Lens.^. darsMarker

instance Lude.AWSRequest DescribeActivities where
  type Rs DescribeActivities = DescribeActivitiesResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeActivitiesResponse'
            Lude.<$> (x Lude..?> "UserActivities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeActivities where
  toHeaders DescribeActivities' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeActivities where
  toPath = Lude.const "/api/v1/activities"

instance Lude.ToQuery DescribeActivities where
  toQuery DescribeActivities' {..} =
    Lude.mconcat
      [ "resourceId" Lude.=: resourceId,
        "includeIndirectActivities" Lude.=: includeIndirectActivities,
        "startTime" Lude.=: startTime,
        "userId" Lude.=: userId,
        "marker" Lude.=: marker,
        "endTime" Lude.=: endTime,
        "limit" Lude.=: limit,
        "activityTypes" Lude.=: activityTypes,
        "organizationId" Lude.=: organizationId
      ]

-- | /See:/ 'mkDescribeActivitiesResponse' smart constructor.
data DescribeActivitiesResponse = DescribeActivitiesResponse'
  { -- | The list of activities for the specified user and time period.
    userActivities :: Lude.Maybe [Activity],
    -- | The marker for the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivitiesResponse' with the minimum fields required to make a request.
--
-- * 'userActivities' - The list of activities for the specified user and time period.
-- * 'marker' - The marker for the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeActivitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeActivitiesResponse
mkDescribeActivitiesResponse pResponseStatus_ =
  DescribeActivitiesResponse'
    { userActivities = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of activities for the specified user and time period.
--
-- /Note:/ Consider using 'userActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsUserActivities :: Lens.Lens' DescribeActivitiesResponse (Lude.Maybe [Activity])
darsUserActivities = Lens.lens (userActivities :: DescribeActivitiesResponse -> Lude.Maybe [Activity]) (\s a -> s {userActivities = a} :: DescribeActivitiesResponse)
{-# DEPRECATED darsUserActivities "Use generic-lens or generic-optics with 'userActivities' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsMarker :: Lens.Lens' DescribeActivitiesResponse (Lude.Maybe Lude.Text)
darsMarker = Lens.lens (marker :: DescribeActivitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeActivitiesResponse)
{-# DEPRECATED darsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeActivitiesResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeActivitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeActivitiesResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
