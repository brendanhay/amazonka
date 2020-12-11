{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListUserHierarchyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the hierarchy groups for the specified Amazon Connect instance.
--
-- For more information about agent hierarchies, see <https://docs.aws.amazon.com/connect/latest/adminguide/agent-hierarchy.html Set Up Agent Hierarchies> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUserHierarchyGroups
  ( -- * Creating a request
    ListUserHierarchyGroups (..),
    mkListUserHierarchyGroups,

    -- ** Request lenses
    luhgNextToken,
    luhgMaxResults,
    luhgInstanceId,

    -- * Destructuring the response
    ListUserHierarchyGroupsResponse (..),
    mkListUserHierarchyGroupsResponse,

    -- ** Response lenses
    luhgrsNextToken,
    luhgrsUserHierarchyGroupSummaryList,
    luhgrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUserHierarchyGroups' smart constructor.
data ListUserHierarchyGroups = ListUserHierarchyGroups'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserHierarchyGroups' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListUserHierarchyGroups ::
  -- | 'instanceId'
  Lude.Text ->
  ListUserHierarchyGroups
mkListUserHierarchyGroups pInstanceId_ =
  ListUserHierarchyGroups'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgNextToken :: Lens.Lens' ListUserHierarchyGroups (Lude.Maybe Lude.Text)
luhgNextToken = Lens.lens (nextToken :: ListUserHierarchyGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserHierarchyGroups)
{-# DEPRECATED luhgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgMaxResults :: Lens.Lens' ListUserHierarchyGroups (Lude.Maybe Lude.Natural)
luhgMaxResults = Lens.lens (maxResults :: ListUserHierarchyGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUserHierarchyGroups)
{-# DEPRECATED luhgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgInstanceId :: Lens.Lens' ListUserHierarchyGroups Lude.Text
luhgInstanceId = Lens.lens (instanceId :: ListUserHierarchyGroups -> Lude.Text) (\s a -> s {instanceId = a} :: ListUserHierarchyGroups)
{-# DEPRECATED luhgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListUserHierarchyGroups where
  page rq rs
    | Page.stop (rs Lens.^. luhgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. luhgrsUserHierarchyGroupSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& luhgNextToken Lens..~ rs Lens.^. luhgrsNextToken

instance Lude.AWSRequest ListUserHierarchyGroups where
  type Rs ListUserHierarchyGroups = ListUserHierarchyGroupsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserHierarchyGroupsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "UserHierarchyGroupSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserHierarchyGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListUserHierarchyGroups where
  toPath ListUserHierarchyGroups' {..} =
    Lude.mconcat
      ["/user-hierarchy-groups-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListUserHierarchyGroups where
  toQuery ListUserHierarchyGroups' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListUserHierarchyGroupsResponse' smart constructor.
data ListUserHierarchyGroupsResponse = ListUserHierarchyGroupsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    userHierarchyGroupSummaryList ::
      Lude.Maybe
        [HierarchyGroupSummary],
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

-- | Creates a value of 'ListUserHierarchyGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'userHierarchyGroupSummaryList' - Information about the hierarchy groups.
mkListUserHierarchyGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserHierarchyGroupsResponse
mkListUserHierarchyGroupsResponse pResponseStatus_ =
  ListUserHierarchyGroupsResponse'
    { nextToken = Lude.Nothing,
      userHierarchyGroupSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrsNextToken :: Lens.Lens' ListUserHierarchyGroupsResponse (Lude.Maybe Lude.Text)
luhgrsNextToken = Lens.lens (nextToken :: ListUserHierarchyGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserHierarchyGroupsResponse)
{-# DEPRECATED luhgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the hierarchy groups.
--
-- /Note:/ Consider using 'userHierarchyGroupSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrsUserHierarchyGroupSummaryList :: Lens.Lens' ListUserHierarchyGroupsResponse (Lude.Maybe [HierarchyGroupSummary])
luhgrsUserHierarchyGroupSummaryList = Lens.lens (userHierarchyGroupSummaryList :: ListUserHierarchyGroupsResponse -> Lude.Maybe [HierarchyGroupSummary]) (\s a -> s {userHierarchyGroupSummaryList = a} :: ListUserHierarchyGroupsResponse)
{-# DEPRECATED luhgrsUserHierarchyGroupSummaryList "Use generic-lens or generic-optics with 'userHierarchyGroupSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrsResponseStatus :: Lens.Lens' ListUserHierarchyGroupsResponse Lude.Int
luhgrsResponseStatus = Lens.lens (responseStatus :: ListUserHierarchyGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserHierarchyGroupsResponse)
{-# DEPRECATED luhgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
