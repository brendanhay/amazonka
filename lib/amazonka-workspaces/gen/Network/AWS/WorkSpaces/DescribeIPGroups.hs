{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeIPGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeIPGroups
  ( -- * Creating a request
    DescribeIPGroups (..),
    mkDescribeIPGroups,

    -- ** Request lenses
    digGroupIds,
    digNextToken,
    digMaxResults,

    -- * Destructuring the response
    DescribeIPGroupsResponse (..),
    mkDescribeIPGroupsResponse,

    -- ** Response lenses
    digsrsResult,
    digsrsNextToken,
    digsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeIPGroups' smart constructor.
data DescribeIPGroups = DescribeIPGroups'
  { -- | The identifiers of one or more IP access control groups.
    groupIds :: Lude.Maybe [Lude.Text],
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIPGroups' with the minimum fields required to make a request.
--
-- * 'groupIds' - The identifiers of one or more IP access control groups.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
-- * 'maxResults' - The maximum number of items to return.
mkDescribeIPGroups ::
  DescribeIPGroups
mkDescribeIPGroups =
  DescribeIPGroups'
    { groupIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digGroupIds :: Lens.Lens' DescribeIPGroups (Lude.Maybe [Lude.Text])
digGroupIds = Lens.lens (groupIds :: DescribeIPGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {groupIds = a} :: DescribeIPGroups)
{-# DEPRECATED digGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digNextToken :: Lens.Lens' DescribeIPGroups (Lude.Maybe Lude.Text)
digNextToken = Lens.lens (nextToken :: DescribeIPGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIPGroups)
{-# DEPRECATED digNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digMaxResults :: Lens.Lens' DescribeIPGroups (Lude.Maybe Lude.Natural)
digMaxResults = Lens.lens (maxResults :: DescribeIPGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeIPGroups)
{-# DEPRECATED digMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeIPGroups where
  page rq rs
    | Page.stop (rs Lens.^. digsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. digsrsResult) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& digNextToken Lens..~ rs Lens.^. digsrsNextToken

instance Lude.AWSRequest DescribeIPGroups where
  type Rs DescribeIPGroups = DescribeIPGroupsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeIPGroupsResponse'
            Lude.<$> (x Lude..?> "Result" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIPGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeIpGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeIPGroups where
  toJSON DescribeIPGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupIds" Lude..=) Lude.<$> groupIds,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeIPGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIPGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeIPGroupsResponse' smart constructor.
data DescribeIPGroupsResponse = DescribeIPGroupsResponse'
  { -- | Information about the IP access control groups.
    result :: Lude.Maybe [WorkspacesIPGroup],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIPGroupsResponse' with the minimum fields required to make a request.
--
-- * 'result' - Information about the IP access control groups.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeIPGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIPGroupsResponse
mkDescribeIPGroupsResponse pResponseStatus_ =
  DescribeIPGroupsResponse'
    { result = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IP access control groups.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsrsResult :: Lens.Lens' DescribeIPGroupsResponse (Lude.Maybe [WorkspacesIPGroup])
digsrsResult = Lens.lens (result :: DescribeIPGroupsResponse -> Lude.Maybe [WorkspacesIPGroup]) (\s a -> s {result = a} :: DescribeIPGroupsResponse)
{-# DEPRECATED digsrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsrsNextToken :: Lens.Lens' DescribeIPGroupsResponse (Lude.Maybe Lude.Text)
digsrsNextToken = Lens.lens (nextToken :: DescribeIPGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIPGroupsResponse)
{-# DEPRECATED digsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsrsResponseStatus :: Lens.Lens' DescribeIPGroupsResponse Lude.Int
digsrsResponseStatus = Lens.lens (responseStatus :: DescribeIPGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIPGroupsResponse)
{-# DEPRECATED digsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
