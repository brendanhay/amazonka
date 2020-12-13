{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of groups.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListGroups
  ( -- * Creating a request
    ListGroups (..),
    mkListGroups,

    -- ** Request lenses
    lgNextToken,
    lgMaxResults,

    -- * Destructuring the response
    ListGroupsResponse (..),
    mkListGroupsResponse,

    -- ** Response lenses
    lgrsGroups,
    lgrsNextToken,
    lgrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListGroups ::
  ListGroups
mkListGroups =
  ListGroups' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgNextToken = Lens.lens (nextToken :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroups)
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGroups (Lude.Maybe Lude.Text)
lgMaxResults = Lens.lens (maxResults :: ListGroups -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListGroups)
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListGroups where
  page rq rs
    | Page.stop (rs Lens.^. lgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgNextToken Lens..~ rs Lens.^. lgrsNextToken

instance Lude.AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Lude.<$> (x Lude..?> "Groups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListGroups where
  toPath = Lude.const "/greengrass/groups"

instance Lude.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | Information about a group.
    groups :: Lude.Maybe [GroupInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- * 'groups' - Information about a group.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupsResponse
mkListGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { groups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a group.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGroups :: Lens.Lens' ListGroupsResponse (Lude.Maybe [GroupInformation])
lgrsGroups = Lens.lens (groups :: ListGroupsResponse -> Lude.Maybe [GroupInformation]) (\s a -> s {groups = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsNextToken :: Lens.Lens' ListGroupsResponse (Lude.Maybe Lude.Text)
lgrsNextToken = Lens.lens (nextToken :: ListGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGroupsResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupsResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
