{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListRoots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the roots that are defined in the current organization.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListRoots
  ( -- * Creating a request
    ListRoots (..),
    mkListRoots,

    -- ** Request lenses
    lrNextToken,
    lrMaxResults,

    -- * Destructuring the response
    ListRootsResponse (..),
    mkListRootsResponse,

    -- ** Response lenses
    lrrsRoots,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRoots' smart constructor.
data ListRoots = ListRoots'
  { nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRoots' with the minimum fields required to make a request.
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
mkListRoots ::
  ListRoots
mkListRoots =
  ListRoots' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRoots (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRoots -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRoots)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRoots (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListRoots -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListRoots)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListRoots where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsRoots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListRoots where
  type Rs ListRoots = ListRootsResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRootsResponse'
            Lude.<$> (x Lude..?> "Roots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRoots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.ListRoots" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRoots where
  toJSON ListRoots' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListRoots where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRoots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListRootsResponse' smart constructor.
data ListRootsResponse = ListRootsResponse'
  { roots ::
      Lude.Maybe [Root],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRootsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
-- * 'roots' - A list of roots that are defined in an organization.
mkListRootsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRootsResponse
mkListRootsResponse pResponseStatus_ =
  ListRootsResponse'
    { roots = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of roots that are defined in an organization.
--
-- /Note:/ Consider using 'roots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRoots :: Lens.Lens' ListRootsResponse (Lude.Maybe [Root])
lrrsRoots = Lens.lens (roots :: ListRootsResponse -> Lude.Maybe [Root]) (\s a -> s {roots = a} :: ListRootsResponse)
{-# DEPRECATED lrrsRoots "Use generic-lens or generic-optics with 'roots' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRootsResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRootsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRootsResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRootsResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRootsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRootsResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
