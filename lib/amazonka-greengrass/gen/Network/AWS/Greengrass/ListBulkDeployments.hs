{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListBulkDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of bulk deployments.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeployments
  ( -- * Creating a request
    ListBulkDeployments (..),
    mkListBulkDeployments,

    -- ** Request lenses
    lbdNextToken,
    lbdMaxResults,

    -- * Destructuring the response
    ListBulkDeploymentsResponse (..),
    mkListBulkDeploymentsResponse,

    -- ** Response lenses
    lbdrsNextToken,
    lbdrsBulkDeployments,
    lbdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBulkDeployments' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListBulkDeployments ::
  ListBulkDeployments
mkListBulkDeployments =
  ListBulkDeployments'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdNextToken :: Lens.Lens' ListBulkDeployments (Lude.Maybe Lude.Text)
lbdNextToken = Lens.lens (nextToken :: ListBulkDeployments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBulkDeployments)
{-# DEPRECATED lbdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdMaxResults :: Lens.Lens' ListBulkDeployments (Lude.Maybe Lude.Text)
lbdMaxResults = Lens.lens (maxResults :: ListBulkDeployments -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListBulkDeployments)
{-# DEPRECATED lbdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBulkDeployments where
  page rq rs
    | Page.stop (rs Lens.^. lbdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbdrsBulkDeployments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbdNextToken Lens..~ rs Lens.^. lbdrsNextToken

instance Lude.AWSRequest ListBulkDeployments where
  type Rs ListBulkDeployments = ListBulkDeploymentsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBulkDeploymentsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "BulkDeployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBulkDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListBulkDeployments where
  toPath = Lude.const "/greengrass/bulk/deployments"

instance Lude.ToQuery ListBulkDeployments where
  toQuery ListBulkDeployments' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    bulkDeployments ::
      Lude.Maybe [BulkDeployment],
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

-- | Creates a value of 'ListBulkDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'bulkDeployments' - A list of bulk deployments.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListBulkDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBulkDeploymentsResponse
mkListBulkDeploymentsResponse pResponseStatus_ =
  ListBulkDeploymentsResponse'
    { nextToken = Lude.Nothing,
      bulkDeployments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrsNextToken :: Lens.Lens' ListBulkDeploymentsResponse (Lude.Maybe Lude.Text)
lbdrsNextToken = Lens.lens (nextToken :: ListBulkDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBulkDeploymentsResponse)
{-# DEPRECATED lbdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of bulk deployments.
--
-- /Note:/ Consider using 'bulkDeployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrsBulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Lude.Maybe [BulkDeployment])
lbdrsBulkDeployments = Lens.lens (bulkDeployments :: ListBulkDeploymentsResponse -> Lude.Maybe [BulkDeployment]) (\s a -> s {bulkDeployments = a} :: ListBulkDeploymentsResponse)
{-# DEPRECATED lbdrsBulkDeployments "Use generic-lens or generic-optics with 'bulkDeployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrsResponseStatus :: Lens.Lens' ListBulkDeploymentsResponse Lude.Int
lbdrsResponseStatus = Lens.lens (responseStatus :: ListBulkDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBulkDeploymentsResponse)
{-# DEPRECATED lbdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
