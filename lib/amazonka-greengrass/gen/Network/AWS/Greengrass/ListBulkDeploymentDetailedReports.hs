{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of the deployments that have been started in a bulk deployment operation, and their current deployment status.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
  ( -- * Creating a request
    ListBulkDeploymentDetailedReports (..),
    mkListBulkDeploymentDetailedReports,

    -- ** Request lenses
    lbddrNextToken,
    lbddrMaxResults,
    lbddrBulkDeploymentId,

    -- * Destructuring the response
    ListBulkDeploymentDetailedReportsResponse (..),
    mkListBulkDeploymentDetailedReportsResponse,

    -- ** Response lenses
    lbddrrsNextToken,
    lbddrrsDeployments,
    lbddrrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Text,
    bulkDeploymentId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBulkDeploymentDetailedReports' with the minimum fields required to make a request.
--
-- * 'bulkDeploymentId' - The ID of the bulk deployment.
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListBulkDeploymentDetailedReports ::
  -- | 'bulkDeploymentId'
  Lude.Text ->
  ListBulkDeploymentDetailedReports
mkListBulkDeploymentDetailedReports pBulkDeploymentId_ =
  ListBulkDeploymentDetailedReports'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      bulkDeploymentId = pBulkDeploymentId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrNextToken :: Lens.Lens' ListBulkDeploymentDetailedReports (Lude.Maybe Lude.Text)
lbddrNextToken = Lens.lens (nextToken :: ListBulkDeploymentDetailedReports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBulkDeploymentDetailedReports)
{-# DEPRECATED lbddrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrMaxResults :: Lens.Lens' ListBulkDeploymentDetailedReports (Lude.Maybe Lude.Text)
lbddrMaxResults = Lens.lens (maxResults :: ListBulkDeploymentDetailedReports -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListBulkDeploymentDetailedReports)
{-# DEPRECATED lbddrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrBulkDeploymentId :: Lens.Lens' ListBulkDeploymentDetailedReports Lude.Text
lbddrBulkDeploymentId = Lens.lens (bulkDeploymentId :: ListBulkDeploymentDetailedReports -> Lude.Text) (\s a -> s {bulkDeploymentId = a} :: ListBulkDeploymentDetailedReports)
{-# DEPRECATED lbddrBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

instance Page.AWSPager ListBulkDeploymentDetailedReports where
  page rq rs
    | Page.stop (rs Lens.^. lbddrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbddrrsDeployments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbddrNextToken Lens..~ rs Lens.^. lbddrrsNextToken

instance Lude.AWSRequest ListBulkDeploymentDetailedReports where
  type
    Rs ListBulkDeploymentDetailedReports =
      ListBulkDeploymentDetailedReportsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBulkDeploymentDetailedReportsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Deployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBulkDeploymentDetailedReports where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListBulkDeploymentDetailedReports where
  toPath ListBulkDeploymentDetailedReports' {..} =
    Lude.mconcat
      [ "/greengrass/bulk/deployments/",
        Lude.toBS bulkDeploymentId,
        "/detailed-reports"
      ]

instance Lude.ToQuery ListBulkDeploymentDetailedReports where
  toQuery ListBulkDeploymentDetailedReports' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    deployments ::
      Lude.Maybe
        [BulkDeploymentResult],
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

-- | Creates a value of 'ListBulkDeploymentDetailedReportsResponse' with the minimum fields required to make a request.
--
-- * 'deployments' - A list of the individual group deployments in the bulk deployment operation.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListBulkDeploymentDetailedReportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBulkDeploymentDetailedReportsResponse
mkListBulkDeploymentDetailedReportsResponse pResponseStatus_ =
  ListBulkDeploymentDetailedReportsResponse'
    { nextToken =
        Lude.Nothing,
      deployments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrsNextToken :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Lude.Maybe Lude.Text)
lbddrrsNextToken = Lens.lens (nextToken :: ListBulkDeploymentDetailedReportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBulkDeploymentDetailedReportsResponse)
{-# DEPRECATED lbddrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the individual group deployments in the bulk deployment operation.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrsDeployments :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Lude.Maybe [BulkDeploymentResult])
lbddrrsDeployments = Lens.lens (deployments :: ListBulkDeploymentDetailedReportsResponse -> Lude.Maybe [BulkDeploymentResult]) (\s a -> s {deployments = a} :: ListBulkDeploymentDetailedReportsResponse)
{-# DEPRECATED lbddrrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrsResponseStatus :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse Lude.Int
lbddrrsResponseStatus = Lens.lens (responseStatus :: ListBulkDeploymentDetailedReportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBulkDeploymentDetailedReportsResponse)
{-# DEPRECATED lbddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
