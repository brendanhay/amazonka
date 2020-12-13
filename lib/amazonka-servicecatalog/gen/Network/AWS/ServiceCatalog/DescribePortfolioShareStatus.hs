{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified portfolio share operation. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
  ( -- * Creating a request
    DescribePortfolioShareStatus (..),
    mkDescribePortfolioShareStatus,

    -- ** Request lenses
    dpssPortfolioShareToken,

    -- * Destructuring the response
    DescribePortfolioShareStatusResponse (..),
    mkDescribePortfolioShareStatusResponse,

    -- ** Response lenses
    dpssrsStatus,
    dpssrsPortfolioShareToken,
    dpssrsShareDetails,
    dpssrsPortfolioId,
    dpssrsOrganizationNodeValue,
    dpssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribePortfolioShareStatus' smart constructor.
newtype DescribePortfolioShareStatus = DescribePortfolioShareStatus'
  { -- | The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
    portfolioShareToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePortfolioShareStatus' with the minimum fields required to make a request.
--
-- * 'portfolioShareToken' - The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
mkDescribePortfolioShareStatus ::
  -- | 'portfolioShareToken'
  Lude.Text ->
  DescribePortfolioShareStatus
mkDescribePortfolioShareStatus pPortfolioShareToken_ =
  DescribePortfolioShareStatus'
    { portfolioShareToken =
        pPortfolioShareToken_
    }

-- | The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPortfolioShareToken :: Lens.Lens' DescribePortfolioShareStatus Lude.Text
dpssPortfolioShareToken = Lens.lens (portfolioShareToken :: DescribePortfolioShareStatus -> Lude.Text) (\s a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatus)
{-# DEPRECATED dpssPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

instance Lude.AWSRequest DescribePortfolioShareStatus where
  type
    Rs DescribePortfolioShareStatus =
      DescribePortfolioShareStatusResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePortfolioShareStatusResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "PortfolioShareToken")
            Lude.<*> (x Lude..?> "ShareDetails")
            Lude.<*> (x Lude..?> "PortfolioId")
            Lude.<*> (x Lude..?> "OrganizationNodeValue")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePortfolioShareStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribePortfolioShareStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("PortfolioShareToken" Lude..= portfolioShareToken)]
      )

instance Lude.ToPath DescribePortfolioShareStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePortfolioShareStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { -- | Status of the portfolio share operation.
    status :: Lude.Maybe ShareStatus,
    -- | The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
    portfolioShareToken :: Lude.Maybe Lude.Text,
    -- | Information about the portfolio share operation.
    shareDetails :: Lude.Maybe ShareDetails,
    -- | The portfolio identifier.
    portfolioId :: Lude.Maybe Lude.Text,
    -- | Organization node identifier. It can be either account id, organizational unit id or organization id.
    organizationNodeValue :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePortfolioShareStatusResponse' with the minimum fields required to make a request.
--
-- * 'status' - Status of the portfolio share operation.
-- * 'portfolioShareToken' - The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
-- * 'shareDetails' - Information about the portfolio share operation.
-- * 'portfolioId' - The portfolio identifier.
-- * 'organizationNodeValue' - Organization node identifier. It can be either account id, organizational unit id or organization id.
-- * 'responseStatus' - The response status code.
mkDescribePortfolioShareStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePortfolioShareStatusResponse
mkDescribePortfolioShareStatusResponse pResponseStatus_ =
  DescribePortfolioShareStatusResponse'
    { status = Lude.Nothing,
      portfolioShareToken = Lude.Nothing,
      shareDetails = Lude.Nothing,
      portfolioId = Lude.Nothing,
      organizationNodeValue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Status of the portfolio share operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsStatus :: Lens.Lens' DescribePortfolioShareStatusResponse (Lude.Maybe ShareStatus)
dpssrsStatus = Lens.lens (status :: DescribePortfolioShareStatusResponse -> Lude.Maybe ShareStatus) (\s a -> s {status = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsPortfolioShareToken :: Lens.Lens' DescribePortfolioShareStatusResponse (Lude.Maybe Lude.Text)
dpssrsPortfolioShareToken = Lens.lens (portfolioShareToken :: DescribePortfolioShareStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {portfolioShareToken = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | Information about the portfolio share operation.
--
-- /Note:/ Consider using 'shareDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsShareDetails :: Lens.Lens' DescribePortfolioShareStatusResponse (Lude.Maybe ShareDetails)
dpssrsShareDetails = Lens.lens (shareDetails :: DescribePortfolioShareStatusResponse -> Lude.Maybe ShareDetails) (\s a -> s {shareDetails = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsShareDetails "Use generic-lens or generic-optics with 'shareDetails' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsPortfolioId :: Lens.Lens' DescribePortfolioShareStatusResponse (Lude.Maybe Lude.Text)
dpssrsPortfolioId = Lens.lens (portfolioId :: DescribePortfolioShareStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {portfolioId = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | Organization node identifier. It can be either account id, organizational unit id or organization id.
--
-- /Note:/ Consider using 'organizationNodeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsOrganizationNodeValue :: Lens.Lens' DescribePortfolioShareStatusResponse (Lude.Maybe Lude.Text)
dpssrsOrganizationNodeValue = Lens.lens (organizationNodeValue :: DescribePortfolioShareStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationNodeValue = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsOrganizationNodeValue "Use generic-lens or generic-optics with 'organizationNodeValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrsResponseStatus :: Lens.Lens' DescribePortfolioShareStatusResponse Lude.Int
dpssrsResponseStatus = Lens.lens (responseStatus :: DescribePortfolioShareStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePortfolioShareStatusResponse)
{-# DEPRECATED dpssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
