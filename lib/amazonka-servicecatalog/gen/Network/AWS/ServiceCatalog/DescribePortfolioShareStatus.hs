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
    dpssrrsOrganizationNodeValue,
    dpssrrsPortfolioId,
    dpssrrsPortfolioShareToken,
    dpssrrsShareDetails,
    dpssrrsStatus,
    dpssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribePortfolioShareStatus' smart constructor.
newtype DescribePortfolioShareStatus = DescribePortfolioShareStatus'
  { -- | The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
    portfolioShareToken :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePortfolioShareStatus' value with any optional fields omitted.
mkDescribePortfolioShareStatus ::
  -- | 'portfolioShareToken'
  Types.Id ->
  DescribePortfolioShareStatus
mkDescribePortfolioShareStatus portfolioShareToken =
  DescribePortfolioShareStatus' {portfolioShareToken}

-- | The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssPortfolioShareToken :: Lens.Lens' DescribePortfolioShareStatus Types.Id
dpssPortfolioShareToken = Lens.field @"portfolioShareToken"
{-# DEPRECATED dpssPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

instance Core.FromJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PortfolioShareToken" Core..= portfolioShareToken)]
      )

instance Core.AWSRequest DescribePortfolioShareStatus where
  type
    Rs DescribePortfolioShareStatus =
      DescribePortfolioShareStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribePortfolioShareStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioShareStatusResponse'
            Core.<$> (x Core..:? "OrganizationNodeValue")
            Core.<*> (x Core..:? "PortfolioId")
            Core.<*> (x Core..:? "PortfolioShareToken")
            Core.<*> (x Core..:? "ShareDetails")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { -- | Organization node identifier. It can be either account id, organizational unit id or organization id.
    organizationNodeValue :: Core.Maybe Types.OrganizationNodeValue,
    -- | The portfolio identifier.
    portfolioId :: Core.Maybe Types.Id,
    -- | The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
    portfolioShareToken :: Core.Maybe Types.Id,
    -- | Information about the portfolio share operation.
    shareDetails :: Core.Maybe Types.ShareDetails,
    -- | Status of the portfolio share operation.
    status :: Core.Maybe Types.ShareStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePortfolioShareStatusResponse' value with any optional fields omitted.
mkDescribePortfolioShareStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePortfolioShareStatusResponse
mkDescribePortfolioShareStatusResponse responseStatus =
  DescribePortfolioShareStatusResponse'
    { organizationNodeValue =
        Core.Nothing,
      portfolioId = Core.Nothing,
      portfolioShareToken = Core.Nothing,
      shareDetails = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Organization node identifier. It can be either account id, organizational unit id or organization id.
--
-- /Note:/ Consider using 'organizationNodeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsOrganizationNodeValue :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Types.OrganizationNodeValue)
dpssrrsOrganizationNodeValue = Lens.field @"organizationNodeValue"
{-# DEPRECATED dpssrrsOrganizationNodeValue "Use generic-lens or generic-optics with 'organizationNodeValue' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsPortfolioId :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Types.Id)
dpssrrsPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED dpssrrsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsPortfolioShareToken :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Types.Id)
dpssrrsPortfolioShareToken = Lens.field @"portfolioShareToken"
{-# DEPRECATED dpssrrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | Information about the portfolio share operation.
--
-- /Note:/ Consider using 'shareDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsShareDetails :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Types.ShareDetails)
dpssrrsShareDetails = Lens.field @"shareDetails"
{-# DEPRECATED dpssrrsShareDetails "Use generic-lens or generic-optics with 'shareDetails' instead." #-}

-- | Status of the portfolio share operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsStatus :: Lens.Lens' DescribePortfolioShareStatusResponse (Core.Maybe Types.ShareStatus)
dpssrrsStatus = Lens.field @"status"
{-# DEPRECATED dpssrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpssrrsResponseStatus :: Lens.Lens' DescribePortfolioShareStatusResponse Core.Int
dpssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
