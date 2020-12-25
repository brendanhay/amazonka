{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail content delivery network (CDN) distributions.
module Network.AWS.Lightsail.GetDistributions
  ( -- * Creating a request
    GetDistributions (..),
    mkGetDistributions,

    -- ** Request lenses
    gdDistributionName,
    gdPageToken,

    -- * Destructuring the response
    GetDistributionsResponse (..),
    mkGetDistributionsResponse,

    -- ** Response lenses
    gdrrsDistributions,
    gdrrsNextPageToken,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { -- | The name of the distribution for which to return information.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    -- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
    distributionName :: Core.Maybe Types.ResourceName,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributions' value with any optional fields omitted.
mkGetDistributions ::
  GetDistributions
mkGetDistributions =
  GetDistributions'
    { distributionName = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDistributionName :: Lens.Lens' GetDistributions (Core.Maybe Types.ResourceName)
gdDistributionName = Lens.field @"distributionName"
{-# DEPRECATED gdDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPageToken :: Lens.Lens' GetDistributions (Core.Maybe Types.String)
gdPageToken = Lens.field @"pageToken"
{-# DEPRECATED gdPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetDistributions where
  toJSON GetDistributions {..} =
    Core.object
      ( Core.catMaybes
          [ ("distributionName" Core..=) Core.<$> distributionName,
            ("pageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest GetDistributions where
  type Rs GetDistributions = GetDistributionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDistributions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionsResponse'
            Core.<$> (x Core..:? "distributions")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { -- | An array of objects that describe your distributions.
    distributions :: Core.Maybe [Types.LightsailDistribution],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDistributionsResponse' value with any optional fields omitted.
mkGetDistributionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDistributionsResponse
mkGetDistributionsResponse responseStatus =
  GetDistributionsResponse'
    { distributions = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe your distributions.
--
-- /Note:/ Consider using 'distributions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDistributions :: Lens.Lens' GetDistributionsResponse (Core.Maybe [Types.LightsailDistribution])
gdrrsDistributions = Lens.field @"distributions"
{-# DEPRECATED gdrrsDistributions "Use generic-lens or generic-optics with 'distributions' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsNextPageToken :: Lens.Lens' GetDistributionsResponse (Core.Maybe Types.String)
gdrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gdrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDistributionsResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
