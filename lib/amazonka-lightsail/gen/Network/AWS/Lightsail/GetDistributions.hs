{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDistributions (..)
    , mkGetDistributions
    -- ** Request lenses
    , gdDistributionName
    , gdPageToken

    -- * Destructuring the response
    , GetDistributionsResponse (..)
    , mkGetDistributionsResponse
    -- ** Response lenses
    , gdrrsDistributions
    , gdrrsNextPageToken
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { distributionName :: Core.Maybe Types.ResourceName
    -- ^ The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributions' value with any optional fields omitted.
mkGetDistributions
    :: GetDistributions
mkGetDistributions
  = GetDistributions'{distributionName = Core.Nothing,
                      pageToken = Core.Nothing}

-- | The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDistributionName :: Lens.Lens' GetDistributions (Core.Maybe Types.ResourceName)
gdDistributionName = Lens.field @"distributionName"
{-# INLINEABLE gdDistributionName #-}
{-# DEPRECATED distributionName "Use generic-lens or generic-optics with 'distributionName' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPageToken :: Lens.Lens' GetDistributions (Core.Maybe Core.Text)
gdPageToken = Lens.field @"pageToken"
{-# INLINEABLE gdPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetDistributions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDistributions where
        toHeaders GetDistributions{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDistributions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDistributions where
        toJSON GetDistributions{..}
          = Core.object
              (Core.catMaybes
                 [("distributionName" Core..=) Core.<$> distributionName,
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetDistributions where
        type Rs GetDistributions = GetDistributionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDistributionsResponse' Core.<$>
                   (x Core..:? "distributions") Core.<*> x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { distributions :: Core.Maybe [Types.LightsailDistribution]
    -- ^ An array of objects that describe your distributions.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDistributionsResponse' value with any optional fields omitted.
mkGetDistributionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDistributionsResponse
mkGetDistributionsResponse responseStatus
  = GetDistributionsResponse'{distributions = Core.Nothing,
                              nextPageToken = Core.Nothing, responseStatus}

-- | An array of objects that describe your distributions.
--
-- /Note:/ Consider using 'distributions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDistributions :: Lens.Lens' GetDistributionsResponse (Core.Maybe [Types.LightsailDistribution])
gdrrsDistributions = Lens.field @"distributions"
{-# INLINEABLE gdrrsDistributions #-}
{-# DEPRECATED distributions "Use generic-lens or generic-optics with 'distributions' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsNextPageToken :: Lens.Lens' GetDistributionsResponse (Core.Maybe Core.Text)
gdrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gdrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDistributionsResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
