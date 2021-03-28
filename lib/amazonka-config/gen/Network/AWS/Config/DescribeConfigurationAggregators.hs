{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationAggregators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more configuration aggregators. If the configuration aggregator is not specified, this action returns the details for all the configuration aggregators associated with the account. 
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigurationAggregators
    (
    -- * Creating a request
      DescribeConfigurationAggregators (..)
    , mkDescribeConfigurationAggregators
    -- ** Request lenses
    , dcaConfigurationAggregatorNames
    , dcaLimit
    , dcaNextToken

    -- * Destructuring the response
    , DescribeConfigurationAggregatorsResponse (..)
    , mkDescribeConfigurationAggregatorsResponse
    -- ** Response lenses
    , dcarrsConfigurationAggregators
    , dcarrsNextToken
    , dcarrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { configurationAggregatorNames :: Core.Maybe [Types.ConfigurationAggregatorName]
    -- ^ The name of the configuration aggregators.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationAggregators' value with any optional fields omitted.
mkDescribeConfigurationAggregators
    :: DescribeConfigurationAggregators
mkDescribeConfigurationAggregators
  = DescribeConfigurationAggregators'{configurationAggregatorNames =
                                        Core.Nothing,
                                      limit = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the configuration aggregators.
--
-- /Note:/ Consider using 'configurationAggregatorNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaConfigurationAggregatorNames :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe [Types.ConfigurationAggregatorName])
dcaConfigurationAggregatorNames = Lens.field @"configurationAggregatorNames"
{-# INLINEABLE dcaConfigurationAggregatorNames #-}
{-# DEPRECATED configurationAggregatorNames "Use generic-lens or generic-optics with 'configurationAggregatorNames' instead"  #-}

-- | The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaLimit :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe Core.Natural)
dcaLimit = Lens.field @"limit"
{-# INLINEABLE dcaLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaNextToken :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe Core.Text)
dcaNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConfigurationAggregators where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConfigurationAggregators where
        toHeaders DescribeConfigurationAggregators{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeConfigurationAggregators")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConfigurationAggregators where
        toJSON DescribeConfigurationAggregators{..}
          = Core.object
              (Core.catMaybes
                 [("ConfigurationAggregatorNames" Core..=) Core.<$>
                    configurationAggregatorNames,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConfigurationAggregators where
        type Rs DescribeConfigurationAggregators =
             DescribeConfigurationAggregatorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConfigurationAggregatorsResponse' Core.<$>
                   (x Core..:? "ConfigurationAggregators") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeConfigurationAggregators where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"configurationAggregators" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { configurationAggregators :: Core.Maybe [Types.ConfigurationAggregator]
    -- ^ Returns a ConfigurationAggregators object.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConfigurationAggregatorsResponse' value with any optional fields omitted.
mkDescribeConfigurationAggregatorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigurationAggregatorsResponse
mkDescribeConfigurationAggregatorsResponse responseStatus
  = DescribeConfigurationAggregatorsResponse'{configurationAggregators
                                                = Core.Nothing,
                                              nextToken = Core.Nothing, responseStatus}

-- | Returns a ConfigurationAggregators object.
--
-- /Note:/ Consider using 'configurationAggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsConfigurationAggregators :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Core.Maybe [Types.ConfigurationAggregator])
dcarrsConfigurationAggregators = Lens.field @"configurationAggregators"
{-# INLINEABLE dcarrsConfigurationAggregators #-}
{-# DEPRECATED configurationAggregators "Use generic-lens or generic-optics with 'configurationAggregators' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsNextToken :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Core.Maybe Core.Text)
dcarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsResponseStatus :: Lens.Lens' DescribeConfigurationAggregatorsResponse Core.Int
dcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
