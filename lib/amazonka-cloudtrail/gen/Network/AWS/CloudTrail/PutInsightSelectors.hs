{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.PutInsightSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lets you enable Insights event logging by specifying the Insights selectors that you want to enable on an existing trail. You also use @PutInsightSelectors@ to turn off Insights event logging, by passing an empty list of insight types. In this release, only @ApiCallRateInsight@ is supported as an Insights selector.
module Network.AWS.CloudTrail.PutInsightSelectors
    (
    -- * Creating a request
      PutInsightSelectors (..)
    , mkPutInsightSelectors
    -- ** Request lenses
    , pisTrailName
    , pisInsightSelectors

    -- * Destructuring the response
    , PutInsightSelectorsResponse (..)
    , mkPutInsightSelectorsResponse
    -- ** Response lenses
    , pisrrsInsightSelectors
    , pisrrsTrailARN
    , pisrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInsightSelectors' smart constructor.
data PutInsightSelectors = PutInsightSelectors'
  { trailName :: Core.Text
    -- ^ The name of the CloudTrail trail for which you want to change or add Insights selectors.
  , insightSelectors :: [Types.InsightSelector]
    -- ^ A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightSelectors' value with any optional fields omitted.
mkPutInsightSelectors
    :: Core.Text -- ^ 'trailName'
    -> PutInsightSelectors
mkPutInsightSelectors trailName
  = PutInsightSelectors'{trailName, insightSelectors = Core.mempty}

-- | The name of the CloudTrail trail for which you want to change or add Insights selectors.
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisTrailName :: Lens.Lens' PutInsightSelectors Core.Text
pisTrailName = Lens.field @"trailName"
{-# INLINEABLE pisTrailName #-}
{-# DEPRECATED trailName "Use generic-lens or generic-optics with 'trailName' instead"  #-}

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisInsightSelectors :: Lens.Lens' PutInsightSelectors [Types.InsightSelector]
pisInsightSelectors = Lens.field @"insightSelectors"
{-# INLINEABLE pisInsightSelectors #-}
{-# DEPRECATED insightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead"  #-}

instance Core.ToQuery PutInsightSelectors where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutInsightSelectors where
        toHeaders PutInsightSelectors{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutInsightSelectors")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutInsightSelectors where
        toJSON PutInsightSelectors{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrailName" Core..= trailName),
                  Core.Just ("InsightSelectors" Core..= insightSelectors)])

instance Core.AWSRequest PutInsightSelectors where
        type Rs PutInsightSelectors = PutInsightSelectorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutInsightSelectorsResponse' Core.<$>
                   (x Core..:? "InsightSelectors") Core.<*> x Core..:? "TrailARN"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutInsightSelectorsResponse' smart constructor.
data PutInsightSelectorsResponse = PutInsightSelectorsResponse'
  { insightSelectors :: Core.Maybe [Types.InsightSelector]
    -- ^ A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
  , trailARN :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightSelectorsResponse' value with any optional fields omitted.
mkPutInsightSelectorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutInsightSelectorsResponse
mkPutInsightSelectorsResponse responseStatus
  = PutInsightSelectorsResponse'{insightSelectors = Core.Nothing,
                                 trailARN = Core.Nothing, responseStatus}

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrrsInsightSelectors :: Lens.Lens' PutInsightSelectorsResponse (Core.Maybe [Types.InsightSelector])
pisrrsInsightSelectors = Lens.field @"insightSelectors"
{-# INLINEABLE pisrrsInsightSelectors #-}
{-# DEPRECATED insightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead"  #-}

-- | The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrrsTrailARN :: Lens.Lens' PutInsightSelectorsResponse (Core.Maybe Core.Text)
pisrrsTrailARN = Lens.field @"trailARN"
{-# INLINEABLE pisrrsTrailARN #-}
{-# DEPRECATED trailARN "Use generic-lens or generic-optics with 'trailARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pisrrsResponseStatus :: Lens.Lens' PutInsightSelectorsResponse Core.Int
pisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
