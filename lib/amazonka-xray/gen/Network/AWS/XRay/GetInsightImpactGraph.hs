{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightImpactGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph structure filtered by the specified insight. The service graph is limited to only structural information. For a complete service graph, use this API with the GetServiceGraph API.
module Network.AWS.XRay.GetInsightImpactGraph
    (
    -- * Creating a request
      GetInsightImpactGraph (..)
    , mkGetInsightImpactGraph
    -- ** Request lenses
    , giigInsightId
    , giigStartTime
    , giigEndTime
    , giigNextToken

    -- * Destructuring the response
    , GetInsightImpactGraphResponse (..)
    , mkGetInsightImpactGraphResponse
    -- ** Response lenses
    , giigrrsEndTime
    , giigrrsInsightId
    , giigrrsNextToken
    , giigrrsServiceGraphEndTime
    , giigrrsServiceGraphStartTime
    , giigrrsServices
    , giigrrsStartTime
    , giigrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetInsightImpactGraph' smart constructor.
data GetInsightImpactGraph = GetInsightImpactGraph'
  { insightId :: Types.InsightId
    -- ^ The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
  , startTime :: Core.NominalDiffTime
    -- ^ The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
  , endTime :: Core.NominalDiffTime
    -- ^ The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Specify the pagination token returned by a previous request to retrieve the next page of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInsightImpactGraph' value with any optional fields omitted.
mkGetInsightImpactGraph
    :: Types.InsightId -- ^ 'insightId'
    -> Core.NominalDiffTime -- ^ 'startTime'
    -> Core.NominalDiffTime -- ^ 'endTime'
    -> GetInsightImpactGraph
mkGetInsightImpactGraph insightId startTime endTime
  = GetInsightImpactGraph'{insightId, startTime, endTime,
                           nextToken = Core.Nothing}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigInsightId :: Lens.Lens' GetInsightImpactGraph Types.InsightId
giigInsightId = Lens.field @"insightId"
{-# INLINEABLE giigInsightId #-}
{-# DEPRECATED insightId "Use generic-lens or generic-optics with 'insightId' instead"  #-}

-- | The estimated start time of the insight, in Unix time seconds. The StartTime is inclusive of the value provided and can't be more than 30 days old.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigStartTime :: Lens.Lens' GetInsightImpactGraph Core.NominalDiffTime
giigStartTime = Lens.field @"startTime"
{-# INLINEABLE giigStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The estimated end time of the insight, in Unix time seconds. The EndTime is exclusive of the value provided. The time range between the start time and end time can't be more than six hours. 
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigEndTime :: Lens.Lens' GetInsightImpactGraph Core.NominalDiffTime
giigEndTime = Lens.field @"endTime"
{-# INLINEABLE giigEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Specify the pagination token returned by a previous request to retrieve the next page of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigNextToken :: Lens.Lens' GetInsightImpactGraph (Core.Maybe Types.NextToken)
giigNextToken = Lens.field @"nextToken"
{-# INLINEABLE giigNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetInsightImpactGraph where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInsightImpactGraph where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetInsightImpactGraph where
        toJSON GetInsightImpactGraph{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InsightId" Core..= insightId),
                  Core.Just ("StartTime" Core..= startTime),
                  Core.Just ("EndTime" Core..= endTime),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetInsightImpactGraph where
        type Rs GetInsightImpactGraph = GetInsightImpactGraphResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/InsightImpactGraph",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInsightImpactGraphResponse' Core.<$>
                   (x Core..:? "EndTime") Core.<*> x Core..:? "InsightId" Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> x Core..:? "ServiceGraphEndTime"
                     Core.<*> x Core..:? "ServiceGraphStartTime"
                     Core.<*> x Core..:? "Services"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInsightImpactGraphResponse' smart constructor.
data GetInsightImpactGraphResponse = GetInsightImpactGraphResponse'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The provided end time. 
  , insightId :: Core.Maybe Types.InsightId
    -- ^ The insight's unique identifier.
  , nextToken :: Core.Maybe Types.Token
    -- ^ Pagination token.
  , serviceGraphEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in Unix seconds, at which the service graph ended.
  , serviceGraphStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in Unix seconds, at which the service graph started.
  , services :: Core.Maybe [Types.InsightImpactGraphService]
    -- ^ The AWS instrumented services related to the insight.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The provided start time.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInsightImpactGraphResponse' value with any optional fields omitted.
mkGetInsightImpactGraphResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInsightImpactGraphResponse
mkGetInsightImpactGraphResponse responseStatus
  = GetInsightImpactGraphResponse'{endTime = Core.Nothing,
                                   insightId = Core.Nothing, nextToken = Core.Nothing,
                                   serviceGraphEndTime = Core.Nothing,
                                   serviceGraphStartTime = Core.Nothing, services = Core.Nothing,
                                   startTime = Core.Nothing, responseStatus}

-- | The provided end time. 
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsEndTime :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Core.NominalDiffTime)
giigrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE giigrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The insight's unique identifier.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsInsightId :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Types.InsightId)
giigrrsInsightId = Lens.field @"insightId"
{-# INLINEABLE giigrrsInsightId #-}
{-# DEPRECATED insightId "Use generic-lens or generic-optics with 'insightId' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsNextToken :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Types.Token)
giigrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE giigrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The time, in Unix seconds, at which the service graph ended.
--
-- /Note:/ Consider using 'serviceGraphEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsServiceGraphEndTime :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Core.NominalDiffTime)
giigrrsServiceGraphEndTime = Lens.field @"serviceGraphEndTime"
{-# INLINEABLE giigrrsServiceGraphEndTime #-}
{-# DEPRECATED serviceGraphEndTime "Use generic-lens or generic-optics with 'serviceGraphEndTime' instead"  #-}

-- | The time, in Unix seconds, at which the service graph started.
--
-- /Note:/ Consider using 'serviceGraphStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsServiceGraphStartTime :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Core.NominalDiffTime)
giigrrsServiceGraphStartTime = Lens.field @"serviceGraphStartTime"
{-# INLINEABLE giigrrsServiceGraphStartTime #-}
{-# DEPRECATED serviceGraphStartTime "Use generic-lens or generic-optics with 'serviceGraphStartTime' instead"  #-}

-- | The AWS instrumented services related to the insight.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsServices :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe [Types.InsightImpactGraphService])
giigrrsServices = Lens.field @"services"
{-# INLINEABLE giigrrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The provided start time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsStartTime :: Lens.Lens' GetInsightImpactGraphResponse (Core.Maybe Core.NominalDiffTime)
giigrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE giigrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giigrrsResponseStatus :: Lens.Lens' GetInsightImpactGraphResponse Core.Int
giigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
