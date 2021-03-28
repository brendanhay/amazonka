{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetServiceGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the <https://docs.aws.amazon.com/xray/index.html AWS X-Ray SDK> . Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetServiceGraph
    (
    -- * Creating a request
      GetServiceGraph (..)
    , mkGetServiceGraph
    -- ** Request lenses
    , gsgStartTime
    , gsgEndTime
    , gsgGroupARN
    , gsgGroupName
    , gsgNextToken

    -- * Destructuring the response
    , GetServiceGraphResponse (..)
    , mkGetServiceGraphResponse
    -- ** Response lenses
    , gsgrrsContainsOldGroupVersions
    , gsgrrsEndTime
    , gsgrrsNextToken
    , gsgrrsServices
    , gsgrrsStartTime
    , gsgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetServiceGraph' smart constructor.
data GetServiceGraph = GetServiceGraph'
  { startTime :: Core.NominalDiffTime
    -- ^ The start of the time frame for which to generate a graph.
  , endTime :: Core.NominalDiffTime
    -- ^ The end of the timeframe for which to generate a graph.
  , groupARN :: Core.Maybe Types.GroupARN
    -- ^ The Amazon Resource Name (ARN) of a group based on which you want to generate a graph.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The name of a group based on which you want to generate a graph.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetServiceGraph' value with any optional fields omitted.
mkGetServiceGraph
    :: Core.NominalDiffTime -- ^ 'startTime'
    -> Core.NominalDiffTime -- ^ 'endTime'
    -> GetServiceGraph
mkGetServiceGraph startTime endTime
  = GetServiceGraph'{startTime, endTime, groupARN = Core.Nothing,
                     groupName = Core.Nothing, nextToken = Core.Nothing}

-- | The start of the time frame for which to generate a graph.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStartTime :: Lens.Lens' GetServiceGraph Core.NominalDiffTime
gsgStartTime = Lens.field @"startTime"
{-# INLINEABLE gsgStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The end of the timeframe for which to generate a graph.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgEndTime :: Lens.Lens' GetServiceGraph Core.NominalDiffTime
gsgEndTime = Lens.field @"endTime"
{-# INLINEABLE gsgEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of a group based on which you want to generate a graph.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGroupARN :: Lens.Lens' GetServiceGraph (Core.Maybe Types.GroupARN)
gsgGroupARN = Lens.field @"groupARN"
{-# INLINEABLE gsgGroupARN #-}
{-# DEPRECATED groupARN "Use generic-lens or generic-optics with 'groupARN' instead"  #-}

-- | The name of a group based on which you want to generate a graph.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgGroupName :: Lens.Lens' GetServiceGraph (Core.Maybe Types.GroupName)
gsgGroupName = Lens.field @"groupName"
{-# INLINEABLE gsgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgNextToken :: Lens.Lens' GetServiceGraph (Core.Maybe Core.Text)
gsgNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetServiceGraph where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetServiceGraph where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetServiceGraph where
        toJSON GetServiceGraph{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartTime" Core..= startTime),
                  Core.Just ("EndTime" Core..= endTime),
                  ("GroupARN" Core..=) Core.<$> groupARN,
                  ("GroupName" Core..=) Core.<$> groupName,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetServiceGraph where
        type Rs GetServiceGraph = GetServiceGraphResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/ServiceGraph",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetServiceGraphResponse' Core.<$>
                   (x Core..:? "ContainsOldGroupVersions") Core.<*>
                     x Core..:? "EndTime"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "Services"
                     Core.<*> x Core..:? "StartTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetServiceGraph where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"services" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetServiceGraphResponse' smart constructor.
data GetServiceGraphResponse = GetServiceGraphResponse'
  { containsOldGroupVersions :: Core.Maybe Core.Bool
    -- ^ A flag indicating whether the group's filter expression has been consistent, or if the returned service graph may show traces from an older version of the group's filter expression.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end of the time frame for which the graph was generated.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  , services :: Core.Maybe [Types.ServiceInfo]
    -- ^ The services that have processed a traced request during the specified time frame.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start of the time frame for which the graph was generated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetServiceGraphResponse' value with any optional fields omitted.
mkGetServiceGraphResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetServiceGraphResponse
mkGetServiceGraphResponse responseStatus
  = GetServiceGraphResponse'{containsOldGroupVersions = Core.Nothing,
                             endTime = Core.Nothing, nextToken = Core.Nothing,
                             services = Core.Nothing, startTime = Core.Nothing, responseStatus}

-- | A flag indicating whether the group's filter expression has been consistent, or if the returned service graph may show traces from an older version of the group's filter expression.
--
-- /Note:/ Consider using 'containsOldGroupVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsContainsOldGroupVersions :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.Bool)
gsgrrsContainsOldGroupVersions = Lens.field @"containsOldGroupVersions"
{-# INLINEABLE gsgrrsContainsOldGroupVersions #-}
{-# DEPRECATED containsOldGroupVersions "Use generic-lens or generic-optics with 'containsOldGroupVersions' instead"  #-}

-- | The end of the time frame for which the graph was generated.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsEndTime :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.NominalDiffTime)
gsgrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE gsgrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsNextToken :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.Text)
gsgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The services that have processed a traced request during the specified time frame.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsServices :: Lens.Lens' GetServiceGraphResponse (Core.Maybe [Types.ServiceInfo])
gsgrrsServices = Lens.field @"services"
{-# INLINEABLE gsgrrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The start of the time frame for which the graph was generated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsStartTime :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.NominalDiffTime)
gsgrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE gsgrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsResponseStatus :: Lens.Lens' GetServiceGraphResponse Core.Int
gsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
