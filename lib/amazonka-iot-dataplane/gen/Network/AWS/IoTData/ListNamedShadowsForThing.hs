{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.ListNamedShadowsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shadows for the specified thing.
module Network.AWS.IoTData.ListNamedShadowsForThing
    (
    -- * Creating a request
      ListNamedShadowsForThing (..)
    , mkListNamedShadowsForThing
    -- ** Request lenses
    , lnsftThingName
    , lnsftNextToken
    , lnsftPageSize

    -- * Destructuring the response
    , ListNamedShadowsForThingResponse (..)
    , mkListNamedShadowsForThingResponse
    -- ** Response lenses
    , lnsftrrsNextToken
    , lnsftrrsResults
    , lnsftrrsTimestamp
    , lnsftrrsResponseStatus
    ) where

import qualified Network.AWS.IoTData.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListNamedShadowsForThing' smart constructor.
data ListNamedShadowsForThing = ListNamedShadowsForThing'
  { thingName :: Types.ThingName
    -- ^ The name of the thing.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The result page size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListNamedShadowsForThing' value with any optional fields omitted.
mkListNamedShadowsForThing
    :: Types.ThingName -- ^ 'thingName'
    -> ListNamedShadowsForThing
mkListNamedShadowsForThing thingName
  = ListNamedShadowsForThing'{thingName, nextToken = Core.Nothing,
                              pageSize = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftThingName :: Lens.Lens' ListNamedShadowsForThing Types.ThingName
lnsftThingName = Lens.field @"thingName"
{-# INLINEABLE lnsftThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftNextToken :: Lens.Lens' ListNamedShadowsForThing (Core.Maybe Types.NextToken)
lnsftNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnsftNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftPageSize :: Lens.Lens' ListNamedShadowsForThing (Core.Maybe Core.Natural)
lnsftPageSize = Lens.field @"pageSize"
{-# INLINEABLE lnsftPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery ListNamedShadowsForThing where
        toQuery ListNamedShadowsForThing{..}
          = Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "pageSize") pageSize

instance Core.ToHeaders ListNamedShadowsForThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListNamedShadowsForThing where
        type Rs ListNamedShadowsForThing = ListNamedShadowsForThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/api/things/shadow/ListNamedShadowsForThing/" Core.<>
                             Core.toText thingName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListNamedShadowsForThingResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "results" Core.<*>
                     x Core..:? "timestamp"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListNamedShadowsForThingResponse' smart constructor.
data ListNamedShadowsForThingResponse = ListNamedShadowsForThingResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results, or null if there are no additional results.
  , results :: Core.Maybe [Types.ShadowName]
    -- ^ The list of shadows for the specified thing.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ The Epoch date and time the response was generated by AWS IoT.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListNamedShadowsForThingResponse' value with any optional fields omitted.
mkListNamedShadowsForThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListNamedShadowsForThingResponse
mkListNamedShadowsForThingResponse responseStatus
  = ListNamedShadowsForThingResponse'{nextToken = Core.Nothing,
                                      results = Core.Nothing, timestamp = Core.Nothing,
                                      responseStatus}

-- | The token for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrrsNextToken :: Lens.Lens' ListNamedShadowsForThingResponse (Core.Maybe Types.NextToken)
lnsftrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnsftrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of shadows for the specified thing.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrrsResults :: Lens.Lens' ListNamedShadowsForThingResponse (Core.Maybe [Types.ShadowName])
lnsftrrsResults = Lens.field @"results"
{-# INLINEABLE lnsftrrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The Epoch date and time the response was generated by AWS IoT.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrrsTimestamp :: Lens.Lens' ListNamedShadowsForThingResponse (Core.Maybe Core.Integer)
lnsftrrsTimestamp = Lens.field @"timestamp"
{-# INLINEABLE lnsftrrsTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrrsResponseStatus :: Lens.Lens' ListNamedShadowsForThingResponse Core.Int
lnsftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lnsftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
