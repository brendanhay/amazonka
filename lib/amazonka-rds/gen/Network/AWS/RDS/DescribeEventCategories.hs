{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> in the /Amazon RDS User Guide./ 
module Network.AWS.RDS.DescribeEventCategories
    (
    -- * Creating a request
      DescribeEventCategories (..)
    , mkDescribeEventCategories
    -- ** Request lenses
    , decFilters
    , decSourceType

    -- * Destructuring the response
    , DescribeEventCategoriesResponse (..)
    , mkDescribeEventCategoriesResponse
    -- ** Response lenses
    , decrrsEventCategoriesMapList
    , decrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , sourceType :: Core.Maybe Core.Text
    -- ^ The type of source that is generating the events.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategories' value with any optional fields omitted.
mkDescribeEventCategories
    :: DescribeEventCategories
mkDescribeEventCategories
  = DescribeEventCategories'{filters = Core.Nothing,
                             sourceType = Core.Nothing}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decFilters :: Lens.Lens' DescribeEventCategories (Core.Maybe [Types.Filter])
decFilters = Lens.field @"filters"
{-# INLINEABLE decFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The type of source that is generating the events.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ | @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ 
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Core.Text)
decSourceType = Lens.field @"sourceType"
{-# INLINEABLE decSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.ToQuery DescribeEventCategories where
        toQuery DescribeEventCategories{..}
          = Core.toQueryPair "Action"
              ("DescribeEventCategories" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceType") sourceType

instance Core.ToHeaders DescribeEventCategories where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories = DescribeEventCategoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeEventCategoriesResult"
              (\ s h x ->
                 DescribeEventCategoriesResponse' Core.<$>
                   (x Core..@? "EventCategoriesMapList" Core..<@>
                      Core.parseXMLList "EventCategoriesMap")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Data returned from the @DescribeEventCategories@ operation.
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { eventCategoriesMapList :: Core.Maybe [Types.EventCategoriesMap]
    -- ^ A list of EventCategoriesMap data types.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategoriesResponse' value with any optional fields omitted.
mkDescribeEventCategoriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventCategoriesResponse
mkDescribeEventCategoriesResponse responseStatus
  = DescribeEventCategoriesResponse'{eventCategoriesMapList =
                                       Core.Nothing,
                                     responseStatus}

-- | A list of EventCategoriesMap data types.
--
-- /Note:/ Consider using 'eventCategoriesMapList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventCategoriesMapList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [Types.EventCategoriesMap])
decrrsEventCategoriesMapList = Lens.field @"eventCategoriesMapList"
{-# INLINEABLE decrrsEventCategoriesMapList #-}
{-# DEPRECATED eventCategoriesMapList "Use generic-lens or generic-optics with 'eventCategoriesMapList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
