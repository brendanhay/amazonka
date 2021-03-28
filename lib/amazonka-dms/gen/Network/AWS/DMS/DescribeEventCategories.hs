{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration Service User Guide./ 
module Network.AWS.DMS.DescribeEventCategories
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
    , decrrsEventCategoryGroupList
    , decrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to the event categories.
  , sourceType :: Core.Maybe Core.Text
    -- ^ The type of AWS DMS resource that generates events. 
--
-- Valid values: replication-instance | replication-task
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategories' value with any optional fields omitted.
mkDescribeEventCategories
    :: DescribeEventCategories
mkDescribeEventCategories
  = DescribeEventCategories'{filters = Core.Nothing,
                             sourceType = Core.Nothing}

-- | Filters applied to the event categories.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decFilters :: Lens.Lens' DescribeEventCategories (Core.Maybe [Types.Filter])
decFilters = Lens.field @"filters"
{-# INLINEABLE decFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The type of AWS DMS resource that generates events. 
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Core.Text)
decSourceType = Lens.field @"sourceType"
{-# INLINEABLE decSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.ToQuery DescribeEventCategories where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventCategories where
        toHeaders DescribeEventCategories{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeEventCategories")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventCategories where
        toJSON DescribeEventCategories{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("SourceType" Core..=) Core.<$> sourceType])

instance Core.AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories = DescribeEventCategoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventCategoriesResponse' Core.<$>
                   (x Core..:? "EventCategoryGroupList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { eventCategoryGroupList :: Core.Maybe [Types.EventCategoryGroup]
    -- ^ A list of event categories.
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
  = DescribeEventCategoriesResponse'{eventCategoryGroupList =
                                       Core.Nothing,
                                     responseStatus}

-- | A list of event categories.
--
-- /Note:/ Consider using 'eventCategoryGroupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventCategoryGroupList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [Types.EventCategoryGroup])
decrrsEventCategoryGroupList = Lens.field @"eventCategoryGroupList"
{-# INLINEABLE decrrsEventCategoryGroupList #-}
{-# DEPRECATED eventCategoryGroupList "Use generic-lens or generic-optics with 'eventCategoryGroupList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
