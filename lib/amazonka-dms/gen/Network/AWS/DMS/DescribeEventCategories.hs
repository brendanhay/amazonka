{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEventCategories (..),
    mkDescribeEventCategories,

    -- ** Request lenses
    decFilters,
    decSourceType,

    -- * Destructuring the response
    DescribeEventCategoriesResponse (..),
    mkDescribeEventCategoriesResponse,

    -- ** Response lenses
    decrrsEventCategoryGroupList,
    decrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | Filters applied to the event categories.
    filters :: Core.Maybe [Types.Filter],
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategories' value with any optional fields omitted.
mkDescribeEventCategories ::
  DescribeEventCategories
mkDescribeEventCategories =
  DescribeEventCategories'
    { filters = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | Filters applied to the event categories.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decFilters :: Lens.Lens' DescribeEventCategories (Core.Maybe [Types.Filter])
decFilters = Lens.field @"filters"
{-# DEPRECATED decFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Types.String)
decSourceType = Lens.field @"sourceType"
{-# DEPRECATED decSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON DescribeEventCategories where
  toJSON DescribeEventCategories {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )

instance Core.AWSRequest DescribeEventCategories where
  type Rs DescribeEventCategories = DescribeEventCategoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DescribeEventCategories")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Core.<$> (x Core..:? "EventCategoryGroupList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories.
    eventCategoryGroupList :: Core.Maybe [Types.EventCategoryGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategoriesResponse' value with any optional fields omitted.
mkDescribeEventCategoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventCategoriesResponse
mkDescribeEventCategoriesResponse responseStatus =
  DescribeEventCategoriesResponse'
    { eventCategoryGroupList =
        Core.Nothing,
      responseStatus
    }

-- | A list of event categories.
--
-- /Note:/ Consider using 'eventCategoryGroupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventCategoryGroupList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [Types.EventCategoryGroup])
decrrsEventCategoryGroupList = Lens.field @"eventCategoryGroupList"
{-# DEPRECATED decrrsEventCategoryGroupList "Use generic-lens or generic-optics with 'eventCategoryGroupList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
