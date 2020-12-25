{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of event categories for all event source types, or for a specified source type. For a list of the event categories and source types, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications> .
module Network.AWS.Redshift.DescribeEventCategories
  ( -- * Creating a request
    DescribeEventCategories (..),
    mkDescribeEventCategories,

    -- ** Request lenses
    decSourceType,

    -- * Destructuring the response
    DescribeEventCategoriesResponse (..),
    mkDescribeEventCategoriesResponse,

    -- ** Response lenses
    decrrsEventCategoriesMapList,
    decrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeEventCategories' smart constructor.
newtype DescribeEventCategories = DescribeEventCategories'
  { -- | The source type, such as cluster or parameter group, to which the described event categories apply.
    --
    -- Valid values: cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, and scheduled-action.
    sourceType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventCategories' value with any optional fields omitted.
mkDescribeEventCategories ::
  DescribeEventCategories
mkDescribeEventCategories =
  DescribeEventCategories' {sourceType = Core.Nothing}

-- | The source type, such as cluster or parameter group, to which the described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group, cluster-security-group, and scheduled-action.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decSourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Types.String)
decSourceType = Lens.field @"sourceType"
{-# DEPRECATED decSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

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
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeEventCategories")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SourceType" Core.<$> sourceType)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEventCategoriesResult"
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Core.<$> ( x Core..@? "EventCategoriesMapList"
                         Core..<@> Core.parseXMLList "EventCategoriesMap"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories descriptions.
    eventCategoriesMapList :: Core.Maybe [Types.EventCategoriesMap],
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
    { eventCategoriesMapList =
        Core.Nothing,
      responseStatus
    }

-- | A list of event categories descriptions.
--
-- /Note:/ Consider using 'eventCategoriesMapList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventCategoriesMapList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [Types.EventCategoriesMap])
decrrsEventCategoriesMapList = Lens.field @"eventCategoriesMapList"
{-# DEPRECATED decrrsEventCategoriesMapList "Use generic-lens or generic-optics with 'eventCategoriesMapList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
