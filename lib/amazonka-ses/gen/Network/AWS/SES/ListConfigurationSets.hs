{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListConfigurationSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the configuration sets associated with your Amazon SES account in the current AWS Region. For information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity> in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the @ListConfigurationSets@ operation again, passing the @NextToken@ parameter and the value of the NextToken element to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListConfigurationSets
  ( -- * Creating a request
    ListConfigurationSets (..),
    mkListConfigurationSets,

    -- ** Request lenses
    lcsMaxItems,
    lcsNextToken,

    -- * Destructuring the response
    ListConfigurationSetsResponse (..),
    mkListConfigurationSetsResponse,

    -- ** Response lenses
    lcsrrsConfigurationSets,
    lcsrrsNextToken,
    lcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { -- | The number of configuration sets to return.
    maxItems :: Core.Maybe Core.Int,
    -- | A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationSets' value with any optional fields omitted.
mkListConfigurationSets ::
  ListConfigurationSets
mkListConfigurationSets =
  ListConfigurationSets'
    { maxItems = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The number of configuration sets to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxItems :: Lens.Lens' ListConfigurationSets (Core.Maybe Core.Int)
lcsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lcsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListConfigurationSets (Core.Maybe Types.NextToken)
lcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListConfigurationSets where
  type Rs ListConfigurationSets = ListConfigurationSetsResponse
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
            ( Core.pure ("Action", "ListConfigurationSets")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListConfigurationSetsResult"
      ( \s h x ->
          ListConfigurationSetsResponse'
            Core.<$> ( x Core..@? "ConfigurationSets"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListConfigurationSets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"configurationSets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { -- | A list of configuration sets.
    configurationSets :: Core.Maybe [Types.ConfigurationSet],
    -- | A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationSetsResponse' value with any optional fields omitted.
mkListConfigurationSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListConfigurationSetsResponse
mkListConfigurationSetsResponse responseStatus =
  ListConfigurationSetsResponse'
    { configurationSets = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of configuration sets.
--
-- /Note:/ Consider using 'configurationSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsConfigurationSets :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe [Types.ConfigurationSet])
lcsrrsConfigurationSets = Lens.field @"configurationSets"
{-# DEPRECATED lcsrrsConfigurationSets "Use generic-lens or generic-optics with 'configurationSets' instead." #-}

-- | A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsNextToken :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe Types.NextToken)
lcsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsResponseStatus :: Lens.Lens' ListConfigurationSetsResponse Core.Int
lcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
