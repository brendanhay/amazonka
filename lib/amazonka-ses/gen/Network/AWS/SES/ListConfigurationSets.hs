{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListConfigurationSets (..)
    , mkListConfigurationSets
    -- ** Request lenses
    , lcsMaxItems
    , lcsNextToken

    -- * Destructuring the response
    , ListConfigurationSetsResponse (..)
    , mkListConfigurationSetsResponse
    -- ** Response lenses
    , lcsrrsConfigurationSets
    , lcsrrsNextToken
    , lcsrrsResponseStatus
    ) where

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
  { maxItems :: Core.Maybe Core.Int
    -- ^ The number of configuration sets to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationSets' value with any optional fields omitted.
mkListConfigurationSets
    :: ListConfigurationSets
mkListConfigurationSets
  = ListConfigurationSets'{maxItems = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The number of configuration sets to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxItems :: Lens.Lens' ListConfigurationSets (Core.Maybe Core.Int)
lcsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lcsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListConfigurationSets (Core.Maybe Types.NextToken)
lcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListConfigurationSets where
        toQuery ListConfigurationSets{..}
          = Core.toQueryPair "Action" ("ListConfigurationSets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListConfigurationSets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListConfigurationSets where
        type Rs ListConfigurationSets = ListConfigurationSetsResponse
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
          = Response.receiveXMLWrapper "ListConfigurationSetsResult"
              (\ s h x ->
                 ListConfigurationSetsResponse' Core.<$>
                   (x Core..@? "ConfigurationSets" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListConfigurationSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"configurationSets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { configurationSets :: Core.Maybe [Types.ConfigurationSet]
    -- ^ A list of configuration sets.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurationSetsResponse' value with any optional fields omitted.
mkListConfigurationSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConfigurationSetsResponse
mkListConfigurationSetsResponse responseStatus
  = ListConfigurationSetsResponse'{configurationSets = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | A list of configuration sets.
--
-- /Note:/ Consider using 'configurationSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsConfigurationSets :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe [Types.ConfigurationSet])
lcsrrsConfigurationSets = Lens.field @"configurationSets"
{-# INLINEABLE lcsrrsConfigurationSets #-}
{-# DEPRECATED configurationSets "Use generic-lens or generic-optics with 'configurationSets' instead"  #-}

-- | A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ . 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsNextToken :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe Types.NextToken)
lcsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsResponseStatus :: Lens.Lens' ListConfigurationSetsResponse Core.Int
lcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
