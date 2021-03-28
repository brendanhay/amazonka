{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all tags added to the specified Amazon SNS topic. For an overview, see <https://docs.aws.amazon.com/sns/latest/dg/sns-tags.html Amazon SNS Tags> in the /Amazon Simple Notification Service Developer Guide/ .
module Network.AWS.SNS.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceArn

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { resourceArn :: Types.AmazonResourceName
    -- ^ The ARN of the topic for which to list tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.AmazonResourceName -- ^ 'resourceArn'
    -> ListTagsForResource
mkListTagsForResource resourceArn
  = ListTagsForResource'{resourceArn}

-- | The ARN of the topic for which to list tags.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceArn :: Lens.Lens' ListTagsForResource Types.AmazonResourceName
ltfrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ltfrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery ListTagsForResource{..}
          = Core.toQueryPair "Action" ("ListTagsForResource" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceArn" resourceArn

instance Core.ToHeaders ListTagsForResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
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
          = Response.receiveXMLWrapper "ListTagsForResourceResult"
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..@? "Tags" Core..<@> Core.parseXMLList "member") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the specified topic.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus
  = ListTagsForResourceResponse'{tags = Core.Nothing, responseStatus}

-- | The tags associated with the specified topic.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfrrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
