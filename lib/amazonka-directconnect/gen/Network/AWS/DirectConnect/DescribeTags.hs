{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified AWS Direct Connect resources.
module Network.AWS.DirectConnect.DescribeTags
    (
    -- * Creating a request
      DescribeTags (..)
    , mkDescribeTags
    -- ** Request lenses
    , dtResourceArns

    -- * Destructuring the response
    , DescribeTagsResponse (..)
    , mkDescribeTagsResponse
    -- ** Response lenses
    , dtrrsResourceTags
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { resourceArns :: [Types.ResourceArn]
    -- ^ The Amazon Resource Names (ARNs) of the resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTags' value with any optional fields omitted.
mkDescribeTags
    :: DescribeTags
mkDescribeTags = DescribeTags'{resourceArns = Core.mempty}

-- | The Amazon Resource Names (ARNs) of the resources.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceArns :: Lens.Lens' DescribeTags [Types.ResourceArn]
dtResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE dtResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

instance Core.ToQuery DescribeTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTags where
        toHeaders DescribeTags{..}
          = Core.pure ("X-Amz-Target", "OvertureService.DescribeTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTags where
        toJSON DescribeTags{..}
          = Core.object
              (Core.catMaybes [Core.Just ("resourceArns" Core..= resourceArns)])

instance Core.AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' Core.<$>
                   (x Core..:? "resourceTags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { resourceTags :: Core.Maybe [Types.ResourceTag]
    -- ^ Information about the tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagsResponse' value with any optional fields omitted.
mkDescribeTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTagsResponse
mkDescribeTagsResponse responseStatus
  = DescribeTagsResponse'{resourceTags = Core.Nothing,
                          responseStatus}

-- | Information about the tags.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResourceTags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Types.ResourceTag])
dtrrsResourceTags = Lens.field @"resourceTags"
{-# INLINEABLE dtrrsResourceTags #-}
{-# DEPRECATED resourceTags "Use generic-lens or generic-optics with 'resourceTags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTagsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
