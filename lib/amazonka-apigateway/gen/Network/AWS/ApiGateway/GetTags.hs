{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'Tags' collection for a given resource.
module Network.AWS.ApiGateway.GetTags
    (
    -- * Creating a request
      GetTags (..)
    , mkGetTags
    -- ** Request lenses
    , gtResourceArn
    , gtLimit
    , gtPosition

    -- * Destructuring the response
    , GetTagsResponse (..)
    , mkGetTagsResponse
    -- ** Response lenses
    , gtrrsTags
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the 'Tags' collection for a given resource.
--
-- /See:/ 'mkGetTags' smart constructor.
data GetTags = GetTags'
  { resourceArn :: Core.Text
    -- ^ [Required] The ARN of a resource that can be tagged.
  , limit :: Core.Maybe Core.Int
    -- ^ (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ (Not currently supported) The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTags' value with any optional fields omitted.
mkGetTags
    :: Core.Text -- ^ 'resourceArn'
    -> GetTags
mkGetTags resourceArn
  = GetTags'{resourceArn, limit = Core.Nothing,
             position = Core.Nothing}

-- | [Required] The ARN of a resource that can be tagged.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtResourceArn :: Lens.Lens' GetTags Core.Text
gtResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE gtResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtLimit :: Lens.Lens' GetTags (Core.Maybe Core.Int)
gtLimit = Lens.field @"limit"
{-# INLINEABLE gtLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | (Not currently supported) The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtPosition :: Lens.Lens' GetTags (Core.Maybe Core.Text)
gtPosition = Lens.field @"position"
{-# INLINEABLE gtPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetTags where
        toQuery GetTags{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetTags where
        toHeaders GetTags{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetTags where
        type Rs GetTags = GetTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/tags/" Core.<> Core.toText resourceArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTagsResponse' Core.<$>
                   (x Core..:? "tags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagsResponse' value with any optional fields omitted.
mkGetTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTagsResponse
mkGetTagsResponse responseStatus
  = GetTagsResponse'{tags = Core.Nothing, responseStatus}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTags :: Lens.Lens' GetTagsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gtrrsTags = Lens.field @"tags"
{-# INLINEABLE gtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTagsResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
