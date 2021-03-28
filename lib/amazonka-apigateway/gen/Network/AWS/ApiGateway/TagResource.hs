{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a tag on a given resource.
module Network.AWS.ApiGateway.TagResource
    (
    -- * Creating a request
      TagResource (..)
    , mkTagResource
    -- ** Request lenses
    , trResourceArn
    , trTags

    -- * Destructuring the response
    , TagResourceResponse (..)
    , mkTagResourceResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Adds or updates a tag on a given resource.
--
-- /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { resourceArn :: Core.Text
    -- ^ [Required] The ARN of a resource that can be tagged.
  , tags :: Core.HashMap Core.Text Core.Text
    -- ^ [Required] The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource
    :: Core.Text -- ^ 'resourceArn'
    -> TagResource
mkTagResource resourceArn
  = TagResource'{resourceArn, tags = Core.mempty}

-- | [Required] The ARN of a resource that can be tagged.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceArn :: Lens.Lens' TagResource Core.Text
trResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE trResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | [Required] The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource (Core.HashMap Core.Text Core.Text)
trTags = Lens.field @"tags"
{-# INLINEABLE trTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagResource where
        toHeaders TagResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON TagResource where
        toJSON TagResource{..}
          = Core.object (Core.catMaybes [Core.Just ("tags" Core..= tags)])

instance Core.AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/tags/" Core.<> Core.toText resourceArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull TagResourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse
    :: TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
