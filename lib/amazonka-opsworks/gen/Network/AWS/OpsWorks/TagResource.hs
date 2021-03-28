{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply cost-allocation tags to a specified stack or layer in AWS OpsWorks Stacks. For more information about how tagging works, see <https://docs.aws.amazon.com/opsworks/latest/userguide/tagging.html Tags> in the AWS OpsWorks User Guide.
module Network.AWS.OpsWorks.TagResource
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { resourceArn :: Types.ResourceArn
    -- ^ The stack or layer's Amazon Resource Number (ARN).
  , tags :: Core.HashMap Types.TagKey Types.TagValue
    -- ^ A map that contains tag keys and tag values that are attached to a stack or layer.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@ 
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@ 
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 40 tags is allowed for any resource.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> TagResource
mkTagResource resourceArn
  = TagResource'{resourceArn, tags = Core.mempty}

-- | The stack or layer's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceArn :: Lens.Lens' TagResource Types.ResourceArn
trResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE trResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | A map that contains tag keys and tag values that are attached to a stack or layer.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@ 
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@ 
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 40 tags is allowed for any resource.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource (Core.HashMap Types.TagKey Types.TagValue)
trTags = Lens.field @"tags"
{-# INLINEABLE trTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagResource where
        toHeaders TagResource{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.TagResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagResource where
        toJSON TagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
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
