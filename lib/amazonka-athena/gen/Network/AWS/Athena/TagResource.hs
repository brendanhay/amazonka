{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an Athena resource. A tag is a label that you assign to a resource. In Athena, a resource can be a workgroup or data catalog. Each tag consists of a key and an optional value, both of which you define. For example, you can use tags to categorize Athena workgroups or data catalogs by purpose, owner, or environment. Use a consistent set of tag keys to make it easier to search and filter workgroups or data catalogs in your account. For best practices, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ Tagging Best Practices> . Tag keys can be from 1 to 128 UTF-8 Unicode characters, and tag values can be from 0 to 256 UTF-8 Unicode characters. Tags can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag keys and values are case-sensitive. Tag keys must be unique per resource. If you specify more than one tag, separate them by commas.
module Network.AWS.Athena.TagResource
    (
    -- * Creating a request
      TagResource (..)
    , mkTagResource
    -- ** Request lenses
    , trResourceARN
    , trTags

    -- * Destructuring the response
    , TagResourceResponse (..)
    , mkTagResourceResponse
    -- ** Response lenses
    , trrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { resourceARN :: Types.AmazonResourceName
    -- ^ Specifies the ARN of the Athena resource (workgroup or data catalog) to which tags are to be added.
  , tags :: [Types.Tag]
    -- ^ A collection of one or more tags, separated by commas, to be added to an Athena workgroup or data catalog resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource
    :: Types.AmazonResourceName -- ^ 'resourceARN'
    -> TagResource
mkTagResource resourceARN
  = TagResource'{resourceARN, tags = Core.mempty}

-- | Specifies the ARN of the Athena resource (workgroup or data catalog) to which tags are to be added.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARN :: Lens.Lens' TagResource Types.AmazonResourceName
trResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE trResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | A collection of one or more tags, separated by commas, to be added to an Athena workgroup or data catalog resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Types.Tag]
trTags = Lens.field @"tags"
{-# INLINEABLE trTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagResource where
        toHeaders TagResource{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.TagResource") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagResource where
        toJSON TagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 TagResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TagResourceResponse
mkTagResourceResponse responseStatus
  = TagResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsResponseStatus :: Lens.Lens' TagResourceResponse Core.Int
trrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE trrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
