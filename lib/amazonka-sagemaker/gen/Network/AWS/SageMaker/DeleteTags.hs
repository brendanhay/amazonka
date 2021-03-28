{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from an Amazon SageMaker resource.
--
-- To list a resource's tags, use the @ListTags@ API. 
module Network.AWS.SageMaker.DeleteTags
    (
    -- * Creating a request
      DeleteTags (..)
    , mkDeleteTags
    -- ** Request lenses
    , dtResourceArn
    , dtTagKeys

    -- * Destructuring the response
    , DeleteTagsResponse (..)
    , mkDeleteTagsResponse
    -- ** Response lenses
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
  , tagKeys :: Core.NonEmpty Types.TagKey
    -- ^ An array or one or more tag keys to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> Core.NonEmpty Types.TagKey -- ^ 'tagKeys'
    -> DeleteTags
mkDeleteTags resourceArn tagKeys
  = DeleteTags'{resourceArn, tagKeys}

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to delete.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceArn :: Lens.Lens' DeleteTags Types.ResourceArn
dtResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE dtResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | An array or one or more tag keys to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DeleteTags (Core.NonEmpty Types.TagKey)
dtTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dtTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery DeleteTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTags where
        toHeaders DeleteTags{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTags where
        toJSON DeleteTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTagsResponse
mkDeleteTagsResponse responseStatus
  = DeleteTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeleteTagsResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
