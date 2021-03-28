{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource from a list of tags. The resource is identified by the @ResourceArn@ input parameter. The tags are identified by the list of keys in the @TagKeys@ input parameter. 
module Network.AWS.CodeDeploy.UntagResource
    (
    -- * Creating a request
      UntagResource (..)
    , mkUntagResource
    -- ** Request lenses
    , urResourceArn
    , urTagKeys

    -- * Destructuring the response
    , UntagResourceResponse (..)
    , mkUntagResourceResponse
    -- ** Response lenses
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { resourceArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that specifies from which resource to disassociate the tags with the keys in the @TagKeys@ input parameter. 
  , tagKeys :: [Types.Key]
    -- ^ A list of keys of @Tag@ objects. The @Tag@ objects identified by the keys are disassociated from the resource specified by the @ResourceArn@ input parameter. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource
    :: Types.Arn -- ^ 'resourceArn'
    -> UntagResource
mkUntagResource resourceArn
  = UntagResource'{resourceArn, tagKeys = Core.mempty}

-- | The Amazon Resource Name (ARN) that specifies from which resource to disassociate the tags with the keys in the @TagKeys@ input parameter. 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceArn :: Lens.Lens' UntagResource Types.Arn
urResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE urResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | A list of keys of @Tag@ objects. The @Tag@ objects identified by the keys are disassociated from the resource specified by the @ResourceArn@ input parameter. 
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Types.Key]
urTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE urTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UntagResource where
        toHeaders UntagResource{..}
          = Core.pure ("X-Amz-Target", "CodeDeploy_20141006.UntagResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UntagResource where
        toJSON UntagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UntagResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourceResponse' value with any optional fields omitted.
mkUntagResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UntagResourceResponse
mkUntagResourceResponse responseStatus
  = UntagResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UntagResourceResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
