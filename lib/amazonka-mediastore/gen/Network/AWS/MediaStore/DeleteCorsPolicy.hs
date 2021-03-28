{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteCorsPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
-- To use this operation, you must have permission to perform the @MediaStore:DeleteCorsPolicy@ action. The container owner has this permission by default and can grant this permission to others.
module Network.AWS.MediaStore.DeleteCorsPolicy
    (
    -- * Creating a request
      DeleteCorsPolicy (..)
    , mkDeleteCorsPolicy
    -- ** Request lenses
    , dcpContainerName

    -- * Destructuring the response
    , DeleteCorsPolicyResponse (..)
    , mkDeleteCorsPolicyResponse
    -- ** Response lenses
    , dcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCorsPolicy' smart constructor.
newtype DeleteCorsPolicy = DeleteCorsPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container to remove the policy from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCorsPolicy' value with any optional fields omitted.
mkDeleteCorsPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> DeleteCorsPolicy
mkDeleteCorsPolicy containerName = DeleteCorsPolicy'{containerName}

-- | The name of the container to remove the policy from.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpContainerName :: Lens.Lens' DeleteCorsPolicy Types.ContainerName
dcpContainerName = Lens.field @"containerName"
{-# INLINEABLE dcpContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery DeleteCorsPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCorsPolicy where
        toHeaders DeleteCorsPolicy{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.DeleteCorsPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCorsPolicy where
        toJSON DeleteCorsPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest DeleteCorsPolicy where
        type Rs DeleteCorsPolicy = DeleteCorsPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCorsPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCorsPolicyResponse' smart constructor.
newtype DeleteCorsPolicyResponse = DeleteCorsPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCorsPolicyResponse' value with any optional fields omitted.
mkDeleteCorsPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCorsPolicyResponse
mkDeleteCorsPolicyResponse responseStatus
  = DeleteCorsPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DeleteCorsPolicyResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
