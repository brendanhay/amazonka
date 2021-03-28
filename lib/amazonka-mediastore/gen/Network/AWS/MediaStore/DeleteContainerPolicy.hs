{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access policy that is associated with the specified container.
module Network.AWS.MediaStore.DeleteContainerPolicy
    (
    -- * Creating a request
      DeleteContainerPolicy (..)
    , mkDeleteContainerPolicy
    -- ** Request lenses
    , dcpfContainerName

    -- * Destructuring the response
    , DeleteContainerPolicyResponse (..)
    , mkDeleteContainerPolicyResponse
    -- ** Response lenses
    , dcprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerPolicy' smart constructor.
newtype DeleteContainerPolicy = DeleteContainerPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that holds the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerPolicy' value with any optional fields omitted.
mkDeleteContainerPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> DeleteContainerPolicy
mkDeleteContainerPolicy containerName
  = DeleteContainerPolicy'{containerName}

-- | The name of the container that holds the policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpfContainerName :: Lens.Lens' DeleteContainerPolicy Types.ContainerName
dcpfContainerName = Lens.field @"containerName"
{-# INLINEABLE dcpfContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery DeleteContainerPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteContainerPolicy where
        toHeaders DeleteContainerPolicy{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.DeleteContainerPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteContainerPolicy where
        toJSON DeleteContainerPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest DeleteContainerPolicy where
        type Rs DeleteContainerPolicy = DeleteContainerPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteContainerPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteContainerPolicyResponse' smart constructor.
newtype DeleteContainerPolicyResponse = DeleteContainerPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerPolicyResponse' value with any optional fields omitted.
mkDeleteContainerPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteContainerPolicyResponse
mkDeleteContainerPolicyResponse responseStatus
  = DeleteContainerPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprfrsResponseStatus :: Lens.Lens' DeleteContainerPolicyResponse Core.Int
dcprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
