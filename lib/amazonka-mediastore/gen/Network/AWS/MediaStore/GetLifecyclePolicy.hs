{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Network.AWS.MediaStore.GetLifecyclePolicy
    (
    -- * Creating a request
      GetLifecyclePolicy (..)
    , mkGetLifecyclePolicy
    -- ** Request lenses
    , glpContainerName

    -- * Destructuring the response
    , GetLifecyclePolicyResponse (..)
    , mkGetLifecyclePolicyResponse
    -- ** Response lenses
    , glprrsLifecyclePolicy
    , glprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
newtype GetLifecyclePolicy = GetLifecyclePolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that the object lifecycle policy is assigned to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicy' value with any optional fields omitted.
mkGetLifecyclePolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> GetLifecyclePolicy
mkGetLifecyclePolicy containerName
  = GetLifecyclePolicy'{containerName}

-- | The name of the container that the object lifecycle policy is assigned to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpContainerName :: Lens.Lens' GetLifecyclePolicy Types.ContainerName
glpContainerName = Lens.field @"containerName"
{-# INLINEABLE glpContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery GetLifecyclePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLifecyclePolicy where
        toHeaders GetLifecyclePolicy{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.GetLifecyclePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLifecyclePolicy where
        toJSON GetLifecyclePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest GetLifecyclePolicy where
        type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyResponse' Core.<$>
                   (x Core..: "LifecyclePolicy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { lifecyclePolicy :: Types.LifecyclePolicy
    -- ^ The object lifecycle policy that is assigned to the container.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicyResponse' value with any optional fields omitted.
mkGetLifecyclePolicyResponse
    :: Types.LifecyclePolicy -- ^ 'lifecyclePolicy'
    -> Core.Int -- ^ 'responseStatus'
    -> GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse lifecyclePolicy responseStatus
  = GetLifecyclePolicyResponse'{lifecyclePolicy, responseStatus}

-- | The object lifecycle policy that is assigned to the container.
--
-- /Note:/ Consider using 'lifecyclePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLifecyclePolicy :: Lens.Lens' GetLifecyclePolicyResponse Types.LifecyclePolicy
glprrsLifecyclePolicy = Lens.field @"lifecyclePolicy"
{-# INLINEABLE glprrsLifecyclePolicy #-}
{-# DEPRECATED lifecyclePolicy "Use generic-lens or generic-optics with 'lifecyclePolicy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
