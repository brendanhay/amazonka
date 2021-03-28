{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
module Network.AWS.MediaStore.GetContainerPolicy
    (
    -- * Creating a request
      GetContainerPolicy (..)
    , mkGetContainerPolicy
    -- ** Request lenses
    , gContainerName

    -- * Destructuring the response
    , GetContainerPolicyResponse (..)
    , mkGetContainerPolicyResponse
    -- ** Response lenses
    , grsPolicy
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerPolicy' smart constructor.
newtype GetContainerPolicy = GetContainerPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerPolicy' value with any optional fields omitted.
mkGetContainerPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> GetContainerPolicy
mkGetContainerPolicy containerName
  = GetContainerPolicy'{containerName}

-- | The name of the container. 
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gContainerName :: Lens.Lens' GetContainerPolicy Types.ContainerName
gContainerName = Lens.field @"containerName"
{-# INLINEABLE gContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery GetContainerPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContainerPolicy where
        toHeaders GetContainerPolicy{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.GetContainerPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContainerPolicy where
        toJSON GetContainerPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest GetContainerPolicy where
        type Rs GetContainerPolicy = GetContainerPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContainerPolicyResponse' Core.<$>
                   (x Core..: "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContainerPolicyResponse' smart constructor.
data GetContainerPolicyResponse = GetContainerPolicyResponse'
  { policy :: Types.ContainerPolicy
    -- ^ The contents of the access policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerPolicyResponse' value with any optional fields omitted.
mkGetContainerPolicyResponse
    :: Types.ContainerPolicy -- ^ 'policy'
    -> Core.Int -- ^ 'responseStatus'
    -> GetContainerPolicyResponse
mkGetContainerPolicyResponse policy responseStatus
  = GetContainerPolicyResponse'{policy, responseStatus}

-- | The contents of the access policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPolicy :: Lens.Lens' GetContainerPolicyResponse Types.ContainerPolicy
grsPolicy = Lens.field @"policy"
{-# INLINEABLE grsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetContainerPolicyResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
