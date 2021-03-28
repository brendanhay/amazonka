{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutCorsPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cross-origin resource sharing (CORS) configuration on a container so that the container can service cross-origin requests. For example, you might want to enable a request whose origin is http://www.example.com to access your AWS Elemental MediaStore container at my.example.container.com by using the browser's XMLHttpRequest capability.
--
-- To enable CORS on a container, you attach a CORS policy to the container. In the CORS policy, you configure rules that identify origins and the HTTP methods that can be executed on your container. The policy can contain up to 398,000 characters. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
-- To learn more about CORS, see <https://docs.aws.amazon.com/mediastore/latest/ug/cors-policy.html Cross-Origin Resource Sharing (CORS) in AWS Elemental MediaStore> .
module Network.AWS.MediaStore.PutCorsPolicy
    (
    -- * Creating a request
      PutCorsPolicy (..)
    , mkPutCorsPolicy
    -- ** Request lenses
    , pcpContainerName
    , pcpCorsPolicy

    -- * Destructuring the response
    , PutCorsPolicyResponse (..)
    , mkPutCorsPolicyResponse
    -- ** Response lenses
    , pcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutCorsPolicy' smart constructor.
data PutCorsPolicy = PutCorsPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that you want to assign the CORS policy to.
  , corsPolicy :: Core.NonEmpty Types.CorsRule
    -- ^ The CORS policy to apply to the container. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutCorsPolicy' value with any optional fields omitted.
mkPutCorsPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> Core.NonEmpty Types.CorsRule -- ^ 'corsPolicy'
    -> PutCorsPolicy
mkPutCorsPolicy containerName corsPolicy
  = PutCorsPolicy'{containerName, corsPolicy}

-- | The name of the container that you want to assign the CORS policy to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpContainerName :: Lens.Lens' PutCorsPolicy Types.ContainerName
pcpContainerName = Lens.field @"containerName"
{-# INLINEABLE pcpContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

-- | The CORS policy to apply to the container. 
--
-- /Note:/ Consider using 'corsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpCorsPolicy :: Lens.Lens' PutCorsPolicy (Core.NonEmpty Types.CorsRule)
pcpCorsPolicy = Lens.field @"corsPolicy"
{-# INLINEABLE pcpCorsPolicy #-}
{-# DEPRECATED corsPolicy "Use generic-lens or generic-optics with 'corsPolicy' instead"  #-}

instance Core.ToQuery PutCorsPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutCorsPolicy where
        toHeaders PutCorsPolicy{..}
          = Core.pure ("X-Amz-Target", "MediaStore_20170901.PutCorsPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutCorsPolicy where
        toJSON PutCorsPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName),
                  Core.Just ("CorsPolicy" Core..= corsPolicy)])

instance Core.AWSRequest PutCorsPolicy where
        type Rs PutCorsPolicy = PutCorsPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutCorsPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutCorsPolicyResponse' smart constructor.
newtype PutCorsPolicyResponse = PutCorsPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutCorsPolicyResponse' value with any optional fields omitted.
mkPutCorsPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutCorsPolicyResponse
mkPutCorsPolicyResponse responseStatus
  = PutCorsPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsResponseStatus :: Lens.Lens' PutCorsPolicyResponse Core.Int
pcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
