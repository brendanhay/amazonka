{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetCorsPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
-- To use this operation, you must have permission to perform the @MediaStore:GetCorsPolicy@ action. By default, the container owner has this permission and can grant it to others.
module Network.AWS.MediaStore.GetCorsPolicy
    (
    -- * Creating a request
      GetCorsPolicy (..)
    , mkGetCorsPolicy
    -- ** Request lenses
    , gcpContainerName

    -- * Destructuring the response
    , GetCorsPolicyResponse (..)
    , mkGetCorsPolicyResponse
    -- ** Response lenses
    , gcprrsCorsPolicy
    , gcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCorsPolicy' smart constructor.
newtype GetCorsPolicy = GetCorsPolicy'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that the policy is assigned to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCorsPolicy' value with any optional fields omitted.
mkGetCorsPolicy
    :: Types.ContainerName -- ^ 'containerName'
    -> GetCorsPolicy
mkGetCorsPolicy containerName = GetCorsPolicy'{containerName}

-- | The name of the container that the policy is assigned to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpContainerName :: Lens.Lens' GetCorsPolicy Types.ContainerName
gcpContainerName = Lens.field @"containerName"
{-# INLINEABLE gcpContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery GetCorsPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCorsPolicy where
        toHeaders GetCorsPolicy{..}
          = Core.pure ("X-Amz-Target", "MediaStore_20170901.GetCorsPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCorsPolicy where
        toJSON GetCorsPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest GetCorsPolicy where
        type Rs GetCorsPolicy = GetCorsPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCorsPolicyResponse' Core.<$>
                   (x Core..: "CorsPolicy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCorsPolicyResponse' smart constructor.
data GetCorsPolicyResponse = GetCorsPolicyResponse'
  { corsPolicy :: Core.NonEmpty Types.CorsRule
    -- ^ The CORS policy assigned to the container.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCorsPolicyResponse' value with any optional fields omitted.
mkGetCorsPolicyResponse
    :: Core.NonEmpty Types.CorsRule -- ^ 'corsPolicy'
    -> Core.Int -- ^ 'responseStatus'
    -> GetCorsPolicyResponse
mkGetCorsPolicyResponse corsPolicy responseStatus
  = GetCorsPolicyResponse'{corsPolicy, responseStatus}

-- | The CORS policy assigned to the container.
--
-- /Note:/ Consider using 'corsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprrsCorsPolicy :: Lens.Lens' GetCorsPolicyResponse (Core.NonEmpty Types.CorsRule)
gcprrsCorsPolicy = Lens.field @"corsPolicy"
{-# INLINEABLE gcprrsCorsPolicy #-}
{-# DEPRECATED corsPolicy "Use generic-lens or generic-optics with 'corsPolicy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprrsResponseStatus :: Lens.Lens' GetCorsPolicyResponse Core.Int
gcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
