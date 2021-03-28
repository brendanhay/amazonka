{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing registry which is used to hold a collection of schemas. The updated properties relate to the registry, and do not modify any of the schemas within the registry. 
module Network.AWS.Glue.UpdateRegistry
    (
    -- * Creating a request
      UpdateRegistry (..)
    , mkUpdateRegistry
    -- ** Request lenses
    , urRegistryId
    , urDescription

    -- * Destructuring the response
    , UpdateRegistryResponse (..)
    , mkUpdateRegistryResponse
    -- ** Response lenses
    , urrrsRegistryArn
    , urrrsRegistryName
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { registryId :: Types.RegistryId
    -- ^ This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
  , description :: Types.DescriptionString
    -- ^ A description of the registry. If description is not provided, this field will not be updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegistry' value with any optional fields omitted.
mkUpdateRegistry
    :: Types.RegistryId -- ^ 'registryId'
    -> Types.DescriptionString -- ^ 'description'
    -> UpdateRegistry
mkUpdateRegistry registryId description
  = UpdateRegistry'{registryId, description}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRegistryId :: Lens.Lens' UpdateRegistry Types.RegistryId
urRegistryId = Lens.field @"registryId"
{-# INLINEABLE urRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | A description of the registry. If description is not provided, this field will not be updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRegistry Types.DescriptionString
urDescription = Lens.field @"description"
{-# INLINEABLE urDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateRegistry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRegistry where
        toHeaders UpdateRegistry{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateRegistry") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRegistry where
        toJSON UpdateRegistry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegistryId" Core..= registryId),
                  Core.Just ("Description" Core..= description)])

instance Core.AWSRequest UpdateRegistry where
        type Rs UpdateRegistry = UpdateRegistryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRegistryResponse' Core.<$>
                   (x Core..:? "RegistryArn") Core.<*> x Core..:? "RegistryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { registryArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource name (ARN) of the updated registry.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name of the updated registry.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegistryResponse' value with any optional fields omitted.
mkUpdateRegistryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRegistryResponse
mkUpdateRegistryResponse responseStatus
  = UpdateRegistryResponse'{registryArn = Core.Nothing,
                            registryName = Core.Nothing, responseStatus}

-- | The Amazon Resource name (ARN) of the updated registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsRegistryArn :: Lens.Lens' UpdateRegistryResponse (Core.Maybe Types.GlueResourceArn)
urrrsRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE urrrsRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

-- | The name of the updated registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsRegistryName :: Lens.Lens' UpdateRegistryResponse (Core.Maybe Types.RegistryName)
urrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE urrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRegistryResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
