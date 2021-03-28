{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified registry in detail.
module Network.AWS.Glue.GetRegistry
    (
    -- * Creating a request
      GetRegistry (..)
    , mkGetRegistry
    -- ** Request lenses
    , grRegistryId

    -- * Destructuring the response
    , GetRegistryResponse (..)
    , mkGetRegistryResponse
    -- ** Response lenses
    , grrrsCreatedTime
    , grrrsDescription
    , grrrsRegistryArn
    , grrrsRegistryName
    , grrrsStatus
    , grrrsUpdatedTime
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRegistry' smart constructor.
newtype GetRegistry = GetRegistry'
  { registryId :: Types.RegistryId
    -- ^ This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegistry' value with any optional fields omitted.
mkGetRegistry
    :: Types.RegistryId -- ^ 'registryId'
    -> GetRegistry
mkGetRegistry registryId = GetRegistry'{registryId}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRegistryId :: Lens.Lens' GetRegistry Types.RegistryId
grRegistryId = Lens.field @"registryId"
{-# INLINEABLE grRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery GetRegistry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRegistry where
        toHeaders GetRegistry{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetRegistry") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRegistry where
        toJSON GetRegistry{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RegistryId" Core..= registryId)])

instance Core.AWSRequest GetRegistry where
        type Rs GetRegistry = GetRegistryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRegistryResponse' Core.<$>
                   (x Core..:? "CreatedTime") Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "RegistryArn"
                     Core.<*> x Core..:? "RegistryName"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "UpdatedTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRegistryResponse' smart constructor.
data GetRegistryResponse = GetRegistryResponse'
  { createdTime :: Core.Maybe Types.CreatedTime
    -- ^ The date and time the registry was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the registry.
  , registryArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the registry.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name of the registry.
  , status :: Core.Maybe Types.RegistryStatus
    -- ^ The status of the registry.
  , updatedTime :: Core.Maybe Types.UpdatedTime
    -- ^ The date and time the registry was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegistryResponse' value with any optional fields omitted.
mkGetRegistryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRegistryResponse
mkGetRegistryResponse responseStatus
  = GetRegistryResponse'{createdTime = Core.Nothing,
                         description = Core.Nothing, registryArn = Core.Nothing,
                         registryName = Core.Nothing, status = Core.Nothing,
                         updatedTime = Core.Nothing, responseStatus}

-- | The date and time the registry was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsCreatedTime :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.CreatedTime)
grrrsCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE grrrsCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | A description of the registry.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsDescription :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.Description)
grrrsDescription = Lens.field @"description"
{-# INLINEABLE grrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRegistryArn :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.GlueResourceArn)
grrrsRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE grrrsRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRegistryName :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.RegistryName)
grrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE grrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The status of the registry.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsStatus :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.RegistryStatus)
grrrsStatus = Lens.field @"status"
{-# INLINEABLE grrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time the registry was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsUpdatedTime :: Lens.Lens' GetRegistryResponse (Core.Maybe Types.UpdatedTime)
grrrsUpdatedTime = Lens.field @"updatedTime"
{-# INLINEABLE grrrsUpdatedTime #-}
{-# DEPRECATED updatedTime "Use generic-lens or generic-optics with 'updatedTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRegistryResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
