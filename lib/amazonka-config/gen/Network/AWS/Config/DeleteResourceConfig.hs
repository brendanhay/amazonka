{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for a custom resource that has been deleted. This API records a new ConfigurationItem with a ResourceDeleted status. You can retrieve the ConfigurationItems recorded for this resource in your AWS Config History. 
module Network.AWS.Config.DeleteResourceConfig
    (
    -- * Creating a request
      DeleteResourceConfig (..)
    , mkDeleteResourceConfig
    -- ** Request lenses
    , drcResourceType
    , drcResourceId

    -- * Destructuring the response
    , DeleteResourceConfigResponse (..)
    , mkDeleteResourceConfigResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourceConfig' smart constructor.
data DeleteResourceConfig = DeleteResourceConfig'
  { resourceType :: Types.ResourceTypeString
    -- ^ The type of the resource.
  , resourceId :: Types.ResourceId
    -- ^ Unique identifier of the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceConfig' value with any optional fields omitted.
mkDeleteResourceConfig
    :: Types.ResourceTypeString -- ^ 'resourceType'
    -> Types.ResourceId -- ^ 'resourceId'
    -> DeleteResourceConfig
mkDeleteResourceConfig resourceType resourceId
  = DeleteResourceConfig'{resourceType, resourceId}

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcResourceType :: Lens.Lens' DeleteResourceConfig Types.ResourceTypeString
drcResourceType = Lens.field @"resourceType"
{-# INLINEABLE drcResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Unique identifier of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcResourceId :: Lens.Lens' DeleteResourceConfig Types.ResourceId
drcResourceId = Lens.field @"resourceId"
{-# INLINEABLE drcResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery DeleteResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResourceConfig where
        toHeaders DeleteResourceConfig{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DeleteResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteResourceConfig where
        toJSON DeleteResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceType" Core..= resourceType),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest DeleteResourceConfig where
        type Rs DeleteResourceConfig = DeleteResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteResourceConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceConfigResponse' smart constructor.
data DeleteResourceConfigResponse = DeleteResourceConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceConfigResponse' value with any optional fields omitted.
mkDeleteResourceConfigResponse
    :: DeleteResourceConfigResponse
mkDeleteResourceConfigResponse = DeleteResourceConfigResponse'
