{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Network.AWS.Greengrass.UpdateDeviceDefinition
    (
    -- * Creating a request
      UpdateDeviceDefinition (..)
    , mkUpdateDeviceDefinition
    -- ** Request lenses
    , uddDeviceDefinitionId
    , uddName

    -- * Destructuring the response
    , UpdateDeviceDefinitionResponse (..)
    , mkUpdateDeviceDefinitionResponse
    -- ** Response lenses
    , uddrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { deviceDefinitionId :: Core.Text
    -- ^ The ID of the device definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceDefinition' value with any optional fields omitted.
mkUpdateDeviceDefinition
    :: Core.Text -- ^ 'deviceDefinitionId'
    -> UpdateDeviceDefinition
mkUpdateDeviceDefinition deviceDefinitionId
  = UpdateDeviceDefinition'{deviceDefinitionId, name = Core.Nothing}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddDeviceDefinitionId :: Lens.Lens' UpdateDeviceDefinition Core.Text
uddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# INLINEABLE uddDeviceDefinitionId #-}
{-# DEPRECATED deviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddName :: Lens.Lens' UpdateDeviceDefinition (Core.Maybe Core.Text)
uddName = Lens.field @"name"
{-# INLINEABLE uddName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateDeviceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDeviceDefinition where
        toHeaders UpdateDeviceDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDeviceDefinition where
        toJSON UpdateDeviceDefinition{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateDeviceDefinition where
        type Rs UpdateDeviceDefinition = UpdateDeviceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/definition/devices/" Core.<>
                             Core.toText deviceDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateDeviceDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDeviceDefinitionResponse' smart constructor.
newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceDefinitionResponse' value with any optional fields omitted.
mkUpdateDeviceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDeviceDefinitionResponse
mkUpdateDeviceDefinitionResponse responseStatus
  = UpdateDeviceDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddrrsResponseStatus :: Lens.Lens' UpdateDeviceDefinitionResponse Core.Int
uddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
