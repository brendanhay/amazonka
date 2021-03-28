{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device definition.
module Network.AWS.Greengrass.DeleteDeviceDefinition
    (
    -- * Creating a request
      DeleteDeviceDefinition (..)
    , mkDeleteDeviceDefinition
    -- ** Request lenses
    , dddDeviceDefinitionId

    -- * Destructuring the response
    , DeleteDeviceDefinitionResponse (..)
    , mkDeleteDeviceDefinitionResponse
    -- ** Response lenses
    , dddrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDeviceDefinition' smart constructor.
newtype DeleteDeviceDefinition = DeleteDeviceDefinition'
  { deviceDefinitionId :: Core.Text
    -- ^ The ID of the device definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceDefinition' value with any optional fields omitted.
mkDeleteDeviceDefinition
    :: Core.Text -- ^ 'deviceDefinitionId'
    -> DeleteDeviceDefinition
mkDeleteDeviceDefinition deviceDefinitionId
  = DeleteDeviceDefinition'{deviceDefinitionId}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDeviceDefinitionId :: Lens.Lens' DeleteDeviceDefinition Core.Text
dddDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# INLINEABLE dddDeviceDefinitionId #-}
{-# DEPRECATED deviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead"  #-}

instance Core.ToQuery DeleteDeviceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDeviceDefinition where
        toHeaders DeleteDeviceDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteDeviceDefinition where
        type Rs DeleteDeviceDefinition = DeleteDeviceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/definition/devices/" Core.<>
                             Core.toText deviceDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDeviceDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDeviceDefinitionResponse' smart constructor.
newtype DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceDefinitionResponse' value with any optional fields omitted.
mkDeleteDeviceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDeviceDefinitionResponse
mkDeleteDeviceDefinitionResponse responseStatus
  = DeleteDeviceDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrrsResponseStatus :: Lens.Lens' DeleteDeviceDefinitionResponse Core.Int
dddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
