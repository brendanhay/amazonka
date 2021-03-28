{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.DeleteVPCEConfiguration
    (
    -- * Creating a request
      DeleteVPCEConfiguration (..)
    , mkDeleteVPCEConfiguration
    -- ** Request lenses
    , dvpcecArn

    -- * Destructuring the response
    , DeleteVPCEConfigurationResponse (..)
    , mkDeleteVPCEConfigurationResponse
    -- ** Response lenses
    , dvpcecrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVPCEConfiguration' smart constructor.
newtype DeleteVPCEConfiguration = DeleteVPCEConfiguration'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCEConfiguration' value with any optional fields omitted.
mkDeleteVPCEConfiguration
    :: Types.Arn -- ^ 'arn'
    -> DeleteVPCEConfiguration
mkDeleteVPCEConfiguration arn = DeleteVPCEConfiguration'{arn}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecArn :: Lens.Lens' DeleteVPCEConfiguration Types.Arn
dvpcecArn = Lens.field @"arn"
{-# INLINEABLE dvpcecArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteVPCEConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVPCEConfiguration where
        toHeaders DeleteVPCEConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.DeleteVPCEConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteVPCEConfiguration where
        toJSON DeleteVPCEConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteVPCEConfiguration where
        type Rs DeleteVPCEConfiguration = DeleteVPCEConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteVPCEConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVPCEConfigurationResponse' smart constructor.
newtype DeleteVPCEConfigurationResponse = DeleteVPCEConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCEConfigurationResponse' value with any optional fields omitted.
mkDeleteVPCEConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVPCEConfigurationResponse
mkDeleteVPCEConfigurationResponse responseStatus
  = DeleteVPCEConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecrrsResponseStatus :: Lens.Lens' DeleteVPCEConfigurationResponse Core.Int
dvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
