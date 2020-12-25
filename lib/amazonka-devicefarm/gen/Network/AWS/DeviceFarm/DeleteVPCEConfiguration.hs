{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteVPCEConfiguration (..),
    mkDeleteVPCEConfiguration,

    -- ** Request lenses
    dvpcecArn,

    -- * Destructuring the response
    DeleteVPCEConfigurationResponse (..),
    mkDeleteVPCEConfigurationResponse,

    -- ** Response lenses
    dvpcecrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVPCEConfiguration' smart constructor.
newtype DeleteVPCEConfiguration = DeleteVPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCEConfiguration' value with any optional fields omitted.
mkDeleteVPCEConfiguration ::
  -- | 'arn'
  Types.Arn ->
  DeleteVPCEConfiguration
mkDeleteVPCEConfiguration arn = DeleteVPCEConfiguration' {arn}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecArn :: Lens.Lens' DeleteVPCEConfiguration Types.Arn
dvpcecArn = Lens.field @"arn"
{-# DEPRECATED dvpcecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON DeleteVPCEConfiguration where
  toJSON DeleteVPCEConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteVPCEConfiguration where
  type Rs DeleteVPCEConfiguration = DeleteVPCEConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.DeleteVPCEConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVPCEConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteVPCEConfigurationResponse' smart constructor.
newtype DeleteVPCEConfigurationResponse = DeleteVPCEConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCEConfigurationResponse' value with any optional fields omitted.
mkDeleteVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteVPCEConfigurationResponse
mkDeleteVPCEConfigurationResponse responseStatus =
  DeleteVPCEConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecrrsResponseStatus :: Lens.Lens' DeleteVPCEConfigurationResponse Core.Int
dvpcecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvpcecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
