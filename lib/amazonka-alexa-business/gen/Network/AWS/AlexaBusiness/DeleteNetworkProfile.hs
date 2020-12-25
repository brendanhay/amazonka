{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.DeleteNetworkProfile
  ( -- * Creating a request
    DeleteNetworkProfile (..),
    mkDeleteNetworkProfile,

    -- ** Request lenses
    dnpNetworkProfileArn,

    -- * Destructuring the response
    DeleteNetworkProfileResponse (..),
    mkDeleteNetworkProfileResponse,

    -- ** Response lenses
    dnprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNetworkProfile' smart constructor.
newtype DeleteNetworkProfile = DeleteNetworkProfile'
  { -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkProfile' value with any optional fields omitted.
mkDeleteNetworkProfile ::
  -- | 'networkProfileArn'
  Types.Arn ->
  DeleteNetworkProfile
mkDeleteNetworkProfile networkProfileArn =
  DeleteNetworkProfile' {networkProfileArn}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpNetworkProfileArn :: Lens.Lens' DeleteNetworkProfile Types.Arn
dnpNetworkProfileArn = Lens.field @"networkProfileArn"
{-# DEPRECATED dnpNetworkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead." #-}

instance Core.FromJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NetworkProfileArn" Core..= networkProfileArn)]
      )

instance Core.AWSRequest DeleteNetworkProfile where
  type Rs DeleteNetworkProfile = DeleteNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteNetworkProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteNetworkProfileResponse' smart constructor.
newtype DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkProfileResponse' value with any optional fields omitted.
mkDeleteNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNetworkProfileResponse
mkDeleteNetworkProfileResponse responseStatus =
  DeleteNetworkProfileResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnprrsResponseStatus :: Lens.Lens' DeleteNetworkProfileResponse Core.Int
dnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
