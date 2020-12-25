{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@ . The @smbguest@ user is the user when the authentication method for the file share is set to @GuestAccess@ .
module Network.AWS.StorageGateway.SetSMBGuestPassword
  ( -- * Creating a request
    SetSMBGuestPassword (..),
    mkSetSMBGuestPassword,

    -- ** Request lenses
    ssmbgpGatewayARN,
    ssmbgpPassword,

    -- * Destructuring the response
    SetSMBGuestPasswordResponse (..),
    mkSetSMBGuestPasswordResponse,

    -- ** Response lenses
    ssmbgprrsGatewayARN,
    ssmbgprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'mkSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { -- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
    gatewayARN :: Types.GatewayARN,
    -- | The password that you want to set for your SMB server.
    password :: Types.Password
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMBGuestPassword' value with any optional fields omitted.
mkSetSMBGuestPassword ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'password'
  Types.Password ->
  SetSMBGuestPassword
mkSetSMBGuestPassword gatewayARN password =
  SetSMBGuestPassword' {gatewayARN, password}

-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpGatewayARN :: Lens.Lens' SetSMBGuestPassword Types.GatewayARN
ssmbgpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ssmbgpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The password that you want to set for your SMB server.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpPassword :: Lens.Lens' SetSMBGuestPassword Types.Password
ssmbgpPassword = Lens.field @"password"
{-# DEPRECATED ssmbgpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Core.FromJSON SetSMBGuestPassword where
  toJSON SetSMBGuestPassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.AWSRequest SetSMBGuestPassword where
  type Rs SetSMBGuestPassword = SetSMBGuestPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.SetSMBGuestPassword")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SetSMBGuestPasswordResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMBGuestPasswordResponse' value with any optional fields omitted.
mkSetSMBGuestPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetSMBGuestPasswordResponse
mkSetSMBGuestPasswordResponse responseStatus =
  SetSMBGuestPasswordResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprrsGatewayARN :: Lens.Lens' SetSMBGuestPasswordResponse (Core.Maybe Types.GatewayARN)
ssmbgprrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ssmbgprrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprrsResponseStatus :: Lens.Lens' SetSMBGuestPasswordResponse Core.Int
ssmbgprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssmbgprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
