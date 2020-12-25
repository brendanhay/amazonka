{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
  ( -- * Creating a request
    AdminUpdateDeviceStatus (..),
    mkAdminUpdateDeviceStatus,

    -- ** Request lenses
    audsUserPoolId,
    audsUsername,
    audsDeviceKey,
    audsDeviceRememberedStatus,

    -- * Destructuring the response
    AdminUpdateDeviceStatusResponse (..),
    mkAdminUpdateDeviceStatusResponse,

    -- ** Response lenses
    audsrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update the device status, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The user name.
    username :: Types.Username,
    -- | The device key.
    deviceKey :: Types.DeviceKeyType,
    -- | The status indicating whether a device has been remembered or not.
    deviceRememberedStatus :: Core.Maybe Types.DeviceRememberedStatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateDeviceStatus' value with any optional fields omitted.
mkAdminUpdateDeviceStatus ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  -- | 'deviceKey'
  Types.DeviceKeyType ->
  AdminUpdateDeviceStatus
mkAdminUpdateDeviceStatus userPoolId username deviceKey =
  AdminUpdateDeviceStatus'
    { userPoolId,
      username,
      deviceKey,
      deviceRememberedStatus = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUserPoolId :: Lens.Lens' AdminUpdateDeviceStatus Types.UserPoolId
audsUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED audsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsUsername :: Lens.Lens' AdminUpdateDeviceStatus Types.Username
audsUsername = Lens.field @"username"
{-# DEPRECATED audsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceKey :: Lens.Lens' AdminUpdateDeviceStatus Types.DeviceKeyType
audsDeviceKey = Lens.field @"deviceKey"
{-# DEPRECATED audsDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

-- | The status indicating whether a device has been remembered or not.
--
-- /Note:/ Consider using 'deviceRememberedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsDeviceRememberedStatus :: Lens.Lens' AdminUpdateDeviceStatus (Core.Maybe Types.DeviceRememberedStatusType)
audsDeviceRememberedStatus = Lens.field @"deviceRememberedStatus"
{-# DEPRECATED audsDeviceRememberedStatus "Use generic-lens or generic-optics with 'deviceRememberedStatus' instead." #-}

instance Core.FromJSON AdminUpdateDeviceStatus where
  toJSON AdminUpdateDeviceStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("DeviceKey" Core..= deviceKey),
            ("DeviceRememberedStatus" Core..=)
              Core.<$> deviceRememberedStatus
          ]
      )

instance Core.AWSRequest AdminUpdateDeviceStatus where
  type Rs AdminUpdateDeviceStatus = AdminUpdateDeviceStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateDeviceStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The status response from the request to update the device, as an administrator.
--
-- /See:/ 'mkAdminUpdateDeviceStatusResponse' smart constructor.
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateDeviceStatusResponse' value with any optional fields omitted.
mkAdminUpdateDeviceStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminUpdateDeviceStatusResponse
mkAdminUpdateDeviceStatusResponse responseStatus =
  AdminUpdateDeviceStatusResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
audsrrsResponseStatus :: Lens.Lens' AdminUpdateDeviceStatusResponse Core.Int
audsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED audsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
