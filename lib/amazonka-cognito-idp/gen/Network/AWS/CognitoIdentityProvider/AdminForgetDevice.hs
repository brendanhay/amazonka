{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminForgetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminForgetDevice
  ( -- * Creating a request
    AdminForgetDevice (..),
    mkAdminForgetDevice,

    -- ** Request lenses
    afdUserPoolId,
    afdUsername,
    afdDeviceKey,

    -- * Destructuring the response
    AdminForgetDeviceResponse (..),
    mkAdminForgetDeviceResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Sends the forgot device request, as an administrator.
--
-- /See:/ 'mkAdminForgetDevice' smart constructor.
data AdminForgetDevice = AdminForgetDevice'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The user name.
    username :: Types.Username,
    -- | The device key.
    deviceKey :: Types.DeviceKeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminForgetDevice' value with any optional fields omitted.
mkAdminForgetDevice ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  -- | 'deviceKey'
  Types.DeviceKeyType ->
  AdminForgetDevice
mkAdminForgetDevice userPoolId username deviceKey =
  AdminForgetDevice' {userPoolId, username, deviceKey}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdUserPoolId :: Lens.Lens' AdminForgetDevice Types.UserPoolId
afdUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED afdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdUsername :: Lens.Lens' AdminForgetDevice Types.Username
afdUsername = Lens.field @"username"
{-# DEPRECATED afdUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afdDeviceKey :: Lens.Lens' AdminForgetDevice Types.DeviceKeyType
afdDeviceKey = Lens.field @"deviceKey"
{-# DEPRECATED afdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Core.FromJSON AdminForgetDevice where
  toJSON AdminForgetDevice {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.AWSRequest AdminForgetDevice where
  type Rs AdminForgetDevice = AdminForgetDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminForgetDevice"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AdminForgetDeviceResponse'

-- | /See:/ 'mkAdminForgetDeviceResponse' smart constructor.
data AdminForgetDeviceResponse = AdminForgetDeviceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminForgetDeviceResponse' value with any optional fields omitted.
mkAdminForgetDeviceResponse ::
  AdminForgetDeviceResponse
mkAdminForgetDeviceResponse = AdminForgetDeviceResponse'
