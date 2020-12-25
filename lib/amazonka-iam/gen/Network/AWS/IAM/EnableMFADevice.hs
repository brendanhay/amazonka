{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified MFA device and associates it with the specified IAM user. When enabled, the MFA device is required for every subsequent login by the IAM user associated with the device.
module Network.AWS.IAM.EnableMFADevice
  ( -- * Creating a request
    EnableMFADevice (..),
    mkEnableMFADevice,

    -- ** Request lenses
    emfadUserName,
    emfadSerialNumber,
    emfadAuthenticationCode1,
    emfadAuthenticationCode2,

    -- * Destructuring the response
    EnableMFADeviceResponse (..),
    mkEnableMFADeviceResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableMFADevice' smart constructor.
data EnableMFADevice = EnableMFADevice'
  { -- | The name of the IAM user for whom you want to enable the MFA device.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
    serialNumber :: Types.SerialNumberType,
    -- | An authentication code emitted by the device.
    --
    -- The format for this parameter is a string of six digits.
    -- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
    authenticationCode1 :: Types.AuthenticationCodeType,
    -- | A subsequent authentication code emitted by the device.
    --
    -- The format for this parameter is a string of six digits.
    -- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
    authenticationCode2 :: Types.AuthenticationCodeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMFADevice' value with any optional fields omitted.
mkEnableMFADevice ::
  -- | 'userName'
  Types.UserName ->
  -- | 'serialNumber'
  Types.SerialNumberType ->
  -- | 'authenticationCode1'
  Types.AuthenticationCodeType ->
  -- | 'authenticationCode2'
  Types.AuthenticationCodeType ->
  EnableMFADevice
mkEnableMFADevice
  userName
  serialNumber
  authenticationCode1
  authenticationCode2 =
    EnableMFADevice'
      { userName,
        serialNumber,
        authenticationCode1,
        authenticationCode2
      }

-- | The name of the IAM user for whom you want to enable the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emfadUserName :: Lens.Lens' EnableMFADevice Types.UserName
emfadUserName = Lens.field @"userName"
{-# DEPRECATED emfadUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emfadSerialNumber :: Lens.Lens' EnableMFADevice Types.SerialNumberType
emfadSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED emfadSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
--
-- /Note:/ Consider using 'authenticationCode1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emfadAuthenticationCode1 :: Lens.Lens' EnableMFADevice Types.AuthenticationCodeType
emfadAuthenticationCode1 = Lens.field @"authenticationCode1"
{-# DEPRECATED emfadAuthenticationCode1 "Use generic-lens or generic-optics with 'authenticationCode1' instead." #-}

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
--
-- /Note:/ Consider using 'authenticationCode2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emfadAuthenticationCode2 :: Lens.Lens' EnableMFADevice Types.AuthenticationCodeType
emfadAuthenticationCode2 = Lens.field @"authenticationCode2"
{-# DEPRECATED emfadAuthenticationCode2 "Use generic-lens or generic-optics with 'authenticationCode2' instead." #-}

instance Core.AWSRequest EnableMFADevice where
  type Rs EnableMFADevice = EnableMFADeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EnableMFADevice")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "SerialNumber" serialNumber)
                Core.<> (Core.toQueryValue "AuthenticationCode1" authenticationCode1)
                Core.<> (Core.toQueryValue "AuthenticationCode2" authenticationCode2)
            )
      }
  response = Response.receiveNull EnableMFADeviceResponse'

-- | /See:/ 'mkEnableMFADeviceResponse' smart constructor.
data EnableMFADeviceResponse = EnableMFADeviceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMFADeviceResponse' value with any optional fields omitted.
mkEnableMFADeviceResponse ::
  EnableMFADeviceResponse
mkEnableMFADeviceResponse = EnableMFADeviceResponse'
