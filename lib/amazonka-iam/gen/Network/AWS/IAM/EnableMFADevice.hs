{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    emdUserName,
    emdSerialNumber,
    emdAuthenticationCode1,
    emdAuthenticationCode2,

    -- * Destructuring the response
    EnableMFADeviceResponse (..),
    mkEnableMFADeviceResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableMFADevice' smart constructor.
data EnableMFADevice = EnableMFADevice'
  { userName :: Lude.Text,
    serialNumber :: Lude.Text,
    authenticationCode1 :: Lude.Text,
    authenticationCode2 :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableMFADevice' with the minimum fields required to make a request.
--
-- * 'authenticationCode1' - An authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
-- * 'authenticationCode2' - A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
-- * 'serialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
-- * 'userName' - The name of the IAM user for whom you want to enable the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkEnableMFADevice ::
  -- | 'userName'
  Lude.Text ->
  -- | 'serialNumber'
  Lude.Text ->
  -- | 'authenticationCode1'
  Lude.Text ->
  -- | 'authenticationCode2'
  Lude.Text ->
  EnableMFADevice
mkEnableMFADevice
  pUserName_
  pSerialNumber_
  pAuthenticationCode1_
  pAuthenticationCode2_ =
    EnableMFADevice'
      { userName = pUserName_,
        serialNumber = pSerialNumber_,
        authenticationCode1 = pAuthenticationCode1_,
        authenticationCode2 = pAuthenticationCode2_
      }

-- | The name of the IAM user for whom you want to enable the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emdUserName :: Lens.Lens' EnableMFADevice Lude.Text
emdUserName = Lens.lens (userName :: EnableMFADevice -> Lude.Text) (\s a -> s {userName = a} :: EnableMFADevice)
{-# DEPRECATED emdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emdSerialNumber :: Lens.Lens' EnableMFADevice Lude.Text
emdSerialNumber = Lens.lens (serialNumber :: EnableMFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: EnableMFADevice)
{-# DEPRECATED emdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
--
-- /Note:/ Consider using 'authenticationCode1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emdAuthenticationCode1 :: Lens.Lens' EnableMFADevice Lude.Text
emdAuthenticationCode1 = Lens.lens (authenticationCode1 :: EnableMFADevice -> Lude.Text) (\s a -> s {authenticationCode1 = a} :: EnableMFADevice)
{-# DEPRECATED emdAuthenticationCode1 "Use generic-lens or generic-optics with 'authenticationCode1' instead." #-}

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a string of six digits.
-- /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
--
-- /Note:/ Consider using 'authenticationCode2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emdAuthenticationCode2 :: Lens.Lens' EnableMFADevice Lude.Text
emdAuthenticationCode2 = Lens.lens (authenticationCode2 :: EnableMFADevice -> Lude.Text) (\s a -> s {authenticationCode2 = a} :: EnableMFADevice)
{-# DEPRECATED emdAuthenticationCode2 "Use generic-lens or generic-optics with 'authenticationCode2' instead." #-}

instance Lude.AWSRequest EnableMFADevice where
  type Rs EnableMFADevice = EnableMFADeviceResponse
  request = Req.postQuery iamService
  response = Res.receiveNull EnableMFADeviceResponse'

instance Lude.ToHeaders EnableMFADevice where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableMFADevice where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableMFADevice where
  toQuery EnableMFADevice' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableMFADevice" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "SerialNumber" Lude.=: serialNumber,
        "AuthenticationCode1" Lude.=: authenticationCode1,
        "AuthenticationCode2" Lude.=: authenticationCode2
      ]

-- | /See:/ 'mkEnableMFADeviceResponse' smart constructor.
data EnableMFADeviceResponse = EnableMFADeviceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableMFADeviceResponse' with the minimum fields required to make a request.
mkEnableMFADeviceResponse ::
  EnableMFADeviceResponse
mkEnableMFADeviceResponse = EnableMFADeviceResponse'
