{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with its IAM resource object on the AWS servers.
--
-- For more information about creating and working with virtual MFA devices, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /IAM User Guide/ .
module Network.AWS.IAM.ResyncMFADevice
  ( -- * Creating a request
    ResyncMFADevice (..),
    mkResyncMFADevice,

    -- ** Request lenses
    rmdAuthenticationCode1,
    rmdUserName,
    rmdAuthenticationCode2,
    rmdSerialNumber,

    -- * Destructuring the response
    ResyncMFADeviceResponse (..),
    mkResyncMFADeviceResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResyncMFADevice' smart constructor.
data ResyncMFADevice = ResyncMFADevice'
  { -- | An authentication code emitted by the device.
    --
    -- The format for this parameter is a sequence of six digits.
    authenticationCode1 :: Lude.Text,
    -- | The name of the user whose MFA device you want to resynchronize.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | A subsequent authentication code emitted by the device.
    --
    -- The format for this parameter is a sequence of six digits.
    authenticationCode2 :: Lude.Text,
    -- | Serial number that uniquely identifies the MFA device.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    serialNumber :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResyncMFADevice' with the minimum fields required to make a request.
--
-- * 'authenticationCode1' - An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
-- * 'userName' - The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'authenticationCode2' - A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
-- * 'serialNumber' - Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkResyncMFADevice ::
  -- | 'authenticationCode1'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'authenticationCode2'
  Lude.Text ->
  -- | 'serialNumber'
  Lude.Text ->
  ResyncMFADevice
mkResyncMFADevice
  pAuthenticationCode1_
  pUserName_
  pAuthenticationCode2_
  pSerialNumber_ =
    ResyncMFADevice'
      { authenticationCode1 = pAuthenticationCode1_,
        userName = pUserName_,
        authenticationCode2 = pAuthenticationCode2_,
        serialNumber = pSerialNumber_
      }

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
--
-- /Note:/ Consider using 'authenticationCode1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdAuthenticationCode1 :: Lens.Lens' ResyncMFADevice Lude.Text
rmdAuthenticationCode1 = Lens.lens (authenticationCode1 :: ResyncMFADevice -> Lude.Text) (\s a -> s {authenticationCode1 = a} :: ResyncMFADevice)
{-# DEPRECATED rmdAuthenticationCode1 "Use generic-lens or generic-optics with 'authenticationCode1' instead." #-}

-- | The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdUserName :: Lens.Lens' ResyncMFADevice Lude.Text
rmdUserName = Lens.lens (userName :: ResyncMFADevice -> Lude.Text) (\s a -> s {userName = a} :: ResyncMFADevice)
{-# DEPRECATED rmdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
--
-- /Note:/ Consider using 'authenticationCode2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdAuthenticationCode2 :: Lens.Lens' ResyncMFADevice Lude.Text
rmdAuthenticationCode2 = Lens.lens (authenticationCode2 :: ResyncMFADevice -> Lude.Text) (\s a -> s {authenticationCode2 = a} :: ResyncMFADevice)
{-# DEPRECATED rmdAuthenticationCode2 "Use generic-lens or generic-optics with 'authenticationCode2' instead." #-}

-- | Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdSerialNumber :: Lens.Lens' ResyncMFADevice Lude.Text
rmdSerialNumber = Lens.lens (serialNumber :: ResyncMFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: ResyncMFADevice)
{-# DEPRECATED rmdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Lude.AWSRequest ResyncMFADevice where
  type Rs ResyncMFADevice = ResyncMFADeviceResponse
  request = Req.postQuery iamService
  response = Res.receiveNull ResyncMFADeviceResponse'

instance Lude.ToHeaders ResyncMFADevice where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResyncMFADevice where
  toPath = Lude.const "/"

instance Lude.ToQuery ResyncMFADevice where
  toQuery ResyncMFADevice' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResyncMFADevice" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "AuthenticationCode1" Lude.=: authenticationCode1,
        "UserName" Lude.=: userName,
        "AuthenticationCode2" Lude.=: authenticationCode2,
        "SerialNumber" Lude.=: serialNumber
      ]

-- | /See:/ 'mkResyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse = ResyncMFADeviceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResyncMFADeviceResponse' with the minimum fields required to make a request.
mkResyncMFADeviceResponse ::
  ResyncMFADeviceResponse
mkResyncMFADeviceResponse = ResyncMFADeviceResponse'
