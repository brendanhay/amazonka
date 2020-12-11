{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified MFA device and removes it from association with the user name for which it was originally enabled.
--
-- For more information about creating and working with virtual MFA devices, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Enabling a Virtual Multi-factor Authentication (MFA) Device> in the /IAM User Guide/ .
module Network.AWS.IAM.DeactivateMFADevice
  ( -- * Creating a request
    DeactivateMFADevice (..),
    mkDeactivateMFADevice,

    -- ** Request lenses
    dmdUserName,
    dmdSerialNumber,

    -- * Destructuring the response
    DeactivateMFADeviceResponse (..),
    mkDeactivateMFADeviceResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeactivateMFADevice' smart constructor.
data DeactivateMFADevice = DeactivateMFADevice'
  { userName ::
      Lude.Text,
    serialNumber :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateMFADevice' with the minimum fields required to make a request.
--
-- * 'serialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
-- * 'userName' - The name of the user whose MFA device you want to deactivate.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeactivateMFADevice ::
  -- | 'userName'
  Lude.Text ->
  -- | 'serialNumber'
  Lude.Text ->
  DeactivateMFADevice
mkDeactivateMFADevice pUserName_ pSerialNumber_ =
  DeactivateMFADevice'
    { userName = pUserName_,
      serialNumber = pSerialNumber_
    }

-- | The name of the user whose MFA device you want to deactivate.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmdUserName :: Lens.Lens' DeactivateMFADevice Lude.Text
dmdUserName = Lens.lens (userName :: DeactivateMFADevice -> Lude.Text) (\s a -> s {userName = a} :: DeactivateMFADevice)
{-# DEPRECATED dmdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmdSerialNumber :: Lens.Lens' DeactivateMFADevice Lude.Text
dmdSerialNumber = Lens.lens (serialNumber :: DeactivateMFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: DeactivateMFADevice)
{-# DEPRECATED dmdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Lude.AWSRequest DeactivateMFADevice where
  type Rs DeactivateMFADevice = DeactivateMFADeviceResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeactivateMFADeviceResponse'

instance Lude.ToHeaders DeactivateMFADevice where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeactivateMFADevice where
  toPath = Lude.const "/"

instance Lude.ToQuery DeactivateMFADevice where
  toQuery DeactivateMFADevice' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeactivateMFADevice" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "SerialNumber" Lude.=: serialNumber
      ]

-- | /See:/ 'mkDeactivateMFADeviceResponse' smart constructor.
data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateMFADeviceResponse' with the minimum fields required to make a request.
mkDeactivateMFADeviceResponse ::
  DeactivateMFADeviceResponse
mkDeactivateMFADeviceResponse = DeactivateMFADeviceResponse'
