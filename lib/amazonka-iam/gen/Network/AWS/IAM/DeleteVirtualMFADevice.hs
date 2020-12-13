{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual MFA device.
module Network.AWS.IAM.DeleteVirtualMFADevice
  ( -- * Creating a request
    DeleteVirtualMFADevice (..),
    mkDeleteVirtualMFADevice,

    -- ** Request lenses
    dvmdSerialNumber,

    -- * Destructuring the response
    DeleteVirtualMFADeviceResponse (..),
    mkDeleteVirtualMFADeviceResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVirtualMFADevice' smart constructor.
newtype DeleteVirtualMFADevice = DeleteVirtualMFADevice'
  { -- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the same as the ARN.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
    serialNumber :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVirtualMFADevice' with the minimum fields required to make a request.
--
-- * 'serialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the same as the ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
mkDeleteVirtualMFADevice ::
  -- | 'serialNumber'
  Lude.Text ->
  DeleteVirtualMFADevice
mkDeleteVirtualMFADevice pSerialNumber_ =
  DeleteVirtualMFADevice' {serialNumber = pSerialNumber_}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the same as the ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmdSerialNumber :: Lens.Lens' DeleteVirtualMFADevice Lude.Text
dvmdSerialNumber = Lens.lens (serialNumber :: DeleteVirtualMFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: DeleteVirtualMFADevice)
{-# DEPRECATED dvmdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Lude.AWSRequest DeleteVirtualMFADevice where
  type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteVirtualMFADeviceResponse'

instance Lude.ToHeaders DeleteVirtualMFADevice where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVirtualMFADevice where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVirtualMFADevice where
  toQuery DeleteVirtualMFADevice' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVirtualMFADevice" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "SerialNumber" Lude.=: serialNumber
      ]

-- | /See:/ 'mkDeleteVirtualMFADeviceResponse' smart constructor.
data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVirtualMFADeviceResponse' with the minimum fields required to make a request.
mkDeleteVirtualMFADeviceResponse ::
  DeleteVirtualMFADeviceResponse
mkDeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
