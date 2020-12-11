{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual MFA device for the AWS account. After creating the virtual MFA, use 'EnableMFADevice' to attach the MFA device to an IAM user. For more information about creating and working with virtual MFA devices, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /IAM User Guide/ .
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
-- /Important:/ The seed information contained in the QR code and the Base32 string should be treated like any other secret access information. In other words, protect the seed information as you would your AWS access keys or your passwords. After you provision your virtual device, you should ensure that the information is destroyed following secure procedures.
module Network.AWS.IAM.CreateVirtualMFADevice
  ( -- * Creating a request
    CreateVirtualMFADevice (..),
    mkCreateVirtualMFADevice,

    -- ** Request lenses
    cvmdPath,
    cvmdVirtualMFADeviceName,

    -- * Destructuring the response
    CreateVirtualMFADeviceResponse (..),
    mkCreateVirtualMFADeviceResponse,

    -- ** Response lenses
    cvmdrsResponseStatus,
    cvmdrsVirtualMFADevice,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVirtualMFADevice' smart constructor.
data CreateVirtualMFADevice = CreateVirtualMFADevice'
  { path ::
      Lude.Maybe Lude.Text,
    virtualMFADeviceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVirtualMFADevice' with the minimum fields required to make a request.
--
-- * 'path' - The path for the virtual MFA device. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'virtualMFADeviceName' - The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkCreateVirtualMFADevice ::
  -- | 'virtualMFADeviceName'
  Lude.Text ->
  CreateVirtualMFADevice
mkCreateVirtualMFADevice pVirtualMFADeviceName_ =
  CreateVirtualMFADevice'
    { path = Lude.Nothing,
      virtualMFADeviceName = pVirtualMFADeviceName_
    }

-- | The path for the virtual MFA device. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmdPath :: Lens.Lens' CreateVirtualMFADevice (Lude.Maybe Lude.Text)
cvmdPath = Lens.lens (path :: CreateVirtualMFADevice -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreateVirtualMFADevice)
{-# DEPRECATED cvmdPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'virtualMFADeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmdVirtualMFADeviceName :: Lens.Lens' CreateVirtualMFADevice Lude.Text
cvmdVirtualMFADeviceName = Lens.lens (virtualMFADeviceName :: CreateVirtualMFADevice -> Lude.Text) (\s a -> s {virtualMFADeviceName = a} :: CreateVirtualMFADevice)
{-# DEPRECATED cvmdVirtualMFADeviceName "Use generic-lens or generic-optics with 'virtualMFADeviceName' instead." #-}

instance Lude.AWSRequest CreateVirtualMFADevice where
  type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateVirtualMFADeviceResult"
      ( \s h x ->
          CreateVirtualMFADeviceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "VirtualMFADevice")
      )

instance Lude.ToHeaders CreateVirtualMFADevice where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVirtualMFADevice where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVirtualMFADevice where
  toQuery CreateVirtualMFADevice' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVirtualMFADevice" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Path" Lude.=: path,
        "VirtualMFADeviceName" Lude.=: virtualMFADeviceName
      ]

-- | Contains the response to a successful 'CreateVirtualMFADevice' request.
--
-- /See:/ 'mkCreateVirtualMFADeviceResponse' smart constructor.
data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'
  { responseStatus ::
      Lude.Int,
    virtualMFADevice ::
      VirtualMFADevice
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVirtualMFADeviceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'virtualMFADevice' - A structure containing details about the new virtual MFA device.
mkCreateVirtualMFADeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'virtualMFADevice'
  VirtualMFADevice ->
  CreateVirtualMFADeviceResponse
mkCreateVirtualMFADeviceResponse
  pResponseStatus_
  pVirtualMFADevice_ =
    CreateVirtualMFADeviceResponse'
      { responseStatus =
          pResponseStatus_,
        virtualMFADevice = pVirtualMFADevice_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmdrsResponseStatus :: Lens.Lens' CreateVirtualMFADeviceResponse Lude.Int
cvmdrsResponseStatus = Lens.lens (responseStatus :: CreateVirtualMFADeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVirtualMFADeviceResponse)
{-# DEPRECATED cvmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the new virtual MFA device.
--
-- /Note:/ Consider using 'virtualMFADevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmdrsVirtualMFADevice :: Lens.Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmdrsVirtualMFADevice = Lens.lens (virtualMFADevice :: CreateVirtualMFADeviceResponse -> VirtualMFADevice) (\s a -> s {virtualMFADevice = a} :: CreateVirtualMFADeviceResponse)
{-# DEPRECATED cvmdrsVirtualMFADevice "Use generic-lens or generic-optics with 'virtualMFADevice' instead." #-}
