{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cvmfadVirtualMFADeviceName,
    cvmfadPath,

    -- * Destructuring the response
    CreateVirtualMFADeviceResponse (..),
    mkCreateVirtualMFADeviceResponse,

    -- ** Response lenses
    cvmfadrrsVirtualMFADevice,
    cvmfadrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVirtualMFADevice' smart constructor.
data CreateVirtualMFADevice = CreateVirtualMFADevice'
  { -- | The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    virtualMFADeviceName :: Types.VirtualMFADeviceName,
    -- | The path for the virtual MFA device. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    --
    -- This parameter is optional. If it is not included, it defaults to a slash (/).
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    path :: Core.Maybe Types.PathType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVirtualMFADevice' value with any optional fields omitted.
mkCreateVirtualMFADevice ::
  -- | 'virtualMFADeviceName'
  Types.VirtualMFADeviceName ->
  CreateVirtualMFADevice
mkCreateVirtualMFADevice virtualMFADeviceName =
  CreateVirtualMFADevice'
    { virtualMFADeviceName,
      path = Core.Nothing
    }

-- | The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'virtualMFADeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmfadVirtualMFADeviceName :: Lens.Lens' CreateVirtualMFADevice Types.VirtualMFADeviceName
cvmfadVirtualMFADeviceName = Lens.field @"virtualMFADeviceName"
{-# DEPRECATED cvmfadVirtualMFADeviceName "Use generic-lens or generic-optics with 'virtualMFADeviceName' instead." #-}

-- | The path for the virtual MFA device. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmfadPath :: Lens.Lens' CreateVirtualMFADevice (Core.Maybe Types.PathType)
cvmfadPath = Lens.field @"path"
{-# DEPRECATED cvmfadPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.AWSRequest CreateVirtualMFADevice where
  type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse
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
            ( Core.pure ("Action", "CreateVirtualMFADevice")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "VirtualMFADeviceName" virtualMFADeviceName)
                Core.<> (Core.toQueryValue "Path" Core.<$> path)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateVirtualMFADeviceResult"
      ( \s h x ->
          CreateVirtualMFADeviceResponse'
            Core.<$> (x Core..@ "VirtualMFADevice")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'CreateVirtualMFADevice' request.
--
-- /See:/ 'mkCreateVirtualMFADeviceResponse' smart constructor.
data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'
  { -- | A structure containing details about the new virtual MFA device.
    virtualMFADevice :: Types.VirtualMFADevice,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateVirtualMFADeviceResponse' value with any optional fields omitted.
mkCreateVirtualMFADeviceResponse ::
  -- | 'virtualMFADevice'
  Types.VirtualMFADevice ->
  -- | 'responseStatus'
  Core.Int ->
  CreateVirtualMFADeviceResponse
mkCreateVirtualMFADeviceResponse virtualMFADevice responseStatus =
  CreateVirtualMFADeviceResponse' {virtualMFADevice, responseStatus}

-- | A structure containing details about the new virtual MFA device.
--
-- /Note:/ Consider using 'virtualMFADevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmfadrrsVirtualMFADevice :: Lens.Lens' CreateVirtualMFADeviceResponse Types.VirtualMFADevice
cvmfadrrsVirtualMFADevice = Lens.field @"virtualMFADevice"
{-# DEPRECATED cvmfadrrsVirtualMFADevice "Use generic-lens or generic-optics with 'virtualMFADevice' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmfadrrsResponseStatus :: Lens.Lens' CreateVirtualMFADeviceResponse Core.Int
cvmfadrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvmfadrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
