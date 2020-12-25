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
    dvmfadSerialNumber,

    -- * Destructuring the response
    DeleteVirtualMFADeviceResponse (..),
    mkDeleteVirtualMFADeviceResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVirtualMFADevice' smart constructor.
newtype DeleteVirtualMFADevice = DeleteVirtualMFADevice'
  { -- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the same as the ARN.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
    serialNumber :: Types.SerialNumberType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVirtualMFADevice' value with any optional fields omitted.
mkDeleteVirtualMFADevice ::
  -- | 'serialNumber'
  Types.SerialNumberType ->
  DeleteVirtualMFADevice
mkDeleteVirtualMFADevice serialNumber =
  DeleteVirtualMFADevice' {serialNumber}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the same as the ARN.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmfadSerialNumber :: Lens.Lens' DeleteVirtualMFADevice Types.SerialNumberType
dvmfadSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED dvmfadSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Core.AWSRequest DeleteVirtualMFADevice where
  type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse
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
            ( Core.pure ("Action", "DeleteVirtualMFADevice")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "SerialNumber" serialNumber)
            )
      }
  response = Response.receiveNull DeleteVirtualMFADeviceResponse'

-- | /See:/ 'mkDeleteVirtualMFADeviceResponse' smart constructor.
data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVirtualMFADeviceResponse' value with any optional fields omitted.
mkDeleteVirtualMFADeviceResponse ::
  DeleteVirtualMFADeviceResponse
mkDeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
