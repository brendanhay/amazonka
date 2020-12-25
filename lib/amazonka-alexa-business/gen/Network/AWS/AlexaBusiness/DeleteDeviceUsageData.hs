{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDeviceUsageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When this action is called for a specified shared device, it allows authorized users to delete the device's entire previous history of voice input data and associated response data. This action can be called once every 24 hours for a specific shared device.
module Network.AWS.AlexaBusiness.DeleteDeviceUsageData
  ( -- * Creating a request
    DeleteDeviceUsageData (..),
    mkDeleteDeviceUsageData,

    -- ** Request lenses
    ddudDeviceArn,
    ddudDeviceUsageType,

    -- * Destructuring the response
    DeleteDeviceUsageDataResponse (..),
    mkDeleteDeviceUsageDataResponse,

    -- ** Response lenses
    ddudrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { -- | The ARN of the device.
    deviceArn :: Types.Arn,
    -- | The type of usage data to delete.
    deviceUsageType :: Types.DeviceUsageType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceUsageData' value with any optional fields omitted.
mkDeleteDeviceUsageData ::
  -- | 'deviceArn'
  Types.Arn ->
  -- | 'deviceUsageType'
  Types.DeviceUsageType ->
  DeleteDeviceUsageData
mkDeleteDeviceUsageData deviceArn deviceUsageType =
  DeleteDeviceUsageData' {deviceArn, deviceUsageType}

-- | The ARN of the device.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudDeviceArn :: Lens.Lens' DeleteDeviceUsageData Types.Arn
ddudDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED ddudDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | The type of usage data to delete.
--
-- /Note:/ Consider using 'deviceUsageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudDeviceUsageType :: Lens.Lens' DeleteDeviceUsageData Types.DeviceUsageType
ddudDeviceUsageType = Lens.field @"deviceUsageType"
{-# DEPRECATED ddudDeviceUsageType "Use generic-lens or generic-optics with 'deviceUsageType' instead." #-}

instance Core.FromJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceArn" Core..= deviceArn),
            Core.Just ("DeviceUsageType" Core..= deviceUsageType)
          ]
      )

instance Core.AWSRequest DeleteDeviceUsageData where
  type Rs DeleteDeviceUsageData = DeleteDeviceUsageDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.DeleteDeviceUsageData")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceUsageDataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDeviceUsageDataResponse' smart constructor.
newtype DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceUsageDataResponse' value with any optional fields omitted.
mkDeleteDeviceUsageDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDeviceUsageDataResponse
mkDeleteDeviceUsageDataResponse responseStatus =
  DeleteDeviceUsageDataResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudrrsResponseStatus :: Lens.Lens' DeleteDeviceUsageDataResponse Core.Int
ddudrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddudrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
