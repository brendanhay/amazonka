{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details for the input device
module Network.AWS.MediaLive.DescribeInputDevice
    (
    -- * Creating a request
      DescribeInputDevice (..)
    , mkDescribeInputDevice
    -- ** Request lenses
    , didInputDeviceId

    -- * Destructuring the response
    , DescribeInputDeviceResponse (..)
    , mkDescribeInputDeviceResponse
    -- ** Response lenses
    , didrrsArn
    , didrrsConnectionState
    , didrrsDeviceSettingsSyncState
    , didrrsDeviceUpdateStatus
    , didrrsHdDeviceSettings
    , didrrsId
    , didrrsMacAddress
    , didrrsName
    , didrrsNetworkSettings
    , didrrsSerialNumber
    , didrrsType
    , didrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputDeviceRequest
--
-- /See:/ 'mkDescribeInputDevice' smart constructor.
newtype DescribeInputDevice = DescribeInputDevice'
  { inputDeviceId :: Core.Text
    -- ^ The unique ID of this input device. For example, hd-123456789abcdef.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputDevice' value with any optional fields omitted.
mkDescribeInputDevice
    :: Core.Text -- ^ 'inputDeviceId'
    -> DescribeInputDevice
mkDescribeInputDevice inputDeviceId
  = DescribeInputDevice'{inputDeviceId}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didInputDeviceId :: Lens.Lens' DescribeInputDevice Core.Text
didInputDeviceId = Lens.field @"inputDeviceId"
{-# INLINEABLE didInputDeviceId #-}
{-# DEPRECATED inputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead"  #-}

instance Core.ToQuery DescribeInputDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInputDevice where
        toHeaders DescribeInputDevice{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeInputDevice where
        type Rs DescribeInputDevice = DescribeInputDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/prod/inputDevices/" Core.<> Core.toText inputDeviceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInputDeviceResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "connectionState" Core.<*>
                     x Core..:? "deviceSettingsSyncState"
                     Core.<*> x Core..:? "deviceUpdateStatus"
                     Core.<*> x Core..:? "hdDeviceSettings"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "macAddress"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "networkSettings"
                     Core.<*> x Core..:? "serialNumber"
                     Core.<*> x Core..:? "type"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DescribeInputDeviceResponse
--
-- /See:/ 'mkDescribeInputDeviceResponse' smart constructor.
data DescribeInputDeviceResponse = DescribeInputDeviceResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The unique ARN of the input device.
  , connectionState :: Core.Maybe Types.InputDeviceConnectionState
    -- ^ The state of the connection between the input device and AWS.
  , deviceSettingsSyncState :: Core.Maybe Types.DeviceSettingsSyncState
    -- ^ The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
  , deviceUpdateStatus :: Core.Maybe Types.DeviceUpdateStatus
    -- ^ The status of software on the input device.
  , hdDeviceSettings :: Core.Maybe Types.InputDeviceHdSettings
    -- ^ Settings that describe an input device that is type HD.
  , id :: Core.Maybe Core.Text
    -- ^ The unique ID of the input device.
  , macAddress :: Core.Maybe Core.Text
    -- ^ The network MAC address of the input device.
  , name :: Core.Maybe Core.Text
    -- ^ A name that you specify for the input device.
  , networkSettings :: Core.Maybe Types.InputDeviceNetworkSettings
    -- ^ The network settings for the input device.
  , serialNumber :: Core.Maybe Core.Text
    -- ^ The unique serial number of the input device.
  , type' :: Core.Maybe Types.InputDeviceType
    -- ^ The type of the input device.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputDeviceResponse' value with any optional fields omitted.
mkDescribeInputDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInputDeviceResponse
mkDescribeInputDeviceResponse responseStatus
  = DescribeInputDeviceResponse'{arn = Core.Nothing,
                                 connectionState = Core.Nothing,
                                 deviceSettingsSyncState = Core.Nothing,
                                 deviceUpdateStatus = Core.Nothing, hdDeviceSettings = Core.Nothing,
                                 id = Core.Nothing, macAddress = Core.Nothing, name = Core.Nothing,
                                 networkSettings = Core.Nothing, serialNumber = Core.Nothing,
                                 type' = Core.Nothing, responseStatus}

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsArn :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Core.Text)
didrrsArn = Lens.field @"arn"
{-# INLINEABLE didrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsConnectionState :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.InputDeviceConnectionState)
didrrsConnectionState = Lens.field @"connectionState"
{-# INLINEABLE didrrsConnectionState #-}
{-# DEPRECATED connectionState "Use generic-lens or generic-optics with 'connectionState' instead"  #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsDeviceSettingsSyncState :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.DeviceSettingsSyncState)
didrrsDeviceSettingsSyncState = Lens.field @"deviceSettingsSyncState"
{-# INLINEABLE didrrsDeviceSettingsSyncState #-}
{-# DEPRECATED deviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead"  #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsDeviceUpdateStatus :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.DeviceUpdateStatus)
didrrsDeviceUpdateStatus = Lens.field @"deviceUpdateStatus"
{-# INLINEABLE didrrsDeviceUpdateStatus #-}
{-# DEPRECATED deviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead"  #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsHdDeviceSettings :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.InputDeviceHdSettings)
didrrsHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# INLINEABLE didrrsHdDeviceSettings #-}
{-# DEPRECATED hdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead"  #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsId :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Core.Text)
didrrsId = Lens.field @"id"
{-# INLINEABLE didrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsMacAddress :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Core.Text)
didrrsMacAddress = Lens.field @"macAddress"
{-# INLINEABLE didrrsMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsName :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Core.Text)
didrrsName = Lens.field @"name"
{-# INLINEABLE didrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsNetworkSettings :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.InputDeviceNetworkSettings)
didrrsNetworkSettings = Lens.field @"networkSettings"
{-# INLINEABLE didrrsNetworkSettings #-}
{-# DEPRECATED networkSettings "Use generic-lens or generic-optics with 'networkSettings' instead"  #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsSerialNumber :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Core.Text)
didrrsSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE didrrsSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsType :: Lens.Lens' DescribeInputDeviceResponse (Core.Maybe Types.InputDeviceType)
didrrsType = Lens.field @"type'"
{-# INLINEABLE didrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrrsResponseStatus :: Lens.Lens' DescribeInputDeviceResponse Core.Int
didrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE didrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
