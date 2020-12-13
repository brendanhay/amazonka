{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSummary
  ( InputDeviceSummary (..),

    -- * Smart constructor
    mkInputDeviceSummary,

    -- * Lenses
    idsARN,
    idsMACAddress,
    idsHdDeviceSettings,
    idsName,
    idsId,
    idsDeviceUpdateStatus,
    idsDeviceSettingsSyncState,
    idsType,
    idsSerialNumber,
    idsNetworkSettings,
    idsConnectionState,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.DeviceSettingsSyncState
import Network.AWS.MediaLive.Types.DeviceUpdateStatus
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceType
import qualified Network.AWS.Prelude as Lude

-- | Details of the input device.
--
-- /See:/ 'mkInputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'
  { -- | The unique ARN of the input device.
    arn :: Lude.Maybe Lude.Text,
    -- | The network MAC address of the input device.
    mACAddress :: Lude.Maybe Lude.Text,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Lude.Maybe InputDeviceHdSettings,
    -- | A name that you specify for the input device.
    name :: Lude.Maybe Lude.Text,
    -- | The unique ID of the input device.
    id :: Lude.Maybe Lude.Text,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Lude.Maybe DeviceUpdateStatus,
    -- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Lude.Maybe DeviceSettingsSyncState,
    -- | The type of the input device.
    type' :: Lude.Maybe InputDeviceType,
    -- | The unique serial number of the input device.
    serialNumber :: Lude.Maybe Lude.Text,
    -- | Network settings for the input device.
    networkSettings :: Lude.Maybe InputDeviceNetworkSettings,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Lude.Maybe InputDeviceConnectionState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDeviceSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The unique ARN of the input device.
-- * 'mACAddress' - The network MAC address of the input device.
-- * 'hdDeviceSettings' - Settings that describe an input device that is type HD.
-- * 'name' - A name that you specify for the input device.
-- * 'id' - The unique ID of the input device.
-- * 'deviceUpdateStatus' - The status of software on the input device.
-- * 'deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
-- * 'type'' - The type of the input device.
-- * 'serialNumber' - The unique serial number of the input device.
-- * 'networkSettings' - Network settings for the input device.
-- * 'connectionState' - The state of the connection between the input device and AWS.
mkInputDeviceSummary ::
  InputDeviceSummary
mkInputDeviceSummary =
  InputDeviceSummary'
    { arn = Lude.Nothing,
      mACAddress = Lude.Nothing,
      hdDeviceSettings = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      deviceUpdateStatus = Lude.Nothing,
      deviceSettingsSyncState = Lude.Nothing,
      type' = Lude.Nothing,
      serialNumber = Lude.Nothing,
      networkSettings = Lude.Nothing,
      connectionState = Lude.Nothing
    }

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsARN :: Lens.Lens' InputDeviceSummary (Lude.Maybe Lude.Text)
idsARN = Lens.lens (arn :: InputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InputDeviceSummary)
{-# DEPRECATED idsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsMACAddress :: Lens.Lens' InputDeviceSummary (Lude.Maybe Lude.Text)
idsMACAddress = Lens.lens (mACAddress :: InputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: InputDeviceSummary)
{-# DEPRECATED idsMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsHdDeviceSettings :: Lens.Lens' InputDeviceSummary (Lude.Maybe InputDeviceHdSettings)
idsHdDeviceSettings = Lens.lens (hdDeviceSettings :: InputDeviceSummary -> Lude.Maybe InputDeviceHdSettings) (\s a -> s {hdDeviceSettings = a} :: InputDeviceSummary)
{-# DEPRECATED idsHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsName :: Lens.Lens' InputDeviceSummary (Lude.Maybe Lude.Text)
idsName = Lens.lens (name :: InputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InputDeviceSummary)
{-# DEPRECATED idsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsId :: Lens.Lens' InputDeviceSummary (Lude.Maybe Lude.Text)
idsId = Lens.lens (id :: InputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InputDeviceSummary)
{-# DEPRECATED idsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDeviceUpdateStatus :: Lens.Lens' InputDeviceSummary (Lude.Maybe DeviceUpdateStatus)
idsDeviceUpdateStatus = Lens.lens (deviceUpdateStatus :: InputDeviceSummary -> Lude.Maybe DeviceUpdateStatus) (\s a -> s {deviceUpdateStatus = a} :: InputDeviceSummary)
{-# DEPRECATED idsDeviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead." #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDeviceSettingsSyncState :: Lens.Lens' InputDeviceSummary (Lude.Maybe DeviceSettingsSyncState)
idsDeviceSettingsSyncState = Lens.lens (deviceSettingsSyncState :: InputDeviceSummary -> Lude.Maybe DeviceSettingsSyncState) (\s a -> s {deviceSettingsSyncState = a} :: InputDeviceSummary)
{-# DEPRECATED idsDeviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead." #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsType :: Lens.Lens' InputDeviceSummary (Lude.Maybe InputDeviceType)
idsType = Lens.lens (type' :: InputDeviceSummary -> Lude.Maybe InputDeviceType) (\s a -> s {type' = a} :: InputDeviceSummary)
{-# DEPRECATED idsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsSerialNumber :: Lens.Lens' InputDeviceSummary (Lude.Maybe Lude.Text)
idsSerialNumber = Lens.lens (serialNumber :: InputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: InputDeviceSummary)
{-# DEPRECATED idsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | Network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsNetworkSettings :: Lens.Lens' InputDeviceSummary (Lude.Maybe InputDeviceNetworkSettings)
idsNetworkSettings = Lens.lens (networkSettings :: InputDeviceSummary -> Lude.Maybe InputDeviceNetworkSettings) (\s a -> s {networkSettings = a} :: InputDeviceSummary)
{-# DEPRECATED idsNetworkSettings "Use generic-lens or generic-optics with 'networkSettings' instead." #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsConnectionState :: Lens.Lens' InputDeviceSummary (Lude.Maybe InputDeviceConnectionState)
idsConnectionState = Lens.lens (connectionState :: InputDeviceSummary -> Lude.Maybe InputDeviceConnectionState) (\s a -> s {connectionState = a} :: InputDeviceSummary)
{-# DEPRECATED idsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

instance Lude.FromJSON InputDeviceSummary where
  parseJSON =
    Lude.withObject
      "InputDeviceSummary"
      ( \x ->
          InputDeviceSummary'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "macAddress")
            Lude.<*> (x Lude..:? "hdDeviceSettings")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "deviceUpdateStatus")
            Lude.<*> (x Lude..:? "deviceSettingsSyncState")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "serialNumber")
            Lude.<*> (x Lude..:? "networkSettings")
            Lude.<*> (x Lude..:? "connectionState")
      )
