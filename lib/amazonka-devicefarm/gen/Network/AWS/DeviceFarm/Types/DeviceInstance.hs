{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceInstance
  ( DeviceInstance (..),

    -- * Smart constructor
    mkDeviceInstance,

    -- * Lenses
    diStatus,
    diUdid,
    diInstanceProfile,
    diArn,
    diDeviceARN,
    diLabels,
  )
where

import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the device instance.
--
-- /See:/ 'mkDeviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { status ::
      Lude.Maybe InstanceStatus,
    udid :: Lude.Maybe Lude.Text,
    instanceProfile :: Lude.Maybe InstanceProfile,
    arn :: Lude.Maybe Lude.Text,
    deviceARN :: Lude.Maybe Lude.Text,
    labels :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceInstance' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the device instance.
-- * 'deviceARN' - The ARN of the device.
-- * 'instanceProfile' - A object that contains information about the instance profile.
-- * 'labels' - An array of strings that describe the device instance.
-- * 'status' - The status of the device instance. Valid values are listed here.
-- * 'udid' - Unique device identifier for the device instance.
mkDeviceInstance ::
  DeviceInstance
mkDeviceInstance =
  DeviceInstance'
    { status = Lude.Nothing,
      udid = Lude.Nothing,
      instanceProfile = Lude.Nothing,
      arn = Lude.Nothing,
      deviceARN = Lude.Nothing,
      labels = Lude.Nothing
    }

-- | The status of the device instance. Valid values are listed here.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatus :: Lens.Lens' DeviceInstance (Lude.Maybe InstanceStatus)
diStatus = Lens.lens (status :: DeviceInstance -> Lude.Maybe InstanceStatus) (\s a -> s {status = a} :: DeviceInstance)
{-# DEPRECATED diStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Unique device identifier for the device instance.
--
-- /Note:/ Consider using 'udid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diUdid :: Lens.Lens' DeviceInstance (Lude.Maybe Lude.Text)
diUdid = Lens.lens (udid :: DeviceInstance -> Lude.Maybe Lude.Text) (\s a -> s {udid = a} :: DeviceInstance)
{-# DEPRECATED diUdid "Use generic-lens or generic-optics with 'udid' instead." #-}

-- | A object that contains information about the instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceProfile :: Lens.Lens' DeviceInstance (Lude.Maybe InstanceProfile)
diInstanceProfile = Lens.lens (instanceProfile :: DeviceInstance -> Lude.Maybe InstanceProfile) (\s a -> s {instanceProfile = a} :: DeviceInstance)
{-# DEPRECATED diInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The Amazon Resource Name (ARN) of the device instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArn :: Lens.Lens' DeviceInstance (Lude.Maybe Lude.Text)
diArn = Lens.lens (arn :: DeviceInstance -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeviceInstance)
{-# DEPRECATED diArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ARN of the device.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeviceARN :: Lens.Lens' DeviceInstance (Lude.Maybe Lude.Text)
diDeviceARN = Lens.lens (deviceARN :: DeviceInstance -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: DeviceInstance)
{-# DEPRECATED diDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | An array of strings that describe the device instance.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLabels :: Lens.Lens' DeviceInstance (Lude.Maybe [Lude.Text])
diLabels = Lens.lens (labels :: DeviceInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: DeviceInstance)
{-# DEPRECATED diLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

instance Lude.FromJSON DeviceInstance where
  parseJSON =
    Lude.withObject
      "DeviceInstance"
      ( \x ->
          DeviceInstance'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "udid")
            Lude.<*> (x Lude..:? "instanceProfile")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "deviceArn")
            Lude.<*> (x Lude..:? "labels" Lude..!= Lude.mempty)
      )
