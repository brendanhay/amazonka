{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.MFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.MFADevice
  ( MFADevice (..),

    -- * Smart constructor
    mkMFADevice,

    -- * Lenses
    mdUserName,
    mdEnableDate,
    mdSerialNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an MFA device.
--
-- This data type is used as a response element in the 'ListMFADevices' operation.
--
-- /See:/ 'mkMFADevice' smart constructor.
data MFADevice = MFADevice'
  { -- | The user with whom the MFA device is associated.
    userName :: Lude.Text,
    -- | The date when the MFA device was enabled for the user.
    enableDate :: Lude.DateTime,
    -- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
    serialNumber :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MFADevice' with the minimum fields required to make a request.
--
-- * 'userName' - The user with whom the MFA device is associated.
-- * 'enableDate' - The date when the MFA device was enabled for the user.
-- * 'serialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
mkMFADevice ::
  -- | 'userName'
  Lude.Text ->
  -- | 'enableDate'
  Lude.DateTime ->
  -- | 'serialNumber'
  Lude.Text ->
  MFADevice
mkMFADevice pUserName_ pEnableDate_ pSerialNumber_ =
  MFADevice'
    { userName = pUserName_,
      enableDate = pEnableDate_,
      serialNumber = pSerialNumber_
    }

-- | The user with whom the MFA device is associated.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdUserName :: Lens.Lens' MFADevice Lude.Text
mdUserName = Lens.lens (userName :: MFADevice -> Lude.Text) (\s a -> s {userName = a} :: MFADevice)
{-# DEPRECATED mdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The date when the MFA device was enabled for the user.
--
-- /Note:/ Consider using 'enableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdEnableDate :: Lens.Lens' MFADevice Lude.DateTime
mdEnableDate = Lens.lens (enableDate :: MFADevice -> Lude.DateTime) (\s a -> s {enableDate = a} :: MFADevice)
{-# DEPRECATED mdEnableDate "Use generic-lens or generic-optics with 'enableDate' instead." #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdSerialNumber :: Lens.Lens' MFADevice Lude.Text
mdSerialNumber = Lens.lens (serialNumber :: MFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: MFADevice)
{-# DEPRECATED mdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Lude.FromXML MFADevice where
  parseXML x =
    MFADevice'
      Lude.<$> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "EnableDate")
      Lude.<*> (x Lude..@ "SerialNumber")
