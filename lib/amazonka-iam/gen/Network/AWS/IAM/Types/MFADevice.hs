{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.MFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.MFADevice
  ( MFADevice (..)
  -- * Smart constructor
  , mkMFADevice
  -- * Lenses
  , mfadUserName
  , mfadSerialNumber
  , mfadEnableDate
  ) where

import qualified Network.AWS.IAM.Types.SerialNumber as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an MFA device.
--
-- This data type is used as a response element in the 'ListMFADevices' operation.
--
-- /See:/ 'mkMFADevice' smart constructor.
data MFADevice = MFADevice'
  { userName :: Types.UserName
    -- ^ The user with whom the MFA device is associated.
  , serialNumber :: Types.SerialNumber
    -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
  , enableDate :: Core.UTCTime
    -- ^ The date when the MFA device was enabled for the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MFADevice' value with any optional fields omitted.
mkMFADevice
    :: Types.UserName -- ^ 'userName'
    -> Types.SerialNumber -- ^ 'serialNumber'
    -> Core.UTCTime -- ^ 'enableDate'
    -> MFADevice
mkMFADevice userName serialNumber enableDate
  = MFADevice'{userName, serialNumber, enableDate}

-- | The user with whom the MFA device is associated.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfadUserName :: Lens.Lens' MFADevice Types.UserName
mfadUserName = Lens.field @"userName"
{-# INLINEABLE mfadUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfadSerialNumber :: Lens.Lens' MFADevice Types.SerialNumber
mfadSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE mfadSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The date when the MFA device was enabled for the user.
--
-- /Note:/ Consider using 'enableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfadEnableDate :: Lens.Lens' MFADevice Core.UTCTime
mfadEnableDate = Lens.field @"enableDate"
{-# INLINEABLE mfadEnableDate #-}
{-# DEPRECATED enableDate "Use generic-lens or generic-optics with 'enableDate' instead"  #-}

instance Core.FromXML MFADevice where
        parseXML x
          = MFADevice' Core.<$>
              (x Core..@ "UserName") Core.<*> x Core..@ "SerialNumber" Core.<*>
                x Core..@ "EnableDate"
