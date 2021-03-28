{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.VirtualMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.VirtualMFADevice
  ( VirtualMFADevice (..)
  -- * Smart constructor
  , mkVirtualMFADevice
  -- * Lenses
  , vmfadSerialNumber
  , vmfadBase32StringSeed
  , vmfadEnableDate
  , vmfadQRCodePNG
  , vmfadUser
  ) where

import qualified Network.AWS.IAM.Types.SerialNumberType as Types
import qualified Network.AWS.IAM.Types.User as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a virtual MFA device.
--
-- /See:/ 'mkVirtualMFADevice' smart constructor.
data VirtualMFADevice = VirtualMFADevice'
  { serialNumber :: Types.SerialNumberType
    -- ^ The serial number associated with @VirtualMFADevice@ .
  , base32StringSeed :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded. 
  , enableDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time on which the virtual MFA device was enabled.
  , qRCodePNG :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded. 
  , user :: Core.Maybe Types.User
    -- ^ The IAM user associated with this virtual MFA device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VirtualMFADevice' value with any optional fields omitted.
mkVirtualMFADevice
    :: Types.SerialNumberType -- ^ 'serialNumber'
    -> VirtualMFADevice
mkVirtualMFADevice serialNumber
  = VirtualMFADevice'{serialNumber, base32StringSeed = Core.Nothing,
                      enableDate = Core.Nothing, qRCodePNG = Core.Nothing,
                      user = Core.Nothing}

-- | The serial number associated with @VirtualMFADevice@ .
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmfadSerialNumber :: Lens.Lens' VirtualMFADevice Types.SerialNumberType
vmfadSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE vmfadSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'base32StringSeed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmfadBase32StringSeed :: Lens.Lens' VirtualMFADevice (Core.Maybe (Core.Sensitive Core.Base64))
vmfadBase32StringSeed = Lens.field @"base32StringSeed"
{-# INLINEABLE vmfadBase32StringSeed #-}
{-# DEPRECATED base32StringSeed "Use generic-lens or generic-optics with 'base32StringSeed' instead"  #-}

-- | The date and time on which the virtual MFA device was enabled.
--
-- /Note:/ Consider using 'enableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmfadEnableDate :: Lens.Lens' VirtualMFADevice (Core.Maybe Core.UTCTime)
vmfadEnableDate = Lens.field @"enableDate"
{-# INLINEABLE vmfadEnableDate #-}
{-# DEPRECATED enableDate "Use generic-lens or generic-optics with 'enableDate' instead"  #-}

-- | A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'qRCodePNG' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmfadQRCodePNG :: Lens.Lens' VirtualMFADevice (Core.Maybe (Core.Sensitive Core.Base64))
vmfadQRCodePNG = Lens.field @"qRCodePNG"
{-# INLINEABLE vmfadQRCodePNG #-}
{-# DEPRECATED qRCodePNG "Use generic-lens or generic-optics with 'qRCodePNG' instead"  #-}

-- | The IAM user associated with this virtual MFA device.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmfadUser :: Lens.Lens' VirtualMFADevice (Core.Maybe Types.User)
vmfadUser = Lens.field @"user"
{-# INLINEABLE vmfadUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

instance Core.FromXML VirtualMFADevice where
        parseXML x
          = VirtualMFADevice' Core.<$>
              (x Core..@ "SerialNumber") Core.<*> x Core..@? "Base32StringSeed"
                Core.<*> x Core..@? "EnableDate"
                Core.<*> x Core..@? "QRCodePNG"
                Core.<*> x Core..@? "User"
