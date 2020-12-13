{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.VirtualMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.VirtualMFADevice
  ( VirtualMFADevice (..),

    -- * Smart constructor
    mkVirtualMFADevice,

    -- * Lenses
    vmdQRCodePNG,
    vmdBase32StringSeed,
    vmdUser,
    vmdEnableDate,
    vmdSerialNumber,
  )
where

import Network.AWS.IAM.Types.User
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a virtual MFA device.
--
-- /See:/ 'mkVirtualMFADevice' smart constructor.
data VirtualMFADevice = VirtualMFADevice'
  { -- | A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded.
    qRCodePNG :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded.
    base32StringSeed :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | The IAM user associated with this virtual MFA device.
    user :: Lude.Maybe User,
    -- | The date and time on which the virtual MFA device was enabled.
    enableDate :: Lude.Maybe Lude.DateTime,
    -- | The serial number associated with @VirtualMFADevice@ .
    serialNumber :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VirtualMFADevice' with the minimum fields required to make a request.
--
-- * 'qRCodePNG' - A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded.
-- * 'base32StringSeed' - The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded.
-- * 'user' - The IAM user associated with this virtual MFA device.
-- * 'enableDate' - The date and time on which the virtual MFA device was enabled.
-- * 'serialNumber' - The serial number associated with @VirtualMFADevice@ .
mkVirtualMFADevice ::
  -- | 'serialNumber'
  Lude.Text ->
  VirtualMFADevice
mkVirtualMFADevice pSerialNumber_ =
  VirtualMFADevice'
    { qRCodePNG = Lude.Nothing,
      base32StringSeed = Lude.Nothing,
      user = Lude.Nothing,
      enableDate = Lude.Nothing,
      serialNumber = pSerialNumber_
    }

-- | A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'qRCodePNG' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmdQRCodePNG :: Lens.Lens' VirtualMFADevice (Lude.Maybe (Lude.Sensitive Lude.Base64))
vmdQRCodePNG = Lens.lens (qRCodePNG :: VirtualMFADevice -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {qRCodePNG = a} :: VirtualMFADevice)
{-# DEPRECATED vmdQRCodePNG "Use generic-lens or generic-optics with 'qRCodePNG' instead." #-}

-- | The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'base32StringSeed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmdBase32StringSeed :: Lens.Lens' VirtualMFADevice (Lude.Maybe (Lude.Sensitive Lude.Base64))
vmdBase32StringSeed = Lens.lens (base32StringSeed :: VirtualMFADevice -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {base32StringSeed = a} :: VirtualMFADevice)
{-# DEPRECATED vmdBase32StringSeed "Use generic-lens or generic-optics with 'base32StringSeed' instead." #-}

-- | The IAM user associated with this virtual MFA device.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmdUser :: Lens.Lens' VirtualMFADevice (Lude.Maybe User)
vmdUser = Lens.lens (user :: VirtualMFADevice -> Lude.Maybe User) (\s a -> s {user = a} :: VirtualMFADevice)
{-# DEPRECATED vmdUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The date and time on which the virtual MFA device was enabled.
--
-- /Note:/ Consider using 'enableDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmdEnableDate :: Lens.Lens' VirtualMFADevice (Lude.Maybe Lude.DateTime)
vmdEnableDate = Lens.lens (enableDate :: VirtualMFADevice -> Lude.Maybe Lude.DateTime) (\s a -> s {enableDate = a} :: VirtualMFADevice)
{-# DEPRECATED vmdEnableDate "Use generic-lens or generic-optics with 'enableDate' instead." #-}

-- | The serial number associated with @VirtualMFADevice@ .
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmdSerialNumber :: Lens.Lens' VirtualMFADevice Lude.Text
vmdSerialNumber = Lens.lens (serialNumber :: VirtualMFADevice -> Lude.Text) (\s a -> s {serialNumber = a} :: VirtualMFADevice)
{-# DEPRECATED vmdSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

instance Lude.FromXML VirtualMFADevice where
  parseXML x =
    VirtualMFADevice'
      Lude.<$> (x Lude..@? "QRCodePNG")
      Lude.<*> (x Lude..@? "Base32StringSeed")
      Lude.<*> (x Lude..@? "User")
      Lude.<*> (x Lude..@? "EnableDate")
      Lude.<*> (x Lude..@ "SerialNumber")
