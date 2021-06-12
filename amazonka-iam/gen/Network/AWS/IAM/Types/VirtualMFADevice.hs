{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.VirtualMFADevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.VirtualMFADevice where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.Tag
import Network.AWS.IAM.Types.User
import qualified Network.AWS.Lens as Lens

-- | Contains information about a virtual MFA device.
--
-- /See:/ 'newVirtualMFADevice' smart constructor.
data VirtualMFADevice = VirtualMFADevice'
  { -- | The IAM user associated with this virtual MFA device.
    user :: Core.Maybe User,
    -- | The date and time on which the virtual MFA device was enabled.
    enableDate :: Core.Maybe Core.ISO8601,
    -- | A QR code PNG image that encodes
    -- @otpauth:\/\/totp\/$virtualMFADeviceName\@$AccountName?secret=$Base32String@
    -- where @$virtualMFADeviceName@ is one of the create call arguments.
    -- @AccountName@ is the user name if set (otherwise, the account ID
    -- otherwise), and @Base32String@ is the seed in base32 format. The
    -- @Base32String@ value is base64-encoded.
    qRCodePNG :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | A list of tags that are attached to the virtual MFA device. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The base32 seed defined as specified in
    -- <https://tools.ietf.org/html/rfc3548.txt RFC3548>. The
    -- @Base32StringSeed@ is base64-encoded.
    base32StringSeed :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | The serial number associated with @VirtualMFADevice@.
    serialNumber :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'VirtualMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'virtualMFADevice_user' - The IAM user associated with this virtual MFA device.
--
-- 'enableDate', 'virtualMFADevice_enableDate' - The date and time on which the virtual MFA device was enabled.
--
-- 'qRCodePNG', 'virtualMFADevice_qRCodePNG' - A QR code PNG image that encodes
-- @otpauth:\/\/totp\/$virtualMFADeviceName\@$AccountName?secret=$Base32String@
-- where @$virtualMFADeviceName@ is one of the create call arguments.
-- @AccountName@ is the user name if set (otherwise, the account ID
-- otherwise), and @Base32String@ is the seed in base32 format. The
-- @Base32String@ value is base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'tags', 'virtualMFADevice_tags' - A list of tags that are attached to the virtual MFA device. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'base32StringSeed', 'virtualMFADevice_base32StringSeed' - The base32 seed defined as specified in
-- <https://tools.ietf.org/html/rfc3548.txt RFC3548>. The
-- @Base32StringSeed@ is base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'serialNumber', 'virtualMFADevice_serialNumber' - The serial number associated with @VirtualMFADevice@.
newVirtualMFADevice ::
  -- | 'serialNumber'
  Core.Text ->
  VirtualMFADevice
newVirtualMFADevice pSerialNumber_ =
  VirtualMFADevice'
    { user = Core.Nothing,
      enableDate = Core.Nothing,
      qRCodePNG = Core.Nothing,
      tags = Core.Nothing,
      base32StringSeed = Core.Nothing,
      serialNumber = pSerialNumber_
    }

-- | The IAM user associated with this virtual MFA device.
virtualMFADevice_user :: Lens.Lens' VirtualMFADevice (Core.Maybe User)
virtualMFADevice_user = Lens.lens (\VirtualMFADevice' {user} -> user) (\s@VirtualMFADevice' {} a -> s {user = a} :: VirtualMFADevice)

-- | The date and time on which the virtual MFA device was enabled.
virtualMFADevice_enableDate :: Lens.Lens' VirtualMFADevice (Core.Maybe Core.UTCTime)
virtualMFADevice_enableDate = Lens.lens (\VirtualMFADevice' {enableDate} -> enableDate) (\s@VirtualMFADevice' {} a -> s {enableDate = a} :: VirtualMFADevice) Core.. Lens.mapping Core._Time

-- | A QR code PNG image that encodes
-- @otpauth:\/\/totp\/$virtualMFADeviceName\@$AccountName?secret=$Base32String@
-- where @$virtualMFADeviceName@ is one of the create call arguments.
-- @AccountName@ is the user name if set (otherwise, the account ID
-- otherwise), and @Base32String@ is the seed in base32 format. The
-- @Base32String@ value is base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
virtualMFADevice_qRCodePNG :: Lens.Lens' VirtualMFADevice (Core.Maybe Core.ByteString)
virtualMFADevice_qRCodePNG = Lens.lens (\VirtualMFADevice' {qRCodePNG} -> qRCodePNG) (\s@VirtualMFADevice' {} a -> s {qRCodePNG = a} :: VirtualMFADevice) Core.. Lens.mapping (Core._Sensitive Core.. Core._Base64)

-- | A list of tags that are attached to the virtual MFA device. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
virtualMFADevice_tags :: Lens.Lens' VirtualMFADevice (Core.Maybe [Tag])
virtualMFADevice_tags = Lens.lens (\VirtualMFADevice' {tags} -> tags) (\s@VirtualMFADevice' {} a -> s {tags = a} :: VirtualMFADevice) Core.. Lens.mapping Lens._Coerce

-- | The base32 seed defined as specified in
-- <https://tools.ietf.org/html/rfc3548.txt RFC3548>. The
-- @Base32StringSeed@ is base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
virtualMFADevice_base32StringSeed :: Lens.Lens' VirtualMFADevice (Core.Maybe Core.ByteString)
virtualMFADevice_base32StringSeed = Lens.lens (\VirtualMFADevice' {base32StringSeed} -> base32StringSeed) (\s@VirtualMFADevice' {} a -> s {base32StringSeed = a} :: VirtualMFADevice) Core.. Lens.mapping (Core._Sensitive Core.. Core._Base64)

-- | The serial number associated with @VirtualMFADevice@.
virtualMFADevice_serialNumber :: Lens.Lens' VirtualMFADevice Core.Text
virtualMFADevice_serialNumber = Lens.lens (\VirtualMFADevice' {serialNumber} -> serialNumber) (\s@VirtualMFADevice' {} a -> s {serialNumber = a} :: VirtualMFADevice)

instance Core.FromXML VirtualMFADevice where
  parseXML x =
    VirtualMFADevice'
      Core.<$> (x Core..@? "User")
      Core.<*> (x Core..@? "EnableDate")
      Core.<*> (x Core..@? "QRCodePNG")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Base32StringSeed")
      Core.<*> (x Core..@ "SerialNumber")

instance Core.Hashable VirtualMFADevice

instance Core.NFData VirtualMFADevice
