{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.VirtualMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.VirtualMFADevice where

import Network.AWS.IAM.Types.User
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a virtual MFA device.
--
--
--
-- /See:/ 'virtualMFADevice' smart constructor.
data VirtualMFADevice = VirtualMFADevice'
  { _vmdQRCodePNG ::
      !(Maybe (Sensitive Base64)),
    _vmdBase32StringSeed :: !(Maybe (Sensitive Base64)),
    _vmdUser :: !(Maybe User),
    _vmdEnableDate :: !(Maybe ISO8601),
    _vmdSerialNumber :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'VirtualMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmdQRCodePNG' - A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'vmdBase32StringSeed' - The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'vmdUser' - The IAM user associated with this virtual MFA device.
--
-- * 'vmdEnableDate' - The date and time on which the virtual MFA device was enabled.
--
-- * 'vmdSerialNumber' - The serial number associated with @VirtualMFADevice@ .
virtualMFADevice ::
  -- | 'vmdSerialNumber'
  Text ->
  VirtualMFADevice
virtualMFADevice pSerialNumber_ =
  VirtualMFADevice'
    { _vmdQRCodePNG = Nothing,
      _vmdBase32StringSeed = Nothing,
      _vmdUser = Nothing,
      _vmdEnableDate = Nothing,
      _vmdSerialNumber = pSerialNumber_
    }

-- | A QR code PNG image that encodes @otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String@ where @> virtualMFADeviceName@ is one of the create call arguments. @AccountName@ is the user name if set (otherwise, the account ID otherwise), and @Base32String@ is the seed in base32 format. The @Base32String@ value is base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
vmdQRCodePNG :: Lens' VirtualMFADevice (Maybe ByteString)
vmdQRCodePNG = lens _vmdQRCodePNG (\s a -> s {_vmdQRCodePNG = a}) . mapping (_Sensitive . _Base64)

-- | The base32 seed defined as specified in <https://tools.ietf.org/html/rfc3548.txt RFC3548> . The @Base32StringSeed@ is base64-encoded. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
vmdBase32StringSeed :: Lens' VirtualMFADevice (Maybe ByteString)
vmdBase32StringSeed = lens _vmdBase32StringSeed (\s a -> s {_vmdBase32StringSeed = a}) . mapping (_Sensitive . _Base64)

-- | The IAM user associated with this virtual MFA device.
vmdUser :: Lens' VirtualMFADevice (Maybe User)
vmdUser = lens _vmdUser (\s a -> s {_vmdUser = a})

-- | The date and time on which the virtual MFA device was enabled.
vmdEnableDate :: Lens' VirtualMFADevice (Maybe UTCTime)
vmdEnableDate = lens _vmdEnableDate (\s a -> s {_vmdEnableDate = a}) . mapping _Time

-- | The serial number associated with @VirtualMFADevice@ .
vmdSerialNumber :: Lens' VirtualMFADevice Text
vmdSerialNumber = lens _vmdSerialNumber (\s a -> s {_vmdSerialNumber = a})

instance FromXML VirtualMFADevice where
  parseXML x =
    VirtualMFADevice'
      <$> (x .@? "QRCodePNG")
      <*> (x .@? "Base32StringSeed")
      <*> (x .@? "User")
      <*> (x .@? "EnableDate")
      <*> (x .@ "SerialNumber")

instance Hashable VirtualMFADevice

instance NFData VirtualMFADevice
