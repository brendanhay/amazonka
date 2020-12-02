{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.MFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.MFADevice where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an MFA device.
--
--
-- This data type is used as a response element in the 'ListMFADevices' operation.
--
--
-- /See:/ 'mfaDevice' smart constructor.
data MFADevice = MFADevice'
  { _mdUserName :: !Text,
    _mdSerialNumber :: !Text,
    _mdEnableDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdUserName' - The user with whom the MFA device is associated.
--
-- * 'mdSerialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
--
-- * 'mdEnableDate' - The date when the MFA device was enabled for the user.
mfaDevice ::
  -- | 'mdUserName'
  Text ->
  -- | 'mdSerialNumber'
  Text ->
  -- | 'mdEnableDate'
  UTCTime ->
  MFADevice
mfaDevice pUserName_ pSerialNumber_ pEnableDate_ =
  MFADevice'
    { _mdUserName = pUserName_,
      _mdSerialNumber = pSerialNumber_,
      _mdEnableDate = _Time # pEnableDate_
    }

-- | The user with whom the MFA device is associated.
mdUserName :: Lens' MFADevice Text
mdUserName = lens _mdUserName (\s a -> s {_mdUserName = a})

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN.
mdSerialNumber :: Lens' MFADevice Text
mdSerialNumber = lens _mdSerialNumber (\s a -> s {_mdSerialNumber = a})

-- | The date when the MFA device was enabled for the user.
mdEnableDate :: Lens' MFADevice UTCTime
mdEnableDate = lens _mdEnableDate (\s a -> s {_mdEnableDate = a}) . _Time

instance FromXML MFADevice where
  parseXML x =
    MFADevice'
      <$> (x .@ "UserName") <*> (x .@ "SerialNumber") <*> (x .@ "EnableDate")

instance Hashable MFADevice

instance NFData MFADevice
