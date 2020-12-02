{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceType where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The device type.
--
--
--
-- /See:/ 'deviceType' smart constructor.
data DeviceType = DeviceType'
  { _dtDeviceLastModifiedDate ::
      !(Maybe POSIX),
    _dtDeviceCreateDate :: !(Maybe POSIX),
    _dtDeviceAttributes :: !(Maybe [AttributeType]),
    _dtDeviceKey :: !(Maybe Text),
    _dtDeviceLastAuthenticatedDate :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDeviceLastModifiedDate' - The last modified date of the device.
--
-- * 'dtDeviceCreateDate' - The creation date of the device.
--
-- * 'dtDeviceAttributes' - The device attributes.
--
-- * 'dtDeviceKey' - The device key.
--
-- * 'dtDeviceLastAuthenticatedDate' - The date in which the device was last authenticated.
deviceType ::
  DeviceType
deviceType =
  DeviceType'
    { _dtDeviceLastModifiedDate = Nothing,
      _dtDeviceCreateDate = Nothing,
      _dtDeviceAttributes = Nothing,
      _dtDeviceKey = Nothing,
      _dtDeviceLastAuthenticatedDate = Nothing
    }

-- | The last modified date of the device.
dtDeviceLastModifiedDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceLastModifiedDate = lens _dtDeviceLastModifiedDate (\s a -> s {_dtDeviceLastModifiedDate = a}) . mapping _Time

-- | The creation date of the device.
dtDeviceCreateDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceCreateDate = lens _dtDeviceCreateDate (\s a -> s {_dtDeviceCreateDate = a}) . mapping _Time

-- | The device attributes.
dtDeviceAttributes :: Lens' DeviceType [AttributeType]
dtDeviceAttributes = lens _dtDeviceAttributes (\s a -> s {_dtDeviceAttributes = a}) . _Default . _Coerce

-- | The device key.
dtDeviceKey :: Lens' DeviceType (Maybe Text)
dtDeviceKey = lens _dtDeviceKey (\s a -> s {_dtDeviceKey = a})

-- | The date in which the device was last authenticated.
dtDeviceLastAuthenticatedDate :: Lens' DeviceType (Maybe UTCTime)
dtDeviceLastAuthenticatedDate = lens _dtDeviceLastAuthenticatedDate (\s a -> s {_dtDeviceLastAuthenticatedDate = a}) . mapping _Time

instance FromJSON DeviceType where
  parseJSON =
    withObject
      "DeviceType"
      ( \x ->
          DeviceType'
            <$> (x .:? "DeviceLastModifiedDate")
            <*> (x .:? "DeviceCreateDate")
            <*> (x .:? "DeviceAttributes" .!= mempty)
            <*> (x .:? "DeviceKey")
            <*> (x .:? "DeviceLastAuthenticatedDate")
      )

instance Hashable DeviceType

instance NFData DeviceType
