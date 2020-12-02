{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Device where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a device.
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dSyncShadow :: !(Maybe Bool),
    _dThingARN :: !Text,
    _dId :: !Text,
    _dCertificateARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSyncShadow' - If true, the device's local shadow will be automatically synced with the cloud.
--
-- * 'dThingARN' - The thing ARN of the device.
--
-- * 'dId' - A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- * 'dCertificateARN' - The ARN of the certificate associated with the device.
device ::
  -- | 'dThingARN'
  Text ->
  -- | 'dId'
  Text ->
  -- | 'dCertificateARN'
  Text ->
  Device
device pThingARN_ pId_ pCertificateARN_ =
  Device'
    { _dSyncShadow = Nothing,
      _dThingARN = pThingARN_,
      _dId = pId_,
      _dCertificateARN = pCertificateARN_
    }

-- | If true, the device's local shadow will be automatically synced with the cloud.
dSyncShadow :: Lens' Device (Maybe Bool)
dSyncShadow = lens _dSyncShadow (\s a -> s {_dSyncShadow = a})

-- | The thing ARN of the device.
dThingARN :: Lens' Device Text
dThingARN = lens _dThingARN (\s a -> s {_dThingARN = a})

-- | A descriptive or arbitrary ID for the device. This value must be unique within the device definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
dId :: Lens' Device Text
dId = lens _dId (\s a -> s {_dId = a})

-- | The ARN of the certificate associated with the device.
dCertificateARN :: Lens' Device Text
dCertificateARN = lens _dCertificateARN (\s a -> s {_dCertificateARN = a})

instance FromJSON Device where
  parseJSON =
    withObject
      "Device"
      ( \x ->
          Device'
            <$> (x .:? "SyncShadow")
            <*> (x .: "ThingArn")
            <*> (x .: "Id")
            <*> (x .: "CertificateArn")
      )

instance Hashable Device

instance NFData Device

instance ToJSON Device where
  toJSON Device' {..} =
    object
      ( catMaybes
          [ ("SyncShadow" .=) <$> _dSyncShadow,
            Just ("ThingArn" .= _dThingARN),
            Just ("Id" .= _dId),
            Just ("CertificateArn" .= _dCertificateARN)
          ]
      )
