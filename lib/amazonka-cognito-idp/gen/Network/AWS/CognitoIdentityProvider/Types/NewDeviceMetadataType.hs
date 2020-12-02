{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The new device metadata type.
--
--
--
-- /See:/ 'newDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { _ndmtDeviceGroupKey ::
      !(Maybe Text),
    _ndmtDeviceKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewDeviceMetadataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ndmtDeviceGroupKey' - The device group key.
--
-- * 'ndmtDeviceKey' - The device key.
newDeviceMetadataType ::
  NewDeviceMetadataType
newDeviceMetadataType =
  NewDeviceMetadataType'
    { _ndmtDeviceGroupKey = Nothing,
      _ndmtDeviceKey = Nothing
    }

-- | The device group key.
ndmtDeviceGroupKey :: Lens' NewDeviceMetadataType (Maybe Text)
ndmtDeviceGroupKey = lens _ndmtDeviceGroupKey (\s a -> s {_ndmtDeviceGroupKey = a})

-- | The device key.
ndmtDeviceKey :: Lens' NewDeviceMetadataType (Maybe Text)
ndmtDeviceKey = lens _ndmtDeviceKey (\s a -> s {_ndmtDeviceKey = a})

instance FromJSON NewDeviceMetadataType where
  parseJSON =
    withObject
      "NewDeviceMetadataType"
      ( \x ->
          NewDeviceMetadataType'
            <$> (x .:? "DeviceGroupKey") <*> (x .:? "DeviceKey")
      )

instance Hashable NewDeviceMetadataType

instance NFData NewDeviceMetadataType
