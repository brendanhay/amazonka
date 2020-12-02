{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotSiteWiseAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotSiteWiseAction where

import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to send data from an MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
--
--
-- /See:/ 'iotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { _iswaPutAssetPropertyValueEntries ::
      !(List1 PutAssetPropertyValueEntry),
    _iswaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IotSiteWiseAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iswaPutAssetPropertyValueEntries' - A list of asset property value entries.
--
-- * 'iswaRoleARN' - The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
iotSiteWiseAction ::
  -- | 'iswaPutAssetPropertyValueEntries'
  NonEmpty PutAssetPropertyValueEntry ->
  -- | 'iswaRoleARN'
  Text ->
  IotSiteWiseAction
iotSiteWiseAction pPutAssetPropertyValueEntries_ pRoleARN_ =
  IotSiteWiseAction'
    { _iswaPutAssetPropertyValueEntries =
        _List1 # pPutAssetPropertyValueEntries_,
      _iswaRoleARN = pRoleARN_
    }

-- | A list of asset property value entries.
iswaPutAssetPropertyValueEntries :: Lens' IotSiteWiseAction (NonEmpty PutAssetPropertyValueEntry)
iswaPutAssetPropertyValueEntries = lens _iswaPutAssetPropertyValueEntries (\s a -> s {_iswaPutAssetPropertyValueEntries = a}) . _List1

-- | The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
iswaRoleARN :: Lens' IotSiteWiseAction Text
iswaRoleARN = lens _iswaRoleARN (\s a -> s {_iswaRoleARN = a})

instance FromJSON IotSiteWiseAction where
  parseJSON =
    withObject
      "IotSiteWiseAction"
      ( \x ->
          IotSiteWiseAction'
            <$> (x .: "putAssetPropertyValueEntries") <*> (x .: "roleArn")
      )

instance Hashable IotSiteWiseAction

instance NFData IotSiteWiseAction

instance ToJSON IotSiteWiseAction where
  toJSON IotSiteWiseAction' {..} =
    object
      ( catMaybes
          [ Just
              ( "putAssetPropertyValueEntries"
                  .= _iswaPutAssetPropertyValueEntries
              ),
            Just ("roleArn" .= _iswaRoleARN)
          ]
      )
