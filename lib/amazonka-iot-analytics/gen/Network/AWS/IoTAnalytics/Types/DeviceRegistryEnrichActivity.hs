{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that adds data from the AWS IoT device registry to your message.
--
--
--
-- /See:/ 'deviceRegistryEnrichActivity' smart constructor.
data DeviceRegistryEnrichActivity = DeviceRegistryEnrichActivity'
  { _dreaNext ::
      !(Maybe Text),
    _dreaName :: !Text,
    _dreaAttribute :: !Text,
    _dreaThingName :: !Text,
    _dreaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceRegistryEnrichActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreaNext' - The next activity in the pipeline.
--
-- * 'dreaName' - The name of the @deviceRegistryEnrich@ activity.
--
-- * 'dreaAttribute' - The name of the attribute that is added to the message.
--
-- * 'dreaThingName' - The name of the IoT device whose registry information is added to the message.
--
-- * 'dreaRoleARN' - The ARN of the role that allows access to the device's registry information.
deviceRegistryEnrichActivity ::
  -- | 'dreaName'
  Text ->
  -- | 'dreaAttribute'
  Text ->
  -- | 'dreaThingName'
  Text ->
  -- | 'dreaRoleARN'
  Text ->
  DeviceRegistryEnrichActivity
deviceRegistryEnrichActivity
  pName_
  pAttribute_
  pThingName_
  pRoleARN_ =
    DeviceRegistryEnrichActivity'
      { _dreaNext = Nothing,
        _dreaName = pName_,
        _dreaAttribute = pAttribute_,
        _dreaThingName = pThingName_,
        _dreaRoleARN = pRoleARN_
      }

-- | The next activity in the pipeline.
dreaNext :: Lens' DeviceRegistryEnrichActivity (Maybe Text)
dreaNext = lens _dreaNext (\s a -> s {_dreaNext = a})

-- | The name of the @deviceRegistryEnrich@ activity.
dreaName :: Lens' DeviceRegistryEnrichActivity Text
dreaName = lens _dreaName (\s a -> s {_dreaName = a})

-- | The name of the attribute that is added to the message.
dreaAttribute :: Lens' DeviceRegistryEnrichActivity Text
dreaAttribute = lens _dreaAttribute (\s a -> s {_dreaAttribute = a})

-- | The name of the IoT device whose registry information is added to the message.
dreaThingName :: Lens' DeviceRegistryEnrichActivity Text
dreaThingName = lens _dreaThingName (\s a -> s {_dreaThingName = a})

-- | The ARN of the role that allows access to the device's registry information.
dreaRoleARN :: Lens' DeviceRegistryEnrichActivity Text
dreaRoleARN = lens _dreaRoleARN (\s a -> s {_dreaRoleARN = a})

instance FromJSON DeviceRegistryEnrichActivity where
  parseJSON =
    withObject
      "DeviceRegistryEnrichActivity"
      ( \x ->
          DeviceRegistryEnrichActivity'
            <$> (x .:? "next")
            <*> (x .: "name")
            <*> (x .: "attribute")
            <*> (x .: "thingName")
            <*> (x .: "roleArn")
      )

instance Hashable DeviceRegistryEnrichActivity

instance NFData DeviceRegistryEnrichActivity

instance ToJSON DeviceRegistryEnrichActivity where
  toJSON DeviceRegistryEnrichActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _dreaNext,
            Just ("name" .= _dreaName),
            Just ("attribute" .= _dreaAttribute),
            Just ("thingName" .= _dreaThingName),
            Just ("roleArn" .= _dreaRoleARN)
          ]
      )
