{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that adds information from the AWS IoT Device Shadow service to a message.
--
--
--
-- /See:/ 'deviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { _dseaNext ::
      !(Maybe Text),
    _dseaName :: !Text,
    _dseaAttribute :: !Text,
    _dseaThingName :: !Text,
    _dseaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceShadowEnrichActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseaNext' - The next activity in the pipeline.
--
-- * 'dseaName' - The name of the @deviceShadowEnrich@ activity.
--
-- * 'dseaAttribute' - The name of the attribute that is added to the message.
--
-- * 'dseaThingName' - The name of the IoT device whose shadow information is added to the message.
--
-- * 'dseaRoleARN' - The ARN of the role that allows access to the device's shadow.
deviceShadowEnrichActivity ::
  -- | 'dseaName'
  Text ->
  -- | 'dseaAttribute'
  Text ->
  -- | 'dseaThingName'
  Text ->
  -- | 'dseaRoleARN'
  Text ->
  DeviceShadowEnrichActivity
deviceShadowEnrichActivity pName_ pAttribute_ pThingName_ pRoleARN_ =
  DeviceShadowEnrichActivity'
    { _dseaNext = Nothing,
      _dseaName = pName_,
      _dseaAttribute = pAttribute_,
      _dseaThingName = pThingName_,
      _dseaRoleARN = pRoleARN_
    }

-- | The next activity in the pipeline.
dseaNext :: Lens' DeviceShadowEnrichActivity (Maybe Text)
dseaNext = lens _dseaNext (\s a -> s {_dseaNext = a})

-- | The name of the @deviceShadowEnrich@ activity.
dseaName :: Lens' DeviceShadowEnrichActivity Text
dseaName = lens _dseaName (\s a -> s {_dseaName = a})

-- | The name of the attribute that is added to the message.
dseaAttribute :: Lens' DeviceShadowEnrichActivity Text
dseaAttribute = lens _dseaAttribute (\s a -> s {_dseaAttribute = a})

-- | The name of the IoT device whose shadow information is added to the message.
dseaThingName :: Lens' DeviceShadowEnrichActivity Text
dseaThingName = lens _dseaThingName (\s a -> s {_dseaThingName = a})

-- | The ARN of the role that allows access to the device's shadow.
dseaRoleARN :: Lens' DeviceShadowEnrichActivity Text
dseaRoleARN = lens _dseaRoleARN (\s a -> s {_dseaRoleARN = a})

instance FromJSON DeviceShadowEnrichActivity where
  parseJSON =
    withObject
      "DeviceShadowEnrichActivity"
      ( \x ->
          DeviceShadowEnrichActivity'
            <$> (x .:? "next")
            <*> (x .: "name")
            <*> (x .: "attribute")
            <*> (x .: "thingName")
            <*> (x .: "roleArn")
      )

instance Hashable DeviceShadowEnrichActivity

instance NFData DeviceShadowEnrichActivity

instance ToJSON DeviceShadowEnrichActivity where
  toJSON DeviceShadowEnrichActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _dseaNext,
            Just ("name" .= _dseaName),
            Just ("attribute" .= _dseaAttribute),
            Just ("thingName" .= _dseaThingName),
            Just ("roleArn" .= _dseaRoleARN)
          ]
      )
