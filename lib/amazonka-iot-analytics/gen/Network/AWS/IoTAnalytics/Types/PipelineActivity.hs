{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineActivity where

import Network.AWS.IoTAnalytics.Types.AddAttributesActivity
import Network.AWS.IoTAnalytics.Types.ChannelActivity
import Network.AWS.IoTAnalytics.Types.DatastoreActivity
import Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Network.AWS.IoTAnalytics.Types.FilterActivity
import Network.AWS.IoTAnalytics.Types.LambdaActivity
import Network.AWS.IoTAnalytics.Types.MathActivity
import Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
import Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that performs a transformation on a message.
--
--
--
-- /See:/ 'pipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { _paSelectAttributes ::
      !(Maybe SelectAttributesActivity),
    _paChannel :: !(Maybe ChannelActivity),
    _paAddAttributes :: !(Maybe AddAttributesActivity),
    _paDeviceRegistryEnrich ::
      !(Maybe DeviceRegistryEnrichActivity),
    _paRemoveAttributes :: !(Maybe RemoveAttributesActivity),
    _paLambda :: !(Maybe LambdaActivity),
    _paDatastore :: !(Maybe DatastoreActivity),
    _paDeviceShadowEnrich ::
      !(Maybe DeviceShadowEnrichActivity),
    _paFilter :: !(Maybe FilterActivity),
    _paMath :: !(Maybe MathActivity)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paSelectAttributes' - Creates a new message using only the specified attributes from the original message.
--
-- * 'paChannel' - Determines the source of the messages to be processed.
--
-- * 'paAddAttributes' - Adds other attributes based on existing attributes in the message.
--
-- * 'paDeviceRegistryEnrich' - Adds data from the AWS IoT device registry to your message.
--
-- * 'paRemoveAttributes' - Removes attributes from a message.
--
-- * 'paLambda' - Runs a Lambda function to modify the message.
--
-- * 'paDatastore' - Specifies where to store the processed message data.
--
-- * 'paDeviceShadowEnrich' - Adds information from the AWS IoT Device Shadow service to a message.
--
-- * 'paFilter' - Filters a message based on its attributes.
--
-- * 'paMath' - Computes an arithmetic expression using the message's attributes and adds it to the message.
pipelineActivity ::
  PipelineActivity
pipelineActivity =
  PipelineActivity'
    { _paSelectAttributes = Nothing,
      _paChannel = Nothing,
      _paAddAttributes = Nothing,
      _paDeviceRegistryEnrich = Nothing,
      _paRemoveAttributes = Nothing,
      _paLambda = Nothing,
      _paDatastore = Nothing,
      _paDeviceShadowEnrich = Nothing,
      _paFilter = Nothing,
      _paMath = Nothing
    }

-- | Creates a new message using only the specified attributes from the original message.
paSelectAttributes :: Lens' PipelineActivity (Maybe SelectAttributesActivity)
paSelectAttributes = lens _paSelectAttributes (\s a -> s {_paSelectAttributes = a})

-- | Determines the source of the messages to be processed.
paChannel :: Lens' PipelineActivity (Maybe ChannelActivity)
paChannel = lens _paChannel (\s a -> s {_paChannel = a})

-- | Adds other attributes based on existing attributes in the message.
paAddAttributes :: Lens' PipelineActivity (Maybe AddAttributesActivity)
paAddAttributes = lens _paAddAttributes (\s a -> s {_paAddAttributes = a})

-- | Adds data from the AWS IoT device registry to your message.
paDeviceRegistryEnrich :: Lens' PipelineActivity (Maybe DeviceRegistryEnrichActivity)
paDeviceRegistryEnrich = lens _paDeviceRegistryEnrich (\s a -> s {_paDeviceRegistryEnrich = a})

-- | Removes attributes from a message.
paRemoveAttributes :: Lens' PipelineActivity (Maybe RemoveAttributesActivity)
paRemoveAttributes = lens _paRemoveAttributes (\s a -> s {_paRemoveAttributes = a})

-- | Runs a Lambda function to modify the message.
paLambda :: Lens' PipelineActivity (Maybe LambdaActivity)
paLambda = lens _paLambda (\s a -> s {_paLambda = a})

-- | Specifies where to store the processed message data.
paDatastore :: Lens' PipelineActivity (Maybe DatastoreActivity)
paDatastore = lens _paDatastore (\s a -> s {_paDatastore = a})

-- | Adds information from the AWS IoT Device Shadow service to a message.
paDeviceShadowEnrich :: Lens' PipelineActivity (Maybe DeviceShadowEnrichActivity)
paDeviceShadowEnrich = lens _paDeviceShadowEnrich (\s a -> s {_paDeviceShadowEnrich = a})

-- | Filters a message based on its attributes.
paFilter :: Lens' PipelineActivity (Maybe FilterActivity)
paFilter = lens _paFilter (\s a -> s {_paFilter = a})

-- | Computes an arithmetic expression using the message's attributes and adds it to the message.
paMath :: Lens' PipelineActivity (Maybe MathActivity)
paMath = lens _paMath (\s a -> s {_paMath = a})

instance FromJSON PipelineActivity where
  parseJSON =
    withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            <$> (x .:? "selectAttributes")
            <*> (x .:? "channel")
            <*> (x .:? "addAttributes")
            <*> (x .:? "deviceRegistryEnrich")
            <*> (x .:? "removeAttributes")
            <*> (x .:? "lambda")
            <*> (x .:? "datastore")
            <*> (x .:? "deviceShadowEnrich")
            <*> (x .:? "filter")
            <*> (x .:? "math")
      )

instance Hashable PipelineActivity

instance NFData PipelineActivity

instance ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    object
      ( catMaybes
          [ ("selectAttributes" .=) <$> _paSelectAttributes,
            ("channel" .=) <$> _paChannel,
            ("addAttributes" .=) <$> _paAddAttributes,
            ("deviceRegistryEnrich" .=) <$> _paDeviceRegistryEnrich,
            ("removeAttributes" .=) <$> _paRemoveAttributes,
            ("lambda" .=) <$> _paLambda,
            ("datastore" .=) <$> _paDatastore,
            ("deviceShadowEnrich" .=) <$> _paDeviceShadowEnrich,
            ("filter" .=) <$> _paFilter,
            ("math" .=) <$> _paMath
          ]
      )
