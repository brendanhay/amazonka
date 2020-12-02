{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination where

import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The destination to which dataset contents are delivered.
--
--
--
-- /See:/ 'datasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { _dcddS3DestinationConfiguration ::
      !( Maybe
           S3DestinationConfiguration
       ),
    _dcddIotEventsDestinationConfiguration ::
      !( Maybe
           IotEventsDestinationConfiguration
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetContentDeliveryDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcddS3DestinationConfiguration' - Configuration information for delivery of dataset contents to Amazon S3.
--
-- * 'dcddIotEventsDestinationConfiguration' - Configuration information for delivery of dataset contents to AWS IoT Events.
datasetContentDeliveryDestination ::
  DatasetContentDeliveryDestination
datasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    { _dcddS3DestinationConfiguration =
        Nothing,
      _dcddIotEventsDestinationConfiguration = Nothing
    }

-- | Configuration information for delivery of dataset contents to Amazon S3.
dcddS3DestinationConfiguration :: Lens' DatasetContentDeliveryDestination (Maybe S3DestinationConfiguration)
dcddS3DestinationConfiguration = lens _dcddS3DestinationConfiguration (\s a -> s {_dcddS3DestinationConfiguration = a})

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
dcddIotEventsDestinationConfiguration :: Lens' DatasetContentDeliveryDestination (Maybe IotEventsDestinationConfiguration)
dcddIotEventsDestinationConfiguration = lens _dcddIotEventsDestinationConfiguration (\s a -> s {_dcddIotEventsDestinationConfiguration = a})

instance FromJSON DatasetContentDeliveryDestination where
  parseJSON =
    withObject
      "DatasetContentDeliveryDestination"
      ( \x ->
          DatasetContentDeliveryDestination'
            <$> (x .:? "s3DestinationConfiguration")
            <*> (x .:? "iotEventsDestinationConfiguration")
      )

instance Hashable DatasetContentDeliveryDestination

instance NFData DatasetContentDeliveryDestination

instance ToJSON DatasetContentDeliveryDestination where
  toJSON DatasetContentDeliveryDestination' {..} =
    object
      ( catMaybes
          [ ("s3DestinationConfiguration" .=)
              <$> _dcddS3DestinationConfiguration,
            ("iotEventsDestinationConfiguration" .=)
              <$> _dcddIotEventsDestinationConfiguration
          ]
      )
