{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationUpdate where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an update for a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationUpdate' smart constructor.
data SplunkDestinationUpdate = SplunkDestinationUpdate'
  { _sduS3BackupMode ::
      !(Maybe SplunkS3BackupMode),
    _sduHECToken :: !(Maybe Text),
    _sduHECEndpointType ::
      !(Maybe HECEndpointType),
    _sduCloudWatchLoggingOptions ::
      !(Maybe CloudWatchLoggingOptions),
    _sduHECAcknowledgmentTimeoutInSeconds ::
      !(Maybe Nat),
    _sduS3Update ::
      !(Maybe S3DestinationUpdate),
    _sduHECEndpoint :: !(Maybe Text),
    _sduRetryOptions ::
      !(Maybe SplunkRetryOptions),
    _sduProcessingConfiguration ::
      !(Maybe ProcessingConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SplunkDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sduS3BackupMode' - Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ . You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- * 'sduHECToken' - A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'sduHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'sduCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'sduHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'sduS3Update' - Your update to the configuration of the backup Amazon S3 location.
--
-- * 'sduHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'sduRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'sduProcessingConfiguration' - The data processing configuration.
splunkDestinationUpdate ::
  SplunkDestinationUpdate
splunkDestinationUpdate =
  SplunkDestinationUpdate'
    { _sduS3BackupMode = Nothing,
      _sduHECToken = Nothing,
      _sduHECEndpointType = Nothing,
      _sduCloudWatchLoggingOptions = Nothing,
      _sduHECAcknowledgmentTimeoutInSeconds = Nothing,
      _sduS3Update = Nothing,
      _sduHECEndpoint = Nothing,
      _sduRetryOptions = Nothing,
      _sduProcessingConfiguration = Nothing
    }

-- | Specifies how you want Kinesis Data Firehose to back up documents to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ . You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
sduS3BackupMode :: Lens' SplunkDestinationUpdate (Maybe SplunkS3BackupMode)
sduS3BackupMode = lens _sduS3BackupMode (\s a -> s {_sduS3BackupMode = a})

-- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
sduHECToken :: Lens' SplunkDestinationUpdate (Maybe Text)
sduHECToken = lens _sduHECToken (\s a -> s {_sduHECToken = a})

-- | This type can be either "Raw" or "Event."
sduHECEndpointType :: Lens' SplunkDestinationUpdate (Maybe HECEndpointType)
sduHECEndpointType = lens _sduHECEndpointType (\s a -> s {_sduHECEndpointType = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
sduCloudWatchLoggingOptions :: Lens' SplunkDestinationUpdate (Maybe CloudWatchLoggingOptions)
sduCloudWatchLoggingOptions = lens _sduCloudWatchLoggingOptions (\s a -> s {_sduCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
sduHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationUpdate (Maybe Natural)
sduHECAcknowledgmentTimeoutInSeconds = lens _sduHECAcknowledgmentTimeoutInSeconds (\s a -> s {_sduHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | Your update to the configuration of the backup Amazon S3 location.
sduS3Update :: Lens' SplunkDestinationUpdate (Maybe S3DestinationUpdate)
sduS3Update = lens _sduS3Update (\s a -> s {_sduS3Update = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
sduHECEndpoint :: Lens' SplunkDestinationUpdate (Maybe Text)
sduHECEndpoint = lens _sduHECEndpoint (\s a -> s {_sduHECEndpoint = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
sduRetryOptions :: Lens' SplunkDestinationUpdate (Maybe SplunkRetryOptions)
sduRetryOptions = lens _sduRetryOptions (\s a -> s {_sduRetryOptions = a})

-- | The data processing configuration.
sduProcessingConfiguration :: Lens' SplunkDestinationUpdate (Maybe ProcessingConfiguration)
sduProcessingConfiguration = lens _sduProcessingConfiguration (\s a -> s {_sduProcessingConfiguration = a})

instance Hashable SplunkDestinationUpdate

instance NFData SplunkDestinationUpdate

instance ToJSON SplunkDestinationUpdate where
  toJSON SplunkDestinationUpdate' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _sduS3BackupMode,
            ("HECToken" .=) <$> _sduHECToken,
            ("HECEndpointType" .=) <$> _sduHECEndpointType,
            ("CloudWatchLoggingOptions" .=) <$> _sduCloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" .=)
              <$> _sduHECAcknowledgmentTimeoutInSeconds,
            ("S3Update" .=) <$> _sduS3Update,
            ("HECEndpoint" .=) <$> _sduHECEndpoint,
            ("RetryOptions" .=) <$> _sduRetryOptions,
            ("ProcessingConfiguration" .=) <$> _sduProcessingConfiguration
          ]
      )
