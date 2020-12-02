{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationConfiguration where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationConfiguration' smart constructor.
data SplunkDestinationConfiguration = SplunkDestinationConfiguration'
  { _splS3BackupMode ::
      !(Maybe SplunkS3BackupMode),
    _splCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _splHECAcknowledgmentTimeoutInSeconds ::
      !(Maybe Nat),
    _splRetryOptions ::
      !(Maybe SplunkRetryOptions),
    _splProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _splHECEndpoint :: !Text,
    _splHECEndpointType ::
      !HECEndpointType,
    _splHECToken :: !Text,
    _splS3Configuration ::
      !S3DestinationConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SplunkDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'splS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ . You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
--
-- * 'splCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'splHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'splRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'splProcessingConfiguration' - The data processing configuration.
--
-- * 'splHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'splHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'splHECToken' - This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'splS3Configuration' - The configuration for the backup Amazon S3 location.
splunkDestinationConfiguration ::
  -- | 'splHECEndpoint'
  Text ->
  -- | 'splHECEndpointType'
  HECEndpointType ->
  -- | 'splHECToken'
  Text ->
  -- | 'splS3Configuration'
  S3DestinationConfiguration ->
  SplunkDestinationConfiguration
splunkDestinationConfiguration
  pHECEndpoint_
  pHECEndpointType_
  pHECToken_
  pS3Configuration_ =
    SplunkDestinationConfiguration'
      { _splS3BackupMode = Nothing,
        _splCloudWatchLoggingOptions = Nothing,
        _splHECAcknowledgmentTimeoutInSeconds = Nothing,
        _splRetryOptions = Nothing,
        _splProcessingConfiguration = Nothing,
        _splHECEndpoint = pHECEndpoint_,
        _splHECEndpointType = pHECEndpointType_,
        _splHECToken = pHECToken_,
        _splS3Configuration = pS3Configuration_
      }

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedEventsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllEvents@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. The default value is @FailedEventsOnly@ . You can update this backup mode from @FailedEventsOnly@ to @AllEvents@ . You can't update it from @AllEvents@ to @FailedEventsOnly@ .
splS3BackupMode :: Lens' SplunkDestinationConfiguration (Maybe SplunkS3BackupMode)
splS3BackupMode = lens _splS3BackupMode (\s a -> s {_splS3BackupMode = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
splCloudWatchLoggingOptions :: Lens' SplunkDestinationConfiguration (Maybe CloudWatchLoggingOptions)
splCloudWatchLoggingOptions = lens _splCloudWatchLoggingOptions (\s a -> s {_splCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
splHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationConfiguration (Maybe Natural)
splHECAcknowledgmentTimeoutInSeconds = lens _splHECAcknowledgmentTimeoutInSeconds (\s a -> s {_splHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
splRetryOptions :: Lens' SplunkDestinationConfiguration (Maybe SplunkRetryOptions)
splRetryOptions = lens _splRetryOptions (\s a -> s {_splRetryOptions = a})

-- | The data processing configuration.
splProcessingConfiguration :: Lens' SplunkDestinationConfiguration (Maybe ProcessingConfiguration)
splProcessingConfiguration = lens _splProcessingConfiguration (\s a -> s {_splProcessingConfiguration = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
splHECEndpoint :: Lens' SplunkDestinationConfiguration Text
splHECEndpoint = lens _splHECEndpoint (\s a -> s {_splHECEndpoint = a})

-- | This type can be either "Raw" or "Event."
splHECEndpointType :: Lens' SplunkDestinationConfiguration HECEndpointType
splHECEndpointType = lens _splHECEndpointType (\s a -> s {_splHECEndpointType = a})

-- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
splHECToken :: Lens' SplunkDestinationConfiguration Text
splHECToken = lens _splHECToken (\s a -> s {_splHECToken = a})

-- | The configuration for the backup Amazon S3 location.
splS3Configuration :: Lens' SplunkDestinationConfiguration S3DestinationConfiguration
splS3Configuration = lens _splS3Configuration (\s a -> s {_splS3Configuration = a})

instance Hashable SplunkDestinationConfiguration

instance NFData SplunkDestinationConfiguration

instance ToJSON SplunkDestinationConfiguration where
  toJSON SplunkDestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _splS3BackupMode,
            ("CloudWatchLoggingOptions" .=) <$> _splCloudWatchLoggingOptions,
            ("HECAcknowledgmentTimeoutInSeconds" .=)
              <$> _splHECAcknowledgmentTimeoutInSeconds,
            ("RetryOptions" .=) <$> _splRetryOptions,
            ("ProcessingConfiguration" .=) <$> _splProcessingConfiguration,
            Just ("HECEndpoint" .= _splHECEndpoint),
            Just ("HECEndpointType" .= _splHECEndpointType),
            Just ("HECToken" .= _splHECToken),
            Just ("S3Configuration" .= _splS3Configuration)
          ]
      )
