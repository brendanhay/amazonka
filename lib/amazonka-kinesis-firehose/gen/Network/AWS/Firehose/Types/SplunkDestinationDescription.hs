{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkDestinationDescription where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationDescription' smart constructor.
data SplunkDestinationDescription = SplunkDestinationDescription'
  { _sddS3BackupMode ::
      !(Maybe SplunkS3BackupMode),
    _sddHECToken :: !(Maybe Text),
    _sddHECEndpointType ::
      !(Maybe HECEndpointType),
    _sddCloudWatchLoggingOptions ::
      !(Maybe CloudWatchLoggingOptions),
    _sddHECAcknowledgmentTimeoutInSeconds ::
      !(Maybe Nat),
    _sddS3DestinationDescription ::
      !(Maybe S3DestinationDescription),
    _sddHECEndpoint :: !(Maybe Text),
    _sddRetryOptions ::
      !(Maybe SplunkRetryOptions),
    _sddProcessingConfiguration ::
      !(Maybe ProcessingConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SplunkDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sddS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
--
-- * 'sddHECToken' - A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'sddHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'sddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'sddHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'sddS3DestinationDescription' - The Amazon S3 destination.>
--
-- * 'sddHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'sddRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'sddProcessingConfiguration' - The data processing configuration.
splunkDestinationDescription ::
  SplunkDestinationDescription
splunkDestinationDescription =
  SplunkDestinationDescription'
    { _sddS3BackupMode = Nothing,
      _sddHECToken = Nothing,
      _sddHECEndpointType = Nothing,
      _sddCloudWatchLoggingOptions = Nothing,
      _sddHECAcknowledgmentTimeoutInSeconds = Nothing,
      _sddS3DestinationDescription = Nothing,
      _sddHECEndpoint = Nothing,
      _sddRetryOptions = Nothing,
      _sddProcessingConfiguration = Nothing
    }

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
sddS3BackupMode :: Lens' SplunkDestinationDescription (Maybe SplunkS3BackupMode)
sddS3BackupMode = lens _sddS3BackupMode (\s a -> s {_sddS3BackupMode = a})

-- | A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
sddHECToken :: Lens' SplunkDestinationDescription (Maybe Text)
sddHECToken = lens _sddHECToken (\s a -> s {_sddHECToken = a})

-- | This type can be either "Raw" or "Event."
sddHECEndpointType :: Lens' SplunkDestinationDescription (Maybe HECEndpointType)
sddHECEndpointType = lens _sddHECEndpointType (\s a -> s {_sddHECEndpointType = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
sddCloudWatchLoggingOptions :: Lens' SplunkDestinationDescription (Maybe CloudWatchLoggingOptions)
sddCloudWatchLoggingOptions = lens _sddCloudWatchLoggingOptions (\s a -> s {_sddCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
sddHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationDescription (Maybe Natural)
sddHECAcknowledgmentTimeoutInSeconds = lens _sddHECAcknowledgmentTimeoutInSeconds (\s a -> s {_sddHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | The Amazon S3 destination.>
sddS3DestinationDescription :: Lens' SplunkDestinationDescription (Maybe S3DestinationDescription)
sddS3DestinationDescription = lens _sddS3DestinationDescription (\s a -> s {_sddS3DestinationDescription = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
sddHECEndpoint :: Lens' SplunkDestinationDescription (Maybe Text)
sddHECEndpoint = lens _sddHECEndpoint (\s a -> s {_sddHECEndpoint = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
sddRetryOptions :: Lens' SplunkDestinationDescription (Maybe SplunkRetryOptions)
sddRetryOptions = lens _sddRetryOptions (\s a -> s {_sddRetryOptions = a})

-- | The data processing configuration.
sddProcessingConfiguration :: Lens' SplunkDestinationDescription (Maybe ProcessingConfiguration)
sddProcessingConfiguration = lens _sddProcessingConfiguration (\s a -> s {_sddProcessingConfiguration = a})

instance FromJSON SplunkDestinationDescription where
  parseJSON =
    withObject
      "SplunkDestinationDescription"
      ( \x ->
          SplunkDestinationDescription'
            <$> (x .:? "S3BackupMode")
            <*> (x .:? "HECToken")
            <*> (x .:? "HECEndpointType")
            <*> (x .:? "CloudWatchLoggingOptions")
            <*> (x .:? "HECAcknowledgmentTimeoutInSeconds")
            <*> (x .:? "S3DestinationDescription")
            <*> (x .:? "HECEndpoint")
            <*> (x .:? "RetryOptions")
            <*> (x .:? "ProcessingConfiguration")
      )

instance Hashable SplunkDestinationDescription

instance NFData SplunkDestinationDescription
