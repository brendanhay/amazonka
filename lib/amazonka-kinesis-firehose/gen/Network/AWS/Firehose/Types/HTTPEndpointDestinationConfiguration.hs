{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationConfiguration where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of the HTTP endpoint destination.
--
--
--
-- /See:/ 'hTTPEndpointDestinationConfiguration' smart constructor.
data HTTPEndpointDestinationConfiguration = HTTPEndpointDestinationConfiguration'
  { _httpedcS3BackupMode ::
      !( Maybe
           HTTPEndpointS3BackupMode
       ),
    _httpedcCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _httpedcBufferingHints ::
      !( Maybe
           HTTPEndpointBufferingHints
       ),
    _httpedcRetryOptions ::
      !( Maybe
           HTTPEndpointRetryOptions
       ),
    _httpedcProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _httpedcRequestConfiguration ::
      !( Maybe
           HTTPEndpointRequestConfiguration
       ),
    _httpedcRoleARN ::
      !(Maybe Text),
    _httpedcEndpointConfiguration ::
      !HTTPEndpointConfiguration,
    _httpedcS3Configuration ::
      !S3DestinationConfiguration
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpedcS3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- * 'httpedcCloudWatchLoggingOptions' - Undocumented member.
--
-- * 'httpedcBufferingHints' - The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
--
-- * 'httpedcRetryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- * 'httpedcProcessingConfiguration' - Undocumented member.
--
-- * 'httpedcRequestConfiguration' - The configuration of the requeste sent to the HTTP endpoint specified as the destination.
--
-- * 'httpedcRoleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- * 'httpedcEndpointConfiguration' - The configuration of the HTTP endpoint selected as the destination.
--
-- * 'httpedcS3Configuration' - Undocumented member.
hTTPEndpointDestinationConfiguration ::
  -- | 'httpedcEndpointConfiguration'
  HTTPEndpointConfiguration ->
  -- | 'httpedcS3Configuration'
  S3DestinationConfiguration ->
  HTTPEndpointDestinationConfiguration
hTTPEndpointDestinationConfiguration
  pEndpointConfiguration_
  pS3Configuration_ =
    HTTPEndpointDestinationConfiguration'
      { _httpedcS3BackupMode =
          Nothing,
        _httpedcCloudWatchLoggingOptions = Nothing,
        _httpedcBufferingHints = Nothing,
        _httpedcRetryOptions = Nothing,
        _httpedcProcessingConfiguration = Nothing,
        _httpedcRequestConfiguration = Nothing,
        _httpedcRoleARN = Nothing,
        _httpedcEndpointConfiguration = pEndpointConfiguration_,
        _httpedcS3Configuration = pS3Configuration_
      }

-- | Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
httpedcS3BackupMode :: Lens' HTTPEndpointDestinationConfiguration (Maybe HTTPEndpointS3BackupMode)
httpedcS3BackupMode = lens _httpedcS3BackupMode (\s a -> s {_httpedcS3BackupMode = a})

-- | Undocumented member.
httpedcCloudWatchLoggingOptions :: Lens' HTTPEndpointDestinationConfiguration (Maybe CloudWatchLoggingOptions)
httpedcCloudWatchLoggingOptions = lens _httpedcCloudWatchLoggingOptions (\s a -> s {_httpedcCloudWatchLoggingOptions = a})

-- | The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
httpedcBufferingHints :: Lens' HTTPEndpointDestinationConfiguration (Maybe HTTPEndpointBufferingHints)
httpedcBufferingHints = lens _httpedcBufferingHints (\s a -> s {_httpedcBufferingHints = a})

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
httpedcRetryOptions :: Lens' HTTPEndpointDestinationConfiguration (Maybe HTTPEndpointRetryOptions)
httpedcRetryOptions = lens _httpedcRetryOptions (\s a -> s {_httpedcRetryOptions = a})

-- | Undocumented member.
httpedcProcessingConfiguration :: Lens' HTTPEndpointDestinationConfiguration (Maybe ProcessingConfiguration)
httpedcProcessingConfiguration = lens _httpedcProcessingConfiguration (\s a -> s {_httpedcProcessingConfiguration = a})

-- | The configuration of the requeste sent to the HTTP endpoint specified as the destination.
httpedcRequestConfiguration :: Lens' HTTPEndpointDestinationConfiguration (Maybe HTTPEndpointRequestConfiguration)
httpedcRequestConfiguration = lens _httpedcRequestConfiguration (\s a -> s {_httpedcRequestConfiguration = a})

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
httpedcRoleARN :: Lens' HTTPEndpointDestinationConfiguration (Maybe Text)
httpedcRoleARN = lens _httpedcRoleARN (\s a -> s {_httpedcRoleARN = a})

-- | The configuration of the HTTP endpoint selected as the destination.
httpedcEndpointConfiguration :: Lens' HTTPEndpointDestinationConfiguration HTTPEndpointConfiguration
httpedcEndpointConfiguration = lens _httpedcEndpointConfiguration (\s a -> s {_httpedcEndpointConfiguration = a})

-- | Undocumented member.
httpedcS3Configuration :: Lens' HTTPEndpointDestinationConfiguration S3DestinationConfiguration
httpedcS3Configuration = lens _httpedcS3Configuration (\s a -> s {_httpedcS3Configuration = a})

instance Hashable HTTPEndpointDestinationConfiguration

instance NFData HTTPEndpointDestinationConfiguration

instance ToJSON HTTPEndpointDestinationConfiguration where
  toJSON HTTPEndpointDestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _httpedcS3BackupMode,
            ("CloudWatchLoggingOptions" .=)
              <$> _httpedcCloudWatchLoggingOptions,
            ("BufferingHints" .=) <$> _httpedcBufferingHints,
            ("RetryOptions" .=) <$> _httpedcRetryOptions,
            ("ProcessingConfiguration" .=) <$> _httpedcProcessingConfiguration,
            ("RequestConfiguration" .=) <$> _httpedcRequestConfiguration,
            ("RoleARN" .=) <$> _httpedcRoleARN,
            Just ("EndpointConfiguration" .= _httpedcEndpointConfiguration),
            Just ("S3Configuration" .= _httpedcS3Configuration)
          ]
      )
