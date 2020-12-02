{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointDescription
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the HTTP endpoint destination.
--
--
--
-- /See:/ 'hTTPEndpointDestinationDescription' smart constructor.
data HTTPEndpointDestinationDescription = HTTPEndpointDestinationDescription'
  { _httpeddS3BackupMode ::
      !( Maybe
           HTTPEndpointS3BackupMode
       ),
    _httpeddCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _httpeddS3DestinationDescription ::
      !( Maybe
           S3DestinationDescription
       ),
    _httpeddBufferingHints ::
      !( Maybe
           HTTPEndpointBufferingHints
       ),
    _httpeddRetryOptions ::
      !( Maybe
           HTTPEndpointRetryOptions
       ),
    _httpeddEndpointConfiguration ::
      !( Maybe
           HTTPEndpointDescription
       ),
    _httpeddProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _httpeddRequestConfiguration ::
      !( Maybe
           HTTPEndpointRequestConfiguration
       ),
    _httpeddRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpeddS3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- * 'httpeddCloudWatchLoggingOptions' - Undocumented member.
--
-- * 'httpeddS3DestinationDescription' - Undocumented member.
--
-- * 'httpeddBufferingHints' - Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- * 'httpeddRetryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- * 'httpeddEndpointConfiguration' - The configuration of the specified HTTP endpoint destination.
--
-- * 'httpeddProcessingConfiguration' - Undocumented member.
--
-- * 'httpeddRequestConfiguration' - The configuration of request sent to the HTTP endpoint specified as the destination.
--
-- * 'httpeddRoleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
hTTPEndpointDestinationDescription ::
  HTTPEndpointDestinationDescription
hTTPEndpointDestinationDescription =
  HTTPEndpointDestinationDescription'
    { _httpeddS3BackupMode =
        Nothing,
      _httpeddCloudWatchLoggingOptions = Nothing,
      _httpeddS3DestinationDescription = Nothing,
      _httpeddBufferingHints = Nothing,
      _httpeddRetryOptions = Nothing,
      _httpeddEndpointConfiguration = Nothing,
      _httpeddProcessingConfiguration = Nothing,
      _httpeddRequestConfiguration = Nothing,
      _httpeddRoleARN = Nothing
    }

-- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
httpeddS3BackupMode :: Lens' HTTPEndpointDestinationDescription (Maybe HTTPEndpointS3BackupMode)
httpeddS3BackupMode = lens _httpeddS3BackupMode (\s a -> s {_httpeddS3BackupMode = a})

-- | Undocumented member.
httpeddCloudWatchLoggingOptions :: Lens' HTTPEndpointDestinationDescription (Maybe CloudWatchLoggingOptions)
httpeddCloudWatchLoggingOptions = lens _httpeddCloudWatchLoggingOptions (\s a -> s {_httpeddCloudWatchLoggingOptions = a})

-- | Undocumented member.
httpeddS3DestinationDescription :: Lens' HTTPEndpointDestinationDescription (Maybe S3DestinationDescription)
httpeddS3DestinationDescription = lens _httpeddS3DestinationDescription (\s a -> s {_httpeddS3DestinationDescription = a})

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
httpeddBufferingHints :: Lens' HTTPEndpointDestinationDescription (Maybe HTTPEndpointBufferingHints)
httpeddBufferingHints = lens _httpeddBufferingHints (\s a -> s {_httpeddBufferingHints = a})

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
httpeddRetryOptions :: Lens' HTTPEndpointDestinationDescription (Maybe HTTPEndpointRetryOptions)
httpeddRetryOptions = lens _httpeddRetryOptions (\s a -> s {_httpeddRetryOptions = a})

-- | The configuration of the specified HTTP endpoint destination.
httpeddEndpointConfiguration :: Lens' HTTPEndpointDestinationDescription (Maybe HTTPEndpointDescription)
httpeddEndpointConfiguration = lens _httpeddEndpointConfiguration (\s a -> s {_httpeddEndpointConfiguration = a})

-- | Undocumented member.
httpeddProcessingConfiguration :: Lens' HTTPEndpointDestinationDescription (Maybe ProcessingConfiguration)
httpeddProcessingConfiguration = lens _httpeddProcessingConfiguration (\s a -> s {_httpeddProcessingConfiguration = a})

-- | The configuration of request sent to the HTTP endpoint specified as the destination.
httpeddRequestConfiguration :: Lens' HTTPEndpointDestinationDescription (Maybe HTTPEndpointRequestConfiguration)
httpeddRequestConfiguration = lens _httpeddRequestConfiguration (\s a -> s {_httpeddRequestConfiguration = a})

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
httpeddRoleARN :: Lens' HTTPEndpointDestinationDescription (Maybe Text)
httpeddRoleARN = lens _httpeddRoleARN (\s a -> s {_httpeddRoleARN = a})

instance FromJSON HTTPEndpointDestinationDescription where
  parseJSON =
    withObject
      "HTTPEndpointDestinationDescription"
      ( \x ->
          HTTPEndpointDestinationDescription'
            <$> (x .:? "S3BackupMode")
            <*> (x .:? "CloudWatchLoggingOptions")
            <*> (x .:? "S3DestinationDescription")
            <*> (x .:? "BufferingHints")
            <*> (x .:? "RetryOptions")
            <*> (x .:? "EndpointConfiguration")
            <*> (x .:? "ProcessingConfiguration")
            <*> (x .:? "RequestConfiguration")
            <*> (x .:? "RoleARN")
      )

instance Hashable HTTPEndpointDestinationDescription

instance NFData HTTPEndpointDestinationDescription
