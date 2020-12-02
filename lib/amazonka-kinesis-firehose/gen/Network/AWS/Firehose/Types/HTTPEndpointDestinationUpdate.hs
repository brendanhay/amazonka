{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationUpdate where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Updates the specified HTTP endpoint destination.
--
--
--
-- /See:/ 'hTTPEndpointDestinationUpdate' smart constructor.
data HTTPEndpointDestinationUpdate = HTTPEndpointDestinationUpdate'
  { _httpeduS3BackupMode ::
      !( Maybe
           HTTPEndpointS3BackupMode
       ),
    _httpeduCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _httpeduS3Update ::
      !(Maybe S3DestinationUpdate),
    _httpeduBufferingHints ::
      !( Maybe
           HTTPEndpointBufferingHints
       ),
    _httpeduRetryOptions ::
      !( Maybe
           HTTPEndpointRetryOptions
       ),
    _httpeduEndpointConfiguration ::
      !( Maybe
           HTTPEndpointConfiguration
       ),
    _httpeduProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _httpeduRequestConfiguration ::
      !( Maybe
           HTTPEndpointRequestConfiguration
       ),
    _httpeduRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpeduS3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- * 'httpeduCloudWatchLoggingOptions' - Undocumented member.
--
-- * 'httpeduS3Update' - Undocumented member.
--
-- * 'httpeduBufferingHints' - Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- * 'httpeduRetryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- * 'httpeduEndpointConfiguration' - Describes the configuration of the HTTP endpoint destination.
--
-- * 'httpeduProcessingConfiguration' - Undocumented member.
--
-- * 'httpeduRequestConfiguration' - The configuration of the request sent to the HTTP endpoint specified as the destination.
--
-- * 'httpeduRoleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
hTTPEndpointDestinationUpdate ::
  HTTPEndpointDestinationUpdate
hTTPEndpointDestinationUpdate =
  HTTPEndpointDestinationUpdate'
    { _httpeduS3BackupMode = Nothing,
      _httpeduCloudWatchLoggingOptions = Nothing,
      _httpeduS3Update = Nothing,
      _httpeduBufferingHints = Nothing,
      _httpeduRetryOptions = Nothing,
      _httpeduEndpointConfiguration = Nothing,
      _httpeduProcessingConfiguration = Nothing,
      _httpeduRequestConfiguration = Nothing,
      _httpeduRoleARN = Nothing
    }

-- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
httpeduS3BackupMode :: Lens' HTTPEndpointDestinationUpdate (Maybe HTTPEndpointS3BackupMode)
httpeduS3BackupMode = lens _httpeduS3BackupMode (\s a -> s {_httpeduS3BackupMode = a})

-- | Undocumented member.
httpeduCloudWatchLoggingOptions :: Lens' HTTPEndpointDestinationUpdate (Maybe CloudWatchLoggingOptions)
httpeduCloudWatchLoggingOptions = lens _httpeduCloudWatchLoggingOptions (\s a -> s {_httpeduCloudWatchLoggingOptions = a})

-- | Undocumented member.
httpeduS3Update :: Lens' HTTPEndpointDestinationUpdate (Maybe S3DestinationUpdate)
httpeduS3Update = lens _httpeduS3Update (\s a -> s {_httpeduS3Update = a})

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
httpeduBufferingHints :: Lens' HTTPEndpointDestinationUpdate (Maybe HTTPEndpointBufferingHints)
httpeduBufferingHints = lens _httpeduBufferingHints (\s a -> s {_httpeduBufferingHints = a})

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
httpeduRetryOptions :: Lens' HTTPEndpointDestinationUpdate (Maybe HTTPEndpointRetryOptions)
httpeduRetryOptions = lens _httpeduRetryOptions (\s a -> s {_httpeduRetryOptions = a})

-- | Describes the configuration of the HTTP endpoint destination.
httpeduEndpointConfiguration :: Lens' HTTPEndpointDestinationUpdate (Maybe HTTPEndpointConfiguration)
httpeduEndpointConfiguration = lens _httpeduEndpointConfiguration (\s a -> s {_httpeduEndpointConfiguration = a})

-- | Undocumented member.
httpeduProcessingConfiguration :: Lens' HTTPEndpointDestinationUpdate (Maybe ProcessingConfiguration)
httpeduProcessingConfiguration = lens _httpeduProcessingConfiguration (\s a -> s {_httpeduProcessingConfiguration = a})

-- | The configuration of the request sent to the HTTP endpoint specified as the destination.
httpeduRequestConfiguration :: Lens' HTTPEndpointDestinationUpdate (Maybe HTTPEndpointRequestConfiguration)
httpeduRequestConfiguration = lens _httpeduRequestConfiguration (\s a -> s {_httpeduRequestConfiguration = a})

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
httpeduRoleARN :: Lens' HTTPEndpointDestinationUpdate (Maybe Text)
httpeduRoleARN = lens _httpeduRoleARN (\s a -> s {_httpeduRoleARN = a})

instance Hashable HTTPEndpointDestinationUpdate

instance NFData HTTPEndpointDestinationUpdate

instance ToJSON HTTPEndpointDestinationUpdate where
  toJSON HTTPEndpointDestinationUpdate' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _httpeduS3BackupMode,
            ("CloudWatchLoggingOptions" .=)
              <$> _httpeduCloudWatchLoggingOptions,
            ("S3Update" .=) <$> _httpeduS3Update,
            ("BufferingHints" .=) <$> _httpeduBufferingHints,
            ("RetryOptions" .=) <$> _httpeduRetryOptions,
            ("EndpointConfiguration" .=) <$> _httpeduEndpointConfiguration,
            ("ProcessingConfiguration" .=) <$> _httpeduProcessingConfiguration,
            ("RequestConfiguration" .=) <$> _httpeduRequestConfiguration,
            ("RoleARN" .=) <$> _httpeduRoleARN
          ]
      )
