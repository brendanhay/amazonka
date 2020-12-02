{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationConfiguration where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { _rdcS3BackupMode ::
      !( Maybe
           RedshiftS3BackupMode
       ),
    _rdcCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _rdcS3BackupConfiguration ::
      !( Maybe
           S3DestinationConfiguration
       ),
    _rdcRetryOptions ::
      !( Maybe
           RedshiftRetryOptions
       ),
    _rdcProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _rdcRoleARN :: !Text,
    _rdcClusterJDBCURL ::
      !Text,
    _rdcCopyCommand ::
      !CopyCommand,
    _rdcUsername ::
      !(Sensitive Text),
    _rdcPassword ::
      !(Sensitive Text),
    _rdcS3Configuration ::
      !S3DestinationConfiguration
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcS3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- * 'rdcCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'rdcS3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- * 'rdcRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rdcProcessingConfiguration' - The data processing configuration.
--
-- * 'rdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'rdcClusterJDBCURL' - The database connection string.
--
-- * 'rdcCopyCommand' - The @COPY@ command.
--
-- * 'rdcUsername' - The name of the user.
--
-- * 'rdcPassword' - The user password.
--
-- * 'rdcS3Configuration' - The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
redshiftDestinationConfiguration ::
  -- | 'rdcRoleARN'
  Text ->
  -- | 'rdcClusterJDBCURL'
  Text ->
  -- | 'rdcCopyCommand'
  CopyCommand ->
  -- | 'rdcUsername'
  Text ->
  -- | 'rdcPassword'
  Text ->
  -- | 'rdcS3Configuration'
  S3DestinationConfiguration ->
  RedshiftDestinationConfiguration
redshiftDestinationConfiguration
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pPassword_
  pS3Configuration_ =
    RedshiftDestinationConfiguration'
      { _rdcS3BackupMode = Nothing,
        _rdcCloudWatchLoggingOptions = Nothing,
        _rdcS3BackupConfiguration = Nothing,
        _rdcRetryOptions = Nothing,
        _rdcProcessingConfiguration = Nothing,
        _rdcRoleARN = pRoleARN_,
        _rdcClusterJDBCURL = pClusterJDBCURL_,
        _rdcCopyCommand = pCopyCommand_,
        _rdcUsername = _Sensitive # pUsername_,
        _rdcPassword = _Sensitive # pPassword_,
        _rdcS3Configuration = pS3Configuration_
      }

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
rdcS3BackupMode :: Lens' RedshiftDestinationConfiguration (Maybe RedshiftS3BackupMode)
rdcS3BackupMode = lens _rdcS3BackupMode (\s a -> s {_rdcS3BackupMode = a})

-- | The CloudWatch logging options for your delivery stream.
rdcCloudWatchLoggingOptions :: Lens' RedshiftDestinationConfiguration (Maybe CloudWatchLoggingOptions)
rdcCloudWatchLoggingOptions = lens _rdcCloudWatchLoggingOptions (\s a -> s {_rdcCloudWatchLoggingOptions = a})

-- | The configuration for backup in Amazon S3.
rdcS3BackupConfiguration :: Lens' RedshiftDestinationConfiguration (Maybe S3DestinationConfiguration)
rdcS3BackupConfiguration = lens _rdcS3BackupConfiguration (\s a -> s {_rdcS3BackupConfiguration = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rdcRetryOptions :: Lens' RedshiftDestinationConfiguration (Maybe RedshiftRetryOptions)
rdcRetryOptions = lens _rdcRetryOptions (\s a -> s {_rdcRetryOptions = a})

-- | The data processing configuration.
rdcProcessingConfiguration :: Lens' RedshiftDestinationConfiguration (Maybe ProcessingConfiguration)
rdcProcessingConfiguration = lens _rdcProcessingConfiguration (\s a -> s {_rdcProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rdcRoleARN :: Lens' RedshiftDestinationConfiguration Text
rdcRoleARN = lens _rdcRoleARN (\s a -> s {_rdcRoleARN = a})

-- | The database connection string.
rdcClusterJDBCURL :: Lens' RedshiftDestinationConfiguration Text
rdcClusterJDBCURL = lens _rdcClusterJDBCURL (\s a -> s {_rdcClusterJDBCURL = a})

-- | The @COPY@ command.
rdcCopyCommand :: Lens' RedshiftDestinationConfiguration CopyCommand
rdcCopyCommand = lens _rdcCopyCommand (\s a -> s {_rdcCopyCommand = a})

-- | The name of the user.
rdcUsername :: Lens' RedshiftDestinationConfiguration Text
rdcUsername = lens _rdcUsername (\s a -> s {_rdcUsername = a}) . _Sensitive

-- | The user password.
rdcPassword :: Lens' RedshiftDestinationConfiguration Text
rdcPassword = lens _rdcPassword (\s a -> s {_rdcPassword = a}) . _Sensitive

-- | The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
rdcS3Configuration :: Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
rdcS3Configuration = lens _rdcS3Configuration (\s a -> s {_rdcS3Configuration = a})

instance Hashable RedshiftDestinationConfiguration

instance NFData RedshiftDestinationConfiguration

instance ToJSON RedshiftDestinationConfiguration where
  toJSON RedshiftDestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("S3BackupMode" .=) <$> _rdcS3BackupMode,
            ("CloudWatchLoggingOptions" .=) <$> _rdcCloudWatchLoggingOptions,
            ("S3BackupConfiguration" .=) <$> _rdcS3BackupConfiguration,
            ("RetryOptions" .=) <$> _rdcRetryOptions,
            ("ProcessingConfiguration" .=) <$> _rdcProcessingConfiguration,
            Just ("RoleARN" .= _rdcRoleARN),
            Just ("ClusterJDBCURL" .= _rdcClusterJDBCURL),
            Just ("CopyCommand" .= _rdcCopyCommand),
            Just ("Username" .= _rdcUsername),
            Just ("Password" .= _rdcPassword),
            Just ("S3Configuration" .= _rdcS3Configuration)
          ]
      )
