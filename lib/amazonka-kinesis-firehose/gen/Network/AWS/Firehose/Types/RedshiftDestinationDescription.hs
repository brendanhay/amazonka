{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationDescription where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
  { _rddS3BackupMode ::
      !(Maybe RedshiftS3BackupMode),
    _rddS3BackupDescription ::
      !( Maybe
           S3DestinationDescription
       ),
    _rddCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _rddRetryOptions ::
      !(Maybe RedshiftRetryOptions),
    _rddProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _rddRoleARN :: !Text,
    _rddClusterJDBCURL :: !Text,
    _rddCopyCommand ::
      !CopyCommand,
    _rddUsername ::
      !(Sensitive Text),
    _rddS3DestinationDescription ::
      !S3DestinationDescription
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'rddS3BackupDescription' - The configuration for backup in Amazon S3.
--
-- * 'rddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'rddRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rddProcessingConfiguration' - The data processing configuration.
--
-- * 'rddRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'rddClusterJDBCURL' - The database connection string.
--
-- * 'rddCopyCommand' - The @COPY@ command.
--
-- * 'rddUsername' - The name of the user.
--
-- * 'rddS3DestinationDescription' - The Amazon S3 destination.
redshiftDestinationDescription ::
  -- | 'rddRoleARN'
  Text ->
  -- | 'rddClusterJDBCURL'
  Text ->
  -- | 'rddCopyCommand'
  CopyCommand ->
  -- | 'rddUsername'
  Text ->
  -- | 'rddS3DestinationDescription'
  S3DestinationDescription ->
  RedshiftDestinationDescription
redshiftDestinationDescription
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pS3DestinationDescription_ =
    RedshiftDestinationDescription'
      { _rddS3BackupMode = Nothing,
        _rddS3BackupDescription = Nothing,
        _rddCloudWatchLoggingOptions = Nothing,
        _rddRetryOptions = Nothing,
        _rddProcessingConfiguration = Nothing,
        _rddRoleARN = pRoleARN_,
        _rddClusterJDBCURL = pClusterJDBCURL_,
        _rddCopyCommand = pCopyCommand_,
        _rddUsername = _Sensitive # pUsername_,
        _rddS3DestinationDescription = pS3DestinationDescription_
      }

-- | The Amazon S3 backup mode.
rddS3BackupMode :: Lens' RedshiftDestinationDescription (Maybe RedshiftS3BackupMode)
rddS3BackupMode = lens _rddS3BackupMode (\s a -> s {_rddS3BackupMode = a})

-- | The configuration for backup in Amazon S3.
rddS3BackupDescription :: Lens' RedshiftDestinationDescription (Maybe S3DestinationDescription)
rddS3BackupDescription = lens _rddS3BackupDescription (\s a -> s {_rddS3BackupDescription = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
rddCloudWatchLoggingOptions :: Lens' RedshiftDestinationDescription (Maybe CloudWatchLoggingOptions)
rddCloudWatchLoggingOptions = lens _rddCloudWatchLoggingOptions (\s a -> s {_rddCloudWatchLoggingOptions = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rddRetryOptions :: Lens' RedshiftDestinationDescription (Maybe RedshiftRetryOptions)
rddRetryOptions = lens _rddRetryOptions (\s a -> s {_rddRetryOptions = a})

-- | The data processing configuration.
rddProcessingConfiguration :: Lens' RedshiftDestinationDescription (Maybe ProcessingConfiguration)
rddProcessingConfiguration = lens _rddProcessingConfiguration (\s a -> s {_rddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rddRoleARN :: Lens' RedshiftDestinationDescription Text
rddRoleARN = lens _rddRoleARN (\s a -> s {_rddRoleARN = a})

-- | The database connection string.
rddClusterJDBCURL :: Lens' RedshiftDestinationDescription Text
rddClusterJDBCURL = lens _rddClusterJDBCURL (\s a -> s {_rddClusterJDBCURL = a})

-- | The @COPY@ command.
rddCopyCommand :: Lens' RedshiftDestinationDescription CopyCommand
rddCopyCommand = lens _rddCopyCommand (\s a -> s {_rddCopyCommand = a})

-- | The name of the user.
rddUsername :: Lens' RedshiftDestinationDescription Text
rddUsername = lens _rddUsername (\s a -> s {_rddUsername = a}) . _Sensitive

-- | The Amazon S3 destination.
rddS3DestinationDescription :: Lens' RedshiftDestinationDescription S3DestinationDescription
rddS3DestinationDescription = lens _rddS3DestinationDescription (\s a -> s {_rddS3DestinationDescription = a})

instance FromJSON RedshiftDestinationDescription where
  parseJSON =
    withObject
      "RedshiftDestinationDescription"
      ( \x ->
          RedshiftDestinationDescription'
            <$> (x .:? "S3BackupMode")
            <*> (x .:? "S3BackupDescription")
            <*> (x .:? "CloudWatchLoggingOptions")
            <*> (x .:? "RetryOptions")
            <*> (x .:? "ProcessingConfiguration")
            <*> (x .: "RoleARN")
            <*> (x .: "ClusterJDBCURL")
            <*> (x .: "CopyCommand")
            <*> (x .: "Username")
            <*> (x .: "S3DestinationDescription")
      )

instance Hashable RedshiftDestinationDescription

instance NFData RedshiftDestinationDescription
