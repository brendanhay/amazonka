{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { -- | The configuration for backup in Amazon S3.
    s3BackupConfiguration :: Core.Maybe S3DestinationConfiguration,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | The CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Core.Maybe RedshiftRetryOptions,
    -- | The Amazon S3 backup mode. After you create a delivery stream, you can
    -- update it to enable Amazon S3 backup if it is disabled. If backup is
    -- enabled, you can\'t update the delivery stream to disable it.
    s3BackupMode :: Core.Maybe RedshiftS3BackupMode,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Core.Text,
    -- | The database connection string.
    clusterJDBCURL :: Core.Text,
    -- | The @COPY@ command.
    copyCommand :: CopyCommand,
    -- | The name of the user.
    username :: Core.Sensitive Core.Text,
    -- | The user password.
    password :: Core.Sensitive Core.Text,
    -- | The configuration for the intermediate Amazon S3 location from which
    -- Amazon Redshift obtains data. Restrictions are described in the topic
    -- for CreateDeliveryStream.
    --
    -- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
    -- @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon
    -- Redshift @COPY@ operation that reads from the S3 bucket doesn\'t support
    -- these compression formats.
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RedshiftDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BackupConfiguration', 'redshiftDestinationConfiguration_s3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- 'processingConfiguration', 'redshiftDestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'cloudWatchLoggingOptions', 'redshiftDestinationConfiguration_cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- 'retryOptions', 'redshiftDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- 's3BackupMode', 'redshiftDestinationConfiguration_s3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
--
-- 'roleARN', 'redshiftDestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'clusterJDBCURL', 'redshiftDestinationConfiguration_clusterJDBCURL' - The database connection string.
--
-- 'copyCommand', 'redshiftDestinationConfiguration_copyCommand' - The @COPY@ command.
--
-- 'username', 'redshiftDestinationConfiguration_username' - The name of the user.
--
-- 'password', 'redshiftDestinationConfiguration_password' - The user password.
--
-- 's3Configuration', 'redshiftDestinationConfiguration_s3Configuration' - The configuration for the intermediate Amazon S3 location from which
-- Amazon Redshift obtains data. Restrictions are described in the topic
-- for CreateDeliveryStream.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon
-- Redshift @COPY@ operation that reads from the S3 bucket doesn\'t support
-- these compression formats.
newRedshiftDestinationConfiguration ::
  -- | 'roleARN'
  Core.Text ->
  -- | 'clusterJDBCURL'
  Core.Text ->
  -- | 'copyCommand'
  CopyCommand ->
  -- | 'username'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  RedshiftDestinationConfiguration
newRedshiftDestinationConfiguration
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pPassword_
  pS3Configuration_ =
    RedshiftDestinationConfiguration'
      { s3BackupConfiguration =
          Core.Nothing,
        processingConfiguration = Core.Nothing,
        cloudWatchLoggingOptions = Core.Nothing,
        retryOptions = Core.Nothing,
        s3BackupMode = Core.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username =
          Core._Sensitive Lens.# pUsername_,
        password =
          Core._Sensitive Lens.# pPassword_,
        s3Configuration = pS3Configuration_
      }

-- | The configuration for backup in Amazon S3.
redshiftDestinationConfiguration_s3BackupConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe S3DestinationConfiguration)
redshiftDestinationConfiguration_s3BackupConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupConfiguration} -> s3BackupConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The data processing configuration.
redshiftDestinationConfiguration_processingConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe ProcessingConfiguration)
redshiftDestinationConfiguration_processingConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {processingConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The CloudWatch logging options for your delivery stream.
redshiftDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe CloudWatchLoggingOptions)
redshiftDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationConfiguration_retryOptions :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe RedshiftRetryOptions)
redshiftDestinationConfiguration_retryOptions = Lens.lens (\RedshiftDestinationConfiguration' {retryOptions} -> retryOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {retryOptions = a} :: RedshiftDestinationConfiguration)

-- | The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
redshiftDestinationConfiguration_s3BackupMode :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe RedshiftS3BackupMode)
redshiftDestinationConfiguration_s3BackupMode = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupMode = a} :: RedshiftDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
redshiftDestinationConfiguration_roleARN :: Lens.Lens' RedshiftDestinationConfiguration Core.Text
redshiftDestinationConfiguration_roleARN = Lens.lens (\RedshiftDestinationConfiguration' {roleARN} -> roleARN) (\s@RedshiftDestinationConfiguration' {} a -> s {roleARN = a} :: RedshiftDestinationConfiguration)

-- | The database connection string.
redshiftDestinationConfiguration_clusterJDBCURL :: Lens.Lens' RedshiftDestinationConfiguration Core.Text
redshiftDestinationConfiguration_clusterJDBCURL = Lens.lens (\RedshiftDestinationConfiguration' {clusterJDBCURL} -> clusterJDBCURL) (\s@RedshiftDestinationConfiguration' {} a -> s {clusterJDBCURL = a} :: RedshiftDestinationConfiguration)

-- | The @COPY@ command.
redshiftDestinationConfiguration_copyCommand :: Lens.Lens' RedshiftDestinationConfiguration CopyCommand
redshiftDestinationConfiguration_copyCommand = Lens.lens (\RedshiftDestinationConfiguration' {copyCommand} -> copyCommand) (\s@RedshiftDestinationConfiguration' {} a -> s {copyCommand = a} :: RedshiftDestinationConfiguration)

-- | The name of the user.
redshiftDestinationConfiguration_username :: Lens.Lens' RedshiftDestinationConfiguration Core.Text
redshiftDestinationConfiguration_username = Lens.lens (\RedshiftDestinationConfiguration' {username} -> username) (\s@RedshiftDestinationConfiguration' {} a -> s {username = a} :: RedshiftDestinationConfiguration) Core.. Core._Sensitive

-- | The user password.
redshiftDestinationConfiguration_password :: Lens.Lens' RedshiftDestinationConfiguration Core.Text
redshiftDestinationConfiguration_password = Lens.lens (\RedshiftDestinationConfiguration' {password} -> password) (\s@RedshiftDestinationConfiguration' {} a -> s {password = a} :: RedshiftDestinationConfiguration) Core.. Core._Sensitive

-- | The configuration for the intermediate Amazon S3 location from which
-- Amazon Redshift obtains data. Restrictions are described in the topic
-- for CreateDeliveryStream.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon
-- Redshift @COPY@ operation that reads from the S3 bucket doesn\'t support
-- these compression formats.
redshiftDestinationConfiguration_s3Configuration :: Lens.Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
redshiftDestinationConfiguration_s3Configuration = Lens.lens (\RedshiftDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@RedshiftDestinationConfiguration' {} a -> s {s3Configuration = a} :: RedshiftDestinationConfiguration)

instance
  Core.Hashable
    RedshiftDestinationConfiguration

instance Core.NFData RedshiftDestinationConfiguration

instance Core.ToJSON RedshiftDestinationConfiguration where
  toJSON RedshiftDestinationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3BackupConfiguration" Core..=)
              Core.<$> s3BackupConfiguration,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            Core.Just ("RoleARN" Core..= roleARN),
            Core.Just ("ClusterJDBCURL" Core..= clusterJDBCURL),
            Core.Just ("CopyCommand" Core..= copyCommand),
            Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password),
            Core.Just
              ("S3Configuration" Core..= s3Configuration)
          ]
      )
