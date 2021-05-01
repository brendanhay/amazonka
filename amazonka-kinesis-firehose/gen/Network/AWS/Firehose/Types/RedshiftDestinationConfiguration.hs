{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { -- | The configuration for backup in Amazon S3.
    s3BackupConfiguration :: Prelude.Maybe S3DestinationConfiguration,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Prelude.Maybe RedshiftRetryOptions,
    -- | The Amazon S3 backup mode. After you create a delivery stream, you can
    -- update it to enable Amazon S3 backup if it is disabled. If backup is
    -- enabled, you can\'t update the delivery stream to disable it.
    s3BackupMode :: Prelude.Maybe RedshiftS3BackupMode,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The database connection string.
    clusterJDBCURL :: Prelude.Text,
    -- | The @COPY@ command.
    copyCommand :: CopyCommand,
    -- | The name of the user.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The user password.
    password :: Prelude.Sensitive Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clusterJDBCURL'
  Prelude.Text ->
  -- | 'copyCommand'
  CopyCommand ->
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
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
          Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username =
          Prelude._Sensitive Lens.# pUsername_,
        password =
          Prelude._Sensitive Lens.# pPassword_,
        s3Configuration = pS3Configuration_
      }

-- | The configuration for backup in Amazon S3.
redshiftDestinationConfiguration_s3BackupConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
redshiftDestinationConfiguration_s3BackupConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupConfiguration} -> s3BackupConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The data processing configuration.
redshiftDestinationConfiguration_processingConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
redshiftDestinationConfiguration_processingConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {processingConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The CloudWatch logging options for your delivery stream.
redshiftDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
redshiftDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationConfiguration_retryOptions :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe RedshiftRetryOptions)
redshiftDestinationConfiguration_retryOptions = Lens.lens (\RedshiftDestinationConfiguration' {retryOptions} -> retryOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {retryOptions = a} :: RedshiftDestinationConfiguration)

-- | The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
redshiftDestinationConfiguration_s3BackupMode :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe RedshiftS3BackupMode)
redshiftDestinationConfiguration_s3BackupMode = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupMode = a} :: RedshiftDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
redshiftDestinationConfiguration_roleARN :: Lens.Lens' RedshiftDestinationConfiguration Prelude.Text
redshiftDestinationConfiguration_roleARN = Lens.lens (\RedshiftDestinationConfiguration' {roleARN} -> roleARN) (\s@RedshiftDestinationConfiguration' {} a -> s {roleARN = a} :: RedshiftDestinationConfiguration)

-- | The database connection string.
redshiftDestinationConfiguration_clusterJDBCURL :: Lens.Lens' RedshiftDestinationConfiguration Prelude.Text
redshiftDestinationConfiguration_clusterJDBCURL = Lens.lens (\RedshiftDestinationConfiguration' {clusterJDBCURL} -> clusterJDBCURL) (\s@RedshiftDestinationConfiguration' {} a -> s {clusterJDBCURL = a} :: RedshiftDestinationConfiguration)

-- | The @COPY@ command.
redshiftDestinationConfiguration_copyCommand :: Lens.Lens' RedshiftDestinationConfiguration CopyCommand
redshiftDestinationConfiguration_copyCommand = Lens.lens (\RedshiftDestinationConfiguration' {copyCommand} -> copyCommand) (\s@RedshiftDestinationConfiguration' {} a -> s {copyCommand = a} :: RedshiftDestinationConfiguration)

-- | The name of the user.
redshiftDestinationConfiguration_username :: Lens.Lens' RedshiftDestinationConfiguration Prelude.Text
redshiftDestinationConfiguration_username = Lens.lens (\RedshiftDestinationConfiguration' {username} -> username) (\s@RedshiftDestinationConfiguration' {} a -> s {username = a} :: RedshiftDestinationConfiguration) Prelude.. Prelude._Sensitive

-- | The user password.
redshiftDestinationConfiguration_password :: Lens.Lens' RedshiftDestinationConfiguration Prelude.Text
redshiftDestinationConfiguration_password = Lens.lens (\RedshiftDestinationConfiguration' {password} -> password) (\s@RedshiftDestinationConfiguration' {} a -> s {password = a} :: RedshiftDestinationConfiguration) Prelude.. Prelude._Sensitive

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
  Prelude.Hashable
    RedshiftDestinationConfiguration

instance
  Prelude.NFData
    RedshiftDestinationConfiguration

instance
  Prelude.ToJSON
    RedshiftDestinationConfiguration
  where
  toJSON RedshiftDestinationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3BackupConfiguration" Prelude..=)
              Prelude.<$> s3BackupConfiguration,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("RetryOptions" Prelude..=) Prelude.<$> retryOptions,
            ("S3BackupMode" Prelude..=) Prelude.<$> s3BackupMode,
            Prelude.Just ("RoleARN" Prelude..= roleARN),
            Prelude.Just
              ("ClusterJDBCURL" Prelude..= clusterJDBCURL),
            Prelude.Just ("CopyCommand" Prelude..= copyCommand),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("Password" Prelude..= password),
            Prelude.Just
              ("S3Configuration" Prelude..= s3Configuration)
          ]
      )
