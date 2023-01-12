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
-- Module      : Amazonka.Firehose.Types.RedshiftDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.RedshiftDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CopyCommand
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.RedshiftRetryOptions
import Amazonka.Firehose.Types.RedshiftS3BackupMode
import Amazonka.Firehose.Types.S3DestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { -- | The CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Prelude.Maybe RedshiftRetryOptions,
    -- | The configuration for backup in Amazon S3.
    s3BackupConfiguration :: Prelude.Maybe S3DestinationConfiguration,
    -- | The Amazon S3 backup mode. After you create a delivery stream, you can
    -- update it to enable Amazon S3 backup if it is disabled. If backup is
    -- enabled, you can\'t update the delivery stream to disable it.
    s3BackupMode :: Prelude.Maybe RedshiftS3BackupMode,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The database connection string.
    clusterJDBCURL :: Prelude.Text,
    -- | The @COPY@ command.
    copyCommand :: CopyCommand,
    -- | The name of the user.
    username :: Data.Sensitive Prelude.Text,
    -- | The user password.
    password :: Data.Sensitive Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 'redshiftDestinationConfiguration_cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- 'processingConfiguration', 'redshiftDestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'retryOptions', 'redshiftDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- 's3BackupConfiguration', 'redshiftDestinationConfiguration_s3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- 's3BackupMode', 'redshiftDestinationConfiguration_s3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
--
-- 'roleARN', 'redshiftDestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
      { cloudWatchLoggingOptions =
          Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        s3BackupConfiguration = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username =
          Data._Sensitive Lens.# pUsername_,
        password =
          Data._Sensitive Lens.# pPassword_,
        s3Configuration = pS3Configuration_
      }

-- | The CloudWatch logging options for your delivery stream.
redshiftDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
redshiftDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationConfiguration)

-- | The data processing configuration.
redshiftDestinationConfiguration_processingConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
redshiftDestinationConfiguration_processingConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {processingConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationConfiguration_retryOptions :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe RedshiftRetryOptions)
redshiftDestinationConfiguration_retryOptions = Lens.lens (\RedshiftDestinationConfiguration' {retryOptions} -> retryOptions) (\s@RedshiftDestinationConfiguration' {} a -> s {retryOptions = a} :: RedshiftDestinationConfiguration)

-- | The configuration for backup in Amazon S3.
redshiftDestinationConfiguration_s3BackupConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
redshiftDestinationConfiguration_s3BackupConfiguration = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupConfiguration} -> s3BackupConfiguration) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupConfiguration = a} :: RedshiftDestinationConfiguration)

-- | The Amazon S3 backup mode. After you create a delivery stream, you can
-- update it to enable Amazon S3 backup if it is disabled. If backup is
-- enabled, you can\'t update the delivery stream to disable it.
redshiftDestinationConfiguration_s3BackupMode :: Lens.Lens' RedshiftDestinationConfiguration (Prelude.Maybe RedshiftS3BackupMode)
redshiftDestinationConfiguration_s3BackupMode = Lens.lens (\RedshiftDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationConfiguration' {} a -> s {s3BackupMode = a} :: RedshiftDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
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
redshiftDestinationConfiguration_username = Lens.lens (\RedshiftDestinationConfiguration' {username} -> username) (\s@RedshiftDestinationConfiguration' {} a -> s {username = a} :: RedshiftDestinationConfiguration) Prelude.. Data._Sensitive

-- | The user password.
redshiftDestinationConfiguration_password :: Lens.Lens' RedshiftDestinationConfiguration Prelude.Text
redshiftDestinationConfiguration_password = Lens.lens (\RedshiftDestinationConfiguration' {password} -> password) (\s@RedshiftDestinationConfiguration' {} a -> s {password = a} :: RedshiftDestinationConfiguration) Prelude.. Data._Sensitive

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
  where
  hashWithSalt
    _salt
    RedshiftDestinationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3BackupConfiguration
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` clusterJDBCURL
        `Prelude.hashWithSalt` copyCommand
        `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    RedshiftDestinationConfiguration
  where
  rnf RedshiftDestinationConfiguration' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf s3BackupConfiguration
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf clusterJDBCURL
      `Prelude.seq` Prelude.rnf copyCommand
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf s3Configuration

instance Data.ToJSON RedshiftDestinationConfiguration where
  toJSON RedshiftDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            ("S3BackupConfiguration" Data..=)
              Prelude.<$> s3BackupConfiguration,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            Prelude.Just ("RoleARN" Data..= roleARN),
            Prelude.Just
              ("ClusterJDBCURL" Data..= clusterJDBCURL),
            Prelude.Just ("CopyCommand" Data..= copyCommand),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just
              ("S3Configuration" Data..= s3Configuration)
          ]
      )
