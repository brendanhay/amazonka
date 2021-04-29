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
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationUpdate where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an update for a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
  { -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 destination.
    --
    -- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
    -- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
    -- operation that reads from the S3 bucket doesn\'t support these
    -- compression formats.
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    -- | The database connection string.
    clusterJDBCURL :: Prelude.Maybe Prelude.Text,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The @COPY@ command.
    copyCommand :: Prelude.Maybe CopyCommand,
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Prelude.Maybe S3DestinationUpdate,
    -- | The user password.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the user.
    username :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Prelude.Maybe RedshiftRetryOptions,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is
    -- disabled. If backup is enabled, you can\'t update the delivery stream to
    -- disable it.
    s3BackupMode :: Prelude.Maybe RedshiftS3BackupMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'redshiftDestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 's3Update', 'redshiftDestinationUpdate_s3Update' - The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
-- operation that reads from the S3 bucket doesn\'t support these
-- compression formats.
--
-- 'clusterJDBCURL', 'redshiftDestinationUpdate_clusterJDBCURL' - The database connection string.
--
-- 'processingConfiguration', 'redshiftDestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'cloudWatchLoggingOptions', 'redshiftDestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'copyCommand', 'redshiftDestinationUpdate_copyCommand' - The @COPY@ command.
--
-- 's3BackupUpdate', 'redshiftDestinationUpdate_s3BackupUpdate' - The Amazon S3 destination for backup.
--
-- 'password', 'redshiftDestinationUpdate_password' - The user password.
--
-- 'username', 'redshiftDestinationUpdate_username' - The name of the user.
--
-- 'retryOptions', 'redshiftDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- 's3BackupMode', 'redshiftDestinationUpdate_s3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
newRedshiftDestinationUpdate ::
  RedshiftDestinationUpdate
newRedshiftDestinationUpdate =
  RedshiftDestinationUpdate'
    { roleARN =
        Prelude.Nothing,
      s3Update = Prelude.Nothing,
      clusterJDBCURL = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      copyCommand = Prelude.Nothing,
      s3BackupUpdate = Prelude.Nothing,
      password = Prelude.Nothing,
      username = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
redshiftDestinationUpdate_roleARN :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_roleARN = Lens.lens (\RedshiftDestinationUpdate' {roleARN} -> roleARN) (\s@RedshiftDestinationUpdate' {} a -> s {roleARN = a} :: RedshiftDestinationUpdate)

-- | The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
-- operation that reads from the S3 bucket doesn\'t support these
-- compression formats.
redshiftDestinationUpdate_s3Update :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
redshiftDestinationUpdate_s3Update = Lens.lens (\RedshiftDestinationUpdate' {s3Update} -> s3Update) (\s@RedshiftDestinationUpdate' {} a -> s {s3Update = a} :: RedshiftDestinationUpdate)

-- | The database connection string.
redshiftDestinationUpdate_clusterJDBCURL :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_clusterJDBCURL = Lens.lens (\RedshiftDestinationUpdate' {clusterJDBCURL} -> clusterJDBCURL) (\s@RedshiftDestinationUpdate' {} a -> s {clusterJDBCURL = a} :: RedshiftDestinationUpdate)

-- | The data processing configuration.
redshiftDestinationUpdate_processingConfiguration :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
redshiftDestinationUpdate_processingConfiguration = Lens.lens (\RedshiftDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationUpdate' {} a -> s {processingConfiguration = a} :: RedshiftDestinationUpdate)

-- | The Amazon CloudWatch logging options for your delivery stream.
redshiftDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
redshiftDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationUpdate)

-- | The @COPY@ command.
redshiftDestinationUpdate_copyCommand :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe CopyCommand)
redshiftDestinationUpdate_copyCommand = Lens.lens (\RedshiftDestinationUpdate' {copyCommand} -> copyCommand) (\s@RedshiftDestinationUpdate' {} a -> s {copyCommand = a} :: RedshiftDestinationUpdate)

-- | The Amazon S3 destination for backup.
redshiftDestinationUpdate_s3BackupUpdate :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
redshiftDestinationUpdate_s3BackupUpdate = Lens.lens (\RedshiftDestinationUpdate' {s3BackupUpdate} -> s3BackupUpdate) (\s@RedshiftDestinationUpdate' {} a -> s {s3BackupUpdate = a} :: RedshiftDestinationUpdate)

-- | The user password.
redshiftDestinationUpdate_password :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_password = Lens.lens (\RedshiftDestinationUpdate' {password} -> password) (\s@RedshiftDestinationUpdate' {} a -> s {password = a} :: RedshiftDestinationUpdate) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the user.
redshiftDestinationUpdate_username :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_username = Lens.lens (\RedshiftDestinationUpdate' {username} -> username) (\s@RedshiftDestinationUpdate' {} a -> s {username = a} :: RedshiftDestinationUpdate) Prelude.. Lens.mapping Prelude._Sensitive

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationUpdate_retryOptions :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe RedshiftRetryOptions)
redshiftDestinationUpdate_retryOptions = Lens.lens (\RedshiftDestinationUpdate' {retryOptions} -> retryOptions) (\s@RedshiftDestinationUpdate' {} a -> s {retryOptions = a} :: RedshiftDestinationUpdate)

-- | You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
redshiftDestinationUpdate_s3BackupMode :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe RedshiftS3BackupMode)
redshiftDestinationUpdate_s3BackupMode = Lens.lens (\RedshiftDestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationUpdate' {} a -> s {s3BackupMode = a} :: RedshiftDestinationUpdate)

instance Prelude.Hashable RedshiftDestinationUpdate

instance Prelude.NFData RedshiftDestinationUpdate

instance Prelude.ToJSON RedshiftDestinationUpdate where
  toJSON RedshiftDestinationUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("S3Update" Prelude..=) Prelude.<$> s3Update,
            ("ClusterJDBCURL" Prelude..=)
              Prelude.<$> clusterJDBCURL,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("CopyCommand" Prelude..=) Prelude.<$> copyCommand,
            ("S3BackupUpdate" Prelude..=)
              Prelude.<$> s3BackupUpdate,
            ("Password" Prelude..=) Prelude.<$> password,
            ("Username" Prelude..=) Prelude.<$> username,
            ("RetryOptions" Prelude..=) Prelude.<$> retryOptions,
            ("S3BackupMode" Prelude..=)
              Prelude.<$> s3BackupMode
          ]
      )
