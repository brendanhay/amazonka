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
-- Module      : Amazonka.Firehose.Types.RedshiftDestinationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.RedshiftDestinationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CopyCommand
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.RedshiftRetryOptions
import Amazonka.Firehose.Types.RedshiftS3BackupMode
import Amazonka.Firehose.Types.S3DestinationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The database connection string.
    clusterJDBCURL :: Prelude.Maybe Prelude.Text,
    -- | The @COPY@ command.
    copyCommand :: Prelude.Maybe CopyCommand,
    -- | The user password.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Prelude.Maybe RedshiftRetryOptions,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is
    -- disabled. If backup is enabled, you can\'t update the delivery stream to
    -- disable it.
    s3BackupMode :: Prelude.Maybe RedshiftS3BackupMode,
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Prelude.Maybe S3DestinationUpdate,
    -- | The Amazon S3 destination.
    --
    -- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
    -- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
    -- operation that reads from the S3 bucket doesn\'t support these
    -- compression formats.
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    -- | The name of the user.
    username :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 'redshiftDestinationUpdate_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'clusterJDBCURL', 'redshiftDestinationUpdate_clusterJDBCURL' - The database connection string.
--
-- 'copyCommand', 'redshiftDestinationUpdate_copyCommand' - The @COPY@ command.
--
-- 'password', 'redshiftDestinationUpdate_password' - The user password.
--
-- 'processingConfiguration', 'redshiftDestinationUpdate_processingConfiguration' - The data processing configuration.
--
-- 'retryOptions', 'redshiftDestinationUpdate_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- 'roleARN', 'redshiftDestinationUpdate_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 's3BackupMode', 'redshiftDestinationUpdate_s3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
--
-- 's3BackupUpdate', 'redshiftDestinationUpdate_s3BackupUpdate' - The Amazon S3 destination for backup.
--
-- 's3Update', 'redshiftDestinationUpdate_s3Update' - The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
-- operation that reads from the S3 bucket doesn\'t support these
-- compression formats.
--
-- 'username', 'redshiftDestinationUpdate_username' - The name of the user.
newRedshiftDestinationUpdate ::
  RedshiftDestinationUpdate
newRedshiftDestinationUpdate =
  RedshiftDestinationUpdate'
    { cloudWatchLoggingOptions =
        Prelude.Nothing,
      clusterJDBCURL = Prelude.Nothing,
      copyCommand = Prelude.Nothing,
      password = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      s3BackupUpdate = Prelude.Nothing,
      s3Update = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The Amazon CloudWatch logging options for your delivery stream.
redshiftDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
redshiftDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationUpdate)

-- | The database connection string.
redshiftDestinationUpdate_clusterJDBCURL :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_clusterJDBCURL = Lens.lens (\RedshiftDestinationUpdate' {clusterJDBCURL} -> clusterJDBCURL) (\s@RedshiftDestinationUpdate' {} a -> s {clusterJDBCURL = a} :: RedshiftDestinationUpdate)

-- | The @COPY@ command.
redshiftDestinationUpdate_copyCommand :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe CopyCommand)
redshiftDestinationUpdate_copyCommand = Lens.lens (\RedshiftDestinationUpdate' {copyCommand} -> copyCommand) (\s@RedshiftDestinationUpdate' {} a -> s {copyCommand = a} :: RedshiftDestinationUpdate)

-- | The user password.
redshiftDestinationUpdate_password :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_password = Lens.lens (\RedshiftDestinationUpdate' {password} -> password) (\s@RedshiftDestinationUpdate' {} a -> s {password = a} :: RedshiftDestinationUpdate) Prelude.. Lens.mapping Data._Sensitive

-- | The data processing configuration.
redshiftDestinationUpdate_processingConfiguration :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
redshiftDestinationUpdate_processingConfiguration = Lens.lens (\RedshiftDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationUpdate' {} a -> s {processingConfiguration = a} :: RedshiftDestinationUpdate)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationUpdate_retryOptions :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe RedshiftRetryOptions)
redshiftDestinationUpdate_retryOptions = Lens.lens (\RedshiftDestinationUpdate' {retryOptions} -> retryOptions) (\s@RedshiftDestinationUpdate' {} a -> s {retryOptions = a} :: RedshiftDestinationUpdate)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
redshiftDestinationUpdate_roleARN :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_roleARN = Lens.lens (\RedshiftDestinationUpdate' {roleARN} -> roleARN) (\s@RedshiftDestinationUpdate' {} a -> s {roleARN = a} :: RedshiftDestinationUpdate)

-- | You can update a delivery stream to enable Amazon S3 backup if it is
-- disabled. If backup is enabled, you can\'t update the delivery stream to
-- disable it.
redshiftDestinationUpdate_s3BackupMode :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe RedshiftS3BackupMode)
redshiftDestinationUpdate_s3BackupMode = Lens.lens (\RedshiftDestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationUpdate' {} a -> s {s3BackupMode = a} :: RedshiftDestinationUpdate)

-- | The Amazon S3 destination for backup.
redshiftDestinationUpdate_s3BackupUpdate :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
redshiftDestinationUpdate_s3BackupUpdate = Lens.lens (\RedshiftDestinationUpdate' {s3BackupUpdate} -> s3BackupUpdate) (\s@RedshiftDestinationUpdate' {} a -> s {s3BackupUpdate = a} :: RedshiftDestinationUpdate)

-- | The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in
-- @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@
-- operation that reads from the S3 bucket doesn\'t support these
-- compression formats.
redshiftDestinationUpdate_s3Update :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
redshiftDestinationUpdate_s3Update = Lens.lens (\RedshiftDestinationUpdate' {s3Update} -> s3Update) (\s@RedshiftDestinationUpdate' {} a -> s {s3Update = a} :: RedshiftDestinationUpdate)

-- | The name of the user.
redshiftDestinationUpdate_username :: Lens.Lens' RedshiftDestinationUpdate (Prelude.Maybe Prelude.Text)
redshiftDestinationUpdate_username = Lens.lens (\RedshiftDestinationUpdate' {username} -> username) (\s@RedshiftDestinationUpdate' {} a -> s {username = a} :: RedshiftDestinationUpdate) Prelude.. Lens.mapping Data._Sensitive

instance Prelude.Hashable RedshiftDestinationUpdate where
  hashWithSalt _salt RedshiftDestinationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLoggingOptions
      `Prelude.hashWithSalt` clusterJDBCURL
      `Prelude.hashWithSalt` copyCommand
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` processingConfiguration
      `Prelude.hashWithSalt` retryOptions
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` s3BackupMode
      `Prelude.hashWithSalt` s3BackupUpdate
      `Prelude.hashWithSalt` s3Update
      `Prelude.hashWithSalt` username

instance Prelude.NFData RedshiftDestinationUpdate where
  rnf RedshiftDestinationUpdate' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf clusterJDBCURL
      `Prelude.seq` Prelude.rnf copyCommand
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf s3BackupUpdate
      `Prelude.seq` Prelude.rnf s3Update
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON RedshiftDestinationUpdate where
  toJSON RedshiftDestinationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("ClusterJDBCURL" Data..=)
              Prelude.<$> clusterJDBCURL,
            ("CopyCommand" Data..=) Prelude.<$> copyCommand,
            ("Password" Data..=) Prelude.<$> password,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            ("S3BackupUpdate" Data..=)
              Prelude.<$> s3BackupUpdate,
            ("S3Update" Data..=) Prelude.<$> s3Update,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
