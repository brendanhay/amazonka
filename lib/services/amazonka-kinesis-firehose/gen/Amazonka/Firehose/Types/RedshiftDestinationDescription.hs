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
-- Module      : Amazonka.Firehose.Types.RedshiftDestinationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.RedshiftDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.CopyCommand
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.RedshiftRetryOptions
import Amazonka.Firehose.Types.RedshiftS3BackupMode
import Amazonka.Firehose.Types.S3DestinationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes a destination in Amazon Redshift.
--
-- /See:/ 'newRedshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Prelude.Maybe RedshiftRetryOptions,
    -- | The configuration for backup in Amazon S3.
    s3BackupDescription :: Prelude.Maybe S3DestinationDescription,
    -- | The Amazon S3 backup mode.
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
    -- | The Amazon S3 destination.
    s3DestinationDescription :: S3DestinationDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptions', 'redshiftDestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'processingConfiguration', 'redshiftDestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 'retryOptions', 'redshiftDestinationDescription_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- 's3BackupDescription', 'redshiftDestinationDescription_s3BackupDescription' - The configuration for backup in Amazon S3.
--
-- 's3BackupMode', 'redshiftDestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 'roleARN', 'redshiftDestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'clusterJDBCURL', 'redshiftDestinationDescription_clusterJDBCURL' - The database connection string.
--
-- 'copyCommand', 'redshiftDestinationDescription_copyCommand' - The @COPY@ command.
--
-- 'username', 'redshiftDestinationDescription_username' - The name of the user.
--
-- 's3DestinationDescription', 'redshiftDestinationDescription_s3DestinationDescription' - The Amazon S3 destination.
newRedshiftDestinationDescription ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'clusterJDBCURL'
  Prelude.Text ->
  -- | 'copyCommand'
  CopyCommand ->
  -- | 'username'
  Prelude.Text ->
  -- | 's3DestinationDescription'
  S3DestinationDescription ->
  RedshiftDestinationDescription
newRedshiftDestinationDescription
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pS3DestinationDescription_ =
    RedshiftDestinationDescription'
      { cloudWatchLoggingOptions =
          Prelude.Nothing,
        processingConfiguration = Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        s3BackupDescription = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username =
          Data._Sensitive Lens.# pUsername_,
        s3DestinationDescription =
          pS3DestinationDescription_
      }

-- | The Amazon CloudWatch logging options for your delivery stream.
redshiftDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
redshiftDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\RedshiftDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@RedshiftDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationDescription)

-- | The data processing configuration.
redshiftDestinationDescription_processingConfiguration :: Lens.Lens' RedshiftDestinationDescription (Prelude.Maybe ProcessingConfiguration)
redshiftDestinationDescription_processingConfiguration = Lens.lens (\RedshiftDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@RedshiftDestinationDescription' {} a -> s {processingConfiguration = a} :: RedshiftDestinationDescription)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon Redshift. Default value is 3600 (60 minutes).
redshiftDestinationDescription_retryOptions :: Lens.Lens' RedshiftDestinationDescription (Prelude.Maybe RedshiftRetryOptions)
redshiftDestinationDescription_retryOptions = Lens.lens (\RedshiftDestinationDescription' {retryOptions} -> retryOptions) (\s@RedshiftDestinationDescription' {} a -> s {retryOptions = a} :: RedshiftDestinationDescription)

-- | The configuration for backup in Amazon S3.
redshiftDestinationDescription_s3BackupDescription :: Lens.Lens' RedshiftDestinationDescription (Prelude.Maybe S3DestinationDescription)
redshiftDestinationDescription_s3BackupDescription = Lens.lens (\RedshiftDestinationDescription' {s3BackupDescription} -> s3BackupDescription) (\s@RedshiftDestinationDescription' {} a -> s {s3BackupDescription = a} :: RedshiftDestinationDescription)

-- | The Amazon S3 backup mode.
redshiftDestinationDescription_s3BackupMode :: Lens.Lens' RedshiftDestinationDescription (Prelude.Maybe RedshiftS3BackupMode)
redshiftDestinationDescription_s3BackupMode = Lens.lens (\RedshiftDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@RedshiftDestinationDescription' {} a -> s {s3BackupMode = a} :: RedshiftDestinationDescription)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services credentials.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
redshiftDestinationDescription_roleARN :: Lens.Lens' RedshiftDestinationDescription Prelude.Text
redshiftDestinationDescription_roleARN = Lens.lens (\RedshiftDestinationDescription' {roleARN} -> roleARN) (\s@RedshiftDestinationDescription' {} a -> s {roleARN = a} :: RedshiftDestinationDescription)

-- | The database connection string.
redshiftDestinationDescription_clusterJDBCURL :: Lens.Lens' RedshiftDestinationDescription Prelude.Text
redshiftDestinationDescription_clusterJDBCURL = Lens.lens (\RedshiftDestinationDescription' {clusterJDBCURL} -> clusterJDBCURL) (\s@RedshiftDestinationDescription' {} a -> s {clusterJDBCURL = a} :: RedshiftDestinationDescription)

-- | The @COPY@ command.
redshiftDestinationDescription_copyCommand :: Lens.Lens' RedshiftDestinationDescription CopyCommand
redshiftDestinationDescription_copyCommand = Lens.lens (\RedshiftDestinationDescription' {copyCommand} -> copyCommand) (\s@RedshiftDestinationDescription' {} a -> s {copyCommand = a} :: RedshiftDestinationDescription)

-- | The name of the user.
redshiftDestinationDescription_username :: Lens.Lens' RedshiftDestinationDescription Prelude.Text
redshiftDestinationDescription_username = Lens.lens (\RedshiftDestinationDescription' {username} -> username) (\s@RedshiftDestinationDescription' {} a -> s {username = a} :: RedshiftDestinationDescription) Prelude.. Data._Sensitive

-- | The Amazon S3 destination.
redshiftDestinationDescription_s3DestinationDescription :: Lens.Lens' RedshiftDestinationDescription S3DestinationDescription
redshiftDestinationDescription_s3DestinationDescription = Lens.lens (\RedshiftDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@RedshiftDestinationDescription' {} a -> s {s3DestinationDescription = a} :: RedshiftDestinationDescription)

instance Data.FromJSON RedshiftDestinationDescription where
  parseJSON =
    Data.withObject
      "RedshiftDestinationDescription"
      ( \x ->
          RedshiftDestinationDescription'
            Prelude.<$> (x Data..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Data..:? "ProcessingConfiguration")
            Prelude.<*> (x Data..:? "RetryOptions")
            Prelude.<*> (x Data..:? "S3BackupDescription")
            Prelude.<*> (x Data..:? "S3BackupMode")
            Prelude.<*> (x Data..: "RoleARN")
            Prelude.<*> (x Data..: "ClusterJDBCURL")
            Prelude.<*> (x Data..: "CopyCommand")
            Prelude.<*> (x Data..: "Username")
            Prelude.<*> (x Data..: "S3DestinationDescription")
      )

instance
  Prelude.Hashable
    RedshiftDestinationDescription
  where
  hashWithSalt
    _salt
    RedshiftDestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3BackupDescription
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` clusterJDBCURL
        `Prelude.hashWithSalt` copyCommand
        `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` s3DestinationDescription

instance
  Prelude.NFData
    RedshiftDestinationDescription
  where
  rnf RedshiftDestinationDescription' {..} =
    Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf s3BackupDescription
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf clusterJDBCURL
      `Prelude.seq` Prelude.rnf copyCommand
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf s3DestinationDescription
