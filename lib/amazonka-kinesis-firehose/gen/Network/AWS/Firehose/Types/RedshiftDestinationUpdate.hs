{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationUpdate
  ( RedshiftDestinationUpdate (..),

    -- * Smart constructor
    mkRedshiftDestinationUpdate,

    -- * Lenses
    rduS3BackupMode,
    rduCloudWatchLoggingOptions,
    rduUsername,
    rduS3Update,
    rduPassword,
    rduS3BackupUpdate,
    rduCopyCommand,
    rduRetryOptions,
    rduProcessingConfiguration,
    rduClusterJDBCURL,
    rduRoleARN,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an update for a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
  { s3BackupMode ::
      Lude.Maybe RedshiftS3BackupMode,
    cloudWatchLoggingOptions ::
      Lude.Maybe CloudWatchLoggingOptions,
    username ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    s3Update ::
      Lude.Maybe S3DestinationUpdate,
    password ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    s3BackupUpdate ::
      Lude.Maybe S3DestinationUpdate,
    copyCommand :: Lude.Maybe CopyCommand,
    retryOptions ::
      Lude.Maybe RedshiftRetryOptions,
    processingConfiguration ::
      Lude.Maybe ProcessingConfiguration,
    clusterJDBCURL :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDestinationUpdate' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'clusterJDBCURL' - The database connection string.
-- * 'copyCommand' - The @COPY@ command.
-- * 'password' - The user password.
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupMode' - You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
-- * 's3BackupUpdate' - The Amazon S3 destination for backup.
-- * 's3Update' - The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
-- * 'username' - The name of the user.
mkRedshiftDestinationUpdate ::
  RedshiftDestinationUpdate
mkRedshiftDestinationUpdate =
  RedshiftDestinationUpdate'
    { s3BackupMode = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      username = Lude.Nothing,
      s3Update = Lude.Nothing,
      password = Lude.Nothing,
      s3BackupUpdate = Lude.Nothing,
      copyCommand = Lude.Nothing,
      retryOptions = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      clusterJDBCURL = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3BackupMode :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe RedshiftS3BackupMode)
rduS3BackupMode = Lens.lens (s3BackupMode :: RedshiftDestinationUpdate -> Lude.Maybe RedshiftS3BackupMode) (\s a -> s {s3BackupMode = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
rduCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: RedshiftDestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduUsername :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe (Lude.Sensitive Lude.Text))
rduUsername = Lens.lens (username :: RedshiftDestinationUpdate -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {username = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3Update :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe S3DestinationUpdate)
rduS3Update = Lens.lens (s3Update :: RedshiftDestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3Update = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

-- | The user password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduPassword :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe (Lude.Sensitive Lude.Text))
rduPassword = Lens.lens (password :: RedshiftDestinationUpdate -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The Amazon S3 destination for backup.
--
-- /Note:/ Consider using 's3BackupUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3BackupUpdate :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe S3DestinationUpdate)
rduS3BackupUpdate = Lens.lens (s3BackupUpdate :: RedshiftDestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3BackupUpdate = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduS3BackupUpdate "Use generic-lens or generic-optics with 's3BackupUpdate' instead." #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduCopyCommand :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe CopyCommand)
rduCopyCommand = Lens.lens (copyCommand :: RedshiftDestinationUpdate -> Lude.Maybe CopyCommand) (\s a -> s {copyCommand = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduCopyCommand "Use generic-lens or generic-optics with 'copyCommand' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduRetryOptions :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe RedshiftRetryOptions)
rduRetryOptions = Lens.lens (retryOptions :: RedshiftDestinationUpdate -> Lude.Maybe RedshiftRetryOptions) (\s a -> s {retryOptions = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduProcessingConfiguration :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe ProcessingConfiguration)
rduProcessingConfiguration = Lens.lens (processingConfiguration :: RedshiftDestinationUpdate -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduClusterJDBCURL :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe Lude.Text)
rduClusterJDBCURL = Lens.lens (clusterJDBCURL :: RedshiftDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {clusterJDBCURL = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduClusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduRoleARN :: Lens.Lens' RedshiftDestinationUpdate (Lude.Maybe Lude.Text)
rduRoleARN = Lens.lens (roleARN :: RedshiftDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: RedshiftDestinationUpdate)
{-# DEPRECATED rduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON RedshiftDestinationUpdate where
  toJSON RedshiftDestinationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("Username" Lude..=) Lude.<$> username,
            ("S3Update" Lude..=) Lude.<$> s3Update,
            ("Password" Lude..=) Lude.<$> password,
            ("S3BackupUpdate" Lude..=) Lude.<$> s3BackupUpdate,
            ("CopyCommand" Lude..=) Lude.<$> copyCommand,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("ClusterJDBCURL" Lude..=) Lude.<$> clusterJDBCURL,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
