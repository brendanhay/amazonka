-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationDescription
  ( RedshiftDestinationDescription (..),

    -- * Smart constructor
    mkRedshiftDestinationDescription,

    -- * Lenses
    rddS3BackupMode,
    rddS3BackupDescription,
    rddCloudWatchLoggingOptions,
    rddRetryOptions,
    rddProcessingConfiguration,
    rddRoleARN,
    rddClusterJDBCURL,
    rddCopyCommand,
    rddUsername,
    rddS3DestinationDescription,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
  { s3BackupMode ::
      Lude.Maybe
        RedshiftS3BackupMode,
    s3BackupDescription ::
      Lude.Maybe
        S3DestinationDescription,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    retryOptions ::
      Lude.Maybe
        RedshiftRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    roleARN :: Lude.Text,
    clusterJDBCURL :: Lude.Text,
    copyCommand :: CopyCommand,
    username ::
      Lude.Sensitive Lude.Text,
    s3DestinationDescription ::
      S3DestinationDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDestinationDescription' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'clusterJDBCURL' - The database connection string.
-- * 'copyCommand' - The @COPY@ command.
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupDescription' - The configuration for backup in Amazon S3.
-- * 's3BackupMode' - The Amazon S3 backup mode.
-- * 's3DestinationDescription' - The Amazon S3 destination.
-- * 'username' - The name of the user.
mkRedshiftDestinationDescription ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'clusterJDBCURL'
  Lude.Text ->
  -- | 'copyCommand'
  CopyCommand ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 's3DestinationDescription'
  S3DestinationDescription ->
  RedshiftDestinationDescription
mkRedshiftDestinationDescription
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pS3DestinationDescription_ =
    RedshiftDestinationDescription'
      { s3BackupMode = Lude.Nothing,
        s3BackupDescription = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username = pUsername_,
        s3DestinationDescription = pS3DestinationDescription_
      }

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3BackupMode :: Lens.Lens' RedshiftDestinationDescription (Lude.Maybe RedshiftS3BackupMode)
rddS3BackupMode = Lens.lens (s3BackupMode :: RedshiftDestinationDescription -> Lude.Maybe RedshiftS3BackupMode) (\s a -> s {s3BackupMode = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3BackupDescription :: Lens.Lens' RedshiftDestinationDescription (Lude.Maybe S3DestinationDescription)
rddS3BackupDescription = Lens.lens (s3BackupDescription :: RedshiftDestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3BackupDescription = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddS3BackupDescription "Use generic-lens or generic-optics with 's3BackupDescription' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
rddCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: RedshiftDestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddRetryOptions :: Lens.Lens' RedshiftDestinationDescription (Lude.Maybe RedshiftRetryOptions)
rddRetryOptions = Lens.lens (retryOptions :: RedshiftDestinationDescription -> Lude.Maybe RedshiftRetryOptions) (\s a -> s {retryOptions = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddProcessingConfiguration :: Lens.Lens' RedshiftDestinationDescription (Lude.Maybe ProcessingConfiguration)
rddProcessingConfiguration = Lens.lens (processingConfiguration :: RedshiftDestinationDescription -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddRoleARN :: Lens.Lens' RedshiftDestinationDescription Lude.Text
rddRoleARN = Lens.lens (roleARN :: RedshiftDestinationDescription -> Lude.Text) (\s a -> s {roleARN = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddClusterJDBCURL :: Lens.Lens' RedshiftDestinationDescription Lude.Text
rddClusterJDBCURL = Lens.lens (clusterJDBCURL :: RedshiftDestinationDescription -> Lude.Text) (\s a -> s {clusterJDBCURL = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddClusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead." #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddCopyCommand :: Lens.Lens' RedshiftDestinationDescription CopyCommand
rddCopyCommand = Lens.lens (copyCommand :: RedshiftDestinationDescription -> CopyCommand) (\s a -> s {copyCommand = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddCopyCommand "Use generic-lens or generic-optics with 'copyCommand' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddUsername :: Lens.Lens' RedshiftDestinationDescription (Lude.Sensitive Lude.Text)
rddUsername = Lens.lens (username :: RedshiftDestinationDescription -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3DestinationDescription :: Lens.Lens' RedshiftDestinationDescription S3DestinationDescription
rddS3DestinationDescription = Lens.lens (s3DestinationDescription :: RedshiftDestinationDescription -> S3DestinationDescription) (\s a -> s {s3DestinationDescription = a} :: RedshiftDestinationDescription)
{-# DEPRECATED rddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

instance Lude.FromJSON RedshiftDestinationDescription where
  parseJSON =
    Lude.withObject
      "RedshiftDestinationDescription"
      ( \x ->
          RedshiftDestinationDescription'
            Lude.<$> (x Lude..:? "S3BackupMode")
            Lude.<*> (x Lude..:? "S3BackupDescription")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "RetryOptions")
            Lude.<*> (x Lude..:? "ProcessingConfiguration")
            Lude.<*> (x Lude..: "RoleARN")
            Lude.<*> (x Lude..: "ClusterJDBCURL")
            Lude.<*> (x Lude..: "CopyCommand")
            Lude.<*> (x Lude..: "Username")
            Lude.<*> (x Lude..: "S3DestinationDescription")
      )
