{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
  ( RedshiftDestinationConfiguration (..),

    -- * Smart constructor
    mkRedshiftDestinationConfiguration,

    -- * Lenses
    rdcS3BackupMode,
    rdcCloudWatchLoggingOptions,
    rdcS3BackupConfiguration,
    rdcRetryOptions,
    rdcProcessingConfiguration,
    rdcRoleARN,
    rdcClusterJDBCURL,
    rdcCopyCommand,
    rdcUsername,
    rdcPassword,
    rdcS3Configuration,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { s3BackupMode ::
      Lude.Maybe
        RedshiftS3BackupMode,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    s3BackupConfiguration ::
      Lude.Maybe
        S3DestinationConfiguration,
    retryOptions ::
      Lude.Maybe
        RedshiftRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    roleARN :: Lude.Text,
    clusterJDBCURL ::
      Lude.Text,
    copyCommand ::
      CopyCommand,
    username ::
      Lude.Sensitive Lude.Text,
    password ::
      Lude.Sensitive Lude.Text,
    s3Configuration ::
      S3DestinationConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
-- * 'clusterJDBCURL' - The database connection string.
-- * 'copyCommand' - The @COPY@ command.
-- * 'password' - The user password.
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupConfiguration' - The configuration for backup in Amazon S3.
-- * 's3BackupMode' - The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
-- * 's3Configuration' - The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
-- * 'username' - The name of the user.
mkRedshiftDestinationConfiguration ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'clusterJDBCURL'
  Lude.Text ->
  -- | 'copyCommand'
  CopyCommand ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  RedshiftDestinationConfiguration
mkRedshiftDestinationConfiguration
  pRoleARN_
  pClusterJDBCURL_
  pCopyCommand_
  pUsername_
  pPassword_
  pS3Configuration_ =
    RedshiftDestinationConfiguration'
      { s3BackupMode = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        s3BackupConfiguration = Lude.Nothing,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        roleARN = pRoleARN_,
        clusterJDBCURL = pClusterJDBCURL_,
        copyCommand = pCopyCommand_,
        username = pUsername_,
        password = pPassword_,
        s3Configuration = pS3Configuration_
      }

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3BackupMode :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Maybe RedshiftS3BackupMode)
rdcS3BackupMode = Lens.lens (s3BackupMode :: RedshiftDestinationConfiguration -> Lude.Maybe RedshiftS3BackupMode) (\s a -> s {s3BackupMode = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
rdcCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: RedshiftDestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3BackupConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Maybe S3DestinationConfiguration)
rdcS3BackupConfiguration = Lens.lens (s3BackupConfiguration :: RedshiftDestinationConfiguration -> Lude.Maybe S3DestinationConfiguration) (\s a -> s {s3BackupConfiguration = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcS3BackupConfiguration "Use generic-lens or generic-optics with 's3BackupConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcRetryOptions :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Maybe RedshiftRetryOptions)
rdcRetryOptions = Lens.lens (retryOptions :: RedshiftDestinationConfiguration -> Lude.Maybe RedshiftRetryOptions) (\s a -> s {retryOptions = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcProcessingConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Maybe ProcessingConfiguration)
rdcProcessingConfiguration = Lens.lens (processingConfiguration :: RedshiftDestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcRoleARN :: Lens.Lens' RedshiftDestinationConfiguration Lude.Text
rdcRoleARN = Lens.lens (roleARN :: RedshiftDestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcClusterJDBCURL :: Lens.Lens' RedshiftDestinationConfiguration Lude.Text
rdcClusterJDBCURL = Lens.lens (clusterJDBCURL :: RedshiftDestinationConfiguration -> Lude.Text) (\s a -> s {clusterJDBCURL = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcClusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead." #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcCopyCommand :: Lens.Lens' RedshiftDestinationConfiguration CopyCommand
rdcCopyCommand = Lens.lens (copyCommand :: RedshiftDestinationConfiguration -> CopyCommand) (\s a -> s {copyCommand = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcCopyCommand "Use generic-lens or generic-optics with 'copyCommand' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcUsername :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Sensitive Lude.Text)
rdcUsername = Lens.lens (username :: RedshiftDestinationConfiguration -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The user password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcPassword :: Lens.Lens' RedshiftDestinationConfiguration (Lude.Sensitive Lude.Text)
rdcPassword = Lens.lens (password :: RedshiftDestinationConfiguration -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3Configuration :: Lens.Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
rdcS3Configuration = Lens.lens (s3Configuration :: RedshiftDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: RedshiftDestinationConfiguration)
{-# DEPRECATED rdcS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

instance Lude.ToJSON RedshiftDestinationConfiguration where
  toJSON RedshiftDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("S3BackupConfiguration" Lude..=) Lude.<$> s3BackupConfiguration,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            Lude.Just ("RoleARN" Lude..= roleARN),
            Lude.Just ("ClusterJDBCURL" Lude..= clusterJDBCURL),
            Lude.Just ("CopyCommand" Lude..= copyCommand),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("Password" Lude..= password),
            Lude.Just ("S3Configuration" Lude..= s3Configuration)
          ]
      )
