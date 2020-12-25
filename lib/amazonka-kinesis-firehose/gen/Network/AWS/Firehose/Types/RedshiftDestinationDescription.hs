{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rddRoleARN,
    rddClusterJDBCURL,
    rddCopyCommand,
    rddUsername,
    rddS3DestinationDescription,
    rddCloudWatchLoggingOptions,
    rddProcessingConfiguration,
    rddRetryOptions,
    rddS3BackupDescription,
    rddS3BackupMode,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ClusterJDBCURL as Types
import qualified Network.AWS.Firehose.Types.CopyCommand as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RedshiftRetryOptions as Types
import qualified Network.AWS.Firehose.Types.RedshiftS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
  { -- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Types.RoleARN,
    -- | The database connection string.
    clusterJDBCURL :: Types.ClusterJDBCURL,
    -- | The @COPY@ command.
    copyCommand :: Types.CopyCommand,
    -- | The name of the user.
    username :: Types.Username,
    -- | The Amazon S3 destination.
    s3DestinationDescription :: Types.S3DestinationDescription,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Core.Maybe Types.RedshiftRetryOptions,
    -- | The configuration for backup in Amazon S3.
    s3BackupDescription :: Core.Maybe Types.S3DestinationDescription,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Core.Maybe Types.RedshiftS3BackupMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDestinationDescription' value with any optional fields omitted.
mkRedshiftDestinationDescription ::
  -- | 'roleARN'
  Types.RoleARN ->
  -- | 'clusterJDBCURL'
  Types.ClusterJDBCURL ->
  -- | 'copyCommand'
  Types.CopyCommand ->
  -- | 'username'
  Types.Username ->
  -- | 's3DestinationDescription'
  Types.S3DestinationDescription ->
  RedshiftDestinationDescription
mkRedshiftDestinationDescription
  roleARN
  clusterJDBCURL
  copyCommand
  username
  s3DestinationDescription =
    RedshiftDestinationDescription'
      { roleARN,
        clusterJDBCURL,
        copyCommand,
        username,
        s3DestinationDescription,
        cloudWatchLoggingOptions = Core.Nothing,
        processingConfiguration = Core.Nothing,
        retryOptions = Core.Nothing,
        s3BackupDescription = Core.Nothing,
        s3BackupMode = Core.Nothing
      }

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddRoleARN :: Lens.Lens' RedshiftDestinationDescription Types.RoleARN
rddRoleARN = Lens.field @"roleARN"
{-# DEPRECATED rddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddClusterJDBCURL :: Lens.Lens' RedshiftDestinationDescription Types.ClusterJDBCURL
rddClusterJDBCURL = Lens.field @"clusterJDBCURL"
{-# DEPRECATED rddClusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead." #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddCopyCommand :: Lens.Lens' RedshiftDestinationDescription Types.CopyCommand
rddCopyCommand = Lens.field @"copyCommand"
{-# DEPRECATED rddCopyCommand "Use generic-lens or generic-optics with 'copyCommand' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddUsername :: Lens.Lens' RedshiftDestinationDescription Types.Username
rddUsername = Lens.field @"username"
{-# DEPRECATED rddUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3DestinationDescription :: Lens.Lens' RedshiftDestinationDescription Types.S3DestinationDescription
rddS3DestinationDescription = Lens.field @"s3DestinationDescription"
{-# DEPRECATED rddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
rddCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED rddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddProcessingConfiguration :: Lens.Lens' RedshiftDestinationDescription (Core.Maybe Types.ProcessingConfiguration)
rddProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED rddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddRetryOptions :: Lens.Lens' RedshiftDestinationDescription (Core.Maybe Types.RedshiftRetryOptions)
rddRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED rddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3BackupDescription :: Lens.Lens' RedshiftDestinationDescription (Core.Maybe Types.S3DestinationDescription)
rddS3BackupDescription = Lens.field @"s3BackupDescription"
{-# DEPRECATED rddS3BackupDescription "Use generic-lens or generic-optics with 's3BackupDescription' instead." #-}

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddS3BackupMode :: Lens.Lens' RedshiftDestinationDescription (Core.Maybe Types.RedshiftS3BackupMode)
rddS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED rddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

instance Core.FromJSON RedshiftDestinationDescription where
  parseJSON =
    Core.withObject "RedshiftDestinationDescription" Core.$
      \x ->
        RedshiftDestinationDescription'
          Core.<$> (x Core..: "RoleARN")
          Core.<*> (x Core..: "ClusterJDBCURL")
          Core.<*> (x Core..: "CopyCommand")
          Core.<*> (x Core..: "Username")
          Core.<*> (x Core..: "S3DestinationDescription")
          Core.<*> (x Core..:? "CloudWatchLoggingOptions")
          Core.<*> (x Core..:? "ProcessingConfiguration")
          Core.<*> (x Core..:? "RetryOptions")
          Core.<*> (x Core..:? "S3BackupDescription")
          Core.<*> (x Core..:? "S3BackupMode")
