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
    rduCloudWatchLoggingOptions,
    rduClusterJDBCURL,
    rduCopyCommand,
    rduPassword,
    rduProcessingConfiguration,
    rduRetryOptions,
    rduRoleARN,
    rduS3BackupMode,
    rduS3BackupUpdate,
    rduS3Update,
    rduUsername,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ClusterJDBCURL as Types
import qualified Network.AWS.Firehose.Types.CopyCommand as Types
import qualified Network.AWS.Firehose.Types.Password as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RedshiftRetryOptions as Types
import qualified Network.AWS.Firehose.Types.RedshiftS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationUpdate as Types
import qualified Network.AWS.Firehose.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an update for a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
  { -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The database connection string.
    clusterJDBCURL :: Core.Maybe Types.ClusterJDBCURL,
    -- | The @COPY@ command.
    copyCommand :: Core.Maybe Types.CopyCommand,
    -- | The user password.
    password :: Core.Maybe Types.Password,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
    retryOptions :: Core.Maybe Types.RedshiftRetryOptions,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Core.Maybe Types.RoleARN,
    -- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
    s3BackupMode :: Core.Maybe Types.RedshiftS3BackupMode,
    -- | The Amazon S3 destination for backup.
    s3BackupUpdate :: Core.Maybe Types.S3DestinationUpdate,
    -- | The Amazon S3 destination.
    --
    -- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
    s3Update :: Core.Maybe Types.S3DestinationUpdate,
    -- | The name of the user.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDestinationUpdate' value with any optional fields omitted.
mkRedshiftDestinationUpdate ::
  RedshiftDestinationUpdate
mkRedshiftDestinationUpdate =
  RedshiftDestinationUpdate'
    { cloudWatchLoggingOptions =
        Core.Nothing,
      clusterJDBCURL = Core.Nothing,
      copyCommand = Core.Nothing,
      password = Core.Nothing,
      processingConfiguration = Core.Nothing,
      retryOptions = Core.Nothing,
      roleARN = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3BackupUpdate = Core.Nothing,
      s3Update = Core.Nothing,
      username = Core.Nothing
    }

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
rduCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED rduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduClusterJDBCURL :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.ClusterJDBCURL)
rduClusterJDBCURL = Lens.field @"clusterJDBCURL"
{-# DEPRECATED rduClusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead." #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduCopyCommand :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.CopyCommand)
rduCopyCommand = Lens.field @"copyCommand"
{-# DEPRECATED rduCopyCommand "Use generic-lens or generic-optics with 'copyCommand' instead." #-}

-- | The user password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduPassword :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.Password)
rduPassword = Lens.field @"password"
{-# DEPRECATED rduPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduProcessingConfiguration :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.ProcessingConfiguration)
rduProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED rduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduRetryOptions :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.RedshiftRetryOptions)
rduRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED rduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduRoleARN :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.RoleARN)
rduRoleARN = Lens.field @"roleARN"
{-# DEPRECATED rduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | You can update a delivery stream to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3BackupMode :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.RedshiftS3BackupMode)
rduS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED rduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Amazon S3 destination for backup.
--
-- /Note:/ Consider using 's3BackupUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3BackupUpdate :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
rduS3BackupUpdate = Lens.field @"s3BackupUpdate"
{-# DEPRECATED rduS3BackupUpdate "Use generic-lens or generic-optics with 's3BackupUpdate' instead." #-}

-- | The Amazon S3 destination.
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduS3Update :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
rduS3Update = Lens.field @"s3Update"
{-# DEPRECATED rduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rduUsername :: Lens.Lens' RedshiftDestinationUpdate (Core.Maybe Types.Username)
rduUsername = Lens.field @"username"
{-# DEPRECATED rduUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON RedshiftDestinationUpdate where
  toJSON RedshiftDestinationUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("ClusterJDBCURL" Core..=) Core.<$> clusterJDBCURL,
            ("CopyCommand" Core..=) Core.<$> copyCommand,
            ("Password" Core..=) Core.<$> password,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("RoleARN" Core..=) Core.<$> roleARN,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            ("S3BackupUpdate" Core..=) Core.<$> s3BackupUpdate,
            ("S3Update" Core..=) Core.<$> s3Update,
            ("Username" Core..=) Core.<$> username
          ]
      )
