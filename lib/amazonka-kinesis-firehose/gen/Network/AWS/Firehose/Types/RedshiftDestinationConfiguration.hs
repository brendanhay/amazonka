{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
  ( RedshiftDestinationConfiguration (..)
  -- * Smart constructor
  , mkRedshiftDestinationConfiguration
  -- * Lenses
  , rdcRoleARN
  , rdcClusterJDBCURL
  , rdcCopyCommand
  , rdcUsername
  , rdcPassword
  , rdcS3Configuration
  , rdcCloudWatchLoggingOptions
  , rdcProcessingConfiguration
  , rdcRetryOptions
  , rdcS3BackupConfiguration
  , rdcS3BackupMode
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ClusterJDBCURL as Types
import qualified Network.AWS.Firehose.Types.CopyCommand as Types
import qualified Network.AWS.Firehose.Types.Password as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RedshiftRetryOptions as Types
import qualified Network.AWS.Firehose.Types.RedshiftS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Firehose.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'mkRedshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { roleARN :: Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , clusterJDBCURL :: Types.ClusterJDBCURL
    -- ^ The database connection string.
  , copyCommand :: Types.CopyCommand
    -- ^ The @COPY@ command.
  , username :: Types.Username
    -- ^ The name of the user.
  , password :: Types.Password
    -- ^ The user password.
  , s3Configuration :: Types.S3DestinationConfiguration
    -- ^ The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The CloudWatch logging options for your delivery stream.
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
    -- ^ The data processing configuration.
  , retryOptions :: Core.Maybe Types.RedshiftRetryOptions
    -- ^ The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
  , s3BackupConfiguration :: Core.Maybe Types.S3DestinationConfiguration
    -- ^ The configuration for backup in Amazon S3.
  , s3BackupMode :: Core.Maybe Types.RedshiftS3BackupMode
    -- ^ The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDestinationConfiguration' value with any optional fields omitted.
mkRedshiftDestinationConfiguration
    :: Types.RoleARN -- ^ 'roleARN'
    -> Types.ClusterJDBCURL -- ^ 'clusterJDBCURL'
    -> Types.CopyCommand -- ^ 'copyCommand'
    -> Types.Username -- ^ 'username'
    -> Types.Password -- ^ 'password'
    -> Types.S3DestinationConfiguration -- ^ 's3Configuration'
    -> RedshiftDestinationConfiguration
mkRedshiftDestinationConfiguration roleARN clusterJDBCURL
  copyCommand username password s3Configuration
  = RedshiftDestinationConfiguration'{roleARN, clusterJDBCURL,
                                      copyCommand, username, password, s3Configuration,
                                      cloudWatchLoggingOptions = Core.Nothing,
                                      processingConfiguration = Core.Nothing,
                                      retryOptions = Core.Nothing,
                                      s3BackupConfiguration = Core.Nothing,
                                      s3BackupMode = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcRoleARN :: Lens.Lens' RedshiftDestinationConfiguration Types.RoleARN
rdcRoleARN = Lens.field @"roleARN"
{-# INLINEABLE rdcRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The database connection string.
--
-- /Note:/ Consider using 'clusterJDBCURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcClusterJDBCURL :: Lens.Lens' RedshiftDestinationConfiguration Types.ClusterJDBCURL
rdcClusterJDBCURL = Lens.field @"clusterJDBCURL"
{-# INLINEABLE rdcClusterJDBCURL #-}
{-# DEPRECATED clusterJDBCURL "Use generic-lens or generic-optics with 'clusterJDBCURL' instead"  #-}

-- | The @COPY@ command.
--
-- /Note:/ Consider using 'copyCommand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcCopyCommand :: Lens.Lens' RedshiftDestinationConfiguration Types.CopyCommand
rdcCopyCommand = Lens.field @"copyCommand"
{-# INLINEABLE rdcCopyCommand #-}
{-# DEPRECATED copyCommand "Use generic-lens or generic-optics with 'copyCommand' instead"  #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcUsername :: Lens.Lens' RedshiftDestinationConfiguration Types.Username
rdcUsername = Lens.field @"username"
{-# INLINEABLE rdcUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The user password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcPassword :: Lens.Lens' RedshiftDestinationConfiguration Types.Password
rdcPassword = Lens.field @"password"
{-# INLINEABLE rdcPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' .
--
-- The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3Configuration :: Lens.Lens' RedshiftDestinationConfiguration Types.S3DestinationConfiguration
rdcS3Configuration = Lens.field @"s3Configuration"
{-# INLINEABLE rdcS3Configuration #-}
{-# DEPRECATED s3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead"  #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcCloudWatchLoggingOptions :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
rdcCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE rdcCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcProcessingConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe Types.ProcessingConfiguration)
rdcProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE rdcProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcRetryOptions :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe Types.RedshiftRetryOptions)
rdcRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE rdcRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | The configuration for backup in Amazon S3.
--
-- /Note:/ Consider using 's3BackupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3BackupConfiguration :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe Types.S3DestinationConfiguration)
rdcS3BackupConfiguration = Lens.field @"s3BackupConfiguration"
{-# INLINEABLE rdcS3BackupConfiguration #-}
{-# DEPRECATED s3BackupConfiguration "Use generic-lens or generic-optics with 's3BackupConfiguration' instead"  #-}

-- | The Amazon S3 backup mode. After you create a delivery stream, you can update it to enable Amazon S3 backup if it is disabled. If backup is enabled, you can't update the delivery stream to disable it. 
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcS3BackupMode :: Lens.Lens' RedshiftDestinationConfiguration (Core.Maybe Types.RedshiftS3BackupMode)
rdcS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE rdcS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

instance Core.FromJSON RedshiftDestinationConfiguration where
        toJSON RedshiftDestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoleARN" Core..= roleARN),
                  Core.Just ("ClusterJDBCURL" Core..= clusterJDBCURL),
                  Core.Just ("CopyCommand" Core..= copyCommand),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("Password" Core..= password),
                  Core.Just ("S3Configuration" Core..= s3Configuration),
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("ProcessingConfiguration" Core..=) Core.<$>
                    processingConfiguration,
                  ("RetryOptions" Core..=) Core.<$> retryOptions,
                  ("S3BackupConfiguration" Core..=) Core.<$> s3BackupConfiguration,
                  ("S3BackupMode" Core..=) Core.<$> s3BackupMode])
