{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.LoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.LoggingStatus
  ( LoggingStatus (..),

    -- * Smart constructor
    mkLoggingStatus,

    -- * Lenses
    lsBucketName,
    lsLastFailureMessage,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsLoggingEnabled,
    lsS3KeyPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'mkLoggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Core.Maybe Types.String,
    -- | The message indicating that logs failed to be delivered.
    lastFailureMessage :: Core.Maybe Types.String,
    -- | The last time when logs failed to be delivered.
    lastFailureTime :: Core.Maybe Core.UTCTime,
    -- | The last time that logs were delivered.
    lastSuccessfulDeliveryTime :: Core.Maybe Core.UTCTime,
    -- | @true@ if logging is on, @false@ if logging is off.
    loggingEnabled :: Core.Maybe Core.Bool,
    -- | The prefix applied to the log file names.
    s3KeyPrefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LoggingStatus' value with any optional fields omitted.
mkLoggingStatus ::
  LoggingStatus
mkLoggingStatus =
  LoggingStatus'
    { bucketName = Core.Nothing,
      lastFailureMessage = Core.Nothing,
      lastFailureTime = Core.Nothing,
      lastSuccessfulDeliveryTime = Core.Nothing,
      loggingEnabled = Core.Nothing,
      s3KeyPrefix = Core.Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsBucketName :: Lens.Lens' LoggingStatus (Core.Maybe Types.String)
lsBucketName = Lens.field @"bucketName"
{-# DEPRECATED lsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The message indicating that logs failed to be delivered.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastFailureMessage :: Lens.Lens' LoggingStatus (Core.Maybe Types.String)
lsLastFailureMessage = Lens.field @"lastFailureMessage"
{-# DEPRECATED lsLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

-- | The last time when logs failed to be delivered.
--
-- /Note:/ Consider using 'lastFailureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastFailureTime :: Lens.Lens' LoggingStatus (Core.Maybe Core.UTCTime)
lsLastFailureTime = Lens.field @"lastFailureTime"
{-# DEPRECATED lsLastFailureTime "Use generic-lens or generic-optics with 'lastFailureTime' instead." #-}

-- | The last time that logs were delivered.
--
-- /Note:/ Consider using 'lastSuccessfulDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastSuccessfulDeliveryTime :: Lens.Lens' LoggingStatus (Core.Maybe Core.UTCTime)
lsLastSuccessfulDeliveryTime = Lens.field @"lastSuccessfulDeliveryTime"
{-# DEPRECATED lsLastSuccessfulDeliveryTime "Use generic-lens or generic-optics with 'lastSuccessfulDeliveryTime' instead." #-}

-- | @true@ if logging is on, @false@ if logging is off.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLoggingEnabled :: Lens.Lens' LoggingStatus (Core.Maybe Core.Bool)
lsLoggingEnabled = Lens.field @"loggingEnabled"
{-# DEPRECATED lsLoggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead." #-}

-- | The prefix applied to the log file names.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsS3KeyPrefix :: Lens.Lens' LoggingStatus (Core.Maybe Types.String)
lsS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED lsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

instance Core.FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      Core.<$> (x Core..@? "BucketName")
      Core.<*> (x Core..@? "LastFailureMessage")
      Core.<*> (x Core..@? "LastFailureTime")
      Core.<*> (x Core..@? "LastSuccessfulDeliveryTime")
      Core.<*> (x Core..@? "LoggingEnabled")
      Core.<*> (x Core..@? "S3KeyPrefix")
