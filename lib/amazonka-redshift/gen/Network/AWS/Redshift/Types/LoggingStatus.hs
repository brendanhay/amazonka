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
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsS3KeyPrefix,
    lsBucketName,
    lsLoggingEnabled,
    lsLastFailureMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'mkLoggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { lastFailureTime ::
      Lude.Maybe Lude.ISO8601,
    lastSuccessfulDeliveryTime :: Lude.Maybe Lude.ISO8601,
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    bucketName :: Lude.Maybe Lude.Text,
    loggingEnabled :: Lude.Maybe Lude.Bool,
    lastFailureMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingStatus' with the minimum fields required to make a request.
--
-- * 'bucketName' - The name of the S3 bucket where the log files are stored.
-- * 'lastFailureMessage' - The message indicating that logs failed to be delivered.
-- * 'lastFailureTime' - The last time when logs failed to be delivered.
-- * 'lastSuccessfulDeliveryTime' - The last time that logs were delivered.
-- * 'loggingEnabled' - @true@ if logging is on, @false@ if logging is off.
-- * 's3KeyPrefix' - The prefix applied to the log file names.
mkLoggingStatus ::
  LoggingStatus
mkLoggingStatus =
  LoggingStatus'
    { lastFailureTime = Lude.Nothing,
      lastSuccessfulDeliveryTime = Lude.Nothing,
      s3KeyPrefix = Lude.Nothing,
      bucketName = Lude.Nothing,
      loggingEnabled = Lude.Nothing,
      lastFailureMessage = Lude.Nothing
    }

-- | The last time when logs failed to be delivered.
--
-- /Note:/ Consider using 'lastFailureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastFailureTime :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.ISO8601)
lsLastFailureTime = Lens.lens (lastFailureTime :: LoggingStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastFailureTime = a} :: LoggingStatus)
{-# DEPRECATED lsLastFailureTime "Use generic-lens or generic-optics with 'lastFailureTime' instead." #-}

-- | The last time that logs were delivered.
--
-- /Note:/ Consider using 'lastSuccessfulDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastSuccessfulDeliveryTime :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.ISO8601)
lsLastSuccessfulDeliveryTime = Lens.lens (lastSuccessfulDeliveryTime :: LoggingStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastSuccessfulDeliveryTime = a} :: LoggingStatus)
{-# DEPRECATED lsLastSuccessfulDeliveryTime "Use generic-lens or generic-optics with 'lastSuccessfulDeliveryTime' instead." #-}

-- | The prefix applied to the log file names.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsS3KeyPrefix :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.Text)
lsS3KeyPrefix = Lens.lens (s3KeyPrefix :: LoggingStatus -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: LoggingStatus)
{-# DEPRECATED lsS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The name of the S3 bucket where the log files are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsBucketName :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.Text)
lsBucketName = Lens.lens (bucketName :: LoggingStatus -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: LoggingStatus)
{-# DEPRECATED lsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | @true@ if logging is on, @false@ if logging is off.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLoggingEnabled :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.Bool)
lsLoggingEnabled = Lens.lens (loggingEnabled :: LoggingStatus -> Lude.Maybe Lude.Bool) (\s a -> s {loggingEnabled = a} :: LoggingStatus)
{-# DEPRECATED lsLoggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead." #-}

-- | The message indicating that logs failed to be delivered.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastFailureMessage :: Lens.Lens' LoggingStatus (Lude.Maybe Lude.Text)
lsLastFailureMessage = Lens.lens (lastFailureMessage :: LoggingStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastFailureMessage = a} :: LoggingStatus)
{-# DEPRECATED lsLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

instance Lude.FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      Lude.<$> (x Lude..@? "LastFailureTime")
      Lude.<*> (x Lude..@? "LastSuccessfulDeliveryTime")
      Lude.<*> (x Lude..@? "S3KeyPrefix")
      Lude.<*> (x Lude..@? "BucketName")
      Lude.<*> (x Lude..@? "LoggingEnabled")
      Lude.<*> (x Lude..@? "LastFailureMessage")
