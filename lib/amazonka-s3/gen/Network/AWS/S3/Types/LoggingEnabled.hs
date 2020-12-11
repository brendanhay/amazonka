-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LoggingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LoggingEnabled
  ( LoggingEnabled (..),

    -- * Smart constructor
    mkLoggingEnabled,

    -- * Lenses
    leTargetGrants,
    leTargetBucket,
    leTargetPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TargetGrant

-- | Describes where logs are stored and the prefix that Amazon S3 assigns to all log object keys for a bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlogging.html PUT Bucket logging> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkLoggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
  { targetGrants ::
      Lude.Maybe [TargetGrant],
    targetBucket :: Lude.Text,
    targetPrefix :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingEnabled' with the minimum fields required to make a request.
--
-- * 'targetBucket' - Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
-- * 'targetGrants' - Container for granting information.
-- * 'targetPrefix' - A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
mkLoggingEnabled ::
  -- | 'targetBucket'
  Lude.Text ->
  -- | 'targetPrefix'
  Lude.Text ->
  LoggingEnabled
mkLoggingEnabled pTargetBucket_ pTargetPrefix_ =
  LoggingEnabled'
    { targetGrants = Lude.Nothing,
      targetBucket = pTargetBucket_,
      targetPrefix = pTargetPrefix_
    }

-- | Container for granting information.
--
-- /Note:/ Consider using 'targetGrants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetGrants :: Lens.Lens' LoggingEnabled (Lude.Maybe [TargetGrant])
leTargetGrants = Lens.lens (targetGrants :: LoggingEnabled -> Lude.Maybe [TargetGrant]) (\s a -> s {targetGrants = a} :: LoggingEnabled)
{-# DEPRECATED leTargetGrants "Use generic-lens or generic-optics with 'targetGrants' instead." #-}

-- | Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
--
-- /Note:/ Consider using 'targetBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetBucket :: Lens.Lens' LoggingEnabled Lude.Text
leTargetBucket = Lens.lens (targetBucket :: LoggingEnabled -> Lude.Text) (\s a -> s {targetBucket = a} :: LoggingEnabled)
{-# DEPRECATED leTargetBucket "Use generic-lens or generic-optics with 'targetBucket' instead." #-}

-- | A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
--
-- /Note:/ Consider using 'targetPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetPrefix :: Lens.Lens' LoggingEnabled Lude.Text
leTargetPrefix = Lens.lens (targetPrefix :: LoggingEnabled -> Lude.Text) (\s a -> s {targetPrefix = a} :: LoggingEnabled)
{-# DEPRECATED leTargetPrefix "Use generic-lens or generic-optics with 'targetPrefix' instead." #-}

instance Lude.FromXML LoggingEnabled where
  parseXML x =
    LoggingEnabled'
      Lude.<$> ( x Lude..@? "TargetGrants" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Grant")
               )
      Lude.<*> (x Lude..@ "TargetBucket")
      Lude.<*> (x Lude..@ "TargetPrefix")

instance Lude.ToXML LoggingEnabled where
  toXML LoggingEnabled' {..} =
    Lude.mconcat
      [ "TargetGrants"
          Lude.@= Lude.toXML (Lude.toXMLList "Grant" Lude.<$> targetGrants),
        "TargetBucket" Lude.@= targetBucket,
        "TargetPrefix" Lude.@= targetPrefix
      ]
