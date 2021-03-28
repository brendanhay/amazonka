{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LoggingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.LoggingEnabled
  ( LoggingEnabled (..)
  -- * Smart constructor
  , mkLoggingEnabled
  -- * Lenses
  , leTargetBucket
  , leTargetPrefix
  , leTargetGrants
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.TargetBucket as Types
import qualified Network.AWS.S3.Types.TargetGrant as Types
import qualified Network.AWS.S3.Types.TargetPrefix as Types

-- | Describes where logs are stored and the prefix that Amazon S3 assigns to all log object keys for a bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlogging.html PUT Bucket logging> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkLoggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
  { targetBucket :: Types.TargetBucket
    -- ^ Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
  , targetPrefix :: Types.TargetPrefix
    -- ^ A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
  , targetGrants :: Core.Maybe [Types.TargetGrant]
    -- ^ Container for granting information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingEnabled' value with any optional fields omitted.
mkLoggingEnabled
    :: Types.TargetBucket -- ^ 'targetBucket'
    -> Types.TargetPrefix -- ^ 'targetPrefix'
    -> LoggingEnabled
mkLoggingEnabled targetBucket targetPrefix
  = LoggingEnabled'{targetBucket, targetPrefix,
                    targetGrants = Core.Nothing}

-- | Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case, you should choose a different @TargetPrefix@ for each source bucket so that the delivered log files can be distinguished by key.
--
-- /Note:/ Consider using 'targetBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetBucket :: Lens.Lens' LoggingEnabled Types.TargetBucket
leTargetBucket = Lens.field @"targetBucket"
{-# INLINEABLE leTargetBucket #-}
{-# DEPRECATED targetBucket "Use generic-lens or generic-optics with 'targetBucket' instead"  #-}

-- | A prefix for all log object keys. If you store log files from multiple Amazon S3 buckets in a single bucket, you can use a prefix to distinguish which log files came from which bucket.
--
-- /Note:/ Consider using 'targetPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetPrefix :: Lens.Lens' LoggingEnabled Types.TargetPrefix
leTargetPrefix = Lens.field @"targetPrefix"
{-# INLINEABLE leTargetPrefix #-}
{-# DEPRECATED targetPrefix "Use generic-lens or generic-optics with 'targetPrefix' instead"  #-}

-- | Container for granting information.
--
-- /Note:/ Consider using 'targetGrants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTargetGrants :: Lens.Lens' LoggingEnabled (Core.Maybe [Types.TargetGrant])
leTargetGrants = Lens.field @"targetGrants"
{-# INLINEABLE leTargetGrants #-}
{-# DEPRECATED targetGrants "Use generic-lens or generic-optics with 'targetGrants' instead"  #-}

instance Core.ToXML LoggingEnabled where
        toXML LoggingEnabled{..}
          = Core.toXMLElement "TargetBucket" targetBucket Core.<>
              Core.toXMLElement "TargetPrefix" targetPrefix
              Core.<>
              Core.toXMLElement "TargetGrants"
                (Core.maybe Core.mempty (Core.toXMLList "Grant") targetGrants)

instance Core.FromXML LoggingEnabled where
        parseXML x
          = LoggingEnabled' Core.<$>
              (x Core..@ "TargetBucket") Core.<*> x Core..@ "TargetPrefix"
                Core.<*>
                x Core..@? "TargetGrants" Core..<@> Core.parseXMLList "Grant"
