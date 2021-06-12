{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LoggingEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LoggingEnabled where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TargetGrant

-- | Describes where logs are stored and the prefix that Amazon S3 assigns to
-- all log object keys for a bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTlogging.html PUT Bucket logging>
-- in the /Amazon Simple Storage Service API Reference/.
--
-- /See:/ 'newLoggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
  { -- | Container for granting information.
    targetGrants :: Core.Maybe [TargetGrant],
    -- | Specifies the bucket where you want Amazon S3 to store server access
    -- logs. You can have your logs delivered to any bucket that you own,
    -- including the same bucket that is being logged. You can also configure
    -- multiple buckets to deliver their logs to the same target bucket. In
    -- this case, you should choose a different @TargetPrefix@ for each source
    -- bucket so that the delivered log files can be distinguished by key.
    targetBucket :: Core.Text,
    -- | A prefix for all log object keys. If you store log files from multiple
    -- Amazon S3 buckets in a single bucket, you can use a prefix to
    -- distinguish which log files came from which bucket.
    targetPrefix :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGrants', 'loggingEnabled_targetGrants' - Container for granting information.
--
-- 'targetBucket', 'loggingEnabled_targetBucket' - Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In
-- this case, you should choose a different @TargetPrefix@ for each source
-- bucket so that the delivered log files can be distinguished by key.
--
-- 'targetPrefix', 'loggingEnabled_targetPrefix' - A prefix for all log object keys. If you store log files from multiple
-- Amazon S3 buckets in a single bucket, you can use a prefix to
-- distinguish which log files came from which bucket.
newLoggingEnabled ::
  -- | 'targetBucket'
  Core.Text ->
  -- | 'targetPrefix'
  Core.Text ->
  LoggingEnabled
newLoggingEnabled pTargetBucket_ pTargetPrefix_ =
  LoggingEnabled'
    { targetGrants = Core.Nothing,
      targetBucket = pTargetBucket_,
      targetPrefix = pTargetPrefix_
    }

-- | Container for granting information.
loggingEnabled_targetGrants :: Lens.Lens' LoggingEnabled (Core.Maybe [TargetGrant])
loggingEnabled_targetGrants = Lens.lens (\LoggingEnabled' {targetGrants} -> targetGrants) (\s@LoggingEnabled' {} a -> s {targetGrants = a} :: LoggingEnabled) Core.. Lens.mapping Lens._Coerce

-- | Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In
-- this case, you should choose a different @TargetPrefix@ for each source
-- bucket so that the delivered log files can be distinguished by key.
loggingEnabled_targetBucket :: Lens.Lens' LoggingEnabled Core.Text
loggingEnabled_targetBucket = Lens.lens (\LoggingEnabled' {targetBucket} -> targetBucket) (\s@LoggingEnabled' {} a -> s {targetBucket = a} :: LoggingEnabled)

-- | A prefix for all log object keys. If you store log files from multiple
-- Amazon S3 buckets in a single bucket, you can use a prefix to
-- distinguish which log files came from which bucket.
loggingEnabled_targetPrefix :: Lens.Lens' LoggingEnabled Core.Text
loggingEnabled_targetPrefix = Lens.lens (\LoggingEnabled' {targetPrefix} -> targetPrefix) (\s@LoggingEnabled' {} a -> s {targetPrefix = a} :: LoggingEnabled)

instance Core.FromXML LoggingEnabled where
  parseXML x =
    LoggingEnabled'
      Core.<$> ( x Core..@? "TargetGrants" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Grant")
               )
      Core.<*> (x Core..@ "TargetBucket")
      Core.<*> (x Core..@ "TargetPrefix")

instance Core.Hashable LoggingEnabled

instance Core.NFData LoggingEnabled

instance Core.ToXML LoggingEnabled where
  toXML LoggingEnabled' {..} =
    Core.mconcat
      [ "TargetGrants"
          Core.@= Core.toXML
            (Core.toXMLList "Grant" Core.<$> targetGrants),
        "TargetBucket" Core.@= targetBucket,
        "TargetPrefix" Core.@= targetPrefix
      ]
