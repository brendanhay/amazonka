{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    targetGrants :: Prelude.Maybe [TargetGrant],
    -- | Specifies the bucket where you want Amazon S3 to store server access
    -- logs. You can have your logs delivered to any bucket that you own,
    -- including the same bucket that is being logged. You can also configure
    -- multiple buckets to deliver their logs to the same target bucket. In
    -- this case, you should choose a different @TargetPrefix@ for each source
    -- bucket so that the delivered log files can be distinguished by key.
    targetBucket :: Prelude.Text,
    -- | A prefix for all log object keys. If you store log files from multiple
    -- Amazon S3 buckets in a single bucket, you can use a prefix to
    -- distinguish which log files came from which bucket.
    targetPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'targetPrefix'
  Prelude.Text ->
  LoggingEnabled
newLoggingEnabled pTargetBucket_ pTargetPrefix_ =
  LoggingEnabled'
    { targetGrants = Prelude.Nothing,
      targetBucket = pTargetBucket_,
      targetPrefix = pTargetPrefix_
    }

-- | Container for granting information.
loggingEnabled_targetGrants :: Lens.Lens' LoggingEnabled (Prelude.Maybe [TargetGrant])
loggingEnabled_targetGrants = Lens.lens (\LoggingEnabled' {targetGrants} -> targetGrants) (\s@LoggingEnabled' {} a -> s {targetGrants = a} :: LoggingEnabled) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In
-- this case, you should choose a different @TargetPrefix@ for each source
-- bucket so that the delivered log files can be distinguished by key.
loggingEnabled_targetBucket :: Lens.Lens' LoggingEnabled Prelude.Text
loggingEnabled_targetBucket = Lens.lens (\LoggingEnabled' {targetBucket} -> targetBucket) (\s@LoggingEnabled' {} a -> s {targetBucket = a} :: LoggingEnabled)

-- | A prefix for all log object keys. If you store log files from multiple
-- Amazon S3 buckets in a single bucket, you can use a prefix to
-- distinguish which log files came from which bucket.
loggingEnabled_targetPrefix :: Lens.Lens' LoggingEnabled Prelude.Text
loggingEnabled_targetPrefix = Lens.lens (\LoggingEnabled' {targetPrefix} -> targetPrefix) (\s@LoggingEnabled' {} a -> s {targetPrefix = a} :: LoggingEnabled)

instance Prelude.FromXML LoggingEnabled where
  parseXML x =
    LoggingEnabled'
      Prelude.<$> ( x Prelude..@? "TargetGrants"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Grant")
                  )
      Prelude.<*> (x Prelude..@ "TargetBucket")
      Prelude.<*> (x Prelude..@ "TargetPrefix")

instance Prelude.Hashable LoggingEnabled

instance Prelude.NFData LoggingEnabled

instance Prelude.ToXML LoggingEnabled where
  toXML LoggingEnabled' {..} =
    Prelude.mconcat
      [ "TargetGrants"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Grant" Prelude.<$> targetGrants),
        "TargetBucket" Prelude.@= targetBucket,
        "TargetPrefix" Prelude.@= targetPrefix
      ]
