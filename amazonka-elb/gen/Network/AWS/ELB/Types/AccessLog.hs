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
-- Module      : Network.AWS.ELB.Types.AccessLog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AccessLog where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the @AccessLog@ attribute.
--
-- /See:/ 'newAccessLog' smart constructor.
data AccessLog = AccessLog'
  { -- | The logical hierarchy you created for your Amazon S3 bucket, for example
    -- @my-bucket-prefix\/prod@. If the prefix is not provided, the log is
    -- placed at the root level of the bucket.
    s3BucketPrefix :: Core.Maybe Core.Text,
    -- | The interval for publishing the access logs. You can specify an interval
    -- of either 5 minutes or 60 minutes.
    --
    -- Default: 60 minutes
    emitInterval :: Core.Maybe Core.Int,
    -- | The name of the Amazon S3 bucket where the access logs are stored.
    s3BucketName :: Core.Maybe Core.Text,
    -- | Specifies whether access logs are enabled for the load balancer.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketPrefix', 'accessLog_s3BucketPrefix' - The logical hierarchy you created for your Amazon S3 bucket, for example
-- @my-bucket-prefix\/prod@. If the prefix is not provided, the log is
-- placed at the root level of the bucket.
--
-- 'emitInterval', 'accessLog_emitInterval' - The interval for publishing the access logs. You can specify an interval
-- of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
--
-- 's3BucketName', 'accessLog_s3BucketName' - The name of the Amazon S3 bucket where the access logs are stored.
--
-- 'enabled', 'accessLog_enabled' - Specifies whether access logs are enabled for the load balancer.
newAccessLog ::
  -- | 'enabled'
  Core.Bool ->
  AccessLog
newAccessLog pEnabled_ =
  AccessLog'
    { s3BucketPrefix = Core.Nothing,
      emitInterval = Core.Nothing,
      s3BucketName = Core.Nothing,
      enabled = pEnabled_
    }

-- | The logical hierarchy you created for your Amazon S3 bucket, for example
-- @my-bucket-prefix\/prod@. If the prefix is not provided, the log is
-- placed at the root level of the bucket.
accessLog_s3BucketPrefix :: Lens.Lens' AccessLog (Core.Maybe Core.Text)
accessLog_s3BucketPrefix = Lens.lens (\AccessLog' {s3BucketPrefix} -> s3BucketPrefix) (\s@AccessLog' {} a -> s {s3BucketPrefix = a} :: AccessLog)

-- | The interval for publishing the access logs. You can specify an interval
-- of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
accessLog_emitInterval :: Lens.Lens' AccessLog (Core.Maybe Core.Int)
accessLog_emitInterval = Lens.lens (\AccessLog' {emitInterval} -> emitInterval) (\s@AccessLog' {} a -> s {emitInterval = a} :: AccessLog)

-- | The name of the Amazon S3 bucket where the access logs are stored.
accessLog_s3BucketName :: Lens.Lens' AccessLog (Core.Maybe Core.Text)
accessLog_s3BucketName = Lens.lens (\AccessLog' {s3BucketName} -> s3BucketName) (\s@AccessLog' {} a -> s {s3BucketName = a} :: AccessLog)

-- | Specifies whether access logs are enabled for the load balancer.
accessLog_enabled :: Lens.Lens' AccessLog Core.Bool
accessLog_enabled = Lens.lens (\AccessLog' {enabled} -> enabled) (\s@AccessLog' {} a -> s {enabled = a} :: AccessLog)

instance Core.FromXML AccessLog where
  parseXML x =
    AccessLog'
      Core.<$> (x Core..@? "S3BucketPrefix")
      Core.<*> (x Core..@? "EmitInterval")
      Core.<*> (x Core..@? "S3BucketName")
      Core.<*> (x Core..@ "Enabled")

instance Core.Hashable AccessLog

instance Core.NFData AccessLog

instance Core.ToQuery AccessLog where
  toQuery AccessLog' {..} =
    Core.mconcat
      [ "S3BucketPrefix" Core.=: s3BucketPrefix,
        "EmitInterval" Core.=: emitInterval,
        "S3BucketName" Core.=: s3BucketName,
        "Enabled" Core.=: enabled
      ]
