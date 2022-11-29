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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerAccessLog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerAccessLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the access log configuration for the load
-- balancer.
--
-- /See:/ 'newAwsElbLoadBalancerAccessLog' smart constructor.
data AwsElbLoadBalancerAccessLog = AwsElbLoadBalancerAccessLog'
  { -- | The logical hierarchy that was created for the S3 bucket.
    --
    -- If a prefix is not provided, the log is placed at the root level of the
    -- bucket.
    s3BucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where the access logs are stored.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether access logs are enabled for the load balancer.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The interval in minutes for publishing the access logs.
    --
    -- You can publish access logs either every 5 minutes or every 60 minutes.
    emitInterval :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerAccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketPrefix', 'awsElbLoadBalancerAccessLog_s3BucketPrefix' - The logical hierarchy that was created for the S3 bucket.
--
-- If a prefix is not provided, the log is placed at the root level of the
-- bucket.
--
-- 's3BucketName', 'awsElbLoadBalancerAccessLog_s3BucketName' - The name of the S3 bucket where the access logs are stored.
--
-- 'enabled', 'awsElbLoadBalancerAccessLog_enabled' - Indicates whether access logs are enabled for the load balancer.
--
-- 'emitInterval', 'awsElbLoadBalancerAccessLog_emitInterval' - The interval in minutes for publishing the access logs.
--
-- You can publish access logs either every 5 minutes or every 60 minutes.
newAwsElbLoadBalancerAccessLog ::
  AwsElbLoadBalancerAccessLog
newAwsElbLoadBalancerAccessLog =
  AwsElbLoadBalancerAccessLog'
    { s3BucketPrefix =
        Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      enabled = Prelude.Nothing,
      emitInterval = Prelude.Nothing
    }

-- | The logical hierarchy that was created for the S3 bucket.
--
-- If a prefix is not provided, the log is placed at the root level of the
-- bucket.
awsElbLoadBalancerAccessLog_s3BucketPrefix :: Lens.Lens' AwsElbLoadBalancerAccessLog (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerAccessLog_s3BucketPrefix = Lens.lens (\AwsElbLoadBalancerAccessLog' {s3BucketPrefix} -> s3BucketPrefix) (\s@AwsElbLoadBalancerAccessLog' {} a -> s {s3BucketPrefix = a} :: AwsElbLoadBalancerAccessLog)

-- | The name of the S3 bucket where the access logs are stored.
awsElbLoadBalancerAccessLog_s3BucketName :: Lens.Lens' AwsElbLoadBalancerAccessLog (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerAccessLog_s3BucketName = Lens.lens (\AwsElbLoadBalancerAccessLog' {s3BucketName} -> s3BucketName) (\s@AwsElbLoadBalancerAccessLog' {} a -> s {s3BucketName = a} :: AwsElbLoadBalancerAccessLog)

-- | Indicates whether access logs are enabled for the load balancer.
awsElbLoadBalancerAccessLog_enabled :: Lens.Lens' AwsElbLoadBalancerAccessLog (Prelude.Maybe Prelude.Bool)
awsElbLoadBalancerAccessLog_enabled = Lens.lens (\AwsElbLoadBalancerAccessLog' {enabled} -> enabled) (\s@AwsElbLoadBalancerAccessLog' {} a -> s {enabled = a} :: AwsElbLoadBalancerAccessLog)

-- | The interval in minutes for publishing the access logs.
--
-- You can publish access logs either every 5 minutes or every 60 minutes.
awsElbLoadBalancerAccessLog_emitInterval :: Lens.Lens' AwsElbLoadBalancerAccessLog (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerAccessLog_emitInterval = Lens.lens (\AwsElbLoadBalancerAccessLog' {emitInterval} -> emitInterval) (\s@AwsElbLoadBalancerAccessLog' {} a -> s {emitInterval = a} :: AwsElbLoadBalancerAccessLog)

instance Core.FromJSON AwsElbLoadBalancerAccessLog where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerAccessLog"
      ( \x ->
          AwsElbLoadBalancerAccessLog'
            Prelude.<$> (x Core..:? "S3BucketPrefix")
            Prelude.<*> (x Core..:? "S3BucketName")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "EmitInterval")
      )

instance Prelude.Hashable AwsElbLoadBalancerAccessLog where
  hashWithSalt _salt AwsElbLoadBalancerAccessLog' {..} =
    _salt `Prelude.hashWithSalt` s3BucketPrefix
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` emitInterval

instance Prelude.NFData AwsElbLoadBalancerAccessLog where
  rnf AwsElbLoadBalancerAccessLog' {..} =
    Prelude.rnf s3BucketPrefix
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf emitInterval

instance Core.ToJSON AwsElbLoadBalancerAccessLog where
  toJSON AwsElbLoadBalancerAccessLog' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3BucketPrefix" Core..=)
              Prelude.<$> s3BucketPrefix,
            ("S3BucketName" Core..=) Prelude.<$> s3BucketName,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("EmitInterval" Core..=) Prelude.<$> emitInterval
          ]
      )
