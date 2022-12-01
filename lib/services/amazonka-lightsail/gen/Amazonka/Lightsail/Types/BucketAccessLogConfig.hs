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
-- Module      : Amazonka.Lightsail.Types.BucketAccessLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.BucketAccessLogConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the access log configuration for a bucket in the Amazon
-- Lightsail object storage service.
--
-- For more information about bucket access logs, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-bucket-access-logs Logging bucket requests using access logging in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newBucketAccessLogConfig' smart constructor.
data BucketAccessLogConfig = BucketAccessLogConfig'
  { -- | The name of the bucket where the access logs are saved. The destination
    -- can be a Lightsail bucket in the same account, and in the same Amazon
    -- Web Services Region as the source bucket.
    --
    -- This parameter is required when enabling the access log for a bucket,
    -- and should be omitted when disabling the access log.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The optional object prefix for the bucket access log.
    --
    -- The prefix is an optional addition to the object key that organizes your
    -- access log files in the destination bucket. For example, if you specify
    -- a @logs\/@ prefix, then each log object will begin with the @logs\/@
    -- prefix in its key (for example,
    -- @logs\/2021-11-01-21-32-16-E568B2907131C0C0@).
    --
    -- This parameter can be optionally specified when enabling the access log
    -- for a bucket, and should be omitted when disabling the access log.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether bucket access logging is enabled
    -- for the bucket.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketAccessLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'bucketAccessLogConfig_destination' - The name of the bucket where the access logs are saved. The destination
-- can be a Lightsail bucket in the same account, and in the same Amazon
-- Web Services Region as the source bucket.
--
-- This parameter is required when enabling the access log for a bucket,
-- and should be omitted when disabling the access log.
--
-- 'prefix', 'bucketAccessLogConfig_prefix' - The optional object prefix for the bucket access log.
--
-- The prefix is an optional addition to the object key that organizes your
-- access log files in the destination bucket. For example, if you specify
-- a @logs\/@ prefix, then each log object will begin with the @logs\/@
-- prefix in its key (for example,
-- @logs\/2021-11-01-21-32-16-E568B2907131C0C0@).
--
-- This parameter can be optionally specified when enabling the access log
-- for a bucket, and should be omitted when disabling the access log.
--
-- 'enabled', 'bucketAccessLogConfig_enabled' - A Boolean value that indicates whether bucket access logging is enabled
-- for the bucket.
newBucketAccessLogConfig ::
  -- | 'enabled'
  Prelude.Bool ->
  BucketAccessLogConfig
newBucketAccessLogConfig pEnabled_ =
  BucketAccessLogConfig'
    { destination =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The name of the bucket where the access logs are saved. The destination
-- can be a Lightsail bucket in the same account, and in the same Amazon
-- Web Services Region as the source bucket.
--
-- This parameter is required when enabling the access log for a bucket,
-- and should be omitted when disabling the access log.
bucketAccessLogConfig_destination :: Lens.Lens' BucketAccessLogConfig (Prelude.Maybe Prelude.Text)
bucketAccessLogConfig_destination = Lens.lens (\BucketAccessLogConfig' {destination} -> destination) (\s@BucketAccessLogConfig' {} a -> s {destination = a} :: BucketAccessLogConfig)

-- | The optional object prefix for the bucket access log.
--
-- The prefix is an optional addition to the object key that organizes your
-- access log files in the destination bucket. For example, if you specify
-- a @logs\/@ prefix, then each log object will begin with the @logs\/@
-- prefix in its key (for example,
-- @logs\/2021-11-01-21-32-16-E568B2907131C0C0@).
--
-- This parameter can be optionally specified when enabling the access log
-- for a bucket, and should be omitted when disabling the access log.
bucketAccessLogConfig_prefix :: Lens.Lens' BucketAccessLogConfig (Prelude.Maybe Prelude.Text)
bucketAccessLogConfig_prefix = Lens.lens (\BucketAccessLogConfig' {prefix} -> prefix) (\s@BucketAccessLogConfig' {} a -> s {prefix = a} :: BucketAccessLogConfig)

-- | A Boolean value that indicates whether bucket access logging is enabled
-- for the bucket.
bucketAccessLogConfig_enabled :: Lens.Lens' BucketAccessLogConfig Prelude.Bool
bucketAccessLogConfig_enabled = Lens.lens (\BucketAccessLogConfig' {enabled} -> enabled) (\s@BucketAccessLogConfig' {} a -> s {enabled = a} :: BucketAccessLogConfig)

instance Core.FromJSON BucketAccessLogConfig where
  parseJSON =
    Core.withObject
      "BucketAccessLogConfig"
      ( \x ->
          BucketAccessLogConfig'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "prefix")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable BucketAccessLogConfig where
  hashWithSalt _salt BucketAccessLogConfig' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData BucketAccessLogConfig where
  rnf BucketAccessLogConfig' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON BucketAccessLogConfig where
  toJSON BucketAccessLogConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destination" Core..=) Prelude.<$> destination,
            ("prefix" Core..=) Prelude.<$> prefix,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
