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
-- Module      : Amazonka.CloudFront.Types.StreamingLoggingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.StreamingLoggingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that controls whether access logs are written for this
-- streaming distribution.
--
-- /See:/ 'newStreamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
  { -- | Specifies whether you want CloudFront to save access logs to an Amazon
    -- S3 bucket. If you don\'t want to enable logging when you create a
    -- streaming distribution or if you want to disable logging for an existing
    -- streaming distribution, specify @false@ for @Enabled@, and specify
    -- @empty Bucket@ and @Prefix@ elements. If you specify @false@ for
    -- @Enabled@ but you specify values for @Bucket@ and @Prefix@, the values
    -- are automatically deleted.
    enabled :: Prelude.Bool,
    -- | The Amazon S3 bucket to store the access logs in, for example,
    -- @myawslogbucket.s3.amazonaws.com@.
    bucket :: Prelude.Text,
    -- | An optional string that you want CloudFront to prefix to the access log
    -- filenames for this streaming distribution, for example, @myprefix\/@. If
    -- you want to enable logging, but you don\'t want to specify a prefix, you
    -- still must include an empty @Prefix@ element in the @Logging@ element.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingLoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'streamingLoggingConfig_enabled' - Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you don\'t want to enable logging when you create a
-- streaming distribution or if you want to disable logging for an existing
-- streaming distribution, specify @false@ for @Enabled@, and specify
-- @empty Bucket@ and @Prefix@ elements. If you specify @false@ for
-- @Enabled@ but you specify values for @Bucket@ and @Prefix@, the values
-- are automatically deleted.
--
-- 'bucket', 'streamingLoggingConfig_bucket' - The Amazon S3 bucket to store the access logs in, for example,
-- @myawslogbucket.s3.amazonaws.com@.
--
-- 'prefix', 'streamingLoggingConfig_prefix' - An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, @myprefix\/@. If
-- you want to enable logging, but you don\'t want to specify a prefix, you
-- still must include an empty @Prefix@ element in the @Logging@ element.
newStreamingLoggingConfig ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'bucket'
  Prelude.Text ->
  -- | 'prefix'
  Prelude.Text ->
  StreamingLoggingConfig
newStreamingLoggingConfig pEnabled_ pBucket_ pPrefix_ =
  StreamingLoggingConfig'
    { enabled = pEnabled_,
      bucket = pBucket_,
      prefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you don\'t want to enable logging when you create a
-- streaming distribution or if you want to disable logging for an existing
-- streaming distribution, specify @false@ for @Enabled@, and specify
-- @empty Bucket@ and @Prefix@ elements. If you specify @false@ for
-- @Enabled@ but you specify values for @Bucket@ and @Prefix@, the values
-- are automatically deleted.
streamingLoggingConfig_enabled :: Lens.Lens' StreamingLoggingConfig Prelude.Bool
streamingLoggingConfig_enabled = Lens.lens (\StreamingLoggingConfig' {enabled} -> enabled) (\s@StreamingLoggingConfig' {} a -> s {enabled = a} :: StreamingLoggingConfig)

-- | The Amazon S3 bucket to store the access logs in, for example,
-- @myawslogbucket.s3.amazonaws.com@.
streamingLoggingConfig_bucket :: Lens.Lens' StreamingLoggingConfig Prelude.Text
streamingLoggingConfig_bucket = Lens.lens (\StreamingLoggingConfig' {bucket} -> bucket) (\s@StreamingLoggingConfig' {} a -> s {bucket = a} :: StreamingLoggingConfig)

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, @myprefix\/@. If
-- you want to enable logging, but you don\'t want to specify a prefix, you
-- still must include an empty @Prefix@ element in the @Logging@ element.
streamingLoggingConfig_prefix :: Lens.Lens' StreamingLoggingConfig Prelude.Text
streamingLoggingConfig_prefix = Lens.lens (\StreamingLoggingConfig' {prefix} -> prefix) (\s@StreamingLoggingConfig' {} a -> s {prefix = a} :: StreamingLoggingConfig)

instance Data.FromXML StreamingLoggingConfig where
  parseXML x =
    StreamingLoggingConfig'
      Prelude.<$> (x Data..@ "Enabled")
      Prelude.<*> (x Data..@ "Bucket")
      Prelude.<*> (x Data..@ "Prefix")

instance Prelude.Hashable StreamingLoggingConfig where
  hashWithSalt _salt StreamingLoggingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData StreamingLoggingConfig where
  rnf StreamingLoggingConfig' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf prefix

instance Data.ToXML StreamingLoggingConfig where
  toXML StreamingLoggingConfig' {..} =
    Prelude.mconcat
      [ "Enabled" Data.@= enabled,
        "Bucket" Data.@= bucket,
        "Prefix" Data.@= prefix
      ]
