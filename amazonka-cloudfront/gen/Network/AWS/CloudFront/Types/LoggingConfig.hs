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
-- Module      : Network.AWS.CloudFront.Types.LoggingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LoggingConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- /See:/ 'newLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { -- | Specifies whether you want CloudFront to save access logs to an Amazon
    -- S3 bucket. If you don\'t want to enable logging when you create a
    -- distribution or if you want to disable logging for an existing
    -- distribution, specify @false@ for @Enabled@, and specify empty @Bucket@
    -- and @Prefix@ elements. If you specify @false@ for @Enabled@ but you
    -- specify values for @Bucket@, @prefix@, and @IncludeCookies@, the values
    -- are automatically deleted.
    enabled :: Prelude.Bool,
    -- | Specifies whether you want CloudFront to include cookies in access logs,
    -- specify @true@ for @IncludeCookies@. If you choose to include cookies in
    -- logs, CloudFront logs all cookies regardless of how you configure the
    -- cache behaviors for this distribution. If you don\'t want to include
    -- cookies when you create a distribution or if you want to disable include
    -- cookies for an existing distribution, specify @false@ for
    -- @IncludeCookies@.
    includeCookies :: Prelude.Bool,
    -- | The Amazon S3 bucket to store the access logs in, for example,
    -- @myawslogbucket.s3.amazonaws.com@.
    bucket :: Prelude.Text,
    -- | An optional string that you want CloudFront to prefix to the access log
    -- @filenames@ for this distribution, for example, @myprefix\/@. If you
    -- want to enable logging, but you don\'t want to specify a prefix, you
    -- still must include an empty @Prefix@ element in the @Logging@ element.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'loggingConfig_enabled' - Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you don\'t want to enable logging when you create a
-- distribution or if you want to disable logging for an existing
-- distribution, specify @false@ for @Enabled@, and specify empty @Bucket@
-- and @Prefix@ elements. If you specify @false@ for @Enabled@ but you
-- specify values for @Bucket@, @prefix@, and @IncludeCookies@, the values
-- are automatically deleted.
--
-- 'includeCookies', 'loggingConfig_includeCookies' - Specifies whether you want CloudFront to include cookies in access logs,
-- specify @true@ for @IncludeCookies@. If you choose to include cookies in
-- logs, CloudFront logs all cookies regardless of how you configure the
-- cache behaviors for this distribution. If you don\'t want to include
-- cookies when you create a distribution or if you want to disable include
-- cookies for an existing distribution, specify @false@ for
-- @IncludeCookies@.
--
-- 'bucket', 'loggingConfig_bucket' - The Amazon S3 bucket to store the access logs in, for example,
-- @myawslogbucket.s3.amazonaws.com@.
--
-- 'prefix', 'loggingConfig_prefix' - An optional string that you want CloudFront to prefix to the access log
-- @filenames@ for this distribution, for example, @myprefix\/@. If you
-- want to enable logging, but you don\'t want to specify a prefix, you
-- still must include an empty @Prefix@ element in the @Logging@ element.
newLoggingConfig ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'includeCookies'
  Prelude.Bool ->
  -- | 'bucket'
  Prelude.Text ->
  -- | 'prefix'
  Prelude.Text ->
  LoggingConfig
newLoggingConfig
  pEnabled_
  pIncludeCookies_
  pBucket_
  pPrefix_ =
    LoggingConfig'
      { enabled = pEnabled_,
        includeCookies = pIncludeCookies_,
        bucket = pBucket_,
        prefix = pPrefix_
      }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you don\'t want to enable logging when you create a
-- distribution or if you want to disable logging for an existing
-- distribution, specify @false@ for @Enabled@, and specify empty @Bucket@
-- and @Prefix@ elements. If you specify @false@ for @Enabled@ but you
-- specify values for @Bucket@, @prefix@, and @IncludeCookies@, the values
-- are automatically deleted.
loggingConfig_enabled :: Lens.Lens' LoggingConfig Prelude.Bool
loggingConfig_enabled = Lens.lens (\LoggingConfig' {enabled} -> enabled) (\s@LoggingConfig' {} a -> s {enabled = a} :: LoggingConfig)

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify @true@ for @IncludeCookies@. If you choose to include cookies in
-- logs, CloudFront logs all cookies regardless of how you configure the
-- cache behaviors for this distribution. If you don\'t want to include
-- cookies when you create a distribution or if you want to disable include
-- cookies for an existing distribution, specify @false@ for
-- @IncludeCookies@.
loggingConfig_includeCookies :: Lens.Lens' LoggingConfig Prelude.Bool
loggingConfig_includeCookies = Lens.lens (\LoggingConfig' {includeCookies} -> includeCookies) (\s@LoggingConfig' {} a -> s {includeCookies = a} :: LoggingConfig)

-- | The Amazon S3 bucket to store the access logs in, for example,
-- @myawslogbucket.s3.amazonaws.com@.
loggingConfig_bucket :: Lens.Lens' LoggingConfig Prelude.Text
loggingConfig_bucket = Lens.lens (\LoggingConfig' {bucket} -> bucket) (\s@LoggingConfig' {} a -> s {bucket = a} :: LoggingConfig)

-- | An optional string that you want CloudFront to prefix to the access log
-- @filenames@ for this distribution, for example, @myprefix\/@. If you
-- want to enable logging, but you don\'t want to specify a prefix, you
-- still must include an empty @Prefix@ element in the @Logging@ element.
loggingConfig_prefix :: Lens.Lens' LoggingConfig Prelude.Text
loggingConfig_prefix = Lens.lens (\LoggingConfig' {prefix} -> prefix) (\s@LoggingConfig' {} a -> s {prefix = a} :: LoggingConfig)

instance Prelude.FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      Prelude.<$> (x Prelude..@ "Enabled")
      Prelude.<*> (x Prelude..@ "IncludeCookies")
      Prelude.<*> (x Prelude..@ "Bucket")
      Prelude.<*> (x Prelude..@ "Prefix")

instance Prelude.Hashable LoggingConfig

instance Prelude.NFData LoggingConfig

instance Prelude.ToXML LoggingConfig where
  toXML LoggingConfig' {..} =
    Prelude.mconcat
      [ "Enabled" Prelude.@= enabled,
        "IncludeCookies" Prelude.@= includeCookies,
        "Bucket" Prelude.@= bucket,
        "Prefix" Prelude.@= prefix
      ]
