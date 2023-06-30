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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that controls whether access logs are written for the
-- CloudFront distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionLogging' smart constructor.
data AwsCloudFrontDistributionLogging = AwsCloudFrontDistributionLogging'
  { -- | The S3 bucket to store the access logs in.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | With this field, you can enable or disable the selected distribution.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether you want CloudFront to include cookies in access logs.
    includeCookies :: Prelude.Maybe Prelude.Bool,
    -- | An optional string that you want CloudFront to use as a prefix to the
    -- access log filenames for this distribution.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'awsCloudFrontDistributionLogging_bucket' - The S3 bucket to store the access logs in.
--
-- 'enabled', 'awsCloudFrontDistributionLogging_enabled' - With this field, you can enable or disable the selected distribution.
--
-- 'includeCookies', 'awsCloudFrontDistributionLogging_includeCookies' - Specifies whether you want CloudFront to include cookies in access logs.
--
-- 'prefix', 'awsCloudFrontDistributionLogging_prefix' - An optional string that you want CloudFront to use as a prefix to the
-- access log filenames for this distribution.
newAwsCloudFrontDistributionLogging ::
  AwsCloudFrontDistributionLogging
newAwsCloudFrontDistributionLogging =
  AwsCloudFrontDistributionLogging'
    { bucket =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      includeCookies = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The S3 bucket to store the access logs in.
awsCloudFrontDistributionLogging_bucket :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionLogging_bucket = Lens.lens (\AwsCloudFrontDistributionLogging' {bucket} -> bucket) (\s@AwsCloudFrontDistributionLogging' {} a -> s {bucket = a} :: AwsCloudFrontDistributionLogging)

-- | With this field, you can enable or disable the selected distribution.
awsCloudFrontDistributionLogging_enabled :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Bool)
awsCloudFrontDistributionLogging_enabled = Lens.lens (\AwsCloudFrontDistributionLogging' {enabled} -> enabled) (\s@AwsCloudFrontDistributionLogging' {} a -> s {enabled = a} :: AwsCloudFrontDistributionLogging)

-- | Specifies whether you want CloudFront to include cookies in access logs.
awsCloudFrontDistributionLogging_includeCookies :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Bool)
awsCloudFrontDistributionLogging_includeCookies = Lens.lens (\AwsCloudFrontDistributionLogging' {includeCookies} -> includeCookies) (\s@AwsCloudFrontDistributionLogging' {} a -> s {includeCookies = a} :: AwsCloudFrontDistributionLogging)

-- | An optional string that you want CloudFront to use as a prefix to the
-- access log filenames for this distribution.
awsCloudFrontDistributionLogging_prefix :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionLogging_prefix = Lens.lens (\AwsCloudFrontDistributionLogging' {prefix} -> prefix) (\s@AwsCloudFrontDistributionLogging' {} a -> s {prefix = a} :: AwsCloudFrontDistributionLogging)

instance
  Data.FromJSON
    AwsCloudFrontDistributionLogging
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionLogging"
      ( \x ->
          AwsCloudFrontDistributionLogging'
            Prelude.<$> (x Data..:? "Bucket")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "IncludeCookies")
            Prelude.<*> (x Data..:? "Prefix")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionLogging
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionLogging' {..} =
      _salt
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` includeCookies
        `Prelude.hashWithSalt` prefix

instance
  Prelude.NFData
    AwsCloudFrontDistributionLogging
  where
  rnf AwsCloudFrontDistributionLogging' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf includeCookies
      `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON AwsCloudFrontDistributionLogging where
  toJSON AwsCloudFrontDistributionLogging' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bucket" Data..=) Prelude.<$> bucket,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("IncludeCookies" Data..=)
              Prelude.<$> includeCookies,
            ("Prefix" Data..=) Prelude.<$> prefix
          ]
      )
