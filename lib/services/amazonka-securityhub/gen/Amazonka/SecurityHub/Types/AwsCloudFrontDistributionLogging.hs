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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionLogging' smart constructor.
data AwsCloudFrontDistributionLogging = AwsCloudFrontDistributionLogging'
  { -- | With this field, you can enable or disable the selected distribution.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An optional string that you want CloudFront to use as a prefix to the
    -- access log filenames for this distribution.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket to store the access logs in.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you want CloudFront to include cookies in access logs.
    includeCookies :: Prelude.Maybe Prelude.Bool
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
-- 'enabled', 'awsCloudFrontDistributionLogging_enabled' - With this field, you can enable or disable the selected distribution.
--
-- 'prefix', 'awsCloudFrontDistributionLogging_prefix' - An optional string that you want CloudFront to use as a prefix to the
-- access log filenames for this distribution.
--
-- 'bucket', 'awsCloudFrontDistributionLogging_bucket' - The S3 bucket to store the access logs in.
--
-- 'includeCookies', 'awsCloudFrontDistributionLogging_includeCookies' - Specifies whether you want CloudFront to include cookies in access logs.
newAwsCloudFrontDistributionLogging ::
  AwsCloudFrontDistributionLogging
newAwsCloudFrontDistributionLogging =
  AwsCloudFrontDistributionLogging'
    { enabled =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      bucket = Prelude.Nothing,
      includeCookies = Prelude.Nothing
    }

-- | With this field, you can enable or disable the selected distribution.
awsCloudFrontDistributionLogging_enabled :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Bool)
awsCloudFrontDistributionLogging_enabled = Lens.lens (\AwsCloudFrontDistributionLogging' {enabled} -> enabled) (\s@AwsCloudFrontDistributionLogging' {} a -> s {enabled = a} :: AwsCloudFrontDistributionLogging)

-- | An optional string that you want CloudFront to use as a prefix to the
-- access log filenames for this distribution.
awsCloudFrontDistributionLogging_prefix :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionLogging_prefix = Lens.lens (\AwsCloudFrontDistributionLogging' {prefix} -> prefix) (\s@AwsCloudFrontDistributionLogging' {} a -> s {prefix = a} :: AwsCloudFrontDistributionLogging)

-- | The S3 bucket to store the access logs in.
awsCloudFrontDistributionLogging_bucket :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionLogging_bucket = Lens.lens (\AwsCloudFrontDistributionLogging' {bucket} -> bucket) (\s@AwsCloudFrontDistributionLogging' {} a -> s {bucket = a} :: AwsCloudFrontDistributionLogging)

-- | Specifies whether you want CloudFront to include cookies in access logs.
awsCloudFrontDistributionLogging_includeCookies :: Lens.Lens' AwsCloudFrontDistributionLogging (Prelude.Maybe Prelude.Bool)
awsCloudFrontDistributionLogging_includeCookies = Lens.lens (\AwsCloudFrontDistributionLogging' {includeCookies} -> includeCookies) (\s@AwsCloudFrontDistributionLogging' {} a -> s {includeCookies = a} :: AwsCloudFrontDistributionLogging)

instance
  Core.FromJSON
    AwsCloudFrontDistributionLogging
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionLogging"
      ( \x ->
          AwsCloudFrontDistributionLogging'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "Prefix")
            Prelude.<*> (x Core..:? "Bucket")
            Prelude.<*> (x Core..:? "IncludeCookies")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionLogging

instance
  Prelude.NFData
    AwsCloudFrontDistributionLogging

instance Core.ToJSON AwsCloudFrontDistributionLogging where
  toJSON AwsCloudFrontDistributionLogging' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("Prefix" Core..=) Prelude.<$> prefix,
            ("Bucket" Core..=) Prelude.<$> bucket,
            ("IncludeCookies" Core..=)
              Prelude.<$> includeCookies
          ]
      )
