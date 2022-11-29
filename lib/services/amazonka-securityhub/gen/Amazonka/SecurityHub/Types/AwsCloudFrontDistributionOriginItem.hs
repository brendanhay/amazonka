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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginCustomOriginConfig
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginS3OriginConfig

-- | A complex type that describes the Amazon S3 bucket, HTTP server (for
-- example, a web server), AWS Elemental MediaStore, or other server from
-- which CloudFront gets your files.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginItem' smart constructor.
data AwsCloudFrontDistributionOriginItem = AwsCloudFrontDistributionOriginItem'
  { -- | Amazon S3 origins: The DNS name of the S3 bucket from which you want
    -- CloudFront to get objects for this origin.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the origin or origin group.
    id :: Prelude.Maybe Prelude.Text,
    -- | An origin that is an S3 bucket that is not configured with static
    -- website hosting.
    s3OriginConfig :: Prelude.Maybe AwsCloudFrontDistributionOriginS3OriginConfig,
    -- | An optional element that causes CloudFront to request your content from
    -- a directory in your Amazon S3 bucket or your custom origin.
    originPath :: Prelude.Maybe Prelude.Text,
    -- | An origin that is not an Amazon S3 bucket, with one exception. If the
    -- Amazon S3 bucket is configured with static website hosting, use this
    -- attribute. If the Amazon S3 bucket is not configured with static website
    -- hosting, use the @S3OriginConfig@ type instead.
    customOriginConfig :: Prelude.Maybe AwsCloudFrontDistributionOriginCustomOriginConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'awsCloudFrontDistributionOriginItem_domainName' - Amazon S3 origins: The DNS name of the S3 bucket from which you want
-- CloudFront to get objects for this origin.
--
-- 'id', 'awsCloudFrontDistributionOriginItem_id' - A unique identifier for the origin or origin group.
--
-- 's3OriginConfig', 'awsCloudFrontDistributionOriginItem_s3OriginConfig' - An origin that is an S3 bucket that is not configured with static
-- website hosting.
--
-- 'originPath', 'awsCloudFrontDistributionOriginItem_originPath' - An optional element that causes CloudFront to request your content from
-- a directory in your Amazon S3 bucket or your custom origin.
--
-- 'customOriginConfig', 'awsCloudFrontDistributionOriginItem_customOriginConfig' - An origin that is not an Amazon S3 bucket, with one exception. If the
-- Amazon S3 bucket is configured with static website hosting, use this
-- attribute. If the Amazon S3 bucket is not configured with static website
-- hosting, use the @S3OriginConfig@ type instead.
newAwsCloudFrontDistributionOriginItem ::
  AwsCloudFrontDistributionOriginItem
newAwsCloudFrontDistributionOriginItem =
  AwsCloudFrontDistributionOriginItem'
    { domainName =
        Prelude.Nothing,
      id = Prelude.Nothing,
      s3OriginConfig = Prelude.Nothing,
      originPath = Prelude.Nothing,
      customOriginConfig = Prelude.Nothing
    }

-- | Amazon S3 origins: The DNS name of the S3 bucket from which you want
-- CloudFront to get objects for this origin.
awsCloudFrontDistributionOriginItem_domainName :: Lens.Lens' AwsCloudFrontDistributionOriginItem (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionOriginItem_domainName = Lens.lens (\AwsCloudFrontDistributionOriginItem' {domainName} -> domainName) (\s@AwsCloudFrontDistributionOriginItem' {} a -> s {domainName = a} :: AwsCloudFrontDistributionOriginItem)

-- | A unique identifier for the origin or origin group.
awsCloudFrontDistributionOriginItem_id :: Lens.Lens' AwsCloudFrontDistributionOriginItem (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionOriginItem_id = Lens.lens (\AwsCloudFrontDistributionOriginItem' {id} -> id) (\s@AwsCloudFrontDistributionOriginItem' {} a -> s {id = a} :: AwsCloudFrontDistributionOriginItem)

-- | An origin that is an S3 bucket that is not configured with static
-- website hosting.
awsCloudFrontDistributionOriginItem_s3OriginConfig :: Lens.Lens' AwsCloudFrontDistributionOriginItem (Prelude.Maybe AwsCloudFrontDistributionOriginS3OriginConfig)
awsCloudFrontDistributionOriginItem_s3OriginConfig = Lens.lens (\AwsCloudFrontDistributionOriginItem' {s3OriginConfig} -> s3OriginConfig) (\s@AwsCloudFrontDistributionOriginItem' {} a -> s {s3OriginConfig = a} :: AwsCloudFrontDistributionOriginItem)

-- | An optional element that causes CloudFront to request your content from
-- a directory in your Amazon S3 bucket or your custom origin.
awsCloudFrontDistributionOriginItem_originPath :: Lens.Lens' AwsCloudFrontDistributionOriginItem (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionOriginItem_originPath = Lens.lens (\AwsCloudFrontDistributionOriginItem' {originPath} -> originPath) (\s@AwsCloudFrontDistributionOriginItem' {} a -> s {originPath = a} :: AwsCloudFrontDistributionOriginItem)

-- | An origin that is not an Amazon S3 bucket, with one exception. If the
-- Amazon S3 bucket is configured with static website hosting, use this
-- attribute. If the Amazon S3 bucket is not configured with static website
-- hosting, use the @S3OriginConfig@ type instead.
awsCloudFrontDistributionOriginItem_customOriginConfig :: Lens.Lens' AwsCloudFrontDistributionOriginItem (Prelude.Maybe AwsCloudFrontDistributionOriginCustomOriginConfig)
awsCloudFrontDistributionOriginItem_customOriginConfig = Lens.lens (\AwsCloudFrontDistributionOriginItem' {customOriginConfig} -> customOriginConfig) (\s@AwsCloudFrontDistributionOriginItem' {} a -> s {customOriginConfig = a} :: AwsCloudFrontDistributionOriginItem)

instance
  Core.FromJSON
    AwsCloudFrontDistributionOriginItem
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionOriginItem"
      ( \x ->
          AwsCloudFrontDistributionOriginItem'
            Prelude.<$> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "S3OriginConfig")
            Prelude.<*> (x Core..:? "OriginPath")
            Prelude.<*> (x Core..:? "CustomOriginConfig")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginItem
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginItem' {..} =
      _salt `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` s3OriginConfig
        `Prelude.hashWithSalt` originPath
        `Prelude.hashWithSalt` customOriginConfig

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginItem
  where
  rnf AwsCloudFrontDistributionOriginItem' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf s3OriginConfig
      `Prelude.seq` Prelude.rnf originPath
      `Prelude.seq` Prelude.rnf customOriginConfig

instance
  Core.ToJSON
    AwsCloudFrontDistributionOriginItem
  where
  toJSON AwsCloudFrontDistributionOriginItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DomainName" Core..=) Prelude.<$> domainName,
            ("Id" Core..=) Prelude.<$> id,
            ("S3OriginConfig" Core..=)
              Prelude.<$> s3OriginConfig,
            ("OriginPath" Core..=) Prelude.<$> originPath,
            ("CustomOriginConfig" Core..=)
              Prelude.<$> customOriginConfig
          ]
      )
