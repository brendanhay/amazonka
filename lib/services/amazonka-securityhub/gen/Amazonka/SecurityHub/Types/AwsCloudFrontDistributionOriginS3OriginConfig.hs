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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginS3OriginConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginS3OriginConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an origin that is an Amazon S3 bucket that is not
-- configured with static website hosting.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginS3OriginConfig' smart constructor.
data AwsCloudFrontDistributionOriginS3OriginConfig = AwsCloudFrontDistributionOriginS3OriginConfig'
  { -- | The CloudFront origin access identity to associate with the origin.
    originAccessIdentity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginS3OriginConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessIdentity', 'awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity' - The CloudFront origin access identity to associate with the origin.
newAwsCloudFrontDistributionOriginS3OriginConfig ::
  AwsCloudFrontDistributionOriginS3OriginConfig
newAwsCloudFrontDistributionOriginS3OriginConfig =
  AwsCloudFrontDistributionOriginS3OriginConfig'
    { originAccessIdentity =
        Prelude.Nothing
    }

-- | The CloudFront origin access identity to associate with the origin.
awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity :: Lens.Lens' AwsCloudFrontDistributionOriginS3OriginConfig (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity = Lens.lens (\AwsCloudFrontDistributionOriginS3OriginConfig' {originAccessIdentity} -> originAccessIdentity) (\s@AwsCloudFrontDistributionOriginS3OriginConfig' {} a -> s {originAccessIdentity = a} :: AwsCloudFrontDistributionOriginS3OriginConfig)

instance
  Core.FromJSON
    AwsCloudFrontDistributionOriginS3OriginConfig
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionOriginS3OriginConfig"
      ( \x ->
          AwsCloudFrontDistributionOriginS3OriginConfig'
            Prelude.<$> (x Core..:? "OriginAccessIdentity")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginS3OriginConfig
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginS3OriginConfig' {..} =
      _salt `Prelude.hashWithSalt` originAccessIdentity

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginS3OriginConfig
  where
  rnf
    AwsCloudFrontDistributionOriginS3OriginConfig' {..} =
      Prelude.rnf originAccessIdentity

instance
  Core.ToJSON
    AwsCloudFrontDistributionOriginS3OriginConfig
  where
  toJSON
    AwsCloudFrontDistributionOriginS3OriginConfig' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("OriginAccessIdentity" Core..=)
                Prelude.<$> originAccessIdentity
            ]
        )
