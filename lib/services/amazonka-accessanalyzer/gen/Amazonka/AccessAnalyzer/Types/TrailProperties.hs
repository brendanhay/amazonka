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
-- Module      : Amazonka.AccessAnalyzer.Types.TrailProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.TrailProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the CloudTrail trail being analyzed to generate a
-- policy.
--
-- /See:/ 'newTrailProperties' smart constructor.
data TrailProperties = TrailProperties'
  { -- | A list of regions to get CloudTrail data from and analyze to generate a
    -- policy.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | Possible values are @true@ or @false@. If set to @true@, IAM Access
    -- Analyzer retrieves CloudTrail data from all regions to analyze and
    -- generate a policy.
    allRegions :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the ARN of the trail. The format of a trail ARN is
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
    cloudTrailArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrailProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'trailProperties_regions' - A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
--
-- 'allRegions', 'trailProperties_allRegions' - Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
--
-- 'cloudTrailArn', 'trailProperties_cloudTrailArn' - Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
newTrailProperties ::
  -- | 'cloudTrailArn'
  Prelude.Text ->
  TrailProperties
newTrailProperties pCloudTrailArn_ =
  TrailProperties'
    { regions = Prelude.Nothing,
      allRegions = Prelude.Nothing,
      cloudTrailArn = pCloudTrailArn_
    }

-- | A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
trailProperties_regions :: Lens.Lens' TrailProperties (Prelude.Maybe [Prelude.Text])
trailProperties_regions = Lens.lens (\TrailProperties' {regions} -> regions) (\s@TrailProperties' {} a -> s {regions = a} :: TrailProperties) Prelude.. Lens.mapping Lens.coerced

-- | Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
trailProperties_allRegions :: Lens.Lens' TrailProperties (Prelude.Maybe Prelude.Bool)
trailProperties_allRegions = Lens.lens (\TrailProperties' {allRegions} -> allRegions) (\s@TrailProperties' {} a -> s {allRegions = a} :: TrailProperties)

-- | Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
trailProperties_cloudTrailArn :: Lens.Lens' TrailProperties Prelude.Text
trailProperties_cloudTrailArn = Lens.lens (\TrailProperties' {cloudTrailArn} -> cloudTrailArn) (\s@TrailProperties' {} a -> s {cloudTrailArn = a} :: TrailProperties)

instance Core.FromJSON TrailProperties where
  parseJSON =
    Core.withObject
      "TrailProperties"
      ( \x ->
          TrailProperties'
            Prelude.<$> (x Core..:? "regions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allRegions")
            Prelude.<*> (x Core..: "cloudTrailArn")
      )

instance Prelude.Hashable TrailProperties where
  hashWithSalt _salt TrailProperties' {..} =
    _salt `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` allRegions
      `Prelude.hashWithSalt` cloudTrailArn

instance Prelude.NFData TrailProperties where
  rnf TrailProperties' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf allRegions
      `Prelude.seq` Prelude.rnf cloudTrailArn
