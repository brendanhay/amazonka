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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the CloudTrail trail being analyzed to generate a
-- policy.
--
-- /See:/ 'newTrailProperties' smart constructor.
data TrailProperties = TrailProperties'
  { -- | Possible values are @true@ or @false@. If set to @true@, IAM Access
    -- Analyzer retrieves CloudTrail data from all regions to analyze and
    -- generate a policy.
    allRegions :: Prelude.Maybe Prelude.Bool,
    -- | A list of regions to get CloudTrail data from and analyze to generate a
    -- policy.
    regions :: Prelude.Maybe [Prelude.Text],
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
-- 'allRegions', 'trailProperties_allRegions' - Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
--
-- 'regions', 'trailProperties_regions' - A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
--
-- 'cloudTrailArn', 'trailProperties_cloudTrailArn' - Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
newTrailProperties ::
  -- | 'cloudTrailArn'
  Prelude.Text ->
  TrailProperties
newTrailProperties pCloudTrailArn_ =
  TrailProperties'
    { allRegions = Prelude.Nothing,
      regions = Prelude.Nothing,
      cloudTrailArn = pCloudTrailArn_
    }

-- | Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
trailProperties_allRegions :: Lens.Lens' TrailProperties (Prelude.Maybe Prelude.Bool)
trailProperties_allRegions = Lens.lens (\TrailProperties' {allRegions} -> allRegions) (\s@TrailProperties' {} a -> s {allRegions = a} :: TrailProperties)

-- | A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
trailProperties_regions :: Lens.Lens' TrailProperties (Prelude.Maybe [Prelude.Text])
trailProperties_regions = Lens.lens (\TrailProperties' {regions} -> regions) (\s@TrailProperties' {} a -> s {regions = a} :: TrailProperties) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
trailProperties_cloudTrailArn :: Lens.Lens' TrailProperties Prelude.Text
trailProperties_cloudTrailArn = Lens.lens (\TrailProperties' {cloudTrailArn} -> cloudTrailArn) (\s@TrailProperties' {} a -> s {cloudTrailArn = a} :: TrailProperties)

instance Data.FromJSON TrailProperties where
  parseJSON =
    Data.withObject
      "TrailProperties"
      ( \x ->
          TrailProperties'
            Prelude.<$> (x Data..:? "allRegions")
            Prelude.<*> (x Data..:? "regions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "cloudTrailArn")
      )

instance Prelude.Hashable TrailProperties where
  hashWithSalt _salt TrailProperties' {..} =
    _salt `Prelude.hashWithSalt` allRegions
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` cloudTrailArn

instance Prelude.NFData TrailProperties where
  rnf TrailProperties' {..} =
    Prelude.rnf allRegions
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf cloudTrailArn
