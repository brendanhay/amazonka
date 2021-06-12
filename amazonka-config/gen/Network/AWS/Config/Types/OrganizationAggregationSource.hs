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
-- Module      : Network.AWS.Config.Types.OrganizationAggregationSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationAggregationSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This object contains regions to set up the aggregator and an IAM role to
-- retrieve organization details.
--
-- /See:/ 'newOrganizationAggregationSource' smart constructor.
data OrganizationAggregationSource = OrganizationAggregationSource'
  { -- | If true, aggregate existing AWS Config regions and future regions.
    allAwsRegions :: Core.Maybe Core.Bool,
    -- | The source regions being aggregated.
    awsRegions :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | ARN of the IAM role used to retrieve AWS Organization details associated
    -- with the aggregator account.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationAggregationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allAwsRegions', 'organizationAggregationSource_allAwsRegions' - If true, aggregate existing AWS Config regions and future regions.
--
-- 'awsRegions', 'organizationAggregationSource_awsRegions' - The source regions being aggregated.
--
-- 'roleArn', 'organizationAggregationSource_roleArn' - ARN of the IAM role used to retrieve AWS Organization details associated
-- with the aggregator account.
newOrganizationAggregationSource ::
  -- | 'roleArn'
  Core.Text ->
  OrganizationAggregationSource
newOrganizationAggregationSource pRoleArn_ =
  OrganizationAggregationSource'
    { allAwsRegions =
        Core.Nothing,
      awsRegions = Core.Nothing,
      roleArn = pRoleArn_
    }

-- | If true, aggregate existing AWS Config regions and future regions.
organizationAggregationSource_allAwsRegions :: Lens.Lens' OrganizationAggregationSource (Core.Maybe Core.Bool)
organizationAggregationSource_allAwsRegions = Lens.lens (\OrganizationAggregationSource' {allAwsRegions} -> allAwsRegions) (\s@OrganizationAggregationSource' {} a -> s {allAwsRegions = a} :: OrganizationAggregationSource)

-- | The source regions being aggregated.
organizationAggregationSource_awsRegions :: Lens.Lens' OrganizationAggregationSource (Core.Maybe (Core.NonEmpty Core.Text))
organizationAggregationSource_awsRegions = Lens.lens (\OrganizationAggregationSource' {awsRegions} -> awsRegions) (\s@OrganizationAggregationSource' {} a -> s {awsRegions = a} :: OrganizationAggregationSource) Core.. Lens.mapping Lens._Coerce

-- | ARN of the IAM role used to retrieve AWS Organization details associated
-- with the aggregator account.
organizationAggregationSource_roleArn :: Lens.Lens' OrganizationAggregationSource Core.Text
organizationAggregationSource_roleArn = Lens.lens (\OrganizationAggregationSource' {roleArn} -> roleArn) (\s@OrganizationAggregationSource' {} a -> s {roleArn = a} :: OrganizationAggregationSource)

instance Core.FromJSON OrganizationAggregationSource where
  parseJSON =
    Core.withObject
      "OrganizationAggregationSource"
      ( \x ->
          OrganizationAggregationSource'
            Core.<$> (x Core..:? "AllAwsRegions")
            Core.<*> (x Core..:? "AwsRegions")
            Core.<*> (x Core..: "RoleArn")
      )

instance Core.Hashable OrganizationAggregationSource

instance Core.NFData OrganizationAggregationSource

instance Core.ToJSON OrganizationAggregationSource where
  toJSON OrganizationAggregationSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllAwsRegions" Core..=) Core.<$> allAwsRegions,
            ("AwsRegions" Core..=) Core.<$> awsRegions,
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )
