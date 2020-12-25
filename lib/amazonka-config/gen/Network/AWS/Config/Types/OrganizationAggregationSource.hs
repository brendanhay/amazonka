{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationAggregationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationAggregationSource
  ( OrganizationAggregationSource (..),

    -- * Smart constructor
    mkOrganizationAggregationSource,

    -- * Lenses
    oasRoleArn,
    oasAllAwsRegions,
    oasAwsRegions,
  )
where

import qualified Network.AWS.Config.Types.RoleArn as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This object contains regions to set up the aggregator and an IAM role to retrieve organization details.
--
-- /See:/ 'mkOrganizationAggregationSource' smart constructor.
data OrganizationAggregationSource = OrganizationAggregationSource'
  { -- | ARN of the IAM role used to retrieve AWS Organization details associated with the aggregator account.
    roleArn :: Types.RoleArn,
    -- | If true, aggregate existing AWS Config regions and future regions.
    allAwsRegions :: Core.Maybe Core.Bool,
    -- | The source regions being aggregated.
    awsRegions :: Core.Maybe (Core.NonEmpty Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationAggregationSource' value with any optional fields omitted.
mkOrganizationAggregationSource ::
  -- | 'roleArn'
  Types.RoleArn ->
  OrganizationAggregationSource
mkOrganizationAggregationSource roleArn =
  OrganizationAggregationSource'
    { roleArn,
      allAwsRegions = Core.Nothing,
      awsRegions = Core.Nothing
    }

-- | ARN of the IAM role used to retrieve AWS Organization details associated with the aggregator account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasRoleArn :: Lens.Lens' OrganizationAggregationSource Types.RoleArn
oasRoleArn = Lens.field @"roleArn"
{-# DEPRECATED oasRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | If true, aggregate existing AWS Config regions and future regions.
--
-- /Note:/ Consider using 'allAwsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasAllAwsRegions :: Lens.Lens' OrganizationAggregationSource (Core.Maybe Core.Bool)
oasAllAwsRegions = Lens.field @"allAwsRegions"
{-# DEPRECATED oasAllAwsRegions "Use generic-lens or generic-optics with 'allAwsRegions' instead." #-}

-- | The source regions being aggregated.
--
-- /Note:/ Consider using 'awsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasAwsRegions :: Lens.Lens' OrganizationAggregationSource (Core.Maybe (Core.NonEmpty Types.String))
oasAwsRegions = Lens.field @"awsRegions"
{-# DEPRECATED oasAwsRegions "Use generic-lens or generic-optics with 'awsRegions' instead." #-}

instance Core.FromJSON OrganizationAggregationSource where
  toJSON OrganizationAggregationSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RoleArn" Core..= roleArn),
            ("AllAwsRegions" Core..=) Core.<$> allAwsRegions,
            ("AwsRegions" Core..=) Core.<$> awsRegions
          ]
      )

instance Core.FromJSON OrganizationAggregationSource where
  parseJSON =
    Core.withObject "OrganizationAggregationSource" Core.$
      \x ->
        OrganizationAggregationSource'
          Core.<$> (x Core..: "RoleArn")
          Core.<*> (x Core..:? "AllAwsRegions")
          Core.<*> (x Core..:? "AwsRegions")
