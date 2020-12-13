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
    oasAWSRegions,
    oasAllAWSRegions,
    oasRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This object contains regions to set up the aggregator and an IAM role to retrieve organization details.
--
-- /See:/ 'mkOrganizationAggregationSource' smart constructor.
data OrganizationAggregationSource = OrganizationAggregationSource'
  { -- | The source regions being aggregated.
    awsRegions :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | If true, aggregate existing AWS Config regions and future regions.
    allAWSRegions :: Lude.Maybe Lude.Bool,
    -- | ARN of the IAM role used to retrieve AWS Organization details associated with the aggregator account.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationAggregationSource' with the minimum fields required to make a request.
--
-- * 'awsRegions' - The source regions being aggregated.
-- * 'allAWSRegions' - If true, aggregate existing AWS Config regions and future regions.
-- * 'roleARN' - ARN of the IAM role used to retrieve AWS Organization details associated with the aggregator account.
mkOrganizationAggregationSource ::
  -- | 'roleARN'
  Lude.Text ->
  OrganizationAggregationSource
mkOrganizationAggregationSource pRoleARN_ =
  OrganizationAggregationSource'
    { awsRegions = Lude.Nothing,
      allAWSRegions = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The source regions being aggregated.
--
-- /Note:/ Consider using 'awsRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasAWSRegions :: Lens.Lens' OrganizationAggregationSource (Lude.Maybe (Lude.NonEmpty Lude.Text))
oasAWSRegions = Lens.lens (awsRegions :: OrganizationAggregationSource -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {awsRegions = a} :: OrganizationAggregationSource)
{-# DEPRECATED oasAWSRegions "Use generic-lens or generic-optics with 'awsRegions' instead." #-}

-- | If true, aggregate existing AWS Config regions and future regions.
--
-- /Note:/ Consider using 'allAWSRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasAllAWSRegions :: Lens.Lens' OrganizationAggregationSource (Lude.Maybe Lude.Bool)
oasAllAWSRegions = Lens.lens (allAWSRegions :: OrganizationAggregationSource -> Lude.Maybe Lude.Bool) (\s a -> s {allAWSRegions = a} :: OrganizationAggregationSource)
{-# DEPRECATED oasAllAWSRegions "Use generic-lens or generic-optics with 'allAWSRegions' instead." #-}

-- | ARN of the IAM role used to retrieve AWS Organization details associated with the aggregator account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oasRoleARN :: Lens.Lens' OrganizationAggregationSource Lude.Text
oasRoleARN = Lens.lens (roleARN :: OrganizationAggregationSource -> Lude.Text) (\s a -> s {roleARN = a} :: OrganizationAggregationSource)
{-# DEPRECATED oasRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON OrganizationAggregationSource where
  parseJSON =
    Lude.withObject
      "OrganizationAggregationSource"
      ( \x ->
          OrganizationAggregationSource'
            Lude.<$> (x Lude..:? "AwsRegions")
            Lude.<*> (x Lude..:? "AllAwsRegions")
            Lude.<*> (x Lude..: "RoleArn")
      )

instance Lude.ToJSON OrganizationAggregationSource where
  toJSON OrganizationAggregationSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AwsRegions" Lude..=) Lude.<$> awsRegions,
            ("AllAwsRegions" Lude..=) Lude.<$> allAWSRegions,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )
