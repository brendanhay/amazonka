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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the instances distribution.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
  { -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances.
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | The percentage of On-Demand Instances and Spot Instances for additional
    -- capacity beyond @OnDemandBaseCapacity@.
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | How to allocate instance types to fulfill On-Demand capacity. The valid
    -- value is @prioritized@.
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances.
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    spotMaxPrice :: Prelude.Maybe Prelude.Text,
    -- | How to allocate instances across Spot Instance pools. Valid values are
    -- as follows:
    --
    -- -   @lowest-price@
    --
    -- -   @capacity-optimized@
    --
    -- -   @capacity-optimized-prioritized@
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandBaseCapacity', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances.
--
-- 'onDemandPercentageAboveBaseCapacity', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity' - The percentage of On-Demand Instances and Spot Instances for additional
-- capacity beyond @OnDemandBaseCapacity@.
--
-- 'onDemandAllocationStrategy', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy' - How to allocate instance types to fulfill On-Demand capacity. The valid
-- value is @prioritized@.
--
-- 'spotInstancePools', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances.
--
-- 'spotMaxPrice', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
--
-- 'spotAllocationStrategy', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy' - How to allocate instances across Spot Instance pools. Valid values are
-- as follows:
--
-- -   @lowest-price@
--
-- -   @capacity-optimized@
--
-- -   @capacity-optimized-prioritized@
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
    { onDemandBaseCapacity =
        Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing,
      onDemandAllocationStrategy =
        Prelude.Nothing,
      spotInstancePools =
        Prelude.Nothing,
      spotMaxPrice =
        Prelude.Nothing,
      spotAllocationStrategy =
        Prelude.Nothing
    }

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandBaseCapacity = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The percentage of On-Demand Instances and Spot Instances for additional
-- capacity beyond @OnDemandBaseCapacity@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | How to allocate instance types to fulfill On-Demand capacity. The valid
-- value is @prioritized@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandAllocationStrategy = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {spotInstancePools} -> spotInstancePools) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {spotInstancePools = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {spotMaxPrice} -> spotMaxPrice) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {spotMaxPrice = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | How to allocate instances across Spot Instance pools. Valid values are
-- as follows:
--
-- -   @lowest-price@
--
-- -   @capacity-optimized@
--
-- -   @capacity-optimized-prioritized@
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {spotAllocationStrategy} -> spotAllocationStrategy) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {spotAllocationStrategy = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

instance
  Core.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  parseJSON =
    Core.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
            Prelude.<$> (x Core..:? "OnDemandBaseCapacity")
              Prelude.<*> (x Core..:? "OnDemandPercentageAboveBaseCapacity")
              Prelude.<*> (x Core..:? "OnDemandAllocationStrategy")
              Prelude.<*> (x Core..:? "SpotInstancePools")
              Prelude.<*> (x Core..:? "SpotMaxPrice")
              Prelude.<*> (x Core..:? "SpotAllocationStrategy")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      _salt `Prelude.hashWithSalt` onDemandBaseCapacity
        `Prelude.hashWithSalt` onDemandPercentageAboveBaseCapacity
        `Prelude.hashWithSalt` onDemandAllocationStrategy
        `Prelude.hashWithSalt` spotInstancePools
        `Prelude.hashWithSalt` spotMaxPrice
        `Prelude.hashWithSalt` spotAllocationStrategy

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      Prelude.rnf onDemandBaseCapacity
        `Prelude.seq` Prelude.rnf onDemandPercentageAboveBaseCapacity
        `Prelude.seq` Prelude.rnf onDemandAllocationStrategy
        `Prelude.seq` Prelude.rnf spotInstancePools
        `Prelude.seq` Prelude.rnf spotMaxPrice
        `Prelude.seq` Prelude.rnf spotAllocationStrategy

instance
  Core.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("OnDemandBaseCapacity" Core..=)
                Prelude.<$> onDemandBaseCapacity,
              ("OnDemandPercentageAboveBaseCapacity" Core..=)
                Prelude.<$> onDemandPercentageAboveBaseCapacity,
              ("OnDemandAllocationStrategy" Core..=)
                Prelude.<$> onDemandAllocationStrategy,
              ("SpotInstancePools" Core..=)
                Prelude.<$> spotInstancePools,
              ("SpotMaxPrice" Core..=) Prelude.<$> spotMaxPrice,
              ("SpotAllocationStrategy" Core..=)
                Prelude.<$> spotAllocationStrategy
            ]
        )
