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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the instances distribution.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
  { -- | How to allocate instance types to fulfill On-Demand capacity. The valid
    -- value is @prioritized@.
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances.
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | The percentage of On-Demand Instances and Spot Instances for additional
    -- capacity beyond @OnDemandBaseCapacity@.
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | How to allocate instances across Spot Instance pools. Valid values are
    -- as follows:
    --
    -- -   @lowest-price@
    --
    -- -   @capacity-optimized@
    --
    -- -   @capacity-optimized-prioritized@
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances.
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    spotMaxPrice :: Prelude.Maybe Prelude.Text
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
-- 'onDemandAllocationStrategy', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy' - How to allocate instance types to fulfill On-Demand capacity. The valid
-- value is @prioritized@.
--
-- 'onDemandBaseCapacity', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances.
--
-- 'onDemandPercentageAboveBaseCapacity', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity' - The percentage of On-Demand Instances and Spot Instances for additional
-- capacity beyond @OnDemandBaseCapacity@.
--
-- 'spotAllocationStrategy', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy' - How to allocate instances across Spot Instance pools. Valid values are
-- as follows:
--
-- -   @lowest-price@
--
-- -   @capacity-optimized@
--
-- -   @capacity-optimized-prioritized@
--
-- 'spotInstancePools', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances.
--
-- 'spotMaxPrice', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
    { onDemandAllocationStrategy =
        Prelude.Nothing,
      onDemandBaseCapacity =
        Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing,
      spotAllocationStrategy =
        Prelude.Nothing,
      spotInstancePools =
        Prelude.Nothing,
      spotMaxPrice =
        Prelude.Nothing
    }

-- | How to allocate instance types to fulfill On-Demand capacity. The valid
-- value is @prioritized@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandAllocationStrategy = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandBaseCapacity = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The percentage of On-Demand Instances and Spot Instances for additional
-- capacity beyond @OnDemandBaseCapacity@.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

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

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Int)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {spotInstancePools} -> spotInstancePools) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {spotInstancePools = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {spotMaxPrice} -> spotMaxPrice) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {} a -> s {spotMaxPrice = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails'
            Prelude.<$> (x Data..:? "OnDemandAllocationStrategy")
            Prelude.<*> (x Data..:? "OnDemandBaseCapacity")
            Prelude.<*> (x Data..:? "OnDemandPercentageAboveBaseCapacity")
            Prelude.<*> (x Data..:? "SpotAllocationStrategy")
            Prelude.<*> (x Data..:? "SpotInstancePools")
            Prelude.<*> (x Data..:? "SpotMaxPrice")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      _salt
        `Prelude.hashWithSalt` onDemandAllocationStrategy
        `Prelude.hashWithSalt` onDemandBaseCapacity
        `Prelude.hashWithSalt` onDemandPercentageAboveBaseCapacity
        `Prelude.hashWithSalt` spotAllocationStrategy
        `Prelude.hashWithSalt` spotInstancePools
        `Prelude.hashWithSalt` spotMaxPrice

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      Prelude.rnf onDemandAllocationStrategy
        `Prelude.seq` Prelude.rnf onDemandBaseCapacity
        `Prelude.seq` Prelude.rnf onDemandPercentageAboveBaseCapacity
        `Prelude.seq` Prelude.rnf spotAllocationStrategy
        `Prelude.seq` Prelude.rnf spotInstancePools
        `Prelude.seq` Prelude.rnf spotMaxPrice

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("OnDemandAllocationStrategy" Data..=)
                Prelude.<$> onDemandAllocationStrategy,
              ("OnDemandBaseCapacity" Data..=)
                Prelude.<$> onDemandBaseCapacity,
              ("OnDemandPercentageAboveBaseCapacity" Data..=)
                Prelude.<$> onDemandPercentageAboveBaseCapacity,
              ("SpotAllocationStrategy" Data..=)
                Prelude.<$> spotAllocationStrategy,
              ("SpotInstancePools" Data..=)
                Prelude.<$> spotInstancePools,
              ("SpotMaxPrice" Data..=) Prelude.<$> spotMaxPrice
            ]
        )
