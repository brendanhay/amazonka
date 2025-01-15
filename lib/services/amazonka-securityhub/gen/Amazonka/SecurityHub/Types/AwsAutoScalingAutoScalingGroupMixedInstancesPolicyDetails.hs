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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails

-- | The mixed instances policy for the automatic scaling group.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails = AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails'
  { -- | The instances distribution. The instances distribution specifies the
    -- distribution of On-Demand Instances and Spot Instances, the maximum
    -- price to pay for Spot Instances, and how the Auto Scaling group
    -- allocates instance types to fulfill On-Demand and Spot capacity.
    instancesDistribution :: Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails,
    -- | The launch template to use and the instance types (overrides) to use to
    -- provision EC2 instances to fulfill On-Demand and Spot capacities.
    launchTemplate :: Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesDistribution', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution' - The instances distribution. The instances distribution specifies the
-- distribution of On-Demand Instances and Spot Instances, the maximum
-- price to pay for Spot Instances, and how the Auto Scaling group
-- allocates instance types to fulfill On-Demand and Spot capacity.
--
-- 'launchTemplate', 'awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate' - The launch template to use and the instance types (overrides) to use to
-- provision EC2 instances to fulfill On-Demand and Spot capacities.
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails ::
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
newAwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails =
  AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails'
    { instancesDistribution =
        Prelude.Nothing,
      launchTemplate =
        Prelude.Nothing
    }

-- | The instances distribution. The instances distribution specifies the
-- distribution of On-Demand Instances and Spot Instances, the maximum
-- price to pay for Spot Instances, and how the Auto Scaling group
-- allocates instance types to fulfill On-Demand and Spot capacity.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {instancesDistribution} -> instancesDistribution) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {} a -> s {instancesDistribution = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails)

-- | The launch template to use and the instance types (overrides) to use to
-- provision EC2 instances to fulfill On-Demand and Spot capacities.
awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate :: Lens.Lens' AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails)
awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate = Lens.lens (\AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {launchTemplate} -> launchTemplate) (\s@AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {} a -> s {launchTemplate = a} :: AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails)

instance
  Data.FromJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
  where
  parseJSON =
    Data.withObject
      "AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails'
            Prelude.<$> (x Data..:? "InstancesDistribution")
            Prelude.<*> (x Data..:? "LaunchTemplate")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {..} =
      _salt
        `Prelude.hashWithSalt` instancesDistribution
        `Prelude.hashWithSalt` launchTemplate

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {..} =
      Prelude.rnf instancesDistribution `Prelude.seq`
        Prelude.rnf launchTemplate

instance
  Data.ToJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("InstancesDistribution" Data..=)
                Prelude.<$> instancesDistribution,
              ("LaunchTemplate" Data..=)
                Prelude.<$> launchTemplate
            ]
        )
