{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.Types.MixedInstancesPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MixedInstancesPolicy where

import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchTemplate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a mixed instances policy for an Auto Scaling group. With mixed
-- instances, your Auto Scaling group can provision a combination of
-- On-Demand Instances and Spot Instances across multiple instance types.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- You can create a mixed instances policy for a new Auto Scaling group, or
-- you can create it for an existing group by updating the group to specify
-- @MixedInstancesPolicy@ as the top-level parameter instead of a launch
-- configuration or launch template.
--
-- /See:/ 'newMixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'
  { -- | Specifies the instances distribution. If not provided, the value for
    -- each parameter in @InstancesDistribution@ uses a default value.
    instancesDistribution :: Prelude.Maybe InstancesDistribution,
    -- | Specifies the launch template to use and optionally the instance types
    -- (overrides) that are used to provision EC2 instances to fulfill
    -- On-Demand and Spot capacities. Required when creating a mixed instances
    -- policy.
    launchTemplate :: Prelude.Maybe LaunchTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MixedInstancesPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesDistribution', 'mixedInstancesPolicy_instancesDistribution' - Specifies the instances distribution. If not provided, the value for
-- each parameter in @InstancesDistribution@ uses a default value.
--
-- 'launchTemplate', 'mixedInstancesPolicy_launchTemplate' - Specifies the launch template to use and optionally the instance types
-- (overrides) that are used to provision EC2 instances to fulfill
-- On-Demand and Spot capacities. Required when creating a mixed instances
-- policy.
newMixedInstancesPolicy ::
  MixedInstancesPolicy
newMixedInstancesPolicy =
  MixedInstancesPolicy'
    { instancesDistribution =
        Prelude.Nothing,
      launchTemplate = Prelude.Nothing
    }

-- | Specifies the instances distribution. If not provided, the value for
-- each parameter in @InstancesDistribution@ uses a default value.
mixedInstancesPolicy_instancesDistribution :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe InstancesDistribution)
mixedInstancesPolicy_instancesDistribution = Lens.lens (\MixedInstancesPolicy' {instancesDistribution} -> instancesDistribution) (\s@MixedInstancesPolicy' {} a -> s {instancesDistribution = a} :: MixedInstancesPolicy)

-- | Specifies the launch template to use and optionally the instance types
-- (overrides) that are used to provision EC2 instances to fulfill
-- On-Demand and Spot capacities. Required when creating a mixed instances
-- policy.
mixedInstancesPolicy_launchTemplate :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe LaunchTemplate)
mixedInstancesPolicy_launchTemplate = Lens.lens (\MixedInstancesPolicy' {launchTemplate} -> launchTemplate) (\s@MixedInstancesPolicy' {} a -> s {launchTemplate = a} :: MixedInstancesPolicy)

instance Prelude.FromXML MixedInstancesPolicy where
  parseXML x =
    MixedInstancesPolicy'
      Prelude.<$> (x Prelude..@? "InstancesDistribution")
      Prelude.<*> (x Prelude..@? "LaunchTemplate")

instance Prelude.Hashable MixedInstancesPolicy

instance Prelude.NFData MixedInstancesPolicy

instance Prelude.ToQuery MixedInstancesPolicy where
  toQuery MixedInstancesPolicy' {..} =
    Prelude.mconcat
      [ "InstancesDistribution"
          Prelude.=: instancesDistribution,
        "LaunchTemplate" Prelude.=: launchTemplate
      ]
