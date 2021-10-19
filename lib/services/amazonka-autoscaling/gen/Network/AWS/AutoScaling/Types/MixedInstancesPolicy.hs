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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a mixed instances policy. A mixed instances policy contains
-- the instance types Amazon EC2 Auto Scaling can launch, and other
-- information Amazon EC2 Auto Scaling can use to launch instances to help
-- you optimize your costs. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newMixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'
  { -- | Specifies the launch template to use and the instance types (overrides)
    -- that are used to provision EC2 instances to fulfill On-Demand and Spot
    -- capacities. Required when creating a mixed instances policy.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | Specifies the instances distribution. If not provided, the value for
    -- each property in @InstancesDistribution@ uses a default value.
    instancesDistribution :: Prelude.Maybe InstancesDistribution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MixedInstancesPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'mixedInstancesPolicy_launchTemplate' - Specifies the launch template to use and the instance types (overrides)
-- that are used to provision EC2 instances to fulfill On-Demand and Spot
-- capacities. Required when creating a mixed instances policy.
--
-- 'instancesDistribution', 'mixedInstancesPolicy_instancesDistribution' - Specifies the instances distribution. If not provided, the value for
-- each property in @InstancesDistribution@ uses a default value.
newMixedInstancesPolicy ::
  MixedInstancesPolicy
newMixedInstancesPolicy =
  MixedInstancesPolicy'
    { launchTemplate =
        Prelude.Nothing,
      instancesDistribution = Prelude.Nothing
    }

-- | Specifies the launch template to use and the instance types (overrides)
-- that are used to provision EC2 instances to fulfill On-Demand and Spot
-- capacities. Required when creating a mixed instances policy.
mixedInstancesPolicy_launchTemplate :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe LaunchTemplate)
mixedInstancesPolicy_launchTemplate = Lens.lens (\MixedInstancesPolicy' {launchTemplate} -> launchTemplate) (\s@MixedInstancesPolicy' {} a -> s {launchTemplate = a} :: MixedInstancesPolicy)

-- | Specifies the instances distribution. If not provided, the value for
-- each property in @InstancesDistribution@ uses a default value.
mixedInstancesPolicy_instancesDistribution :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe InstancesDistribution)
mixedInstancesPolicy_instancesDistribution = Lens.lens (\MixedInstancesPolicy' {instancesDistribution} -> instancesDistribution) (\s@MixedInstancesPolicy' {} a -> s {instancesDistribution = a} :: MixedInstancesPolicy)

instance Core.FromXML MixedInstancesPolicy where
  parseXML x =
    MixedInstancesPolicy'
      Prelude.<$> (x Core..@? "LaunchTemplate")
      Prelude.<*> (x Core..@? "InstancesDistribution")

instance Prelude.Hashable MixedInstancesPolicy

instance Prelude.NFData MixedInstancesPolicy

instance Core.ToQuery MixedInstancesPolicy where
  toQuery MixedInstancesPolicy' {..} =
    Prelude.mconcat
      [ "LaunchTemplate" Core.=: launchTemplate,
        "InstancesDistribution"
          Core.=: instancesDistribution
      ]
