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
-- Module      : Amazonka.AutoScaling.Types.MixedInstancesPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MixedInstancesPolicy where

import Amazonka.AutoScaling.Types.InstancesDistribution
import Amazonka.AutoScaling.Types.LaunchTemplate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to launch multiple instance types and On-Demand
-- Instances and Spot Instances within a single Auto Scaling group.
--
-- A mixed instances policy contains information that Amazon EC2 Auto
-- Scaling can use to launch instances and help optimize your costs. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newMixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'
  { -- | The instances distribution.
    instancesDistribution :: Prelude.Maybe InstancesDistribution,
    -- | One or more launch templates and the instance types (overrides) that are
    -- used to launch EC2 instances to fulfill On-Demand and Spot capacities.
    launchTemplate :: Prelude.Maybe LaunchTemplate
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
-- 'instancesDistribution', 'mixedInstancesPolicy_instancesDistribution' - The instances distribution.
--
-- 'launchTemplate', 'mixedInstancesPolicy_launchTemplate' - One or more launch templates and the instance types (overrides) that are
-- used to launch EC2 instances to fulfill On-Demand and Spot capacities.
newMixedInstancesPolicy ::
  MixedInstancesPolicy
newMixedInstancesPolicy =
  MixedInstancesPolicy'
    { instancesDistribution =
        Prelude.Nothing,
      launchTemplate = Prelude.Nothing
    }

-- | The instances distribution.
mixedInstancesPolicy_instancesDistribution :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe InstancesDistribution)
mixedInstancesPolicy_instancesDistribution = Lens.lens (\MixedInstancesPolicy' {instancesDistribution} -> instancesDistribution) (\s@MixedInstancesPolicy' {} a -> s {instancesDistribution = a} :: MixedInstancesPolicy)

-- | One or more launch templates and the instance types (overrides) that are
-- used to launch EC2 instances to fulfill On-Demand and Spot capacities.
mixedInstancesPolicy_launchTemplate :: Lens.Lens' MixedInstancesPolicy (Prelude.Maybe LaunchTemplate)
mixedInstancesPolicy_launchTemplate = Lens.lens (\MixedInstancesPolicy' {launchTemplate} -> launchTemplate) (\s@MixedInstancesPolicy' {} a -> s {launchTemplate = a} :: MixedInstancesPolicy)

instance Data.FromXML MixedInstancesPolicy where
  parseXML x =
    MixedInstancesPolicy'
      Prelude.<$> (x Data..@? "InstancesDistribution")
      Prelude.<*> (x Data..@? "LaunchTemplate")

instance Prelude.Hashable MixedInstancesPolicy where
  hashWithSalt _salt MixedInstancesPolicy' {..} =
    _salt `Prelude.hashWithSalt` instancesDistribution
      `Prelude.hashWithSalt` launchTemplate

instance Prelude.NFData MixedInstancesPolicy where
  rnf MixedInstancesPolicy' {..} =
    Prelude.rnf instancesDistribution
      `Prelude.seq` Prelude.rnf launchTemplate

instance Data.ToQuery MixedInstancesPolicy where
  toQuery MixedInstancesPolicy' {..} =
    Prelude.mconcat
      [ "InstancesDistribution"
          Data.=: instancesDistribution,
        "LaunchTemplate" Data.=: launchTemplate
      ]
