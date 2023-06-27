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
-- Module      : Amazonka.AutoScaling.Types.DesiredConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.DesiredConfiguration where

import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import Amazonka.AutoScaling.Types.MixedInstancesPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the desired configuration for an instance refresh.
--
-- If you specify a desired configuration, you must specify either a
-- @LaunchTemplate@ or a @MixedInstancesPolicy@.
--
-- /See:/ 'newDesiredConfiguration' smart constructor.
data DesiredConfiguration = DesiredConfiguration'
  { -- | Describes the launch template and the version of the launch template
    -- that Amazon EC2 Auto Scaling uses to launch Amazon EC2 instances. For
    -- more information about launch templates, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchTemplates.html Launch templates>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | Use this structure to launch multiple instance types and On-Demand
    -- Instances and Spot Instances within a single Auto Scaling group.
    --
    -- A mixed instances policy contains information that Amazon EC2 Auto
    -- Scaling can use to launch instances and help optimize your costs. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DesiredConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'desiredConfiguration_launchTemplate' - Describes the launch template and the version of the launch template
-- that Amazon EC2 Auto Scaling uses to launch Amazon EC2 instances. For
-- more information about launch templates, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchTemplates.html Launch templates>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'mixedInstancesPolicy', 'desiredConfiguration_mixedInstancesPolicy' - Use this structure to launch multiple instance types and On-Demand
-- Instances and Spot Instances within a single Auto Scaling group.
--
-- A mixed instances policy contains information that Amazon EC2 Auto
-- Scaling can use to launch instances and help optimize your costs. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
newDesiredConfiguration ::
  DesiredConfiguration
newDesiredConfiguration =
  DesiredConfiguration'
    { launchTemplate =
        Prelude.Nothing,
      mixedInstancesPolicy = Prelude.Nothing
    }

-- | Describes the launch template and the version of the launch template
-- that Amazon EC2 Auto Scaling uses to launch Amazon EC2 instances. For
-- more information about launch templates, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchTemplates.html Launch templates>
-- in the /Amazon EC2 Auto Scaling User Guide/.
desiredConfiguration_launchTemplate :: Lens.Lens' DesiredConfiguration (Prelude.Maybe LaunchTemplateSpecification)
desiredConfiguration_launchTemplate = Lens.lens (\DesiredConfiguration' {launchTemplate} -> launchTemplate) (\s@DesiredConfiguration' {} a -> s {launchTemplate = a} :: DesiredConfiguration)

-- | Use this structure to launch multiple instance types and On-Demand
-- Instances and Spot Instances within a single Auto Scaling group.
--
-- A mixed instances policy contains information that Amazon EC2 Auto
-- Scaling can use to launch instances and help optimize your costs. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
desiredConfiguration_mixedInstancesPolicy :: Lens.Lens' DesiredConfiguration (Prelude.Maybe MixedInstancesPolicy)
desiredConfiguration_mixedInstancesPolicy = Lens.lens (\DesiredConfiguration' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@DesiredConfiguration' {} a -> s {mixedInstancesPolicy = a} :: DesiredConfiguration)

instance Data.FromXML DesiredConfiguration where
  parseXML x =
    DesiredConfiguration'
      Prelude.<$> (x Data..@? "LaunchTemplate")
      Prelude.<*> (x Data..@? "MixedInstancesPolicy")

instance Prelude.Hashable DesiredConfiguration where
  hashWithSalt _salt DesiredConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` mixedInstancesPolicy

instance Prelude.NFData DesiredConfiguration where
  rnf DesiredConfiguration' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy

instance Data.ToQuery DesiredConfiguration where
  toQuery DesiredConfiguration' {..} =
    Prelude.mconcat
      [ "LaunchTemplate" Data.=: launchTemplate,
        "MixedInstancesPolicy" Data.=: mixedInstancesPolicy
      ]
