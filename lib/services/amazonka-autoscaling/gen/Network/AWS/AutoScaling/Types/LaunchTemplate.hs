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
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplate where

import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a launch template and overrides.
--
-- You specify these properties as part of a mixed instances policy.
--
-- When you update the launch template or overrides in the
-- UpdateAutoScalingGroup API call, existing Amazon EC2 instances continue
-- to run. When scale out occurs, Amazon EC2 Auto Scaling launches
-- instances to match the new settings. When scale in occurs, Amazon EC2
-- Auto Scaling terminates instances according to the group\'s termination
-- policies.
--
-- /See:/ 'newLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | Any properties that you specify override the same properties in the
    -- launch template. If not provided, Amazon EC2 Auto Scaling uses the
    -- instance type specified in the launch template when it launches an
    -- instance.
    overrides :: Prelude.Maybe [LaunchTemplateOverrides],
    -- | The launch template to use.
    launchTemplateSpecification :: Prelude.Maybe LaunchTemplateSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'launchTemplate_overrides' - Any properties that you specify override the same properties in the
-- launch template. If not provided, Amazon EC2 Auto Scaling uses the
-- instance type specified in the launch template when it launches an
-- instance.
--
-- 'launchTemplateSpecification', 'launchTemplate_launchTemplateSpecification' - The launch template to use.
newLaunchTemplate ::
  LaunchTemplate
newLaunchTemplate =
  LaunchTemplate'
    { overrides = Prelude.Nothing,
      launchTemplateSpecification = Prelude.Nothing
    }

-- | Any properties that you specify override the same properties in the
-- launch template. If not provided, Amazon EC2 Auto Scaling uses the
-- instance type specified in the launch template when it launches an
-- instance.
launchTemplate_overrides :: Lens.Lens' LaunchTemplate (Prelude.Maybe [LaunchTemplateOverrides])
launchTemplate_overrides = Lens.lens (\LaunchTemplate' {overrides} -> overrides) (\s@LaunchTemplate' {} a -> s {overrides = a} :: LaunchTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The launch template to use.
launchTemplate_launchTemplateSpecification :: Lens.Lens' LaunchTemplate (Prelude.Maybe LaunchTemplateSpecification)
launchTemplate_launchTemplateSpecification = Lens.lens (\LaunchTemplate' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplate' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplate)

instance Core.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Prelude.<$> ( x Core..@? "Overrides" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "LaunchTemplateSpecification")

instance Prelude.Hashable LaunchTemplate

instance Prelude.NFData LaunchTemplate

instance Core.ToQuery LaunchTemplate where
  toQuery LaunchTemplate' {..} =
    Prelude.mconcat
      [ "Overrides"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> overrides),
        "LaunchTemplateSpecification"
          Core.=: launchTemplateSpecification
      ]
