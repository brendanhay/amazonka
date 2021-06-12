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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import qualified Network.AWS.Lens as Lens

-- | Describes a launch template and overrides.
--
-- /See:/ 'newLaunchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Core.Maybe FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Core.Maybe [LaunchTemplateOverrides]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateSpecification', 'launchTemplateConfig_launchTemplateSpecification' - The launch template.
--
-- 'overrides', 'launchTemplateConfig_overrides' - Any parameters that you specify override the same parameters in the
-- launch template.
newLaunchTemplateConfig ::
  LaunchTemplateConfig
newLaunchTemplateConfig =
  LaunchTemplateConfig'
    { launchTemplateSpecification =
        Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template.
launchTemplateConfig_launchTemplateSpecification :: Lens.Lens' LaunchTemplateConfig (Core.Maybe FleetLaunchTemplateSpecification)
launchTemplateConfig_launchTemplateSpecification = Lens.lens (\LaunchTemplateConfig' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateConfig' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateConfig)

-- | Any parameters that you specify override the same parameters in the
-- launch template.
launchTemplateConfig_overrides :: Lens.Lens' LaunchTemplateConfig (Core.Maybe [LaunchTemplateOverrides])
launchTemplateConfig_overrides = Lens.lens (\LaunchTemplateConfig' {overrides} -> overrides) (\s@LaunchTemplateConfig' {} a -> s {overrides = a} :: LaunchTemplateConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      Core.<$> (x Core..@? "launchTemplateSpecification")
      Core.<*> ( x Core..@? "overrides" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable LaunchTemplateConfig

instance Core.NFData LaunchTemplateConfig

instance Core.ToQuery LaunchTemplateConfig where
  toQuery LaunchTemplateConfig' {..} =
    Core.mconcat
      [ "LaunchTemplateSpecification"
          Core.=: launchTemplateSpecification,
        Core.toQuery
          (Core.toQueryList "Overrides" Core.<$> overrides)
      ]
