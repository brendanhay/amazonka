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
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens

-- | Describes a launch template and overrides.
--
-- /See:/ 'newFleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Core.Maybe FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Core.Maybe [FleetLaunchTemplateOverrides]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateSpecification', 'fleetLaunchTemplateConfig_launchTemplateSpecification' - The launch template.
--
-- 'overrides', 'fleetLaunchTemplateConfig_overrides' - Any parameters that you specify override the same parameters in the
-- launch template.
newFleetLaunchTemplateConfig ::
  FleetLaunchTemplateConfig
newFleetLaunchTemplateConfig =
  FleetLaunchTemplateConfig'
    { launchTemplateSpecification =
        Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template.
fleetLaunchTemplateConfig_launchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfig (Core.Maybe FleetLaunchTemplateSpecification)
fleetLaunchTemplateConfig_launchTemplateSpecification = Lens.lens (\FleetLaunchTemplateConfig' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@FleetLaunchTemplateConfig' {} a -> s {launchTemplateSpecification = a} :: FleetLaunchTemplateConfig)

-- | Any parameters that you specify override the same parameters in the
-- launch template.
fleetLaunchTemplateConfig_overrides :: Lens.Lens' FleetLaunchTemplateConfig (Core.Maybe [FleetLaunchTemplateOverrides])
fleetLaunchTemplateConfig_overrides = Lens.lens (\FleetLaunchTemplateConfig' {overrides} -> overrides) (\s@FleetLaunchTemplateConfig' {} a -> s {overrides = a} :: FleetLaunchTemplateConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML FleetLaunchTemplateConfig where
  parseXML x =
    FleetLaunchTemplateConfig'
      Core.<$> (x Core..@? "launchTemplateSpecification")
      Core.<*> ( x Core..@? "overrides" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable FleetLaunchTemplateConfig

instance Core.NFData FleetLaunchTemplateConfig
