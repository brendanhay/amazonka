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
-- Module      : Amazonka.EC2.Types.FleetLaunchTemplateConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetLaunchTemplateConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetLaunchTemplateOverrides
import Amazonka.EC2.Types.FleetLaunchTemplateSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template and overrides.
--
-- /See:/ 'newFleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Prelude.Maybe FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Prelude.Maybe [FleetLaunchTemplateOverrides]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | The launch template.
fleetLaunchTemplateConfig_launchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfig (Prelude.Maybe FleetLaunchTemplateSpecification)
fleetLaunchTemplateConfig_launchTemplateSpecification = Lens.lens (\FleetLaunchTemplateConfig' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@FleetLaunchTemplateConfig' {} a -> s {launchTemplateSpecification = a} :: FleetLaunchTemplateConfig)

-- | Any parameters that you specify override the same parameters in the
-- launch template.
fleetLaunchTemplateConfig_overrides :: Lens.Lens' FleetLaunchTemplateConfig (Prelude.Maybe [FleetLaunchTemplateOverrides])
fleetLaunchTemplateConfig_overrides = Lens.lens (\FleetLaunchTemplateConfig' {overrides} -> overrides) (\s@FleetLaunchTemplateConfig' {} a -> s {overrides = a} :: FleetLaunchTemplateConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML FleetLaunchTemplateConfig where
  parseXML x =
    FleetLaunchTemplateConfig'
      Prelude.<$> (x Data..@? "launchTemplateSpecification")
      Prelude.<*> ( x
                      Data..@? "overrides"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable FleetLaunchTemplateConfig where
  hashWithSalt _salt FleetLaunchTemplateConfig' {..} =
    _salt
      `Prelude.hashWithSalt` launchTemplateSpecification
      `Prelude.hashWithSalt` overrides

instance Prelude.NFData FleetLaunchTemplateConfig where
  rnf FleetLaunchTemplateConfig' {..} =
    Prelude.rnf launchTemplateSpecification
      `Prelude.seq` Prelude.rnf overrides
