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
-- Module      : Amazonka.EC2.Types.LaunchTemplateConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateConfig where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetLaunchTemplateSpecification
import Amazonka.EC2.Types.LaunchTemplateOverrides
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template and overrides.
--
-- /See:/ 'newLaunchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Prelude.Maybe [LaunchTemplateOverrides],
    -- | The launch template.
    launchTemplateSpecification :: Prelude.Maybe FleetLaunchTemplateSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'launchTemplateConfig_overrides' - Any parameters that you specify override the same parameters in the
-- launch template.
--
-- 'launchTemplateSpecification', 'launchTemplateConfig_launchTemplateSpecification' - The launch template.
newLaunchTemplateConfig ::
  LaunchTemplateConfig
newLaunchTemplateConfig =
  LaunchTemplateConfig'
    { overrides = Prelude.Nothing,
      launchTemplateSpecification = Prelude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the
-- launch template.
launchTemplateConfig_overrides :: Lens.Lens' LaunchTemplateConfig (Prelude.Maybe [LaunchTemplateOverrides])
launchTemplateConfig_overrides = Lens.lens (\LaunchTemplateConfig' {overrides} -> overrides) (\s@LaunchTemplateConfig' {} a -> s {overrides = a} :: LaunchTemplateConfig) Prelude.. Lens.mapping Lens.coerced

-- | The launch template.
launchTemplateConfig_launchTemplateSpecification :: Lens.Lens' LaunchTemplateConfig (Prelude.Maybe FleetLaunchTemplateSpecification)
launchTemplateConfig_launchTemplateSpecification = Lens.lens (\LaunchTemplateConfig' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateConfig' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateConfig)

instance Core.FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      Prelude.<$> ( x Core..@? "overrides" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "launchTemplateSpecification")

instance Prelude.Hashable LaunchTemplateConfig where
  hashWithSalt salt' LaunchTemplateConfig' {..} =
    salt'
      `Prelude.hashWithSalt` launchTemplateSpecification
      `Prelude.hashWithSalt` overrides

instance Prelude.NFData LaunchTemplateConfig where
  rnf LaunchTemplateConfig' {..} =
    Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf launchTemplateSpecification

instance Core.ToQuery LaunchTemplateConfig where
  toQuery LaunchTemplateConfig' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Overrides" Prelude.<$> overrides),
        "LaunchTemplateSpecification"
          Core.=: launchTemplateSpecification
      ]
