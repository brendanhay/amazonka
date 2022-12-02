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
-- Module      : Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetLaunchTemplateOverrides
import Amazonka.EC2.Types.FleetLaunchTemplateSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch template and overrides.
--
-- /See:/ 'newLaunchTemplateAndOverridesResponse' smart constructor.
data LaunchTemplateAndOverridesResponse = LaunchTemplateAndOverridesResponse'
  { -- | The launch template.
    launchTemplateSpecification :: Prelude.Maybe FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Prelude.Maybe FleetLaunchTemplateOverrides
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateAndOverridesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateSpecification', 'launchTemplateAndOverridesResponse_launchTemplateSpecification' - The launch template.
--
-- 'overrides', 'launchTemplateAndOverridesResponse_overrides' - Any parameters that you specify override the same parameters in the
-- launch template.
newLaunchTemplateAndOverridesResponse ::
  LaunchTemplateAndOverridesResponse
newLaunchTemplateAndOverridesResponse =
  LaunchTemplateAndOverridesResponse'
    { launchTemplateSpecification =
        Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | The launch template.
launchTemplateAndOverridesResponse_launchTemplateSpecification :: Lens.Lens' LaunchTemplateAndOverridesResponse (Prelude.Maybe FleetLaunchTemplateSpecification)
launchTemplateAndOverridesResponse_launchTemplateSpecification = Lens.lens (\LaunchTemplateAndOverridesResponse' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateAndOverridesResponse' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateAndOverridesResponse)

-- | Any parameters that you specify override the same parameters in the
-- launch template.
launchTemplateAndOverridesResponse_overrides :: Lens.Lens' LaunchTemplateAndOverridesResponse (Prelude.Maybe FleetLaunchTemplateOverrides)
launchTemplateAndOverridesResponse_overrides = Lens.lens (\LaunchTemplateAndOverridesResponse' {overrides} -> overrides) (\s@LaunchTemplateAndOverridesResponse' {} a -> s {overrides = a} :: LaunchTemplateAndOverridesResponse)

instance
  Data.FromXML
    LaunchTemplateAndOverridesResponse
  where
  parseXML x =
    LaunchTemplateAndOverridesResponse'
      Prelude.<$> (x Data..@? "launchTemplateSpecification")
      Prelude.<*> (x Data..@? "overrides")

instance
  Prelude.Hashable
    LaunchTemplateAndOverridesResponse
  where
  hashWithSalt
    _salt
    LaunchTemplateAndOverridesResponse' {..} =
      _salt
        `Prelude.hashWithSalt` launchTemplateSpecification
        `Prelude.hashWithSalt` overrides

instance
  Prelude.NFData
    LaunchTemplateAndOverridesResponse
  where
  rnf LaunchTemplateAndOverridesResponse' {..} =
    Prelude.rnf launchTemplateSpecification
      `Prelude.seq` Prelude.rnf overrides
