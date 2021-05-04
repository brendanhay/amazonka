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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a launch template and overrides.
--
-- /See:/ 'newLaunchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Prelude.Maybe FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the
    -- launch template.
    overrides :: Prelude.Maybe [LaunchTemplateOverrides]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | The launch template.
launchTemplateConfig_launchTemplateSpecification :: Lens.Lens' LaunchTemplateConfig (Prelude.Maybe FleetLaunchTemplateSpecification)
launchTemplateConfig_launchTemplateSpecification = Lens.lens (\LaunchTemplateConfig' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateConfig' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateConfig)

-- | Any parameters that you specify override the same parameters in the
-- launch template.
launchTemplateConfig_overrides :: Lens.Lens' LaunchTemplateConfig (Prelude.Maybe [LaunchTemplateOverrides])
launchTemplateConfig_overrides = Lens.lens (\LaunchTemplateConfig' {overrides} -> overrides) (\s@LaunchTemplateConfig' {} a -> s {overrides = a} :: LaunchTemplateConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      Prelude.<$> (x Prelude..@? "launchTemplateSpecification")
      Prelude.<*> ( x Prelude..@? "overrides" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable LaunchTemplateConfig

instance Prelude.NFData LaunchTemplateConfig

instance Prelude.ToQuery LaunchTemplateConfig where
  toQuery LaunchTemplateConfig' {..} =
    Prelude.mconcat
      [ "LaunchTemplateSpecification"
          Prelude.=: launchTemplateSpecification,
        Prelude.toQuery
          ( Prelude.toQueryList "Overrides"
              Prelude.<$> overrides
          )
      ]
