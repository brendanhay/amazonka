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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateAutoRecoveryState
import qualified Amazonka.Prelude as Prelude

-- | The maintenance options of your instance.
--
-- /See:/ 'newLaunchTemplateInstanceMaintenanceOptions' smart constructor.
data LaunchTemplateInstanceMaintenanceOptions = LaunchTemplateInstanceMaintenanceOptions'
  { -- | Disables the automatic recovery behavior of your instance or sets it to
    -- default.
    autoRecovery :: Prelude.Maybe LaunchTemplateAutoRecoveryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMaintenanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'launchTemplateInstanceMaintenanceOptions_autoRecovery' - Disables the automatic recovery behavior of your instance or sets it to
-- default.
newLaunchTemplateInstanceMaintenanceOptions ::
  LaunchTemplateInstanceMaintenanceOptions
newLaunchTemplateInstanceMaintenanceOptions =
  LaunchTemplateInstanceMaintenanceOptions'
    { autoRecovery =
        Prelude.Nothing
    }

-- | Disables the automatic recovery behavior of your instance or sets it to
-- default.
launchTemplateInstanceMaintenanceOptions_autoRecovery :: Lens.Lens' LaunchTemplateInstanceMaintenanceOptions (Prelude.Maybe LaunchTemplateAutoRecoveryState)
launchTemplateInstanceMaintenanceOptions_autoRecovery = Lens.lens (\LaunchTemplateInstanceMaintenanceOptions' {autoRecovery} -> autoRecovery) (\s@LaunchTemplateInstanceMaintenanceOptions' {} a -> s {autoRecovery = a} :: LaunchTemplateInstanceMaintenanceOptions)

instance
  Data.FromXML
    LaunchTemplateInstanceMaintenanceOptions
  where
  parseXML x =
    LaunchTemplateInstanceMaintenanceOptions'
      Prelude.<$> (x Data..@? "autoRecovery")

instance
  Prelude.Hashable
    LaunchTemplateInstanceMaintenanceOptions
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceMaintenanceOptions' {..} =
      _salt `Prelude.hashWithSalt` autoRecovery

instance
  Prelude.NFData
    LaunchTemplateInstanceMaintenanceOptions
  where
  rnf LaunchTemplateInstanceMaintenanceOptions' {..} =
    Prelude.rnf autoRecovery
