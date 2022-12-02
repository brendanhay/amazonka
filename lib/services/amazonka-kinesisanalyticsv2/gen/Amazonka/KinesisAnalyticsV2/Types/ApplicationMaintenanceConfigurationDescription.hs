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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the maintenance configuration for the application.
--
-- /See:/ 'newApplicationMaintenanceConfigurationDescription' smart constructor.
data ApplicationMaintenanceConfigurationDescription = ApplicationMaintenanceConfigurationDescription'
  { -- | The start time for the maintenance window.
    applicationMaintenanceWindowStartTime :: Prelude.Text,
    -- | The end time for the maintenance window.
    applicationMaintenanceWindowEndTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationMaintenanceConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationMaintenanceWindowStartTime', 'applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowStartTime' - The start time for the maintenance window.
--
-- 'applicationMaintenanceWindowEndTime', 'applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowEndTime' - The end time for the maintenance window.
newApplicationMaintenanceConfigurationDescription ::
  -- | 'applicationMaintenanceWindowStartTime'
  Prelude.Text ->
  -- | 'applicationMaintenanceWindowEndTime'
  Prelude.Text ->
  ApplicationMaintenanceConfigurationDescription
newApplicationMaintenanceConfigurationDescription
  pApplicationMaintenanceWindowStartTime_
  pApplicationMaintenanceWindowEndTime_ =
    ApplicationMaintenanceConfigurationDescription'
      { applicationMaintenanceWindowStartTime =
          pApplicationMaintenanceWindowStartTime_,
        applicationMaintenanceWindowEndTime =
          pApplicationMaintenanceWindowEndTime_
      }

-- | The start time for the maintenance window.
applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowStartTime :: Lens.Lens' ApplicationMaintenanceConfigurationDescription Prelude.Text
applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowStartTime = Lens.lens (\ApplicationMaintenanceConfigurationDescription' {applicationMaintenanceWindowStartTime} -> applicationMaintenanceWindowStartTime) (\s@ApplicationMaintenanceConfigurationDescription' {} a -> s {applicationMaintenanceWindowStartTime = a} :: ApplicationMaintenanceConfigurationDescription)

-- | The end time for the maintenance window.
applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowEndTime :: Lens.Lens' ApplicationMaintenanceConfigurationDescription Prelude.Text
applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowEndTime = Lens.lens (\ApplicationMaintenanceConfigurationDescription' {applicationMaintenanceWindowEndTime} -> applicationMaintenanceWindowEndTime) (\s@ApplicationMaintenanceConfigurationDescription' {} a -> s {applicationMaintenanceWindowEndTime = a} :: ApplicationMaintenanceConfigurationDescription)

instance
  Data.FromJSON
    ApplicationMaintenanceConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "ApplicationMaintenanceConfigurationDescription"
      ( \x ->
          ApplicationMaintenanceConfigurationDescription'
            Prelude.<$> (x Data..: "ApplicationMaintenanceWindowStartTime")
              Prelude.<*> (x Data..: "ApplicationMaintenanceWindowEndTime")
      )

instance
  Prelude.Hashable
    ApplicationMaintenanceConfigurationDescription
  where
  hashWithSalt
    _salt
    ApplicationMaintenanceConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` applicationMaintenanceWindowStartTime
        `Prelude.hashWithSalt` applicationMaintenanceWindowEndTime

instance
  Prelude.NFData
    ApplicationMaintenanceConfigurationDescription
  where
  rnf
    ApplicationMaintenanceConfigurationDescription' {..} =
      Prelude.rnf applicationMaintenanceWindowStartTime
        `Prelude.seq` Prelude.rnf applicationMaintenanceWindowEndTime
