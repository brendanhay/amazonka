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
-- Module      : Amazonka.EMRServerless.Types.MonitoringConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.MonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRServerless.Types.ManagedPersistenceMonitoringConfiguration
import Amazonka.EMRServerless.Types.S3MonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration setting for monitoring.
--
-- /See:/ 'newMonitoringConfiguration' smart constructor.
data MonitoringConfiguration = MonitoringConfiguration'
  { -- | The managed log persistence configuration for a job run.
    managedPersistenceMonitoringConfiguration :: Prelude.Maybe ManagedPersistenceMonitoringConfiguration,
    -- | The Amazon S3 configuration for monitoring log publishing.
    s3MonitoringConfiguration :: Prelude.Maybe S3MonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedPersistenceMonitoringConfiguration', 'monitoringConfiguration_managedPersistenceMonitoringConfiguration' - The managed log persistence configuration for a job run.
--
-- 's3MonitoringConfiguration', 'monitoringConfiguration_s3MonitoringConfiguration' - The Amazon S3 configuration for monitoring log publishing.
newMonitoringConfiguration ::
  MonitoringConfiguration
newMonitoringConfiguration =
  MonitoringConfiguration'
    { managedPersistenceMonitoringConfiguration =
        Prelude.Nothing,
      s3MonitoringConfiguration = Prelude.Nothing
    }

-- | The managed log persistence configuration for a job run.
monitoringConfiguration_managedPersistenceMonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe ManagedPersistenceMonitoringConfiguration)
monitoringConfiguration_managedPersistenceMonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {managedPersistenceMonitoringConfiguration} -> managedPersistenceMonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {managedPersistenceMonitoringConfiguration = a} :: MonitoringConfiguration)

-- | The Amazon S3 configuration for monitoring log publishing.
monitoringConfiguration_s3MonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe S3MonitoringConfiguration)
monitoringConfiguration_s3MonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {s3MonitoringConfiguration} -> s3MonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {s3MonitoringConfiguration = a} :: MonitoringConfiguration)

instance Core.FromJSON MonitoringConfiguration where
  parseJSON =
    Core.withObject
      "MonitoringConfiguration"
      ( \x ->
          MonitoringConfiguration'
            Prelude.<$> ( x
                            Core..:? "managedPersistenceMonitoringConfiguration"
                        )
            Prelude.<*> (x Core..:? "s3MonitoringConfiguration")
      )

instance Prelude.Hashable MonitoringConfiguration where
  hashWithSalt _salt MonitoringConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` managedPersistenceMonitoringConfiguration
      `Prelude.hashWithSalt` s3MonitoringConfiguration

instance Prelude.NFData MonitoringConfiguration where
  rnf MonitoringConfiguration' {..} =
    Prelude.rnf
      managedPersistenceMonitoringConfiguration
      `Prelude.seq` Prelude.rnf s3MonitoringConfiguration

instance Core.ToJSON MonitoringConfiguration where
  toJSON MonitoringConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "managedPersistenceMonitoringConfiguration"
                Core..=
            )
              Prelude.<$> managedPersistenceMonitoringConfiguration,
            ("s3MonitoringConfiguration" Core..=)
              Prelude.<$> s3MonitoringConfiguration
          ]
      )
