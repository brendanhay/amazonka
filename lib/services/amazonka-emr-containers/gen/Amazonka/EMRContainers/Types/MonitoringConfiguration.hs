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
-- Module      : Amazonka.EMRContainers.Types.MonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.MonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.PersistentAppUI
import Amazonka.EMRContainers.Types.S3MonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration setting for monitoring.
--
-- /See:/ 'newMonitoringConfiguration' smart constructor.
data MonitoringConfiguration = MonitoringConfiguration'
  { -- | Monitoring configurations for CloudWatch.
    cloudWatchMonitoringConfiguration :: Prelude.Maybe CloudWatchMonitoringConfiguration,
    -- | Monitoring configurations for the persistent application UI.
    persistentAppUI :: Prelude.Maybe PersistentAppUI,
    -- | Amazon S3 configuration for monitoring log publishing.
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
-- 'cloudWatchMonitoringConfiguration', 'monitoringConfiguration_cloudWatchMonitoringConfiguration' - Monitoring configurations for CloudWatch.
--
-- 'persistentAppUI', 'monitoringConfiguration_persistentAppUI' - Monitoring configurations for the persistent application UI.
--
-- 's3MonitoringConfiguration', 'monitoringConfiguration_s3MonitoringConfiguration' - Amazon S3 configuration for monitoring log publishing.
newMonitoringConfiguration ::
  MonitoringConfiguration
newMonitoringConfiguration =
  MonitoringConfiguration'
    { cloudWatchMonitoringConfiguration =
        Prelude.Nothing,
      persistentAppUI = Prelude.Nothing,
      s3MonitoringConfiguration = Prelude.Nothing
    }

-- | Monitoring configurations for CloudWatch.
monitoringConfiguration_cloudWatchMonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe CloudWatchMonitoringConfiguration)
monitoringConfiguration_cloudWatchMonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {cloudWatchMonitoringConfiguration} -> cloudWatchMonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {cloudWatchMonitoringConfiguration = a} :: MonitoringConfiguration)

-- | Monitoring configurations for the persistent application UI.
monitoringConfiguration_persistentAppUI :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe PersistentAppUI)
monitoringConfiguration_persistentAppUI = Lens.lens (\MonitoringConfiguration' {persistentAppUI} -> persistentAppUI) (\s@MonitoringConfiguration' {} a -> s {persistentAppUI = a} :: MonitoringConfiguration)

-- | Amazon S3 configuration for monitoring log publishing.
monitoringConfiguration_s3MonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe S3MonitoringConfiguration)
monitoringConfiguration_s3MonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {s3MonitoringConfiguration} -> s3MonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {s3MonitoringConfiguration = a} :: MonitoringConfiguration)

instance Data.FromJSON MonitoringConfiguration where
  parseJSON =
    Data.withObject
      "MonitoringConfiguration"
      ( \x ->
          MonitoringConfiguration'
            Prelude.<$> (x Data..:? "cloudWatchMonitoringConfiguration")
            Prelude.<*> (x Data..:? "persistentAppUI")
            Prelude.<*> (x Data..:? "s3MonitoringConfiguration")
      )

instance Prelude.Hashable MonitoringConfiguration where
  hashWithSalt _salt MonitoringConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchMonitoringConfiguration
      `Prelude.hashWithSalt` persistentAppUI
      `Prelude.hashWithSalt` s3MonitoringConfiguration

instance Prelude.NFData MonitoringConfiguration where
  rnf MonitoringConfiguration' {..} =
    Prelude.rnf cloudWatchMonitoringConfiguration `Prelude.seq`
      Prelude.rnf persistentAppUI `Prelude.seq`
        Prelude.rnf s3MonitoringConfiguration

instance Data.ToJSON MonitoringConfiguration where
  toJSON MonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchMonitoringConfiguration" Data..=)
              Prelude.<$> cloudWatchMonitoringConfiguration,
            ("persistentAppUI" Data..=)
              Prelude.<$> persistentAppUI,
            ("s3MonitoringConfiguration" Data..=)
              Prelude.<$> s3MonitoringConfiguration
          ]
      )
