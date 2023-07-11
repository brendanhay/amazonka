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
-- Module      : Amazonka.EMRContainers.Types.ParametricMonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ParametricMonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.ParametricCloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricS3MonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration setting for monitoring. This data type allows job template
-- parameters to be specified within.
--
-- /See:/ 'newParametricMonitoringConfiguration' smart constructor.
data ParametricMonitoringConfiguration = ParametricMonitoringConfiguration'
  { -- | Monitoring configurations for CloudWatch.
    cloudWatchMonitoringConfiguration :: Prelude.Maybe ParametricCloudWatchMonitoringConfiguration,
    -- | Monitoring configurations for the persistent application UI.
    persistentAppUI :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 configuration for monitoring log publishing.
    s3MonitoringConfiguration :: Prelude.Maybe ParametricS3MonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParametricMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMonitoringConfiguration', 'parametricMonitoringConfiguration_cloudWatchMonitoringConfiguration' - Monitoring configurations for CloudWatch.
--
-- 'persistentAppUI', 'parametricMonitoringConfiguration_persistentAppUI' - Monitoring configurations for the persistent application UI.
--
-- 's3MonitoringConfiguration', 'parametricMonitoringConfiguration_s3MonitoringConfiguration' - Amazon S3 configuration for monitoring log publishing.
newParametricMonitoringConfiguration ::
  ParametricMonitoringConfiguration
newParametricMonitoringConfiguration =
  ParametricMonitoringConfiguration'
    { cloudWatchMonitoringConfiguration =
        Prelude.Nothing,
      persistentAppUI = Prelude.Nothing,
      s3MonitoringConfiguration =
        Prelude.Nothing
    }

-- | Monitoring configurations for CloudWatch.
parametricMonitoringConfiguration_cloudWatchMonitoringConfiguration :: Lens.Lens' ParametricMonitoringConfiguration (Prelude.Maybe ParametricCloudWatchMonitoringConfiguration)
parametricMonitoringConfiguration_cloudWatchMonitoringConfiguration = Lens.lens (\ParametricMonitoringConfiguration' {cloudWatchMonitoringConfiguration} -> cloudWatchMonitoringConfiguration) (\s@ParametricMonitoringConfiguration' {} a -> s {cloudWatchMonitoringConfiguration = a} :: ParametricMonitoringConfiguration)

-- | Monitoring configurations for the persistent application UI.
parametricMonitoringConfiguration_persistentAppUI :: Lens.Lens' ParametricMonitoringConfiguration (Prelude.Maybe Prelude.Text)
parametricMonitoringConfiguration_persistentAppUI = Lens.lens (\ParametricMonitoringConfiguration' {persistentAppUI} -> persistentAppUI) (\s@ParametricMonitoringConfiguration' {} a -> s {persistentAppUI = a} :: ParametricMonitoringConfiguration)

-- | Amazon S3 configuration for monitoring log publishing.
parametricMonitoringConfiguration_s3MonitoringConfiguration :: Lens.Lens' ParametricMonitoringConfiguration (Prelude.Maybe ParametricS3MonitoringConfiguration)
parametricMonitoringConfiguration_s3MonitoringConfiguration = Lens.lens (\ParametricMonitoringConfiguration' {s3MonitoringConfiguration} -> s3MonitoringConfiguration) (\s@ParametricMonitoringConfiguration' {} a -> s {s3MonitoringConfiguration = a} :: ParametricMonitoringConfiguration)

instance
  Data.FromJSON
    ParametricMonitoringConfiguration
  where
  parseJSON =
    Data.withObject
      "ParametricMonitoringConfiguration"
      ( \x ->
          ParametricMonitoringConfiguration'
            Prelude.<$> (x Data..:? "cloudWatchMonitoringConfiguration")
            Prelude.<*> (x Data..:? "persistentAppUI")
            Prelude.<*> (x Data..:? "s3MonitoringConfiguration")
      )

instance
  Prelude.Hashable
    ParametricMonitoringConfiguration
  where
  hashWithSalt
    _salt
    ParametricMonitoringConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchMonitoringConfiguration
        `Prelude.hashWithSalt` persistentAppUI
        `Prelude.hashWithSalt` s3MonitoringConfiguration

instance
  Prelude.NFData
    ParametricMonitoringConfiguration
  where
  rnf ParametricMonitoringConfiguration' {..} =
    Prelude.rnf cloudWatchMonitoringConfiguration
      `Prelude.seq` Prelude.rnf persistentAppUI
      `Prelude.seq` Prelude.rnf s3MonitoringConfiguration

instance
  Data.ToJSON
    ParametricMonitoringConfiguration
  where
  toJSON ParametricMonitoringConfiguration' {..} =
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
