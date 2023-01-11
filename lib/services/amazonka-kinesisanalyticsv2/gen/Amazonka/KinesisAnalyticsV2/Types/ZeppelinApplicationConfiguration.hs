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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfiguration
import Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfiguration
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newZeppelinApplicationConfiguration' smart constructor.
data ZeppelinApplicationConfiguration = ZeppelinApplicationConfiguration'
  { -- | The Amazon Glue Data Catalog that you use in queries in a Kinesis Data
    -- Analytics Studio notebook.
    catalogConfiguration :: Prelude.Maybe CatalogConfiguration,
    -- | Custom artifacts are dependency JARs and user-defined functions (UDF).
    customArtifactsConfiguration :: Prelude.Maybe [CustomArtifactConfiguration],
    -- | The information required to deploy a Kinesis Data Analytics Studio
    -- notebook as an application with durable state.
    deployAsApplicationConfiguration :: Prelude.Maybe DeployAsApplicationConfiguration,
    -- | The monitoring configuration of a Kinesis Data Analytics Studio
    -- notebook.
    monitoringConfiguration :: Prelude.Maybe ZeppelinMonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZeppelinApplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogConfiguration', 'zeppelinApplicationConfiguration_catalogConfiguration' - The Amazon Glue Data Catalog that you use in queries in a Kinesis Data
-- Analytics Studio notebook.
--
-- 'customArtifactsConfiguration', 'zeppelinApplicationConfiguration_customArtifactsConfiguration' - Custom artifacts are dependency JARs and user-defined functions (UDF).
--
-- 'deployAsApplicationConfiguration', 'zeppelinApplicationConfiguration_deployAsApplicationConfiguration' - The information required to deploy a Kinesis Data Analytics Studio
-- notebook as an application with durable state.
--
-- 'monitoringConfiguration', 'zeppelinApplicationConfiguration_monitoringConfiguration' - The monitoring configuration of a Kinesis Data Analytics Studio
-- notebook.
newZeppelinApplicationConfiguration ::
  ZeppelinApplicationConfiguration
newZeppelinApplicationConfiguration =
  ZeppelinApplicationConfiguration'
    { catalogConfiguration =
        Prelude.Nothing,
      customArtifactsConfiguration =
        Prelude.Nothing,
      deployAsApplicationConfiguration =
        Prelude.Nothing,
      monitoringConfiguration = Prelude.Nothing
    }

-- | The Amazon Glue Data Catalog that you use in queries in a Kinesis Data
-- Analytics Studio notebook.
zeppelinApplicationConfiguration_catalogConfiguration :: Lens.Lens' ZeppelinApplicationConfiguration (Prelude.Maybe CatalogConfiguration)
zeppelinApplicationConfiguration_catalogConfiguration = Lens.lens (\ZeppelinApplicationConfiguration' {catalogConfiguration} -> catalogConfiguration) (\s@ZeppelinApplicationConfiguration' {} a -> s {catalogConfiguration = a} :: ZeppelinApplicationConfiguration)

-- | Custom artifacts are dependency JARs and user-defined functions (UDF).
zeppelinApplicationConfiguration_customArtifactsConfiguration :: Lens.Lens' ZeppelinApplicationConfiguration (Prelude.Maybe [CustomArtifactConfiguration])
zeppelinApplicationConfiguration_customArtifactsConfiguration = Lens.lens (\ZeppelinApplicationConfiguration' {customArtifactsConfiguration} -> customArtifactsConfiguration) (\s@ZeppelinApplicationConfiguration' {} a -> s {customArtifactsConfiguration = a} :: ZeppelinApplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The information required to deploy a Kinesis Data Analytics Studio
-- notebook as an application with durable state.
zeppelinApplicationConfiguration_deployAsApplicationConfiguration :: Lens.Lens' ZeppelinApplicationConfiguration (Prelude.Maybe DeployAsApplicationConfiguration)
zeppelinApplicationConfiguration_deployAsApplicationConfiguration = Lens.lens (\ZeppelinApplicationConfiguration' {deployAsApplicationConfiguration} -> deployAsApplicationConfiguration) (\s@ZeppelinApplicationConfiguration' {} a -> s {deployAsApplicationConfiguration = a} :: ZeppelinApplicationConfiguration)

-- | The monitoring configuration of a Kinesis Data Analytics Studio
-- notebook.
zeppelinApplicationConfiguration_monitoringConfiguration :: Lens.Lens' ZeppelinApplicationConfiguration (Prelude.Maybe ZeppelinMonitoringConfiguration)
zeppelinApplicationConfiguration_monitoringConfiguration = Lens.lens (\ZeppelinApplicationConfiguration' {monitoringConfiguration} -> monitoringConfiguration) (\s@ZeppelinApplicationConfiguration' {} a -> s {monitoringConfiguration = a} :: ZeppelinApplicationConfiguration)

instance
  Prelude.Hashable
    ZeppelinApplicationConfiguration
  where
  hashWithSalt
    _salt
    ZeppelinApplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` catalogConfiguration
        `Prelude.hashWithSalt` customArtifactsConfiguration
        `Prelude.hashWithSalt` deployAsApplicationConfiguration
        `Prelude.hashWithSalt` monitoringConfiguration

instance
  Prelude.NFData
    ZeppelinApplicationConfiguration
  where
  rnf ZeppelinApplicationConfiguration' {..} =
    Prelude.rnf catalogConfiguration
      `Prelude.seq` Prelude.rnf customArtifactsConfiguration
      `Prelude.seq` Prelude.rnf deployAsApplicationConfiguration
      `Prelude.seq` Prelude.rnf monitoringConfiguration

instance Data.ToJSON ZeppelinApplicationConfiguration where
  toJSON ZeppelinApplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogConfiguration" Data..=)
              Prelude.<$> catalogConfiguration,
            ("CustomArtifactsConfiguration" Data..=)
              Prelude.<$> customArtifactsConfiguration,
            ("DeployAsApplicationConfiguration" Data..=)
              Prelude.<$> deployAsApplicationConfiguration,
            ("MonitoringConfiguration" Data..=)
              Prelude.<$> monitoringConfiguration
          ]
      )
