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
-- Module      : Amazonka.GuardDuty.Types.DataSourceConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourceConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.KubernetesConfiguration
import Amazonka.GuardDuty.Types.MalwareProtectionConfiguration
import Amazonka.GuardDuty.Types.S3LogsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which data sources are enabled.
--
-- /See:/ 'newDataSourceConfigurations' smart constructor.
data DataSourceConfigurations = DataSourceConfigurations'
  { -- | Describes whether any Kubernetes logs are enabled as data sources.
    kubernetes :: Prelude.Maybe KubernetesConfiguration,
    -- | Describes whether Malware Protection is enabled as a data source.
    malwareProtection :: Prelude.Maybe MalwareProtectionConfiguration,
    -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: Prelude.Maybe S3LogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetes', 'dataSourceConfigurations_kubernetes' - Describes whether any Kubernetes logs are enabled as data sources.
--
-- 'malwareProtection', 'dataSourceConfigurations_malwareProtection' - Describes whether Malware Protection is enabled as a data source.
--
-- 's3Logs', 'dataSourceConfigurations_s3Logs' - Describes whether S3 data event logs are enabled as a data source.
newDataSourceConfigurations ::
  DataSourceConfigurations
newDataSourceConfigurations =
  DataSourceConfigurations'
    { kubernetes =
        Prelude.Nothing,
      malwareProtection = Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | Describes whether any Kubernetes logs are enabled as data sources.
dataSourceConfigurations_kubernetes :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe KubernetesConfiguration)
dataSourceConfigurations_kubernetes = Lens.lens (\DataSourceConfigurations' {kubernetes} -> kubernetes) (\s@DataSourceConfigurations' {} a -> s {kubernetes = a} :: DataSourceConfigurations)

-- | Describes whether Malware Protection is enabled as a data source.
dataSourceConfigurations_malwareProtection :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe MalwareProtectionConfiguration)
dataSourceConfigurations_malwareProtection = Lens.lens (\DataSourceConfigurations' {malwareProtection} -> malwareProtection) (\s@DataSourceConfigurations' {} a -> s {malwareProtection = a} :: DataSourceConfigurations)

-- | Describes whether S3 data event logs are enabled as a data source.
dataSourceConfigurations_s3Logs :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe S3LogsConfiguration)
dataSourceConfigurations_s3Logs = Lens.lens (\DataSourceConfigurations' {s3Logs} -> s3Logs) (\s@DataSourceConfigurations' {} a -> s {s3Logs = a} :: DataSourceConfigurations)

instance Prelude.Hashable DataSourceConfigurations where
  hashWithSalt _salt DataSourceConfigurations' {..} =
    _salt `Prelude.hashWithSalt` kubernetes
      `Prelude.hashWithSalt` malwareProtection
      `Prelude.hashWithSalt` s3Logs

instance Prelude.NFData DataSourceConfigurations where
  rnf DataSourceConfigurations' {..} =
    Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs

instance Data.ToJSON DataSourceConfigurations where
  toJSON DataSourceConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kubernetes" Data..=) Prelude.<$> kubernetes,
            ("malwareProtection" Data..=)
              Prelude.<$> malwareProtection,
            ("s3Logs" Data..=) Prelude.<$> s3Logs
          ]
      )
