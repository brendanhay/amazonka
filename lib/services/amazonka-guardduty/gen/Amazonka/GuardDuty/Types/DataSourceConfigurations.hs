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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourceConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.KubernetesConfiguration
import Amazonka.GuardDuty.Types.MalwareProtectionConfiguration
import Amazonka.GuardDuty.Types.S3LogsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which data sources are enabled.
--
-- /See:/ 'newDataSourceConfigurations' smart constructor.
data DataSourceConfigurations = DataSourceConfigurations'
  { -- | Describes whether Malware Protection is enabled as a data source.
    malwareProtection :: Prelude.Maybe MalwareProtectionConfiguration,
    -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: Prelude.Maybe S3LogsConfiguration,
    -- | Describes whether any Kubernetes logs are enabled as data sources.
    kubernetes :: Prelude.Maybe KubernetesConfiguration
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
-- 'malwareProtection', 'dataSourceConfigurations_malwareProtection' - Describes whether Malware Protection is enabled as a data source.
--
-- 's3Logs', 'dataSourceConfigurations_s3Logs' - Describes whether S3 data event logs are enabled as a data source.
--
-- 'kubernetes', 'dataSourceConfigurations_kubernetes' - Describes whether any Kubernetes logs are enabled as data sources.
newDataSourceConfigurations ::
  DataSourceConfigurations
newDataSourceConfigurations =
  DataSourceConfigurations'
    { malwareProtection =
        Prelude.Nothing,
      s3Logs = Prelude.Nothing,
      kubernetes = Prelude.Nothing
    }

-- | Describes whether Malware Protection is enabled as a data source.
dataSourceConfigurations_malwareProtection :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe MalwareProtectionConfiguration)
dataSourceConfigurations_malwareProtection = Lens.lens (\DataSourceConfigurations' {malwareProtection} -> malwareProtection) (\s@DataSourceConfigurations' {} a -> s {malwareProtection = a} :: DataSourceConfigurations)

-- | Describes whether S3 data event logs are enabled as a data source.
dataSourceConfigurations_s3Logs :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe S3LogsConfiguration)
dataSourceConfigurations_s3Logs = Lens.lens (\DataSourceConfigurations' {s3Logs} -> s3Logs) (\s@DataSourceConfigurations' {} a -> s {s3Logs = a} :: DataSourceConfigurations)

-- | Describes whether any Kubernetes logs are enabled as data sources.
dataSourceConfigurations_kubernetes :: Lens.Lens' DataSourceConfigurations (Prelude.Maybe KubernetesConfiguration)
dataSourceConfigurations_kubernetes = Lens.lens (\DataSourceConfigurations' {kubernetes} -> kubernetes) (\s@DataSourceConfigurations' {} a -> s {kubernetes = a} :: DataSourceConfigurations)

instance Prelude.Hashable DataSourceConfigurations where
  hashWithSalt _salt DataSourceConfigurations' {..} =
    _salt `Prelude.hashWithSalt` malwareProtection
      `Prelude.hashWithSalt` s3Logs
      `Prelude.hashWithSalt` kubernetes

instance Prelude.NFData DataSourceConfigurations where
  rnf DataSourceConfigurations' {..} =
    Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs
      `Prelude.seq` Prelude.rnf kubernetes

instance Core.ToJSON DataSourceConfigurations where
  toJSON DataSourceConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("malwareProtection" Core..=)
              Prelude.<$> malwareProtection,
            ("s3Logs" Core..=) Prelude.<$> s3Logs,
            ("kubernetes" Core..=) Prelude.<$> kubernetes
          ]
      )
