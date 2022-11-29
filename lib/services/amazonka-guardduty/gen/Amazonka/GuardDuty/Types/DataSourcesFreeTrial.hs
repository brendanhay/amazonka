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
-- Module      : Amazonka.GuardDuty.Types.DataSourcesFreeTrial
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourcesFreeTrial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.DataSourceFreeTrial
import Amazonka.GuardDuty.Types.KubernetesDataSourceFreeTrial
import Amazonka.GuardDuty.Types.MalwareProtectionDataSourceFreeTrial
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which data sources are enabled for the
-- GuardDuty member account.
--
-- /See:/ 'newDataSourcesFreeTrial' smart constructor.
data DataSourcesFreeTrial = DataSourcesFreeTrial'
  { -- | Describes whether Malware Protection is enabled as a data source.
    malwareProtection :: Prelude.Maybe MalwareProtectionDataSourceFreeTrial,
    -- | Describes whether any Amazon Web Services CloudTrail management event
    -- logs are enabled as data sources.
    cloudTrail :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any DNS logs are enabled as data sources.
    dnsLogs :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any S3 data event logs are enabled as data sources.
    s3Logs :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any Kubernetes logs are enabled as data sources.
    kubernetes :: Prelude.Maybe KubernetesDataSourceFreeTrial,
    -- | Describes whether any VPC Flow logs are enabled as data sources.
    flowLogs :: Prelude.Maybe DataSourceFreeTrial
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourcesFreeTrial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'malwareProtection', 'dataSourcesFreeTrial_malwareProtection' - Describes whether Malware Protection is enabled as a data source.
--
-- 'cloudTrail', 'dataSourcesFreeTrial_cloudTrail' - Describes whether any Amazon Web Services CloudTrail management event
-- logs are enabled as data sources.
--
-- 'dnsLogs', 'dataSourcesFreeTrial_dnsLogs' - Describes whether any DNS logs are enabled as data sources.
--
-- 's3Logs', 'dataSourcesFreeTrial_s3Logs' - Describes whether any S3 data event logs are enabled as data sources.
--
-- 'kubernetes', 'dataSourcesFreeTrial_kubernetes' - Describes whether any Kubernetes logs are enabled as data sources.
--
-- 'flowLogs', 'dataSourcesFreeTrial_flowLogs' - Describes whether any VPC Flow logs are enabled as data sources.
newDataSourcesFreeTrial ::
  DataSourcesFreeTrial
newDataSourcesFreeTrial =
  DataSourcesFreeTrial'
    { malwareProtection =
        Prelude.Nothing,
      cloudTrail = Prelude.Nothing,
      dnsLogs = Prelude.Nothing,
      s3Logs = Prelude.Nothing,
      kubernetes = Prelude.Nothing,
      flowLogs = Prelude.Nothing
    }

-- | Describes whether Malware Protection is enabled as a data source.
dataSourcesFreeTrial_malwareProtection :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe MalwareProtectionDataSourceFreeTrial)
dataSourcesFreeTrial_malwareProtection = Lens.lens (\DataSourcesFreeTrial' {malwareProtection} -> malwareProtection) (\s@DataSourcesFreeTrial' {} a -> s {malwareProtection = a} :: DataSourcesFreeTrial)

-- | Describes whether any Amazon Web Services CloudTrail management event
-- logs are enabled as data sources.
dataSourcesFreeTrial_cloudTrail :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_cloudTrail = Lens.lens (\DataSourcesFreeTrial' {cloudTrail} -> cloudTrail) (\s@DataSourcesFreeTrial' {} a -> s {cloudTrail = a} :: DataSourcesFreeTrial)

-- | Describes whether any DNS logs are enabled as data sources.
dataSourcesFreeTrial_dnsLogs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_dnsLogs = Lens.lens (\DataSourcesFreeTrial' {dnsLogs} -> dnsLogs) (\s@DataSourcesFreeTrial' {} a -> s {dnsLogs = a} :: DataSourcesFreeTrial)

-- | Describes whether any S3 data event logs are enabled as data sources.
dataSourcesFreeTrial_s3Logs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_s3Logs = Lens.lens (\DataSourcesFreeTrial' {s3Logs} -> s3Logs) (\s@DataSourcesFreeTrial' {} a -> s {s3Logs = a} :: DataSourcesFreeTrial)

-- | Describes whether any Kubernetes logs are enabled as data sources.
dataSourcesFreeTrial_kubernetes :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe KubernetesDataSourceFreeTrial)
dataSourcesFreeTrial_kubernetes = Lens.lens (\DataSourcesFreeTrial' {kubernetes} -> kubernetes) (\s@DataSourcesFreeTrial' {} a -> s {kubernetes = a} :: DataSourcesFreeTrial)

-- | Describes whether any VPC Flow logs are enabled as data sources.
dataSourcesFreeTrial_flowLogs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_flowLogs = Lens.lens (\DataSourcesFreeTrial' {flowLogs} -> flowLogs) (\s@DataSourcesFreeTrial' {} a -> s {flowLogs = a} :: DataSourcesFreeTrial)

instance Core.FromJSON DataSourcesFreeTrial where
  parseJSON =
    Core.withObject
      "DataSourcesFreeTrial"
      ( \x ->
          DataSourcesFreeTrial'
            Prelude.<$> (x Core..:? "malwareProtection")
            Prelude.<*> (x Core..:? "cloudTrail")
            Prelude.<*> (x Core..:? "dnsLogs")
            Prelude.<*> (x Core..:? "s3Logs")
            Prelude.<*> (x Core..:? "kubernetes")
            Prelude.<*> (x Core..:? "flowLogs")
      )

instance Prelude.Hashable DataSourcesFreeTrial where
  hashWithSalt _salt DataSourcesFreeTrial' {..} =
    _salt `Prelude.hashWithSalt` malwareProtection
      `Prelude.hashWithSalt` cloudTrail
      `Prelude.hashWithSalt` dnsLogs
      `Prelude.hashWithSalt` s3Logs
      `Prelude.hashWithSalt` kubernetes
      `Prelude.hashWithSalt` flowLogs

instance Prelude.NFData DataSourcesFreeTrial where
  rnf DataSourcesFreeTrial' {..} =
    Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf cloudTrail
      `Prelude.seq` Prelude.rnf dnsLogs
      `Prelude.seq` Prelude.rnf s3Logs
      `Prelude.seq` Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf flowLogs
