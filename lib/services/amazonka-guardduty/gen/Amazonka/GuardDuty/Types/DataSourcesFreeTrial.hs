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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourcesFreeTrial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceFreeTrial
import Amazonka.GuardDuty.Types.KubernetesDataSourceFreeTrial
import Amazonka.GuardDuty.Types.MalwareProtectionDataSourceFreeTrial
import qualified Amazonka.Prelude as Prelude

-- | Contains information about which data sources are enabled for the
-- GuardDuty member account.
--
-- /See:/ 'newDataSourcesFreeTrial' smart constructor.
data DataSourcesFreeTrial = DataSourcesFreeTrial'
  { -- | Describes whether any Amazon Web Services CloudTrail management event
    -- logs are enabled as data sources.
    cloudTrail :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any DNS logs are enabled as data sources.
    dnsLogs :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any VPC Flow logs are enabled as data sources.
    flowLogs :: Prelude.Maybe DataSourceFreeTrial,
    -- | Describes whether any Kubernetes logs are enabled as data sources.
    kubernetes :: Prelude.Maybe KubernetesDataSourceFreeTrial,
    -- | Describes whether Malware Protection is enabled as a data source.
    malwareProtection :: Prelude.Maybe MalwareProtectionDataSourceFreeTrial,
    -- | Describes whether any S3 data event logs are enabled as data sources.
    s3Logs :: Prelude.Maybe DataSourceFreeTrial
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
-- 'cloudTrail', 'dataSourcesFreeTrial_cloudTrail' - Describes whether any Amazon Web Services CloudTrail management event
-- logs are enabled as data sources.
--
-- 'dnsLogs', 'dataSourcesFreeTrial_dnsLogs' - Describes whether any DNS logs are enabled as data sources.
--
-- 'flowLogs', 'dataSourcesFreeTrial_flowLogs' - Describes whether any VPC Flow logs are enabled as data sources.
--
-- 'kubernetes', 'dataSourcesFreeTrial_kubernetes' - Describes whether any Kubernetes logs are enabled as data sources.
--
-- 'malwareProtection', 'dataSourcesFreeTrial_malwareProtection' - Describes whether Malware Protection is enabled as a data source.
--
-- 's3Logs', 'dataSourcesFreeTrial_s3Logs' - Describes whether any S3 data event logs are enabled as data sources.
newDataSourcesFreeTrial ::
  DataSourcesFreeTrial
newDataSourcesFreeTrial =
  DataSourcesFreeTrial'
    { cloudTrail = Prelude.Nothing,
      dnsLogs = Prelude.Nothing,
      flowLogs = Prelude.Nothing,
      kubernetes = Prelude.Nothing,
      malwareProtection = Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | Describes whether any Amazon Web Services CloudTrail management event
-- logs are enabled as data sources.
dataSourcesFreeTrial_cloudTrail :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_cloudTrail = Lens.lens (\DataSourcesFreeTrial' {cloudTrail} -> cloudTrail) (\s@DataSourcesFreeTrial' {} a -> s {cloudTrail = a} :: DataSourcesFreeTrial)

-- | Describes whether any DNS logs are enabled as data sources.
dataSourcesFreeTrial_dnsLogs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_dnsLogs = Lens.lens (\DataSourcesFreeTrial' {dnsLogs} -> dnsLogs) (\s@DataSourcesFreeTrial' {} a -> s {dnsLogs = a} :: DataSourcesFreeTrial)

-- | Describes whether any VPC Flow logs are enabled as data sources.
dataSourcesFreeTrial_flowLogs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_flowLogs = Lens.lens (\DataSourcesFreeTrial' {flowLogs} -> flowLogs) (\s@DataSourcesFreeTrial' {} a -> s {flowLogs = a} :: DataSourcesFreeTrial)

-- | Describes whether any Kubernetes logs are enabled as data sources.
dataSourcesFreeTrial_kubernetes :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe KubernetesDataSourceFreeTrial)
dataSourcesFreeTrial_kubernetes = Lens.lens (\DataSourcesFreeTrial' {kubernetes} -> kubernetes) (\s@DataSourcesFreeTrial' {} a -> s {kubernetes = a} :: DataSourcesFreeTrial)

-- | Describes whether Malware Protection is enabled as a data source.
dataSourcesFreeTrial_malwareProtection :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe MalwareProtectionDataSourceFreeTrial)
dataSourcesFreeTrial_malwareProtection = Lens.lens (\DataSourcesFreeTrial' {malwareProtection} -> malwareProtection) (\s@DataSourcesFreeTrial' {} a -> s {malwareProtection = a} :: DataSourcesFreeTrial)

-- | Describes whether any S3 data event logs are enabled as data sources.
dataSourcesFreeTrial_s3Logs :: Lens.Lens' DataSourcesFreeTrial (Prelude.Maybe DataSourceFreeTrial)
dataSourcesFreeTrial_s3Logs = Lens.lens (\DataSourcesFreeTrial' {s3Logs} -> s3Logs) (\s@DataSourcesFreeTrial' {} a -> s {s3Logs = a} :: DataSourcesFreeTrial)

instance Data.FromJSON DataSourcesFreeTrial where
  parseJSON =
    Data.withObject
      "DataSourcesFreeTrial"
      ( \x ->
          DataSourcesFreeTrial'
            Prelude.<$> (x Data..:? "cloudTrail")
            Prelude.<*> (x Data..:? "dnsLogs")
            Prelude.<*> (x Data..:? "flowLogs")
            Prelude.<*> (x Data..:? "kubernetes")
            Prelude.<*> (x Data..:? "malwareProtection")
            Prelude.<*> (x Data..:? "s3Logs")
      )

instance Prelude.Hashable DataSourcesFreeTrial where
  hashWithSalt _salt DataSourcesFreeTrial' {..} =
    _salt
      `Prelude.hashWithSalt` cloudTrail
      `Prelude.hashWithSalt` dnsLogs
      `Prelude.hashWithSalt` flowLogs
      `Prelude.hashWithSalt` kubernetes
      `Prelude.hashWithSalt` malwareProtection
      `Prelude.hashWithSalt` s3Logs

instance Prelude.NFData DataSourcesFreeTrial where
  rnf DataSourcesFreeTrial' {..} =
    Prelude.rnf cloudTrail
      `Prelude.seq` Prelude.rnf dnsLogs
      `Prelude.seq` Prelude.rnf flowLogs
      `Prelude.seq` Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs
