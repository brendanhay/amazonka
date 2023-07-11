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
-- Module      : Amazonka.GuardDuty.Types.DataSourceConfigurationsResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DataSourceConfigurationsResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CloudTrailConfigurationResult
import Amazonka.GuardDuty.Types.DNSLogsConfigurationResult
import Amazonka.GuardDuty.Types.FlowLogsConfigurationResult
import Amazonka.GuardDuty.Types.KubernetesConfigurationResult
import Amazonka.GuardDuty.Types.MalwareProtectionConfigurationResult
import Amazonka.GuardDuty.Types.S3LogsConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the status of data sources for the detector.
--
-- /See:/ 'newDataSourceConfigurationsResult' smart constructor.
data DataSourceConfigurationsResult = DataSourceConfigurationsResult'
  { -- | An object that contains information on the status of all Kubernetes data
    -- sources.
    kubernetes :: Prelude.Maybe KubernetesConfigurationResult,
    -- | Describes the configuration of Malware Protection data sources.
    malwareProtection :: Prelude.Maybe MalwareProtectionConfigurationResult,
    -- | An object that contains information on the status of CloudTrail as a
    -- data source.
    cloudTrail :: CloudTrailConfigurationResult,
    -- | An object that contains information on the status of DNS logs as a data
    -- source.
    dNSLogs :: DNSLogsConfigurationResult,
    -- | An object that contains information on the status of VPC flow logs as a
    -- data source.
    flowLogs :: FlowLogsConfigurationResult,
    -- | An object that contains information on the status of S3 Data event logs
    -- as a data source.
    s3Logs :: S3LogsConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfigurationsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetes', 'dataSourceConfigurationsResult_kubernetes' - An object that contains information on the status of all Kubernetes data
-- sources.
--
-- 'malwareProtection', 'dataSourceConfigurationsResult_malwareProtection' - Describes the configuration of Malware Protection data sources.
--
-- 'cloudTrail', 'dataSourceConfigurationsResult_cloudTrail' - An object that contains information on the status of CloudTrail as a
-- data source.
--
-- 'dNSLogs', 'dataSourceConfigurationsResult_dNSLogs' - An object that contains information on the status of DNS logs as a data
-- source.
--
-- 'flowLogs', 'dataSourceConfigurationsResult_flowLogs' - An object that contains information on the status of VPC flow logs as a
-- data source.
--
-- 's3Logs', 'dataSourceConfigurationsResult_s3Logs' - An object that contains information on the status of S3 Data event logs
-- as a data source.
newDataSourceConfigurationsResult ::
  -- | 'cloudTrail'
  CloudTrailConfigurationResult ->
  -- | 'dNSLogs'
  DNSLogsConfigurationResult ->
  -- | 'flowLogs'
  FlowLogsConfigurationResult ->
  -- | 's3Logs'
  S3LogsConfigurationResult ->
  DataSourceConfigurationsResult
newDataSourceConfigurationsResult
  pCloudTrail_
  pDNSLogs_
  pFlowLogs_
  pS3Logs_ =
    DataSourceConfigurationsResult'
      { kubernetes =
          Prelude.Nothing,
        malwareProtection = Prelude.Nothing,
        cloudTrail = pCloudTrail_,
        dNSLogs = pDNSLogs_,
        flowLogs = pFlowLogs_,
        s3Logs = pS3Logs_
      }

-- | An object that contains information on the status of all Kubernetes data
-- sources.
dataSourceConfigurationsResult_kubernetes :: Lens.Lens' DataSourceConfigurationsResult (Prelude.Maybe KubernetesConfigurationResult)
dataSourceConfigurationsResult_kubernetes = Lens.lens (\DataSourceConfigurationsResult' {kubernetes} -> kubernetes) (\s@DataSourceConfigurationsResult' {} a -> s {kubernetes = a} :: DataSourceConfigurationsResult)

-- | Describes the configuration of Malware Protection data sources.
dataSourceConfigurationsResult_malwareProtection :: Lens.Lens' DataSourceConfigurationsResult (Prelude.Maybe MalwareProtectionConfigurationResult)
dataSourceConfigurationsResult_malwareProtection = Lens.lens (\DataSourceConfigurationsResult' {malwareProtection} -> malwareProtection) (\s@DataSourceConfigurationsResult' {} a -> s {malwareProtection = a} :: DataSourceConfigurationsResult)

-- | An object that contains information on the status of CloudTrail as a
-- data source.
dataSourceConfigurationsResult_cloudTrail :: Lens.Lens' DataSourceConfigurationsResult CloudTrailConfigurationResult
dataSourceConfigurationsResult_cloudTrail = Lens.lens (\DataSourceConfigurationsResult' {cloudTrail} -> cloudTrail) (\s@DataSourceConfigurationsResult' {} a -> s {cloudTrail = a} :: DataSourceConfigurationsResult)

-- | An object that contains information on the status of DNS logs as a data
-- source.
dataSourceConfigurationsResult_dNSLogs :: Lens.Lens' DataSourceConfigurationsResult DNSLogsConfigurationResult
dataSourceConfigurationsResult_dNSLogs = Lens.lens (\DataSourceConfigurationsResult' {dNSLogs} -> dNSLogs) (\s@DataSourceConfigurationsResult' {} a -> s {dNSLogs = a} :: DataSourceConfigurationsResult)

-- | An object that contains information on the status of VPC flow logs as a
-- data source.
dataSourceConfigurationsResult_flowLogs :: Lens.Lens' DataSourceConfigurationsResult FlowLogsConfigurationResult
dataSourceConfigurationsResult_flowLogs = Lens.lens (\DataSourceConfigurationsResult' {flowLogs} -> flowLogs) (\s@DataSourceConfigurationsResult' {} a -> s {flowLogs = a} :: DataSourceConfigurationsResult)

-- | An object that contains information on the status of S3 Data event logs
-- as a data source.
dataSourceConfigurationsResult_s3Logs :: Lens.Lens' DataSourceConfigurationsResult S3LogsConfigurationResult
dataSourceConfigurationsResult_s3Logs = Lens.lens (\DataSourceConfigurationsResult' {s3Logs} -> s3Logs) (\s@DataSourceConfigurationsResult' {} a -> s {s3Logs = a} :: DataSourceConfigurationsResult)

instance Data.FromJSON DataSourceConfigurationsResult where
  parseJSON =
    Data.withObject
      "DataSourceConfigurationsResult"
      ( \x ->
          DataSourceConfigurationsResult'
            Prelude.<$> (x Data..:? "kubernetes")
            Prelude.<*> (x Data..:? "malwareProtection")
            Prelude.<*> (x Data..: "cloudTrail")
            Prelude.<*> (x Data..: "dnsLogs")
            Prelude.<*> (x Data..: "flowLogs")
            Prelude.<*> (x Data..: "s3Logs")
      )

instance
  Prelude.Hashable
    DataSourceConfigurationsResult
  where
  hashWithSalt
    _salt
    DataSourceConfigurationsResult' {..} =
      _salt
        `Prelude.hashWithSalt` kubernetes
        `Prelude.hashWithSalt` malwareProtection
        `Prelude.hashWithSalt` cloudTrail
        `Prelude.hashWithSalt` dNSLogs
        `Prelude.hashWithSalt` flowLogs
        `Prelude.hashWithSalt` s3Logs

instance
  Prelude.NFData
    DataSourceConfigurationsResult
  where
  rnf DataSourceConfigurationsResult' {..} =
    Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf cloudTrail
      `Prelude.seq` Prelude.rnf dNSLogs
      `Prelude.seq` Prelude.rnf flowLogs
      `Prelude.seq` Prelude.rnf s3Logs
