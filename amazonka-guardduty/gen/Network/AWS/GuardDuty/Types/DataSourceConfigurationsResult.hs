{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult where

import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the status of data sources for the detector.
--
-- /See:/ 'newDataSourceConfigurationsResult' smart constructor.
data DataSourceConfigurationsResult = DataSourceConfigurationsResult'
  { -- | An object that contains information on the status of CloudTrail as a
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfigurationsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
      { cloudTrail =
          pCloudTrail_,
        dNSLogs = pDNSLogs_,
        flowLogs = pFlowLogs_,
        s3Logs = pS3Logs_
      }

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

instance
  Prelude.FromJSON
    DataSourceConfigurationsResult
  where
  parseJSON =
    Prelude.withObject
      "DataSourceConfigurationsResult"
      ( \x ->
          DataSourceConfigurationsResult'
            Prelude.<$> (x Prelude..: "cloudTrail")
            Prelude.<*> (x Prelude..: "dnsLogs")
            Prelude.<*> (x Prelude..: "flowLogs")
            Prelude.<*> (x Prelude..: "s3Logs")
      )

instance
  Prelude.Hashable
    DataSourceConfigurationsResult

instance
  Prelude.NFData
    DataSourceConfigurationsResult
