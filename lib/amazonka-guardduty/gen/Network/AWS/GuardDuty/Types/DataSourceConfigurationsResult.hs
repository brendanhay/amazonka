{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult where

import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the status of data sources for the detector.
--
--
--
-- /See:/ 'dataSourceConfigurationsResult' smart constructor.
data DataSourceConfigurationsResult = DataSourceConfigurationsResult'
  { _dscrCloudTrail ::
      !CloudTrailConfigurationResult,
    _dscrDNSLogs ::
      !DNSLogsConfigurationResult,
    _dscrFlowLogs ::
      !FlowLogsConfigurationResult,
    _dscrS3Logs ::
      !S3LogsConfigurationResult
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSourceConfigurationsResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrCloudTrail' - An object that contains information on the status of CloudTrail as a data source.
--
-- * 'dscrDNSLogs' - An object that contains information on the status of DNS logs as a data source.
--
-- * 'dscrFlowLogs' - An object that contains information on the status of VPC flow logs as a data source.
--
-- * 'dscrS3Logs' - An object that contains information on the status of S3 Data event logs as a data source.
dataSourceConfigurationsResult ::
  -- | 'dscrCloudTrail'
  CloudTrailConfigurationResult ->
  -- | 'dscrDNSLogs'
  DNSLogsConfigurationResult ->
  -- | 'dscrFlowLogs'
  FlowLogsConfigurationResult ->
  -- | 'dscrS3Logs'
  S3LogsConfigurationResult ->
  DataSourceConfigurationsResult
dataSourceConfigurationsResult
  pCloudTrail_
  pDNSLogs_
  pFlowLogs_
  pS3Logs_ =
    DataSourceConfigurationsResult'
      { _dscrCloudTrail = pCloudTrail_,
        _dscrDNSLogs = pDNSLogs_,
        _dscrFlowLogs = pFlowLogs_,
        _dscrS3Logs = pS3Logs_
      }

-- | An object that contains information on the status of CloudTrail as a data source.
dscrCloudTrail :: Lens' DataSourceConfigurationsResult CloudTrailConfigurationResult
dscrCloudTrail = lens _dscrCloudTrail (\s a -> s {_dscrCloudTrail = a})

-- | An object that contains information on the status of DNS logs as a data source.
dscrDNSLogs :: Lens' DataSourceConfigurationsResult DNSLogsConfigurationResult
dscrDNSLogs = lens _dscrDNSLogs (\s a -> s {_dscrDNSLogs = a})

-- | An object that contains information on the status of VPC flow logs as a data source.
dscrFlowLogs :: Lens' DataSourceConfigurationsResult FlowLogsConfigurationResult
dscrFlowLogs = lens _dscrFlowLogs (\s a -> s {_dscrFlowLogs = a})

-- | An object that contains information on the status of S3 Data event logs as a data source.
dscrS3Logs :: Lens' DataSourceConfigurationsResult S3LogsConfigurationResult
dscrS3Logs = lens _dscrS3Logs (\s a -> s {_dscrS3Logs = a})

instance FromJSON DataSourceConfigurationsResult where
  parseJSON =
    withObject
      "DataSourceConfigurationsResult"
      ( \x ->
          DataSourceConfigurationsResult'
            <$> (x .: "cloudTrail")
            <*> (x .: "dnsLogs")
            <*> (x .: "flowLogs")
            <*> (x .: "s3Logs")
      )

instance Hashable DataSourceConfigurationsResult

instance NFData DataSourceConfigurationsResult
