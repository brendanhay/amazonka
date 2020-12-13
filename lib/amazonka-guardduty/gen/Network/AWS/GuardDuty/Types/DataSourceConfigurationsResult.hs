{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
  ( DataSourceConfigurationsResult (..),

    -- * Smart constructor
    mkDataSourceConfigurationsResult,

    -- * Lenses
    dscrS3Logs,
    dscrCloudTrail,
    dscrFlowLogs,
    dscrDNSLogs,
  )
where

import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the status of data sources for the detector.
--
-- /See:/ 'mkDataSourceConfigurationsResult' smart constructor.
data DataSourceConfigurationsResult = DataSourceConfigurationsResult'
  { -- | An object that contains information on the status of S3 Data event logs as a data source.
    s3Logs :: S3LogsConfigurationResult,
    -- | An object that contains information on the status of CloudTrail as a data source.
    cloudTrail :: CloudTrailConfigurationResult,
    -- | An object that contains information on the status of VPC flow logs as a data source.
    flowLogs :: FlowLogsConfigurationResult,
    -- | An object that contains information on the status of DNS logs as a data source.
    dnsLogs :: DNSLogsConfigurationResult
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSourceConfigurationsResult' with the minimum fields required to make a request.
--
-- * 's3Logs' - An object that contains information on the status of S3 Data event logs as a data source.
-- * 'cloudTrail' - An object that contains information on the status of CloudTrail as a data source.
-- * 'flowLogs' - An object that contains information on the status of VPC flow logs as a data source.
-- * 'dnsLogs' - An object that contains information on the status of DNS logs as a data source.
mkDataSourceConfigurationsResult ::
  -- | 's3Logs'
  S3LogsConfigurationResult ->
  -- | 'cloudTrail'
  CloudTrailConfigurationResult ->
  -- | 'flowLogs'
  FlowLogsConfigurationResult ->
  -- | 'dnsLogs'
  DNSLogsConfigurationResult ->
  DataSourceConfigurationsResult
mkDataSourceConfigurationsResult
  pS3Logs_
  pCloudTrail_
  pFlowLogs_
  pDNSLogs_ =
    DataSourceConfigurationsResult'
      { s3Logs = pS3Logs_,
        cloudTrail = pCloudTrail_,
        flowLogs = pFlowLogs_,
        dnsLogs = pDNSLogs_
      }

-- | An object that contains information on the status of S3 Data event logs as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrS3Logs :: Lens.Lens' DataSourceConfigurationsResult S3LogsConfigurationResult
dscrS3Logs = Lens.lens (s3Logs :: DataSourceConfigurationsResult -> S3LogsConfigurationResult) (\s a -> s {s3Logs = a} :: DataSourceConfigurationsResult)
{-# DEPRECATED dscrS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

-- | An object that contains information on the status of CloudTrail as a data source.
--
-- /Note:/ Consider using 'cloudTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrCloudTrail :: Lens.Lens' DataSourceConfigurationsResult CloudTrailConfigurationResult
dscrCloudTrail = Lens.lens (cloudTrail :: DataSourceConfigurationsResult -> CloudTrailConfigurationResult) (\s a -> s {cloudTrail = a} :: DataSourceConfigurationsResult)
{-# DEPRECATED dscrCloudTrail "Use generic-lens or generic-optics with 'cloudTrail' instead." #-}

-- | An object that contains information on the status of VPC flow logs as a data source.
--
-- /Note:/ Consider using 'flowLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrFlowLogs :: Lens.Lens' DataSourceConfigurationsResult FlowLogsConfigurationResult
dscrFlowLogs = Lens.lens (flowLogs :: DataSourceConfigurationsResult -> FlowLogsConfigurationResult) (\s a -> s {flowLogs = a} :: DataSourceConfigurationsResult)
{-# DEPRECATED dscrFlowLogs "Use generic-lens or generic-optics with 'flowLogs' instead." #-}

-- | An object that contains information on the status of DNS logs as a data source.
--
-- /Note:/ Consider using 'dnsLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrDNSLogs :: Lens.Lens' DataSourceConfigurationsResult DNSLogsConfigurationResult
dscrDNSLogs = Lens.lens (dnsLogs :: DataSourceConfigurationsResult -> DNSLogsConfigurationResult) (\s a -> s {dnsLogs = a} :: DataSourceConfigurationsResult)
{-# DEPRECATED dscrDNSLogs "Use generic-lens or generic-optics with 'dnsLogs' instead." #-}

instance Lude.FromJSON DataSourceConfigurationsResult where
  parseJSON =
    Lude.withObject
      "DataSourceConfigurationsResult"
      ( \x ->
          DataSourceConfigurationsResult'
            Lude.<$> (x Lude..: "s3Logs")
            Lude.<*> (x Lude..: "cloudTrail")
            Lude.<*> (x Lude..: "flowLogs")
            Lude.<*> (x Lude..: "dnsLogs")
      )
