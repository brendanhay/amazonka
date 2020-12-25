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
    dscrCloudTrail,
    dscrDNSLogs,
    dscrFlowLogs,
    dscrS3Logs,
  )
where

import qualified Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult as Types
import qualified Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult as Types
import qualified Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult as Types
import qualified Network.AWS.GuardDuty.Types.S3LogsConfigurationResult as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the status of data sources for the detector.
--
-- /See:/ 'mkDataSourceConfigurationsResult' smart constructor.
data DataSourceConfigurationsResult = DataSourceConfigurationsResult'
  { -- | An object that contains information on the status of CloudTrail as a data source.
    cloudTrail :: Types.CloudTrailConfigurationResult,
    -- | An object that contains information on the status of DNS logs as a data source.
    dNSLogs :: Types.DNSLogsConfigurationResult,
    -- | An object that contains information on the status of VPC flow logs as a data source.
    flowLogs :: Types.FlowLogsConfigurationResult,
    -- | An object that contains information on the status of S3 Data event logs as a data source.
    s3Logs :: Types.S3LogsConfigurationResult
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataSourceConfigurationsResult' value with any optional fields omitted.
mkDataSourceConfigurationsResult ::
  -- | 'cloudTrail'
  Types.CloudTrailConfigurationResult ->
  -- | 'dNSLogs'
  Types.DNSLogsConfigurationResult ->
  -- | 'flowLogs'
  Types.FlowLogsConfigurationResult ->
  -- | 's3Logs'
  Types.S3LogsConfigurationResult ->
  DataSourceConfigurationsResult
mkDataSourceConfigurationsResult cloudTrail dNSLogs flowLogs s3Logs =
  DataSourceConfigurationsResult'
    { cloudTrail,
      dNSLogs,
      flowLogs,
      s3Logs
    }

-- | An object that contains information on the status of CloudTrail as a data source.
--
-- /Note:/ Consider using 'cloudTrail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrCloudTrail :: Lens.Lens' DataSourceConfigurationsResult Types.CloudTrailConfigurationResult
dscrCloudTrail = Lens.field @"cloudTrail"
{-# DEPRECATED dscrCloudTrail "Use generic-lens or generic-optics with 'cloudTrail' instead." #-}

-- | An object that contains information on the status of DNS logs as a data source.
--
-- /Note:/ Consider using 'dNSLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrDNSLogs :: Lens.Lens' DataSourceConfigurationsResult Types.DNSLogsConfigurationResult
dscrDNSLogs = Lens.field @"dNSLogs"
{-# DEPRECATED dscrDNSLogs "Use generic-lens or generic-optics with 'dNSLogs' instead." #-}

-- | An object that contains information on the status of VPC flow logs as a data source.
--
-- /Note:/ Consider using 'flowLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrFlowLogs :: Lens.Lens' DataSourceConfigurationsResult Types.FlowLogsConfigurationResult
dscrFlowLogs = Lens.field @"flowLogs"
{-# DEPRECATED dscrFlowLogs "Use generic-lens or generic-optics with 'flowLogs' instead." #-}

-- | An object that contains information on the status of S3 Data event logs as a data source.
--
-- /Note:/ Consider using 's3Logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrS3Logs :: Lens.Lens' DataSourceConfigurationsResult Types.S3LogsConfigurationResult
dscrS3Logs = Lens.field @"s3Logs"
{-# DEPRECATED dscrS3Logs "Use generic-lens or generic-optics with 's3Logs' instead." #-}

instance Core.FromJSON DataSourceConfigurationsResult where
  parseJSON =
    Core.withObject "DataSourceConfigurationsResult" Core.$
      \x ->
        DataSourceConfigurationsResult'
          Core.<$> (x Core..: "cloudTrail")
          Core.<*> (x Core..: "dnsLogs")
          Core.<*> (x Core..: "flowLogs")
          Core.<*> (x Core..: "s3Logs")
