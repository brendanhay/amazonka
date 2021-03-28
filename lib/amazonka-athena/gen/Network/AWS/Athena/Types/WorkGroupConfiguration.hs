{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.WorkGroupConfiguration
  ( WorkGroupConfiguration (..)
  -- * Smart constructor
  , mkWorkGroupConfiguration
  -- * Lenses
  , wgcBytesScannedCutoffPerQuery
  , wgcEnforceWorkGroupConfiguration
  , wgcPublishCloudWatchMetricsEnabled
  , wgcRequesterPaysEnabled
  , wgcResultConfiguration
  ) where

import qualified Network.AWS.Athena.Types.ResultConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup and whether workgroup settings override query settings, and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' . 
--
-- /See:/ 'mkWorkGroupConfiguration' smart constructor.
data WorkGroupConfiguration = WorkGroupConfiguration'
  { bytesScannedCutoffPerQuery :: Core.Maybe Core.Natural
    -- ^ The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
  , enforceWorkGroupConfiguration :: Core.Maybe Core.Bool
    -- ^ If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
  , publishCloudWatchMetricsEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
  , requesterPaysEnabled :: Core.Maybe Core.Bool
    -- ^ If set to @true@ , allows members assigned to a workgroup to reference Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
  , resultConfiguration :: Core.Maybe Types.ResultConfiguration
    -- ^ The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. To run the query, you must specify the query results location using one of the ways: either in the workgroup using this setting, or for individual queries (client-side), using 'ResultConfiguration$OutputLocation' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkGroupConfiguration' value with any optional fields omitted.
mkWorkGroupConfiguration
    :: WorkGroupConfiguration
mkWorkGroupConfiguration
  = WorkGroupConfiguration'{bytesScannedCutoffPerQuery =
                              Core.Nothing,
                            enforceWorkGroupConfiguration = Core.Nothing,
                            publishCloudWatchMetricsEnabled = Core.Nothing,
                            requesterPaysEnabled = Core.Nothing,
                            resultConfiguration = Core.Nothing}

-- | The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- /Note:/ Consider using 'bytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfiguration (Core.Maybe Core.Natural)
wgcBytesScannedCutoffPerQuery = Lens.field @"bytesScannedCutoffPerQuery"
{-# INLINEABLE wgcBytesScannedCutoffPerQuery #-}
{-# DEPRECATED bytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'bytesScannedCutoffPerQuery' instead"  #-}

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'enforceWorkGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcEnforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfiguration (Core.Maybe Core.Bool)
wgcEnforceWorkGroupConfiguration = Lens.field @"enforceWorkGroupConfiguration"
{-# INLINEABLE wgcEnforceWorkGroupConfiguration #-}
{-# DEPRECATED enforceWorkGroupConfiguration "Use generic-lens or generic-optics with 'enforceWorkGroupConfiguration' instead"  #-}

-- | Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
--
-- /Note:/ Consider using 'publishCloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcPublishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfiguration (Core.Maybe Core.Bool)
wgcPublishCloudWatchMetricsEnabled = Lens.field @"publishCloudWatchMetricsEnabled"
{-# INLINEABLE wgcPublishCloudWatchMetricsEnabled #-}
{-# DEPRECATED publishCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'publishCloudWatchMetricsEnabled' instead"  #-}

-- | If set to @true@ , allows members assigned to a workgroup to reference Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'requesterPaysEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcRequesterPaysEnabled :: Lens.Lens' WorkGroupConfiguration (Core.Maybe Core.Bool)
wgcRequesterPaysEnabled = Lens.field @"requesterPaysEnabled"
{-# INLINEABLE wgcRequesterPaysEnabled #-}
{-# DEPRECATED requesterPaysEnabled "Use generic-lens or generic-optics with 'requesterPaysEnabled' instead"  #-}

-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. To run the query, you must specify the query results location using one of the ways: either in the workgroup using this setting, or for individual queries (client-side), using 'ResultConfiguration$OutputLocation' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> .
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcResultConfiguration :: Lens.Lens' WorkGroupConfiguration (Core.Maybe Types.ResultConfiguration)
wgcResultConfiguration = Lens.field @"resultConfiguration"
{-# INLINEABLE wgcResultConfiguration #-}
{-# DEPRECATED resultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead"  #-}

instance Core.FromJSON WorkGroupConfiguration where
        toJSON WorkGroupConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("BytesScannedCutoffPerQuery" Core..=) Core.<$>
                    bytesScannedCutoffPerQuery,
                  ("EnforceWorkGroupConfiguration" Core..=) Core.<$>
                    enforceWorkGroupConfiguration,
                  ("PublishCloudWatchMetricsEnabled" Core..=) Core.<$>
                    publishCloudWatchMetricsEnabled,
                  ("RequesterPaysEnabled" Core..=) Core.<$> requesterPaysEnabled,
                  ("ResultConfiguration" Core..=) Core.<$> resultConfiguration])

instance Core.FromJSON WorkGroupConfiguration where
        parseJSON
          = Core.withObject "WorkGroupConfiguration" Core.$
              \ x ->
                WorkGroupConfiguration' Core.<$>
                  (x Core..:? "BytesScannedCutoffPerQuery") Core.<*>
                    x Core..:? "EnforceWorkGroupConfiguration"
                    Core.<*> x Core..:? "PublishCloudWatchMetricsEnabled"
                    Core.<*> x Core..:? "RequesterPaysEnabled"
                    Core.<*> x Core..:? "ResultConfiguration"
