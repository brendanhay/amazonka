{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
  ( WorkGroupConfigurationUpdates (..)
  -- * Smart constructor
  , mkWorkGroupConfigurationUpdates
  -- * Lenses
  , wgcuBytesScannedCutoffPerQuery
  , wgcuEnforceWorkGroupConfiguration
  , wgcuPublishCloudWatchMetricsEnabled
  , wgcuRemoveBytesScannedCutoffPerQuery
  , wgcuRequesterPaysEnabled
  , wgcuResultConfigurationUpdates
  ) where

import qualified Network.AWS.Athena.Types.ResultConfigurationUpdates as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration information that will be updated for this workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, whether the workgroup settings override the client-side settings, and the data usage limit for the amount of bytes scanned per query, if it is specified.
--
-- /See:/ 'mkWorkGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { bytesScannedCutoffPerQuery :: Core.Maybe Core.Natural
    -- ^ The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
  , enforceWorkGroupConfiguration :: Core.Maybe Core.Bool
    -- ^ If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
  , publishCloudWatchMetricsEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
  , removeBytesScannedCutoffPerQuery :: Core.Maybe Core.Bool
    -- ^ Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery' 
  , requesterPaysEnabled :: Core.Maybe Core.Bool
    -- ^ If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
  , resultConfigurationUpdates :: Core.Maybe Types.ResultConfigurationUpdates
    -- ^ The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkGroupConfigurationUpdates' value with any optional fields omitted.
mkWorkGroupConfigurationUpdates
    :: WorkGroupConfigurationUpdates
mkWorkGroupConfigurationUpdates
  = WorkGroupConfigurationUpdates'{bytesScannedCutoffPerQuery =
                                     Core.Nothing,
                                   enforceWorkGroupConfiguration = Core.Nothing,
                                   publishCloudWatchMetricsEnabled = Core.Nothing,
                                   removeBytesScannedCutoffPerQuery = Core.Nothing,
                                   requesterPaysEnabled = Core.Nothing,
                                   resultConfigurationUpdates = Core.Nothing}

-- | The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- /Note:/ Consider using 'bytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Natural)
wgcuBytesScannedCutoffPerQuery = Lens.field @"bytesScannedCutoffPerQuery"
{-# INLINEABLE wgcuBytesScannedCutoffPerQuery #-}
{-# DEPRECATED bytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'bytesScannedCutoffPerQuery' instead"  #-}

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'enforceWorkGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuEnforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
wgcuEnforceWorkGroupConfiguration = Lens.field @"enforceWorkGroupConfiguration"
{-# INLINEABLE wgcuEnforceWorkGroupConfiguration #-}
{-# DEPRECATED enforceWorkGroupConfiguration "Use generic-lens or generic-optics with 'enforceWorkGroupConfiguration' instead"  #-}

-- | Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
--
-- /Note:/ Consider using 'publishCloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuPublishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
wgcuPublishCloudWatchMetricsEnabled = Lens.field @"publishCloudWatchMetricsEnabled"
{-# INLINEABLE wgcuPublishCloudWatchMetricsEnabled #-}
{-# DEPRECATED publishCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'publishCloudWatchMetricsEnabled' instead"  #-}

-- | Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery' 
--
-- /Note:/ Consider using 'removeBytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuRemoveBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
wgcuRemoveBytesScannedCutoffPerQuery = Lens.field @"removeBytesScannedCutoffPerQuery"
{-# INLINEABLE wgcuRemoveBytesScannedCutoffPerQuery #-}
{-# DEPRECATED removeBytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'removeBytesScannedCutoffPerQuery' instead"  #-}

-- | If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'requesterPaysEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuRequesterPaysEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
wgcuRequesterPaysEnabled = Lens.field @"requesterPaysEnabled"
{-# INLINEABLE wgcuRequesterPaysEnabled #-}
{-# DEPRECATED requesterPaysEnabled "Use generic-lens or generic-optics with 'requesterPaysEnabled' instead"  #-}

-- | The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
--
-- /Note:/ Consider using 'resultConfigurationUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuResultConfigurationUpdates :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Types.ResultConfigurationUpdates)
wgcuResultConfigurationUpdates = Lens.field @"resultConfigurationUpdates"
{-# INLINEABLE wgcuResultConfigurationUpdates #-}
{-# DEPRECATED resultConfigurationUpdates "Use generic-lens or generic-optics with 'resultConfigurationUpdates' instead"  #-}

instance Core.FromJSON WorkGroupConfigurationUpdates where
        toJSON WorkGroupConfigurationUpdates{..}
          = Core.object
              (Core.catMaybes
                 [("BytesScannedCutoffPerQuery" Core..=) Core.<$>
                    bytesScannedCutoffPerQuery,
                  ("EnforceWorkGroupConfiguration" Core..=) Core.<$>
                    enforceWorkGroupConfiguration,
                  ("PublishCloudWatchMetricsEnabled" Core..=) Core.<$>
                    publishCloudWatchMetricsEnabled,
                  ("RemoveBytesScannedCutoffPerQuery" Core..=) Core.<$>
                    removeBytesScannedCutoffPerQuery,
                  ("RequesterPaysEnabled" Core..=) Core.<$> requesterPaysEnabled,
                  ("ResultConfigurationUpdates" Core..=) Core.<$>
                    resultConfigurationUpdates])
