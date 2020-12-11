-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
  ( WorkGroupConfigurationUpdates (..),

    -- * Smart constructor
    mkWorkGroupConfigurationUpdates,

    -- * Lenses
    wgcuRequesterPaysEnabled,
    wgcuResultConfigurationUpdates,
    wgcuBytesScannedCutoffPerQuery,
    wgcuRemoveBytesScannedCutoffPerQuery,
    wgcuEnforceWorkGroupConfiguration,
    wgcuPublishCloudWatchMetricsEnabled,
  )
where

import Network.AWS.Athena.Types.ResultConfigurationUpdates
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration information that will be updated for this workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, whether the workgroup settings override the client-side settings, and the data usage limit for the amount of bytes scanned per query, if it is specified.
--
-- /See:/ 'mkWorkGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { requesterPaysEnabled ::
      Lude.Maybe Lude.Bool,
    resultConfigurationUpdates ::
      Lude.Maybe
        ResultConfigurationUpdates,
    bytesScannedCutoffPerQuery ::
      Lude.Maybe Lude.Natural,
    removeBytesScannedCutoffPerQuery ::
      Lude.Maybe Lude.Bool,
    enforceWorkGroupConfiguration ::
      Lude.Maybe Lude.Bool,
    publishCloudWatchMetricsEnabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkGroupConfigurationUpdates' with the minimum fields required to make a request.
--
-- * 'bytesScannedCutoffPerQuery' - The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
-- * 'enforceWorkGroupConfiguration' - If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
-- * 'publishCloudWatchMetricsEnabled' - Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
-- * 'removeBytesScannedCutoffPerQuery' - Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
-- * 'requesterPaysEnabled' - If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'resultConfigurationUpdates' - The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
mkWorkGroupConfigurationUpdates ::
  WorkGroupConfigurationUpdates
mkWorkGroupConfigurationUpdates =
  WorkGroupConfigurationUpdates'
    { requesterPaysEnabled =
        Lude.Nothing,
      resultConfigurationUpdates = Lude.Nothing,
      bytesScannedCutoffPerQuery = Lude.Nothing,
      removeBytesScannedCutoffPerQuery = Lude.Nothing,
      enforceWorkGroupConfiguration = Lude.Nothing,
      publishCloudWatchMetricsEnabled = Lude.Nothing
    }

-- | If set to @true@ , allows members assigned to a workgroup to specify Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'requesterPaysEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuRequesterPaysEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe Lude.Bool)
wgcuRequesterPaysEnabled = Lens.lens (requesterPaysEnabled :: WorkGroupConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPaysEnabled = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuRequesterPaysEnabled "Use generic-lens or generic-optics with 'requesterPaysEnabled' instead." #-}

-- | The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
--
-- /Note:/ Consider using 'resultConfigurationUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuResultConfigurationUpdates :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe ResultConfigurationUpdates)
wgcuResultConfigurationUpdates = Lens.lens (resultConfigurationUpdates :: WorkGroupConfigurationUpdates -> Lude.Maybe ResultConfigurationUpdates) (\s a -> s {resultConfigurationUpdates = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuResultConfigurationUpdates "Use generic-lens or generic-optics with 'resultConfigurationUpdates' instead." #-}

-- | The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- /Note:/ Consider using 'bytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe Lude.Natural)
wgcuBytesScannedCutoffPerQuery = Lens.lens (bytesScannedCutoffPerQuery :: WorkGroupConfigurationUpdates -> Lude.Maybe Lude.Natural) (\s a -> s {bytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuBytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'bytesScannedCutoffPerQuery' instead." #-}

-- | Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
--
-- /Note:/ Consider using 'removeBytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuRemoveBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe Lude.Bool)
wgcuRemoveBytesScannedCutoffPerQuery = Lens.lens (removeBytesScannedCutoffPerQuery :: WorkGroupConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {removeBytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuRemoveBytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'removeBytesScannedCutoffPerQuery' instead." #-}

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'enforceWorkGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuEnforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe Lude.Bool)
wgcuEnforceWorkGroupConfiguration = Lens.lens (enforceWorkGroupConfiguration :: WorkGroupConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {enforceWorkGroupConfiguration = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuEnforceWorkGroupConfiguration "Use generic-lens or generic-optics with 'enforceWorkGroupConfiguration' instead." #-}

-- | Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
--
-- /Note:/ Consider using 'publishCloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcuPublishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Lude.Maybe Lude.Bool)
wgcuPublishCloudWatchMetricsEnabled = Lens.lens (publishCloudWatchMetricsEnabled :: WorkGroupConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {publishCloudWatchMetricsEnabled = a} :: WorkGroupConfigurationUpdates)
{-# DEPRECATED wgcuPublishCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'publishCloudWatchMetricsEnabled' instead." #-}

instance Lude.ToJSON WorkGroupConfigurationUpdates where
  toJSON WorkGroupConfigurationUpdates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RequesterPaysEnabled" Lude..=) Lude.<$> requesterPaysEnabled,
            ("ResultConfigurationUpdates" Lude..=)
              Lude.<$> resultConfigurationUpdates,
            ("BytesScannedCutoffPerQuery" Lude..=)
              Lude.<$> bytesScannedCutoffPerQuery,
            ("RemoveBytesScannedCutoffPerQuery" Lude..=)
              Lude.<$> removeBytesScannedCutoffPerQuery,
            ("EnforceWorkGroupConfiguration" Lude..=)
              Lude.<$> enforceWorkGroupConfiguration,
            ("PublishCloudWatchMetricsEnabled" Lude..=)
              Lude.<$> publishCloudWatchMetricsEnabled
          ]
      )
