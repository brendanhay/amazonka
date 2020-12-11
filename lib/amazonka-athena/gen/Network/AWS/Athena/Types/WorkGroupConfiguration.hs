-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupConfiguration
  ( WorkGroupConfiguration (..),

    -- * Smart constructor
    mkWorkGroupConfiguration,

    -- * Lenses
    wgcRequesterPaysEnabled,
    wgcResultConfiguration,
    wgcBytesScannedCutoffPerQuery,
    wgcEnforceWorkGroupConfiguration,
    wgcPublishCloudWatchMetricsEnabled,
  )
where

import Network.AWS.Athena.Types.ResultConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup and whether workgroup settings override query settings, and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /See:/ 'mkWorkGroupConfiguration' smart constructor.
data WorkGroupConfiguration = WorkGroupConfiguration'
  { requesterPaysEnabled ::
      Lude.Maybe Lude.Bool,
    resultConfiguration ::
      Lude.Maybe ResultConfiguration,
    bytesScannedCutoffPerQuery ::
      Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'WorkGroupConfiguration' with the minimum fields required to make a request.
--
-- * 'bytesScannedCutoffPerQuery' - The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
-- * 'enforceWorkGroupConfiguration' - If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
-- * 'publishCloudWatchMetricsEnabled' - Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
-- * 'requesterPaysEnabled' - If set to @true@ , allows members assigned to a workgroup to reference Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'resultConfiguration' - The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. To run the query, you must specify the query results location using one of the ways: either in the workgroup using this setting, or for individual queries (client-side), using 'ResultConfiguration$OutputLocation' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> .
mkWorkGroupConfiguration ::
  WorkGroupConfiguration
mkWorkGroupConfiguration =
  WorkGroupConfiguration'
    { requesterPaysEnabled = Lude.Nothing,
      resultConfiguration = Lude.Nothing,
      bytesScannedCutoffPerQuery = Lude.Nothing,
      enforceWorkGroupConfiguration = Lude.Nothing,
      publishCloudWatchMetricsEnabled = Lude.Nothing
    }

-- | If set to @true@ , allows members assigned to a workgroup to reference Amazon S3 Requester Pays buckets in queries. If set to @false@ , workgroup members cannot query data from Requester Pays buckets, and queries that retrieve data from Requester Pays buckets cause an error. The default is @false@ . For more information about Requester Pays buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'requesterPaysEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcRequesterPaysEnabled :: Lens.Lens' WorkGroupConfiguration (Lude.Maybe Lude.Bool)
wgcRequesterPaysEnabled = Lens.lens (requesterPaysEnabled :: WorkGroupConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPaysEnabled = a} :: WorkGroupConfiguration)
{-# DEPRECATED wgcRequesterPaysEnabled "Use generic-lens or generic-optics with 'requesterPaysEnabled' instead." #-}

-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. To run the query, you must specify the query results location using one of the ways: either in the workgroup using this setting, or for individual queries (client-side), using 'ResultConfiguration$OutputLocation' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> .
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcResultConfiguration :: Lens.Lens' WorkGroupConfiguration (Lude.Maybe ResultConfiguration)
wgcResultConfiguration = Lens.lens (resultConfiguration :: WorkGroupConfiguration -> Lude.Maybe ResultConfiguration) (\s a -> s {resultConfiguration = a} :: WorkGroupConfiguration)
{-# DEPRECATED wgcResultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead." #-}

-- | The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- /Note:/ Consider using 'bytesScannedCutoffPerQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfiguration (Lude.Maybe Lude.Natural)
wgcBytesScannedCutoffPerQuery = Lens.lens (bytesScannedCutoffPerQuery :: WorkGroupConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {bytesScannedCutoffPerQuery = a} :: WorkGroupConfiguration)
{-# DEPRECATED wgcBytesScannedCutoffPerQuery "Use generic-lens or generic-optics with 'bytesScannedCutoffPerQuery' instead." #-}

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'enforceWorkGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcEnforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfiguration (Lude.Maybe Lude.Bool)
wgcEnforceWorkGroupConfiguration = Lens.lens (enforceWorkGroupConfiguration :: WorkGroupConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enforceWorkGroupConfiguration = a} :: WorkGroupConfiguration)
{-# DEPRECATED wgcEnforceWorkGroupConfiguration "Use generic-lens or generic-optics with 'enforceWorkGroupConfiguration' instead." #-}

-- | Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
--
-- /Note:/ Consider using 'publishCloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgcPublishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfiguration (Lude.Maybe Lude.Bool)
wgcPublishCloudWatchMetricsEnabled = Lens.lens (publishCloudWatchMetricsEnabled :: WorkGroupConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {publishCloudWatchMetricsEnabled = a} :: WorkGroupConfiguration)
{-# DEPRECATED wgcPublishCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'publishCloudWatchMetricsEnabled' instead." #-}

instance Lude.FromJSON WorkGroupConfiguration where
  parseJSON =
    Lude.withObject
      "WorkGroupConfiguration"
      ( \x ->
          WorkGroupConfiguration'
            Lude.<$> (x Lude..:? "RequesterPaysEnabled")
            Lude.<*> (x Lude..:? "ResultConfiguration")
            Lude.<*> (x Lude..:? "BytesScannedCutoffPerQuery")
            Lude.<*> (x Lude..:? "EnforceWorkGroupConfiguration")
            Lude.<*> (x Lude..:? "PublishCloudWatchMetricsEnabled")
      )

instance Lude.ToJSON WorkGroupConfiguration where
  toJSON WorkGroupConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RequesterPaysEnabled" Lude..=) Lude.<$> requesterPaysEnabled,
            ("ResultConfiguration" Lude..=) Lude.<$> resultConfiguration,
            ("BytesScannedCutoffPerQuery" Lude..=)
              Lude.<$> bytesScannedCutoffPerQuery,
            ("EnforceWorkGroupConfiguration" Lude..=)
              Lude.<$> enforceWorkGroupConfiguration,
            ("PublishCloudWatchMetricsEnabled" Lude..=)
              Lude.<$> publishCloudWatchMetricsEnabled
          ]
      )
