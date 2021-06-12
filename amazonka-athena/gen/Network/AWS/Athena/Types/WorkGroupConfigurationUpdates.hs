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
-- Module      : Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupConfigurationUpdates where

import Network.AWS.Athena.Types.EngineVersion
import Network.AWS.Athena.Types.ResultConfigurationUpdates
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The configuration information that will be updated for this workgroup,
-- which includes the location in Amazon S3 where query results are stored,
-- the encryption option, if any, used for query results, whether the
-- Amazon CloudWatch Metrics are enabled for the workgroup, whether the
-- workgroup settings override the client-side settings, and the data usage
-- limit for the amount of bytes scanned per query, if it is specified.
--
-- /See:/ 'newWorkGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { -- | The upper limit (cutoff) for the amount of bytes a single query in a
    -- workgroup is allowed to scan.
    bytesScannedCutoffPerQuery :: Core.Maybe Core.Natural,
    -- | The result configuration information about the queries in this workgroup
    -- that will be updated. Includes the updated results location and an
    -- updated option for encrypting query results.
    resultConfigurationUpdates :: Core.Maybe ResultConfigurationUpdates,
    -- | Indicates whether this workgroup enables publishing metrics to Amazon
    -- CloudWatch.
    publishCloudWatchMetricsEnabled :: Core.Maybe Core.Bool,
    -- | If set to \"true\", the settings for the workgroup override client-side
    -- settings. If set to \"false\" client-side settings are used. For more
    -- information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    enforceWorkGroupConfiguration :: Core.Maybe Core.Bool,
    -- | If set to @true@, allows members assigned to a workgroup to specify
    -- Amazon S3 Requester Pays buckets in queries. If set to @false@,
    -- workgroup members cannot query data from Requester Pays buckets, and
    -- queries that retrieve data from Requester Pays buckets cause an error.
    -- The default is @false@. For more information about Requester Pays
    -- buckets, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    requesterPaysEnabled :: Core.Maybe Core.Bool,
    -- | Indicates that the data usage control limit per query is removed.
    -- WorkGroupConfiguration$BytesScannedCutoffPerQuery
    removeBytesScannedCutoffPerQuery :: Core.Maybe Core.Bool,
    -- | The engine version requested when a workgroup is updated. After the
    -- update, all queries on the workgroup run on the requested engine
    -- version. If no value was previously set, the default is Auto. Queries on
    -- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
    -- engine regardless of this setting.
    engineVersion :: Core.Maybe EngineVersion
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkGroupConfigurationUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesScannedCutoffPerQuery', 'workGroupConfigurationUpdates_bytesScannedCutoffPerQuery' - The upper limit (cutoff) for the amount of bytes a single query in a
-- workgroup is allowed to scan.
--
-- 'resultConfigurationUpdates', 'workGroupConfigurationUpdates_resultConfigurationUpdates' - The result configuration information about the queries in this workgroup
-- that will be updated. Includes the updated results location and an
-- updated option for encrypting query results.
--
-- 'publishCloudWatchMetricsEnabled', 'workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled' - Indicates whether this workgroup enables publishing metrics to Amazon
-- CloudWatch.
--
-- 'enforceWorkGroupConfiguration', 'workGroupConfigurationUpdates_enforceWorkGroupConfiguration' - If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\" client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'requesterPaysEnabled', 'workGroupConfigurationUpdates_requesterPaysEnabled' - If set to @true@, allows members assigned to a workgroup to specify
-- Amazon S3 Requester Pays buckets in queries. If set to @false@,
-- workgroup members cannot query data from Requester Pays buckets, and
-- queries that retrieve data from Requester Pays buckets cause an error.
-- The default is @false@. For more information about Requester Pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'removeBytesScannedCutoffPerQuery', 'workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery' - Indicates that the data usage control limit per query is removed.
-- WorkGroupConfiguration$BytesScannedCutoffPerQuery
--
-- 'engineVersion', 'workGroupConfigurationUpdates_engineVersion' - The engine version requested when a workgroup is updated. After the
-- update, all queries on the workgroup run on the requested engine
-- version. If no value was previously set, the default is Auto. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
newWorkGroupConfigurationUpdates ::
  WorkGroupConfigurationUpdates
newWorkGroupConfigurationUpdates =
  WorkGroupConfigurationUpdates'
    { bytesScannedCutoffPerQuery =
        Core.Nothing,
      resultConfigurationUpdates = Core.Nothing,
      publishCloudWatchMetricsEnabled =
        Core.Nothing,
      enforceWorkGroupConfiguration = Core.Nothing,
      requesterPaysEnabled = Core.Nothing,
      removeBytesScannedCutoffPerQuery =
        Core.Nothing,
      engineVersion = Core.Nothing
    }

-- | The upper limit (cutoff) for the amount of bytes a single query in a
-- workgroup is allowed to scan.
workGroupConfigurationUpdates_bytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Natural)
workGroupConfigurationUpdates_bytesScannedCutoffPerQuery = Lens.lens (\WorkGroupConfigurationUpdates' {bytesScannedCutoffPerQuery} -> bytesScannedCutoffPerQuery) (\s@WorkGroupConfigurationUpdates' {} a -> s {bytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)

-- | The result configuration information about the queries in this workgroup
-- that will be updated. Includes the updated results location and an
-- updated option for encrypting query results.
workGroupConfigurationUpdates_resultConfigurationUpdates :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe ResultConfigurationUpdates)
workGroupConfigurationUpdates_resultConfigurationUpdates = Lens.lens (\WorkGroupConfigurationUpdates' {resultConfigurationUpdates} -> resultConfigurationUpdates) (\s@WorkGroupConfigurationUpdates' {} a -> s {resultConfigurationUpdates = a} :: WorkGroupConfigurationUpdates)

-- | Indicates whether this workgroup enables publishing metrics to Amazon
-- CloudWatch.
workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled = Lens.lens (\WorkGroupConfigurationUpdates' {publishCloudWatchMetricsEnabled} -> publishCloudWatchMetricsEnabled) (\s@WorkGroupConfigurationUpdates' {} a -> s {publishCloudWatchMetricsEnabled = a} :: WorkGroupConfigurationUpdates)

-- | If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\" client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
workGroupConfigurationUpdates_enforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
workGroupConfigurationUpdates_enforceWorkGroupConfiguration = Lens.lens (\WorkGroupConfigurationUpdates' {enforceWorkGroupConfiguration} -> enforceWorkGroupConfiguration) (\s@WorkGroupConfigurationUpdates' {} a -> s {enforceWorkGroupConfiguration = a} :: WorkGroupConfigurationUpdates)

-- | If set to @true@, allows members assigned to a workgroup to specify
-- Amazon S3 Requester Pays buckets in queries. If set to @false@,
-- workgroup members cannot query data from Requester Pays buckets, and
-- queries that retrieve data from Requester Pays buckets cause an error.
-- The default is @false@. For more information about Requester Pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
-- in the /Amazon Simple Storage Service Developer Guide/.
workGroupConfigurationUpdates_requesterPaysEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
workGroupConfigurationUpdates_requesterPaysEnabled = Lens.lens (\WorkGroupConfigurationUpdates' {requesterPaysEnabled} -> requesterPaysEnabled) (\s@WorkGroupConfigurationUpdates' {} a -> s {requesterPaysEnabled = a} :: WorkGroupConfigurationUpdates)

-- | Indicates that the data usage control limit per query is removed.
-- WorkGroupConfiguration$BytesScannedCutoffPerQuery
workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe Core.Bool)
workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery = Lens.lens (\WorkGroupConfigurationUpdates' {removeBytesScannedCutoffPerQuery} -> removeBytesScannedCutoffPerQuery) (\s@WorkGroupConfigurationUpdates' {} a -> s {removeBytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)

-- | The engine version requested when a workgroup is updated. After the
-- update, all queries on the workgroup run on the requested engine
-- version. If no value was previously set, the default is Auto. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
workGroupConfigurationUpdates_engineVersion :: Lens.Lens' WorkGroupConfigurationUpdates (Core.Maybe EngineVersion)
workGroupConfigurationUpdates_engineVersion = Lens.lens (\WorkGroupConfigurationUpdates' {engineVersion} -> engineVersion) (\s@WorkGroupConfigurationUpdates' {} a -> s {engineVersion = a} :: WorkGroupConfigurationUpdates)

instance Core.Hashable WorkGroupConfigurationUpdates

instance Core.NFData WorkGroupConfigurationUpdates

instance Core.ToJSON WorkGroupConfigurationUpdates where
  toJSON WorkGroupConfigurationUpdates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BytesScannedCutoffPerQuery" Core..=)
              Core.<$> bytesScannedCutoffPerQuery,
            ("ResultConfigurationUpdates" Core..=)
              Core.<$> resultConfigurationUpdates,
            ("PublishCloudWatchMetricsEnabled" Core..=)
              Core.<$> publishCloudWatchMetricsEnabled,
            ("EnforceWorkGroupConfiguration" Core..=)
              Core.<$> enforceWorkGroupConfiguration,
            ("RequesterPaysEnabled" Core..=)
              Core.<$> requesterPaysEnabled,
            ("RemoveBytesScannedCutoffPerQuery" Core..=)
              Core.<$> removeBytesScannedCutoffPerQuery,
            ("EngineVersion" Core..=) Core.<$> engineVersion
          ]
      )
