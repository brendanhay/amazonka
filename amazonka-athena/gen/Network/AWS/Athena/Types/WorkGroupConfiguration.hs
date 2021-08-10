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
-- Module      : Network.AWS.Athena.Types.WorkGroupConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupConfiguration where

import Network.AWS.Athena.Types.EngineVersion
import Network.AWS.Athena.Types.ResultConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration of the workgroup, which includes the location in
-- Amazon S3 where query results are stored, the encryption option, if any,
-- used for query results, whether the Amazon CloudWatch Metrics are
-- enabled for the workgroup and whether workgroup settings override query
-- settings, and the data usage limits for the amount of data scanned per
-- query or per workgroup. The workgroup settings override is specified in
-- EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- /See:/ 'newWorkGroupConfiguration' smart constructor.
data WorkGroupConfiguration = WorkGroupConfiguration'
  { -- | The upper data usage limit (cutoff) for the amount of bytes a single
    -- query in a workgroup is allowed to scan.
    bytesScannedCutoffPerQuery :: Prelude.Maybe Prelude.Natural,
    -- | Indicates that the Amazon CloudWatch metrics are enabled for the
    -- workgroup.
    publishCloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | If set to \"true\", the settings for the workgroup override client-side
    -- settings. If set to \"false\", client-side settings are used. For more
    -- information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    enforceWorkGroupConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | If set to @true@, allows members assigned to a workgroup to reference
    -- Amazon S3 Requester Pays buckets in queries. If set to @false@,
    -- workgroup members cannot query data from Requester Pays buckets, and
    -- queries that retrieve data from Requester Pays buckets cause an error.
    -- The default is @false@. For more information about Requester Pays
    -- buckets, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    requesterPaysEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The engine version that all queries running on the workgroup use.
    -- Queries on the @AmazonAthenaPreviewFunctionality@ workgroup run on the
    -- preview engine regardless of this setting.
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | The configuration for the workgroup, which includes the location in
    -- Amazon S3 where query results are stored and the encryption option, if
    -- any, used for query results. To run the query, you must specify the
    -- query results location using one of the ways: either in the workgroup
    -- using this setting, or for individual queries (client-side), using
    -- ResultConfiguration$OutputLocation. If none of them is set, Athena
    -- issues an error that no output location is provided. For more
    -- information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
    resultConfiguration :: Prelude.Maybe ResultConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesScannedCutoffPerQuery', 'workGroupConfiguration_bytesScannedCutoffPerQuery' - The upper data usage limit (cutoff) for the amount of bytes a single
-- query in a workgroup is allowed to scan.
--
-- 'publishCloudWatchMetricsEnabled', 'workGroupConfiguration_publishCloudWatchMetricsEnabled' - Indicates that the Amazon CloudWatch metrics are enabled for the
-- workgroup.
--
-- 'enforceWorkGroupConfiguration', 'workGroupConfiguration_enforceWorkGroupConfiguration' - If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\", client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'requesterPaysEnabled', 'workGroupConfiguration_requesterPaysEnabled' - If set to @true@, allows members assigned to a workgroup to reference
-- Amazon S3 Requester Pays buckets in queries. If set to @false@,
-- workgroup members cannot query data from Requester Pays buckets, and
-- queries that retrieve data from Requester Pays buckets cause an error.
-- The default is @false@. For more information about Requester Pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'engineVersion', 'workGroupConfiguration_engineVersion' - The engine version that all queries running on the workgroup use.
-- Queries on the @AmazonAthenaPreviewFunctionality@ workgroup run on the
-- preview engine regardless of this setting.
--
-- 'resultConfiguration', 'workGroupConfiguration_resultConfiguration' - The configuration for the workgroup, which includes the location in
-- Amazon S3 where query results are stored and the encryption option, if
-- any, used for query results. To run the query, you must specify the
-- query results location using one of the ways: either in the workgroup
-- using this setting, or for individual queries (client-side), using
-- ResultConfiguration$OutputLocation. If none of them is set, Athena
-- issues an error that no output location is provided. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
newWorkGroupConfiguration ::
  WorkGroupConfiguration
newWorkGroupConfiguration =
  WorkGroupConfiguration'
    { bytesScannedCutoffPerQuery =
        Prelude.Nothing,
      publishCloudWatchMetricsEnabled = Prelude.Nothing,
      enforceWorkGroupConfiguration = Prelude.Nothing,
      requesterPaysEnabled = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      resultConfiguration = Prelude.Nothing
    }

-- | The upper data usage limit (cutoff) for the amount of bytes a single
-- query in a workgroup is allowed to scan.
workGroupConfiguration_bytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe Prelude.Natural)
workGroupConfiguration_bytesScannedCutoffPerQuery = Lens.lens (\WorkGroupConfiguration' {bytesScannedCutoffPerQuery} -> bytesScannedCutoffPerQuery) (\s@WorkGroupConfiguration' {} a -> s {bytesScannedCutoffPerQuery = a} :: WorkGroupConfiguration)

-- | Indicates that the Amazon CloudWatch metrics are enabled for the
-- workgroup.
workGroupConfiguration_publishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe Prelude.Bool)
workGroupConfiguration_publishCloudWatchMetricsEnabled = Lens.lens (\WorkGroupConfiguration' {publishCloudWatchMetricsEnabled} -> publishCloudWatchMetricsEnabled) (\s@WorkGroupConfiguration' {} a -> s {publishCloudWatchMetricsEnabled = a} :: WorkGroupConfiguration)

-- | If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\", client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
workGroupConfiguration_enforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe Prelude.Bool)
workGroupConfiguration_enforceWorkGroupConfiguration = Lens.lens (\WorkGroupConfiguration' {enforceWorkGroupConfiguration} -> enforceWorkGroupConfiguration) (\s@WorkGroupConfiguration' {} a -> s {enforceWorkGroupConfiguration = a} :: WorkGroupConfiguration)

-- | If set to @true@, allows members assigned to a workgroup to reference
-- Amazon S3 Requester Pays buckets in queries. If set to @false@,
-- workgroup members cannot query data from Requester Pays buckets, and
-- queries that retrieve data from Requester Pays buckets cause an error.
-- The default is @false@. For more information about Requester Pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
-- in the /Amazon Simple Storage Service Developer Guide/.
workGroupConfiguration_requesterPaysEnabled :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe Prelude.Bool)
workGroupConfiguration_requesterPaysEnabled = Lens.lens (\WorkGroupConfiguration' {requesterPaysEnabled} -> requesterPaysEnabled) (\s@WorkGroupConfiguration' {} a -> s {requesterPaysEnabled = a} :: WorkGroupConfiguration)

-- | The engine version that all queries running on the workgroup use.
-- Queries on the @AmazonAthenaPreviewFunctionality@ workgroup run on the
-- preview engine regardless of this setting.
workGroupConfiguration_engineVersion :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe EngineVersion)
workGroupConfiguration_engineVersion = Lens.lens (\WorkGroupConfiguration' {engineVersion} -> engineVersion) (\s@WorkGroupConfiguration' {} a -> s {engineVersion = a} :: WorkGroupConfiguration)

-- | The configuration for the workgroup, which includes the location in
-- Amazon S3 where query results are stored and the encryption option, if
-- any, used for query results. To run the query, you must specify the
-- query results location using one of the ways: either in the workgroup
-- using this setting, or for individual queries (client-side), using
-- ResultConfiguration$OutputLocation. If none of them is set, Athena
-- issues an error that no output location is provided. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>.
workGroupConfiguration_resultConfiguration :: Lens.Lens' WorkGroupConfiguration (Prelude.Maybe ResultConfiguration)
workGroupConfiguration_resultConfiguration = Lens.lens (\WorkGroupConfiguration' {resultConfiguration} -> resultConfiguration) (\s@WorkGroupConfiguration' {} a -> s {resultConfiguration = a} :: WorkGroupConfiguration)

instance Core.FromJSON WorkGroupConfiguration where
  parseJSON =
    Core.withObject
      "WorkGroupConfiguration"
      ( \x ->
          WorkGroupConfiguration'
            Prelude.<$> (x Core..:? "BytesScannedCutoffPerQuery")
            Prelude.<*> (x Core..:? "PublishCloudWatchMetricsEnabled")
            Prelude.<*> (x Core..:? "EnforceWorkGroupConfiguration")
            Prelude.<*> (x Core..:? "RequesterPaysEnabled")
            Prelude.<*> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "ResultConfiguration")
      )

instance Prelude.Hashable WorkGroupConfiguration

instance Prelude.NFData WorkGroupConfiguration

instance Core.ToJSON WorkGroupConfiguration where
  toJSON WorkGroupConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BytesScannedCutoffPerQuery" Core..=)
              Prelude.<$> bytesScannedCutoffPerQuery,
            ("PublishCloudWatchMetricsEnabled" Core..=)
              Prelude.<$> publishCloudWatchMetricsEnabled,
            ("EnforceWorkGroupConfiguration" Core..=)
              Prelude.<$> enforceWorkGroupConfiguration,
            ("RequesterPaysEnabled" Core..=)
              Prelude.<$> requesterPaysEnabled,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("ResultConfiguration" Core..=)
              Prelude.<$> resultConfiguration
          ]
      )
