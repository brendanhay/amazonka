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
-- Module      : Amazonka.Athena.Types.WorkGroupConfigurationUpdates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.WorkGroupConfigurationUpdates where

import Amazonka.Athena.Types.CustomerContentEncryptionConfiguration
import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.ResultConfigurationUpdates
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration information that will be updated for this workgroup,
-- which includes the location in Amazon S3 where query results are stored,
-- the encryption option, if any, used for query results, whether the
-- Amazon CloudWatch Metrics are enabled for the workgroup, whether the
-- workgroup settings override the client-side settings, and the data usage
-- limit for the amount of bytes scanned per query, if it is specified.
--
-- /See:/ 'newWorkGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { -- | Contains a user defined string in JSON format for a Spark-enabled
    -- workgroup.
    additionalConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The upper limit (cutoff) for the amount of bytes a single query in a
    -- workgroup is allowed to scan.
    bytesScannedCutoffPerQuery :: Prelude.Maybe Prelude.Natural,
    customerContentEncryptionConfiguration :: Prelude.Maybe CustomerContentEncryptionConfiguration,
    -- | If set to \"true\", the settings for the workgroup override client-side
    -- settings. If set to \"false\" client-side settings are used. For more
    -- information, see
    -- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
    enforceWorkGroupConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | The engine version requested when a workgroup is updated. After the
    -- update, all queries on the workgroup run on the requested engine
    -- version. If no value was previously set, the default is Auto. Queries on
    -- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
    -- engine regardless of this setting.
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | Contains the ARN of the execution role for the workgroup
    executionRole :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this workgroup enables publishing metrics to Amazon
    -- CloudWatch.
    publishCloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that the data usage control limit per query is removed.
    -- WorkGroupConfiguration$BytesScannedCutoffPerQuery
    removeBytesScannedCutoffPerQuery :: Prelude.Maybe Prelude.Bool,
    -- | Removes content encryption configuration for a workgroup.
    removeCustomerContentEncryptionConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | If set to @true@, allows members assigned to a workgroup to specify
    -- Amazon S3 Requester Pays buckets in queries. If set to @false@,
    -- workgroup members cannot query data from Requester Pays buckets, and
    -- queries that retrieve data from Requester Pays buckets cause an error.
    -- The default is @false@. For more information about Requester Pays
    -- buckets, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    requesterPaysEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The result configuration information about the queries in this workgroup
    -- that will be updated. Includes the updated results location and an
    -- updated option for encrypting query results.
    resultConfigurationUpdates :: Prelude.Maybe ResultConfigurationUpdates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkGroupConfigurationUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'workGroupConfigurationUpdates_additionalConfiguration' - Contains a user defined string in JSON format for a Spark-enabled
-- workgroup.
--
-- 'bytesScannedCutoffPerQuery', 'workGroupConfigurationUpdates_bytesScannedCutoffPerQuery' - The upper limit (cutoff) for the amount of bytes a single query in a
-- workgroup is allowed to scan.
--
-- 'customerContentEncryptionConfiguration', 'workGroupConfigurationUpdates_customerContentEncryptionConfiguration' - Undocumented member.
--
-- 'enforceWorkGroupConfiguration', 'workGroupConfigurationUpdates_enforceWorkGroupConfiguration' - If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\" client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
--
-- 'engineVersion', 'workGroupConfigurationUpdates_engineVersion' - The engine version requested when a workgroup is updated. After the
-- update, all queries on the workgroup run on the requested engine
-- version. If no value was previously set, the default is Auto. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
--
-- 'executionRole', 'workGroupConfigurationUpdates_executionRole' - Contains the ARN of the execution role for the workgroup
--
-- 'publishCloudWatchMetricsEnabled', 'workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled' - Indicates whether this workgroup enables publishing metrics to Amazon
-- CloudWatch.
--
-- 'removeBytesScannedCutoffPerQuery', 'workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery' - Indicates that the data usage control limit per query is removed.
-- WorkGroupConfiguration$BytesScannedCutoffPerQuery
--
-- 'removeCustomerContentEncryptionConfiguration', 'workGroupConfigurationUpdates_removeCustomerContentEncryptionConfiguration' - Removes content encryption configuration for a workgroup.
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
-- 'resultConfigurationUpdates', 'workGroupConfigurationUpdates_resultConfigurationUpdates' - The result configuration information about the queries in this workgroup
-- that will be updated. Includes the updated results location and an
-- updated option for encrypting query results.
newWorkGroupConfigurationUpdates ::
  WorkGroupConfigurationUpdates
newWorkGroupConfigurationUpdates =
  WorkGroupConfigurationUpdates'
    { additionalConfiguration =
        Prelude.Nothing,
      bytesScannedCutoffPerQuery = Prelude.Nothing,
      customerContentEncryptionConfiguration =
        Prelude.Nothing,
      enforceWorkGroupConfiguration =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      executionRole = Prelude.Nothing,
      publishCloudWatchMetricsEnabled =
        Prelude.Nothing,
      removeBytesScannedCutoffPerQuery =
        Prelude.Nothing,
      removeCustomerContentEncryptionConfiguration =
        Prelude.Nothing,
      requesterPaysEnabled = Prelude.Nothing,
      resultConfigurationUpdates = Prelude.Nothing
    }

-- | Contains a user defined string in JSON format for a Spark-enabled
-- workgroup.
workGroupConfigurationUpdates_additionalConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Text)
workGroupConfigurationUpdates_additionalConfiguration = Lens.lens (\WorkGroupConfigurationUpdates' {additionalConfiguration} -> additionalConfiguration) (\s@WorkGroupConfigurationUpdates' {} a -> s {additionalConfiguration = a} :: WorkGroupConfigurationUpdates)

-- | The upper limit (cutoff) for the amount of bytes a single query in a
-- workgroup is allowed to scan.
workGroupConfigurationUpdates_bytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Natural)
workGroupConfigurationUpdates_bytesScannedCutoffPerQuery = Lens.lens (\WorkGroupConfigurationUpdates' {bytesScannedCutoffPerQuery} -> bytesScannedCutoffPerQuery) (\s@WorkGroupConfigurationUpdates' {} a -> s {bytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)

-- | Undocumented member.
workGroupConfigurationUpdates_customerContentEncryptionConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe CustomerContentEncryptionConfiguration)
workGroupConfigurationUpdates_customerContentEncryptionConfiguration = Lens.lens (\WorkGroupConfigurationUpdates' {customerContentEncryptionConfiguration} -> customerContentEncryptionConfiguration) (\s@WorkGroupConfigurationUpdates' {} a -> s {customerContentEncryptionConfiguration = a} :: WorkGroupConfigurationUpdates)

-- | If set to \"true\", the settings for the workgroup override client-side
-- settings. If set to \"false\" client-side settings are used. For more
-- information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings>.
workGroupConfigurationUpdates_enforceWorkGroupConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Bool)
workGroupConfigurationUpdates_enforceWorkGroupConfiguration = Lens.lens (\WorkGroupConfigurationUpdates' {enforceWorkGroupConfiguration} -> enforceWorkGroupConfiguration) (\s@WorkGroupConfigurationUpdates' {} a -> s {enforceWorkGroupConfiguration = a} :: WorkGroupConfigurationUpdates)

-- | The engine version requested when a workgroup is updated. After the
-- update, all queries on the workgroup run on the requested engine
-- version. If no value was previously set, the default is Auto. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
workGroupConfigurationUpdates_engineVersion :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe EngineVersion)
workGroupConfigurationUpdates_engineVersion = Lens.lens (\WorkGroupConfigurationUpdates' {engineVersion} -> engineVersion) (\s@WorkGroupConfigurationUpdates' {} a -> s {engineVersion = a} :: WorkGroupConfigurationUpdates)

-- | Contains the ARN of the execution role for the workgroup
workGroupConfigurationUpdates_executionRole :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Text)
workGroupConfigurationUpdates_executionRole = Lens.lens (\WorkGroupConfigurationUpdates' {executionRole} -> executionRole) (\s@WorkGroupConfigurationUpdates' {} a -> s {executionRole = a} :: WorkGroupConfigurationUpdates)

-- | Indicates whether this workgroup enables publishing metrics to Amazon
-- CloudWatch.
workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Bool)
workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled = Lens.lens (\WorkGroupConfigurationUpdates' {publishCloudWatchMetricsEnabled} -> publishCloudWatchMetricsEnabled) (\s@WorkGroupConfigurationUpdates' {} a -> s {publishCloudWatchMetricsEnabled = a} :: WorkGroupConfigurationUpdates)

-- | Indicates that the data usage control limit per query is removed.
-- WorkGroupConfiguration$BytesScannedCutoffPerQuery
workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Bool)
workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery = Lens.lens (\WorkGroupConfigurationUpdates' {removeBytesScannedCutoffPerQuery} -> removeBytesScannedCutoffPerQuery) (\s@WorkGroupConfigurationUpdates' {} a -> s {removeBytesScannedCutoffPerQuery = a} :: WorkGroupConfigurationUpdates)

-- | Removes content encryption configuration for a workgroup.
workGroupConfigurationUpdates_removeCustomerContentEncryptionConfiguration :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Bool)
workGroupConfigurationUpdates_removeCustomerContentEncryptionConfiguration = Lens.lens (\WorkGroupConfigurationUpdates' {removeCustomerContentEncryptionConfiguration} -> removeCustomerContentEncryptionConfiguration) (\s@WorkGroupConfigurationUpdates' {} a -> s {removeCustomerContentEncryptionConfiguration = a} :: WorkGroupConfigurationUpdates)

-- | If set to @true@, allows members assigned to a workgroup to specify
-- Amazon S3 Requester Pays buckets in queries. If set to @false@,
-- workgroup members cannot query data from Requester Pays buckets, and
-- queries that retrieve data from Requester Pays buckets cause an error.
-- The default is @false@. For more information about Requester Pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>
-- in the /Amazon Simple Storage Service Developer Guide/.
workGroupConfigurationUpdates_requesterPaysEnabled :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe Prelude.Bool)
workGroupConfigurationUpdates_requesterPaysEnabled = Lens.lens (\WorkGroupConfigurationUpdates' {requesterPaysEnabled} -> requesterPaysEnabled) (\s@WorkGroupConfigurationUpdates' {} a -> s {requesterPaysEnabled = a} :: WorkGroupConfigurationUpdates)

-- | The result configuration information about the queries in this workgroup
-- that will be updated. Includes the updated results location and an
-- updated option for encrypting query results.
workGroupConfigurationUpdates_resultConfigurationUpdates :: Lens.Lens' WorkGroupConfigurationUpdates (Prelude.Maybe ResultConfigurationUpdates)
workGroupConfigurationUpdates_resultConfigurationUpdates = Lens.lens (\WorkGroupConfigurationUpdates' {resultConfigurationUpdates} -> resultConfigurationUpdates) (\s@WorkGroupConfigurationUpdates' {} a -> s {resultConfigurationUpdates = a} :: WorkGroupConfigurationUpdates)

instance
  Prelude.Hashable
    WorkGroupConfigurationUpdates
  where
  hashWithSalt _salt WorkGroupConfigurationUpdates' {..} =
    _salt
      `Prelude.hashWithSalt` additionalConfiguration
      `Prelude.hashWithSalt` bytesScannedCutoffPerQuery
      `Prelude.hashWithSalt` customerContentEncryptionConfiguration
      `Prelude.hashWithSalt` enforceWorkGroupConfiguration
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` publishCloudWatchMetricsEnabled
      `Prelude.hashWithSalt` removeBytesScannedCutoffPerQuery
      `Prelude.hashWithSalt` removeCustomerContentEncryptionConfiguration
      `Prelude.hashWithSalt` requesterPaysEnabled
      `Prelude.hashWithSalt` resultConfigurationUpdates

instance Prelude.NFData WorkGroupConfigurationUpdates where
  rnf WorkGroupConfigurationUpdates' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf bytesScannedCutoffPerQuery
      `Prelude.seq` Prelude.rnf customerContentEncryptionConfiguration
      `Prelude.seq` Prelude.rnf enforceWorkGroupConfiguration
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf publishCloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf removeBytesScannedCutoffPerQuery
      `Prelude.seq` Prelude.rnf
        removeCustomerContentEncryptionConfiguration
      `Prelude.seq` Prelude.rnf requesterPaysEnabled
      `Prelude.seq` Prelude.rnf resultConfigurationUpdates

instance Data.ToJSON WorkGroupConfigurationUpdates where
  toJSON WorkGroupConfigurationUpdates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalConfiguration" Data..=)
              Prelude.<$> additionalConfiguration,
            ("BytesScannedCutoffPerQuery" Data..=)
              Prelude.<$> bytesScannedCutoffPerQuery,
            ("CustomerContentEncryptionConfiguration" Data..=)
              Prelude.<$> customerContentEncryptionConfiguration,
            ("EnforceWorkGroupConfiguration" Data..=)
              Prelude.<$> enforceWorkGroupConfiguration,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("ExecutionRole" Data..=) Prelude.<$> executionRole,
            ("PublishCloudWatchMetricsEnabled" Data..=)
              Prelude.<$> publishCloudWatchMetricsEnabled,
            ("RemoveBytesScannedCutoffPerQuery" Data..=)
              Prelude.<$> removeBytesScannedCutoffPerQuery,
            ( "RemoveCustomerContentEncryptionConfiguration"
                Data..=
            )
              Prelude.<$> removeCustomerContentEncryptionConfiguration,
            ("RequesterPaysEnabled" Data..=)
              Prelude.<$> requesterPaysEnabled,
            ("ResultConfigurationUpdates" Data..=)
              Prelude.<$> resultConfigurationUpdates
          ]
      )
