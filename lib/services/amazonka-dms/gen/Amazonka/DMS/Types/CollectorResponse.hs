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
-- Module      : Amazonka.DMS.Types.CollectorResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.CollectorResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.CollectorHealthCheck
import Amazonka.DMS.Types.InventoryData
import Amazonka.DMS.Types.VersionStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a Fleet Advisor collector.
--
-- /See:/ 'newCollectorResponse' smart constructor.
data CollectorResponse = CollectorResponse'
  { collectorHealthCheck :: Prelude.Maybe CollectorHealthCheck,
    -- | The name of the Fleet Advisor collector .
    collectorName :: Prelude.Maybe Prelude.Text,
    -- | The reference ID of the Fleet Advisor collector.
    collectorReferencedId :: Prelude.Maybe Prelude.Text,
    -- | The version of your Fleet Advisor collector, in semantic versioning
    -- format, for example @1.0.2@
    collectorVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when you created the collector, in the following format:
    -- @2022-01-24T19:04:02.596113Z@
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | A summary description of the Fleet Advisor collector.
    description :: Prelude.Maybe Prelude.Text,
    inventoryData :: Prelude.Maybe InventoryData,
    -- | The timestamp of the last time the collector received data, in the
    -- following format: @2022-01-24T19:04:02.596113Z@
    lastDataReceived :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when DMS last modified the collector, in the following
    -- format: @2022-01-24T19:04:02.596113Z@
    modifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when DMS registered the collector, in the following
    -- format: @2022-01-24T19:04:02.596113Z@
    registeredDate :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that the Fleet Advisor collector uses to store
    -- inventory metadata.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that grants permissions to access the specified Amazon S3
    -- bucket.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the collector version is up to date.
    versionStatus :: Prelude.Maybe VersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorHealthCheck', 'collectorResponse_collectorHealthCheck' - Undocumented member.
--
-- 'collectorName', 'collectorResponse_collectorName' - The name of the Fleet Advisor collector .
--
-- 'collectorReferencedId', 'collectorResponse_collectorReferencedId' - The reference ID of the Fleet Advisor collector.
--
-- 'collectorVersion', 'collectorResponse_collectorVersion' - The version of your Fleet Advisor collector, in semantic versioning
-- format, for example @1.0.2@
--
-- 'createdDate', 'collectorResponse_createdDate' - The timestamp when you created the collector, in the following format:
-- @2022-01-24T19:04:02.596113Z@
--
-- 'description', 'collectorResponse_description' - A summary description of the Fleet Advisor collector.
--
-- 'inventoryData', 'collectorResponse_inventoryData' - Undocumented member.
--
-- 'lastDataReceived', 'collectorResponse_lastDataReceived' - The timestamp of the last time the collector received data, in the
-- following format: @2022-01-24T19:04:02.596113Z@
--
-- 'modifiedDate', 'collectorResponse_modifiedDate' - The timestamp when DMS last modified the collector, in the following
-- format: @2022-01-24T19:04:02.596113Z@
--
-- 'registeredDate', 'collectorResponse_registeredDate' - The timestamp when DMS registered the collector, in the following
-- format: @2022-01-24T19:04:02.596113Z@
--
-- 's3BucketName', 'collectorResponse_s3BucketName' - The Amazon S3 bucket that the Fleet Advisor collector uses to store
-- inventory metadata.
--
-- 'serviceAccessRoleArn', 'collectorResponse_serviceAccessRoleArn' - The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
--
-- 'versionStatus', 'collectorResponse_versionStatus' - Whether the collector version is up to date.
newCollectorResponse ::
  CollectorResponse
newCollectorResponse =
  CollectorResponse'
    { collectorHealthCheck =
        Prelude.Nothing,
      collectorName = Prelude.Nothing,
      collectorReferencedId = Prelude.Nothing,
      collectorVersion = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      inventoryData = Prelude.Nothing,
      lastDataReceived = Prelude.Nothing,
      modifiedDate = Prelude.Nothing,
      registeredDate = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      versionStatus = Prelude.Nothing
    }

-- | Undocumented member.
collectorResponse_collectorHealthCheck :: Lens.Lens' CollectorResponse (Prelude.Maybe CollectorHealthCheck)
collectorResponse_collectorHealthCheck = Lens.lens (\CollectorResponse' {collectorHealthCheck} -> collectorHealthCheck) (\s@CollectorResponse' {} a -> s {collectorHealthCheck = a} :: CollectorResponse)

-- | The name of the Fleet Advisor collector .
collectorResponse_collectorName :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_collectorName = Lens.lens (\CollectorResponse' {collectorName} -> collectorName) (\s@CollectorResponse' {} a -> s {collectorName = a} :: CollectorResponse)

-- | The reference ID of the Fleet Advisor collector.
collectorResponse_collectorReferencedId :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_collectorReferencedId = Lens.lens (\CollectorResponse' {collectorReferencedId} -> collectorReferencedId) (\s@CollectorResponse' {} a -> s {collectorReferencedId = a} :: CollectorResponse)

-- | The version of your Fleet Advisor collector, in semantic versioning
-- format, for example @1.0.2@
collectorResponse_collectorVersion :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_collectorVersion = Lens.lens (\CollectorResponse' {collectorVersion} -> collectorVersion) (\s@CollectorResponse' {} a -> s {collectorVersion = a} :: CollectorResponse)

-- | The timestamp when you created the collector, in the following format:
-- @2022-01-24T19:04:02.596113Z@
collectorResponse_createdDate :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_createdDate = Lens.lens (\CollectorResponse' {createdDate} -> createdDate) (\s@CollectorResponse' {} a -> s {createdDate = a} :: CollectorResponse)

-- | A summary description of the Fleet Advisor collector.
collectorResponse_description :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_description = Lens.lens (\CollectorResponse' {description} -> description) (\s@CollectorResponse' {} a -> s {description = a} :: CollectorResponse)

-- | Undocumented member.
collectorResponse_inventoryData :: Lens.Lens' CollectorResponse (Prelude.Maybe InventoryData)
collectorResponse_inventoryData = Lens.lens (\CollectorResponse' {inventoryData} -> inventoryData) (\s@CollectorResponse' {} a -> s {inventoryData = a} :: CollectorResponse)

-- | The timestamp of the last time the collector received data, in the
-- following format: @2022-01-24T19:04:02.596113Z@
collectorResponse_lastDataReceived :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_lastDataReceived = Lens.lens (\CollectorResponse' {lastDataReceived} -> lastDataReceived) (\s@CollectorResponse' {} a -> s {lastDataReceived = a} :: CollectorResponse)

-- | The timestamp when DMS last modified the collector, in the following
-- format: @2022-01-24T19:04:02.596113Z@
collectorResponse_modifiedDate :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_modifiedDate = Lens.lens (\CollectorResponse' {modifiedDate} -> modifiedDate) (\s@CollectorResponse' {} a -> s {modifiedDate = a} :: CollectorResponse)

-- | The timestamp when DMS registered the collector, in the following
-- format: @2022-01-24T19:04:02.596113Z@
collectorResponse_registeredDate :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_registeredDate = Lens.lens (\CollectorResponse' {registeredDate} -> registeredDate) (\s@CollectorResponse' {} a -> s {registeredDate = a} :: CollectorResponse)

-- | The Amazon S3 bucket that the Fleet Advisor collector uses to store
-- inventory metadata.
collectorResponse_s3BucketName :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_s3BucketName = Lens.lens (\CollectorResponse' {s3BucketName} -> s3BucketName) (\s@CollectorResponse' {} a -> s {s3BucketName = a} :: CollectorResponse)

-- | The IAM role that grants permissions to access the specified Amazon S3
-- bucket.
collectorResponse_serviceAccessRoleArn :: Lens.Lens' CollectorResponse (Prelude.Maybe Prelude.Text)
collectorResponse_serviceAccessRoleArn = Lens.lens (\CollectorResponse' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@CollectorResponse' {} a -> s {serviceAccessRoleArn = a} :: CollectorResponse)

-- | Whether the collector version is up to date.
collectorResponse_versionStatus :: Lens.Lens' CollectorResponse (Prelude.Maybe VersionStatus)
collectorResponse_versionStatus = Lens.lens (\CollectorResponse' {versionStatus} -> versionStatus) (\s@CollectorResponse' {} a -> s {versionStatus = a} :: CollectorResponse)

instance Data.FromJSON CollectorResponse where
  parseJSON =
    Data.withObject
      "CollectorResponse"
      ( \x ->
          CollectorResponse'
            Prelude.<$> (x Data..:? "CollectorHealthCheck")
            Prelude.<*> (x Data..:? "CollectorName")
            Prelude.<*> (x Data..:? "CollectorReferencedId")
            Prelude.<*> (x Data..:? "CollectorVersion")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "InventoryData")
            Prelude.<*> (x Data..:? "LastDataReceived")
            Prelude.<*> (x Data..:? "ModifiedDate")
            Prelude.<*> (x Data..:? "RegisteredDate")
            Prelude.<*> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "VersionStatus")
      )

instance Prelude.Hashable CollectorResponse where
  hashWithSalt _salt CollectorResponse' {..} =
    _salt
      `Prelude.hashWithSalt` collectorHealthCheck
      `Prelude.hashWithSalt` collectorName
      `Prelude.hashWithSalt` collectorReferencedId
      `Prelude.hashWithSalt` collectorVersion
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inventoryData
      `Prelude.hashWithSalt` lastDataReceived
      `Prelude.hashWithSalt` modifiedDate
      `Prelude.hashWithSalt` registeredDate
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` versionStatus

instance Prelude.NFData CollectorResponse where
  rnf CollectorResponse' {..} =
    Prelude.rnf collectorHealthCheck
      `Prelude.seq` Prelude.rnf collectorName
      `Prelude.seq` Prelude.rnf collectorReferencedId
      `Prelude.seq` Prelude.rnf collectorVersion
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf inventoryData
      `Prelude.seq` Prelude.rnf lastDataReceived
      `Prelude.seq` Prelude.rnf modifiedDate
      `Prelude.seq` Prelude.rnf registeredDate
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf versionStatus
