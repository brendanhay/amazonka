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
-- Module      : Network.AWS.IoTAnalytics.Types.Datastore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Datastore where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatastorePartitions
import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.FileFormatConfiguration
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a data store.
--
-- /See:/ 'newDatastore' smart constructor.
data Datastore = Datastore'
  { -- | The last time when a new message arrived in the data store.
    --
    -- IoT Analytics updates this value at most once per minute for Amazon
    -- Simple Storage Service one data store. Hence, the
    -- @lastMessageArrivalTime@ value is an approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Prelude.Maybe Core.POSIX,
    -- | The status of a data store:
    --
    -- [CREATING]
    --     The data store is being created.
    --
    -- [ACTIVE]
    --     The data store has been created and can be used.
    --
    -- [DELETING]
    --     The data store is being deleted.
    status :: Prelude.Maybe DatastoreStatus,
    -- | When the data store was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Contains information about the partition dimensions in a data store.
    datastorePartitions :: Prelude.Maybe DatastorePartitions,
    -- | The last time the data store was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | Contains the configuration information of file formats. IoT Analytics
    -- data stores support JSON and <https://parquet.apache.org/ Parquet>.
    --
    -- The default file format is JSON. You can specify only one format.
    --
    -- You can\'t change the file format after you create the data store.
    fileFormatConfiguration :: Prelude.Maybe FileFormatConfiguration,
    -- | The ARN of the data store.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, message data is kept for the data store. When
    -- @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | Where data in a data store is stored.. You can choose @serviceManagedS3@
    -- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
    -- storage. The default is @serviceManagedS3@. You can\'t change the choice
    -- of Amazon S3 storage after your data store is created.
    storage :: Prelude.Maybe DatastoreStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Datastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastMessageArrivalTime', 'datastore_lastMessageArrivalTime' - The last time when a new message arrived in the data store.
--
-- IoT Analytics updates this value at most once per minute for Amazon
-- Simple Storage Service one data store. Hence, the
-- @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
--
-- 'status', 'datastore_status' - The status of a data store:
--
-- [CREATING]
--     The data store is being created.
--
-- [ACTIVE]
--     The data store has been created and can be used.
--
-- [DELETING]
--     The data store is being deleted.
--
-- 'creationTime', 'datastore_creationTime' - When the data store was created.
--
-- 'datastorePartitions', 'datastore_datastorePartitions' - Contains information about the partition dimensions in a data store.
--
-- 'lastUpdateTime', 'datastore_lastUpdateTime' - The last time the data store was updated.
--
-- 'fileFormatConfiguration', 'datastore_fileFormatConfiguration' - Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- 'arn', 'datastore_arn' - The ARN of the data store.
--
-- 'name', 'datastore_name' - The name of the data store.
--
-- 'retentionPeriod', 'datastore_retentionPeriod' - How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- 'storage', 'datastore_storage' - Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
newDatastore ::
  Datastore
newDatastore =
  Datastore'
    { lastMessageArrivalTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      datastorePartitions = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      fileFormatConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      storage = Prelude.Nothing
    }

-- | The last time when a new message arrived in the data store.
--
-- IoT Analytics updates this value at most once per minute for Amazon
-- Simple Storage Service one data store. Hence, the
-- @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
datastore_lastMessageArrivalTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_lastMessageArrivalTime = Lens.lens (\Datastore' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Datastore' {} a -> s {lastMessageArrivalTime = a} :: Datastore) Prelude.. Lens.mapping Core._Time

-- | The status of a data store:
--
-- [CREATING]
--     The data store is being created.
--
-- [ACTIVE]
--     The data store has been created and can be used.
--
-- [DELETING]
--     The data store is being deleted.
datastore_status :: Lens.Lens' Datastore (Prelude.Maybe DatastoreStatus)
datastore_status = Lens.lens (\Datastore' {status} -> status) (\s@Datastore' {} a -> s {status = a} :: Datastore)

-- | When the data store was created.
datastore_creationTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_creationTime = Lens.lens (\Datastore' {creationTime} -> creationTime) (\s@Datastore' {} a -> s {creationTime = a} :: Datastore) Prelude.. Lens.mapping Core._Time

-- | Contains information about the partition dimensions in a data store.
datastore_datastorePartitions :: Lens.Lens' Datastore (Prelude.Maybe DatastorePartitions)
datastore_datastorePartitions = Lens.lens (\Datastore' {datastorePartitions} -> datastorePartitions) (\s@Datastore' {} a -> s {datastorePartitions = a} :: Datastore)

-- | The last time the data store was updated.
datastore_lastUpdateTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_lastUpdateTime = Lens.lens (\Datastore' {lastUpdateTime} -> lastUpdateTime) (\s@Datastore' {} a -> s {lastUpdateTime = a} :: Datastore) Prelude.. Lens.mapping Core._Time

-- | Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
datastore_fileFormatConfiguration :: Lens.Lens' Datastore (Prelude.Maybe FileFormatConfiguration)
datastore_fileFormatConfiguration = Lens.lens (\Datastore' {fileFormatConfiguration} -> fileFormatConfiguration) (\s@Datastore' {} a -> s {fileFormatConfiguration = a} :: Datastore)

-- | The ARN of the data store.
datastore_arn :: Lens.Lens' Datastore (Prelude.Maybe Prelude.Text)
datastore_arn = Lens.lens (\Datastore' {arn} -> arn) (\s@Datastore' {} a -> s {arn = a} :: Datastore)

-- | The name of the data store.
datastore_name :: Lens.Lens' Datastore (Prelude.Maybe Prelude.Text)
datastore_name = Lens.lens (\Datastore' {name} -> name) (\s@Datastore' {} a -> s {name = a} :: Datastore)

-- | How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
datastore_retentionPeriod :: Lens.Lens' Datastore (Prelude.Maybe RetentionPeriod)
datastore_retentionPeriod = Lens.lens (\Datastore' {retentionPeriod} -> retentionPeriod) (\s@Datastore' {} a -> s {retentionPeriod = a} :: Datastore)

-- | Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
datastore_storage :: Lens.Lens' Datastore (Prelude.Maybe DatastoreStorage)
datastore_storage = Lens.lens (\Datastore' {storage} -> storage) (\s@Datastore' {} a -> s {storage = a} :: Datastore)

instance Core.FromJSON Datastore where
  parseJSON =
    Core.withObject
      "Datastore"
      ( \x ->
          Datastore'
            Prelude.<$> (x Core..:? "lastMessageArrivalTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "datastorePartitions")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "fileFormatConfiguration")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "retentionPeriod")
            Prelude.<*> (x Core..:? "storage")
      )

instance Prelude.Hashable Datastore

instance Prelude.NFData Datastore
