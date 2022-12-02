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
-- Module      : Amazonka.IoTAnalytics.Types.Datastore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Datastore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DatastorePartitions
import Amazonka.IoTAnalytics.Types.DatastoreStatus
import Amazonka.IoTAnalytics.Types.DatastoreStorage
import Amazonka.IoTAnalytics.Types.FileFormatConfiguration
import Amazonka.IoTAnalytics.Types.RetentionPeriod
import qualified Amazonka.Prelude as Prelude

-- | Information about a data store.
--
-- /See:/ 'newDatastore' smart constructor.
data Datastore = Datastore'
  { -- | The name of the data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the data store.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Where data in a data store is stored.. You can choose @serviceManagedS3@
    -- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
    -- storage. The default is @serviceManagedS3@. You can\'t change the choice
    -- of Amazon S3 storage after your data store is created.
    storage :: Prelude.Maybe DatastoreStorage,
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
    -- | Contains the configuration information of file formats. IoT Analytics
    -- data stores support JSON and <https://parquet.apache.org/ Parquet>.
    --
    -- The default file format is JSON. You can specify only one format.
    --
    -- You can\'t change the file format after you create the data store.
    fileFormatConfiguration :: Prelude.Maybe FileFormatConfiguration,
    -- | How long, in days, message data is kept for the data store. When
    -- @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | When the data store was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the data store was updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The last time when a new message arrived in the data store.
    --
    -- IoT Analytics updates this value at most once per minute for Amazon
    -- Simple Storage Service one data store. Hence, the
    -- @lastMessageArrivalTime@ value is an approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Prelude.Maybe Data.POSIX,
    -- | Contains information about the partition dimensions in a data store.
    datastorePartitions :: Prelude.Maybe DatastorePartitions
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
-- 'name', 'datastore_name' - The name of the data store.
--
-- 'arn', 'datastore_arn' - The ARN of the data store.
--
-- 'storage', 'datastore_storage' - Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
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
-- 'fileFormatConfiguration', 'datastore_fileFormatConfiguration' - Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- 'retentionPeriod', 'datastore_retentionPeriod' - How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- 'creationTime', 'datastore_creationTime' - When the data store was created.
--
-- 'lastUpdateTime', 'datastore_lastUpdateTime' - The last time the data store was updated.
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
-- 'datastorePartitions', 'datastore_datastorePartitions' - Contains information about the partition dimensions in a data store.
newDatastore ::
  Datastore
newDatastore =
  Datastore'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      storage = Prelude.Nothing,
      status = Prelude.Nothing,
      fileFormatConfiguration = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      lastMessageArrivalTime = Prelude.Nothing,
      datastorePartitions = Prelude.Nothing
    }

-- | The name of the data store.
datastore_name :: Lens.Lens' Datastore (Prelude.Maybe Prelude.Text)
datastore_name = Lens.lens (\Datastore' {name} -> name) (\s@Datastore' {} a -> s {name = a} :: Datastore)

-- | The ARN of the data store.
datastore_arn :: Lens.Lens' Datastore (Prelude.Maybe Prelude.Text)
datastore_arn = Lens.lens (\Datastore' {arn} -> arn) (\s@Datastore' {} a -> s {arn = a} :: Datastore)

-- | Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
datastore_storage :: Lens.Lens' Datastore (Prelude.Maybe DatastoreStorage)
datastore_storage = Lens.lens (\Datastore' {storage} -> storage) (\s@Datastore' {} a -> s {storage = a} :: Datastore)

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

-- | Contains the configuration information of file formats. IoT Analytics
-- data stores support JSON and <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
datastore_fileFormatConfiguration :: Lens.Lens' Datastore (Prelude.Maybe FileFormatConfiguration)
datastore_fileFormatConfiguration = Lens.lens (\Datastore' {fileFormatConfiguration} -> fileFormatConfiguration) (\s@Datastore' {} a -> s {fileFormatConfiguration = a} :: Datastore)

-- | How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
datastore_retentionPeriod :: Lens.Lens' Datastore (Prelude.Maybe RetentionPeriod)
datastore_retentionPeriod = Lens.lens (\Datastore' {retentionPeriod} -> retentionPeriod) (\s@Datastore' {} a -> s {retentionPeriod = a} :: Datastore)

-- | When the data store was created.
datastore_creationTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_creationTime = Lens.lens (\Datastore' {creationTime} -> creationTime) (\s@Datastore' {} a -> s {creationTime = a} :: Datastore) Prelude.. Lens.mapping Data._Time

-- | The last time the data store was updated.
datastore_lastUpdateTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_lastUpdateTime = Lens.lens (\Datastore' {lastUpdateTime} -> lastUpdateTime) (\s@Datastore' {} a -> s {lastUpdateTime = a} :: Datastore) Prelude.. Lens.mapping Data._Time

-- | The last time when a new message arrived in the data store.
--
-- IoT Analytics updates this value at most once per minute for Amazon
-- Simple Storage Service one data store. Hence, the
-- @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
datastore_lastMessageArrivalTime :: Lens.Lens' Datastore (Prelude.Maybe Prelude.UTCTime)
datastore_lastMessageArrivalTime = Lens.lens (\Datastore' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Datastore' {} a -> s {lastMessageArrivalTime = a} :: Datastore) Prelude.. Lens.mapping Data._Time

-- | Contains information about the partition dimensions in a data store.
datastore_datastorePartitions :: Lens.Lens' Datastore (Prelude.Maybe DatastorePartitions)
datastore_datastorePartitions = Lens.lens (\Datastore' {datastorePartitions} -> datastorePartitions) (\s@Datastore' {} a -> s {datastorePartitions = a} :: Datastore)

instance Data.FromJSON Datastore where
  parseJSON =
    Data.withObject
      "Datastore"
      ( \x ->
          Datastore'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "storage")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "fileFormatConfiguration")
            Prelude.<*> (x Data..:? "retentionPeriod")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "lastMessageArrivalTime")
            Prelude.<*> (x Data..:? "datastorePartitions")
      )

instance Prelude.Hashable Datastore where
  hashWithSalt _salt Datastore' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` storage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fileFormatConfiguration
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` lastMessageArrivalTime
      `Prelude.hashWithSalt` datastorePartitions

instance Prelude.NFData Datastore where
  rnf Datastore' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf storage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fileFormatConfiguration
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf lastMessageArrivalTime
      `Prelude.seq` Prelude.rnf datastorePartitions
