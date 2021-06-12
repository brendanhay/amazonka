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
import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.FileFormatConfiguration
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens

-- | Information about a data store.
--
-- /See:/ 'newDatastore' smart constructor.
data Datastore = Datastore'
  { -- | The last time when a new message arrived in the data store.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one
    -- data store. Hence, the @lastMessageArrivalTime@ value is an
    -- approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Core.Maybe Core.POSIX,
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
    status :: Core.Maybe DatastoreStatus,
    -- | When the data store was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The last time the data store was updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the data store.
    arn :: Core.Maybe Core.Text,
    -- | Contains the configuration information of file formats. AWS IoT
    -- Analytics data stores support JSON and
    -- <https://parquet.apache.org/ Parquet>.
    --
    -- The default file format is JSON. You can specify only one format.
    --
    -- You can\'t change the file format after you create the data store.
    fileFormatConfiguration :: Core.Maybe FileFormatConfiguration,
    -- | The name of the data store.
    name :: Core.Maybe Core.Text,
    -- | How long, in days, message data is kept for the data store. When
    -- @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Core.Maybe RetentionPeriod,
    -- | Where data store data is stored. You can choose one of
    -- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
    -- default is @serviceManagedS3@. You cannot change this storage option
    -- after the data store is created.
    storage :: Core.Maybe DatastoreStorage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- AWS IoT Analytics updates this value at most once per minute for one
-- data store. Hence, the @lastMessageArrivalTime@ value is an
-- approximation.
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
-- 'lastUpdateTime', 'datastore_lastUpdateTime' - The last time the data store was updated.
--
-- 'arn', 'datastore_arn' - The ARN of the data store.
--
-- 'fileFormatConfiguration', 'datastore_fileFormatConfiguration' - Contains the configuration information of file formats. AWS IoT
-- Analytics data stores support JSON and
-- <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- 'name', 'datastore_name' - The name of the data store.
--
-- 'retentionPeriod', 'datastore_retentionPeriod' - How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- 'storage', 'datastore_storage' - Where data store data is stored. You can choose one of
-- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
-- default is @serviceManagedS3@. You cannot change this storage option
-- after the data store is created.
newDatastore ::
  Datastore
newDatastore =
  Datastore'
    { lastMessageArrivalTime = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      arn = Core.Nothing,
      fileFormatConfiguration = Core.Nothing,
      name = Core.Nothing,
      retentionPeriod = Core.Nothing,
      storage = Core.Nothing
    }

-- | The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- data store. Hence, the @lastMessageArrivalTime@ value is an
-- approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
datastore_lastMessageArrivalTime :: Lens.Lens' Datastore (Core.Maybe Core.UTCTime)
datastore_lastMessageArrivalTime = Lens.lens (\Datastore' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Datastore' {} a -> s {lastMessageArrivalTime = a} :: Datastore) Core.. Lens.mapping Core._Time

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
datastore_status :: Lens.Lens' Datastore (Core.Maybe DatastoreStatus)
datastore_status = Lens.lens (\Datastore' {status} -> status) (\s@Datastore' {} a -> s {status = a} :: Datastore)

-- | When the data store was created.
datastore_creationTime :: Lens.Lens' Datastore (Core.Maybe Core.UTCTime)
datastore_creationTime = Lens.lens (\Datastore' {creationTime} -> creationTime) (\s@Datastore' {} a -> s {creationTime = a} :: Datastore) Core.. Lens.mapping Core._Time

-- | The last time the data store was updated.
datastore_lastUpdateTime :: Lens.Lens' Datastore (Core.Maybe Core.UTCTime)
datastore_lastUpdateTime = Lens.lens (\Datastore' {lastUpdateTime} -> lastUpdateTime) (\s@Datastore' {} a -> s {lastUpdateTime = a} :: Datastore) Core.. Lens.mapping Core._Time

-- | The ARN of the data store.
datastore_arn :: Lens.Lens' Datastore (Core.Maybe Core.Text)
datastore_arn = Lens.lens (\Datastore' {arn} -> arn) (\s@Datastore' {} a -> s {arn = a} :: Datastore)

-- | Contains the configuration information of file formats. AWS IoT
-- Analytics data stores support JSON and
-- <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
datastore_fileFormatConfiguration :: Lens.Lens' Datastore (Core.Maybe FileFormatConfiguration)
datastore_fileFormatConfiguration = Lens.lens (\Datastore' {fileFormatConfiguration} -> fileFormatConfiguration) (\s@Datastore' {} a -> s {fileFormatConfiguration = a} :: Datastore)

-- | The name of the data store.
datastore_name :: Lens.Lens' Datastore (Core.Maybe Core.Text)
datastore_name = Lens.lens (\Datastore' {name} -> name) (\s@Datastore' {} a -> s {name = a} :: Datastore)

-- | How long, in days, message data is kept for the data store. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
datastore_retentionPeriod :: Lens.Lens' Datastore (Core.Maybe RetentionPeriod)
datastore_retentionPeriod = Lens.lens (\Datastore' {retentionPeriod} -> retentionPeriod) (\s@Datastore' {} a -> s {retentionPeriod = a} :: Datastore)

-- | Where data store data is stored. You can choose one of
-- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
-- default is @serviceManagedS3@. You cannot change this storage option
-- after the data store is created.
datastore_storage :: Lens.Lens' Datastore (Core.Maybe DatastoreStorage)
datastore_storage = Lens.lens (\Datastore' {storage} -> storage) (\s@Datastore' {} a -> s {storage = a} :: Datastore)

instance Core.FromJSON Datastore where
  parseJSON =
    Core.withObject
      "Datastore"
      ( \x ->
          Datastore'
            Core.<$> (x Core..:? "lastMessageArrivalTime")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "fileFormatConfiguration")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "retentionPeriod")
            Core.<*> (x Core..:? "storage")
      )

instance Core.Hashable Datastore

instance Core.NFData Datastore
