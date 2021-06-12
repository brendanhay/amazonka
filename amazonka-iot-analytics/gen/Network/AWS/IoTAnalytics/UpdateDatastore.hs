{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdateDatastore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data store.
module Network.AWS.IoTAnalytics.UpdateDatastore
  ( -- * Creating a Request
    UpdateDatastore (..),
    newUpdateDatastore,

    -- * Request Lenses
    updateDatastore_datastoreStorage,
    updateDatastore_fileFormatConfiguration,
    updateDatastore_retentionPeriod,
    updateDatastore_datastoreName,

    -- * Destructuring the Response
    UpdateDatastoreResponse (..),
    newUpdateDatastoreResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDatastore' smart constructor.
data UpdateDatastore = UpdateDatastore'
  { -- | Where data store data is stored. You can choose one of
    -- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
    -- default is@serviceManagedS3@. You cannot change this storage option
    -- after the data store is created.
    datastoreStorage :: Core.Maybe DatastoreStorage,
    -- | Contains the configuration information of file formats. AWS IoT
    -- Analytics data stores support JSON and
    -- <https://parquet.apache.org/ Parquet>.
    --
    -- The default file format is JSON. You can specify only one format.
    --
    -- You can\'t change the file format after you create the data store.
    fileFormatConfiguration :: Core.Maybe FileFormatConfiguration,
    -- | How long, in days, message data is kept for the data store. The
    -- retention period cannot be updated if the data store\'s S3 storage is
    -- customer-managed.
    retentionPeriod :: Core.Maybe RetentionPeriod,
    -- | The name of the data store to be updated.
    datastoreName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreStorage', 'updateDatastore_datastoreStorage' - Where data store data is stored. You can choose one of
-- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
-- default is@serviceManagedS3@. You cannot change this storage option
-- after the data store is created.
--
-- 'fileFormatConfiguration', 'updateDatastore_fileFormatConfiguration' - Contains the configuration information of file formats. AWS IoT
-- Analytics data stores support JSON and
-- <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
--
-- 'retentionPeriod', 'updateDatastore_retentionPeriod' - How long, in days, message data is kept for the data store. The
-- retention period cannot be updated if the data store\'s S3 storage is
-- customer-managed.
--
-- 'datastoreName', 'updateDatastore_datastoreName' - The name of the data store to be updated.
newUpdateDatastore ::
  -- | 'datastoreName'
  Core.Text ->
  UpdateDatastore
newUpdateDatastore pDatastoreName_ =
  UpdateDatastore'
    { datastoreStorage = Core.Nothing,
      fileFormatConfiguration = Core.Nothing,
      retentionPeriod = Core.Nothing,
      datastoreName = pDatastoreName_
    }

-- | Where data store data is stored. You can choose one of
-- @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the
-- default is@serviceManagedS3@. You cannot change this storage option
-- after the data store is created.
updateDatastore_datastoreStorage :: Lens.Lens' UpdateDatastore (Core.Maybe DatastoreStorage)
updateDatastore_datastoreStorage = Lens.lens (\UpdateDatastore' {datastoreStorage} -> datastoreStorage) (\s@UpdateDatastore' {} a -> s {datastoreStorage = a} :: UpdateDatastore)

-- | Contains the configuration information of file formats. AWS IoT
-- Analytics data stores support JSON and
-- <https://parquet.apache.org/ Parquet>.
--
-- The default file format is JSON. You can specify only one format.
--
-- You can\'t change the file format after you create the data store.
updateDatastore_fileFormatConfiguration :: Lens.Lens' UpdateDatastore (Core.Maybe FileFormatConfiguration)
updateDatastore_fileFormatConfiguration = Lens.lens (\UpdateDatastore' {fileFormatConfiguration} -> fileFormatConfiguration) (\s@UpdateDatastore' {} a -> s {fileFormatConfiguration = a} :: UpdateDatastore)

-- | How long, in days, message data is kept for the data store. The
-- retention period cannot be updated if the data store\'s S3 storage is
-- customer-managed.
updateDatastore_retentionPeriod :: Lens.Lens' UpdateDatastore (Core.Maybe RetentionPeriod)
updateDatastore_retentionPeriod = Lens.lens (\UpdateDatastore' {retentionPeriod} -> retentionPeriod) (\s@UpdateDatastore' {} a -> s {retentionPeriod = a} :: UpdateDatastore)

-- | The name of the data store to be updated.
updateDatastore_datastoreName :: Lens.Lens' UpdateDatastore Core.Text
updateDatastore_datastoreName = Lens.lens (\UpdateDatastore' {datastoreName} -> datastoreName) (\s@UpdateDatastore' {} a -> s {datastoreName = a} :: UpdateDatastore)

instance Core.AWSRequest UpdateDatastore where
  type
    AWSResponse UpdateDatastore =
      UpdateDatastoreResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateDatastoreResponse'

instance Core.Hashable UpdateDatastore

instance Core.NFData UpdateDatastore

instance Core.ToHeaders UpdateDatastore where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateDatastore where
  toJSON UpdateDatastore' {..} =
    Core.object
      ( Core.catMaybes
          [ ("datastoreStorage" Core..=)
              Core.<$> datastoreStorage,
            ("fileFormatConfiguration" Core..=)
              Core.<$> fileFormatConfiguration,
            ("retentionPeriod" Core..=)
              Core.<$> retentionPeriod
          ]
      )

instance Core.ToPath UpdateDatastore where
  toPath UpdateDatastore' {..} =
    Core.mconcat
      ["/datastores/", Core.toBS datastoreName]

instance Core.ToQuery UpdateDatastore where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDatastoreResponse' smart constructor.
data UpdateDatastoreResponse = UpdateDatastoreResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDatastoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDatastoreResponse ::
  UpdateDatastoreResponse
newUpdateDatastoreResponse = UpdateDatastoreResponse'

instance Core.NFData UpdateDatastoreResponse
