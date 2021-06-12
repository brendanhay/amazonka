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
-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts updates to records and adds and deletes records for a dataset and
-- user.
--
-- The sync count in the record patch is your last known sync count for
-- that record. The server will reject an UpdateRecords request with a
-- ResourceConflictException if you try to patch a record with a new value
-- but a stale sync count.
--
-- For example, if the sync count on the server is 5 for a key called
-- highScore and you try and submit a new highScore with sync count of 4,
-- the request will be rejected. To obtain the current sync count for a
-- record, call ListRecords. On a successful update of the record, the
-- response returns the new sync count for that record. You should present
-- that sync count the next time you try to update that same record. When
-- the record does not exist, specify the sync count as 0.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.UpdateRecords
  ( -- * Creating a Request
    UpdateRecords (..),
    newUpdateRecords,

    -- * Request Lenses
    updateRecords_recordPatches,
    updateRecords_deviceId,
    updateRecords_clientContext,
    updateRecords_identityPoolId,
    updateRecords_identityId,
    updateRecords_datasetName,
    updateRecords_syncSessionToken,

    -- * Destructuring the Response
    UpdateRecordsResponse (..),
    newUpdateRecordsResponse,

    -- * Response Lenses
    updateRecordsResponse_records,
    updateRecordsResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to post updates to records or add and delete records for a
-- dataset and user.
--
-- /See:/ 'newUpdateRecords' smart constructor.
data UpdateRecords = UpdateRecords'
  { -- | A list of patch operations.
    recordPatches :: Core.Maybe [RecordPatch],
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Core.Maybe Core.Text,
    -- | Intended to supply a device ID that will populate the lastModifiedBy
    -- field referenced in other methods. The ClientContext field is not yet
    -- implemented.
    clientContext :: Core.Maybe Core.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Core.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Core.Text,
    -- | The SyncSessionToken returned by a previous call to ListRecords for this
    -- dataset and identity.
    syncSessionToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordPatches', 'updateRecords_recordPatches' - A list of patch operations.
--
-- 'deviceId', 'updateRecords_deviceId' - The unique ID generated for this device by Cognito.
--
-- 'clientContext', 'updateRecords_clientContext' - Intended to supply a device ID that will populate the lastModifiedBy
-- field referenced in other methods. The ClientContext field is not yet
-- implemented.
--
-- 'identityPoolId', 'updateRecords_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityId', 'updateRecords_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'datasetName', 'updateRecords_datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
--
-- 'syncSessionToken', 'updateRecords_syncSessionToken' - The SyncSessionToken returned by a previous call to ListRecords for this
-- dataset and identity.
newUpdateRecords ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityId'
  Core.Text ->
  -- | 'datasetName'
  Core.Text ->
  -- | 'syncSessionToken'
  Core.Text ->
  UpdateRecords
newUpdateRecords
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_
  pSyncSessionToken_ =
    UpdateRecords'
      { recordPatches = Core.Nothing,
        deviceId = Core.Nothing,
        clientContext = Core.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_,
        syncSessionToken = pSyncSessionToken_
      }

-- | A list of patch operations.
updateRecords_recordPatches :: Lens.Lens' UpdateRecords (Core.Maybe [RecordPatch])
updateRecords_recordPatches = Lens.lens (\UpdateRecords' {recordPatches} -> recordPatches) (\s@UpdateRecords' {} a -> s {recordPatches = a} :: UpdateRecords) Core.. Lens.mapping Lens._Coerce

-- | The unique ID generated for this device by Cognito.
updateRecords_deviceId :: Lens.Lens' UpdateRecords (Core.Maybe Core.Text)
updateRecords_deviceId = Lens.lens (\UpdateRecords' {deviceId} -> deviceId) (\s@UpdateRecords' {} a -> s {deviceId = a} :: UpdateRecords)

-- | Intended to supply a device ID that will populate the lastModifiedBy
-- field referenced in other methods. The ClientContext field is not yet
-- implemented.
updateRecords_clientContext :: Lens.Lens' UpdateRecords (Core.Maybe Core.Text)
updateRecords_clientContext = Lens.lens (\UpdateRecords' {clientContext} -> clientContext) (\s@UpdateRecords' {} a -> s {clientContext = a} :: UpdateRecords)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
updateRecords_identityPoolId :: Lens.Lens' UpdateRecords Core.Text
updateRecords_identityPoolId = Lens.lens (\UpdateRecords' {identityPoolId} -> identityPoolId) (\s@UpdateRecords' {} a -> s {identityPoolId = a} :: UpdateRecords)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
updateRecords_identityId :: Lens.Lens' UpdateRecords Core.Text
updateRecords_identityId = Lens.lens (\UpdateRecords' {identityId} -> identityId) (\s@UpdateRecords' {} a -> s {identityId = a} :: UpdateRecords)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
updateRecords_datasetName :: Lens.Lens' UpdateRecords Core.Text
updateRecords_datasetName = Lens.lens (\UpdateRecords' {datasetName} -> datasetName) (\s@UpdateRecords' {} a -> s {datasetName = a} :: UpdateRecords)

-- | The SyncSessionToken returned by a previous call to ListRecords for this
-- dataset and identity.
updateRecords_syncSessionToken :: Lens.Lens' UpdateRecords Core.Text
updateRecords_syncSessionToken = Lens.lens (\UpdateRecords' {syncSessionToken} -> syncSessionToken) (\s@UpdateRecords' {} a -> s {syncSessionToken = a} :: UpdateRecords)

instance Core.AWSRequest UpdateRecords where
  type
    AWSResponse UpdateRecords =
      UpdateRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecordsResponse'
            Core.<$> (x Core..?> "Records" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRecords

instance Core.NFData UpdateRecords

instance Core.ToHeaders UpdateRecords where
  toHeaders UpdateRecords' {..} =
    Core.mconcat
      [ "x-amz-Client-Context" Core.=# clientContext,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON UpdateRecords where
  toJSON UpdateRecords' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecordPatches" Core..=) Core.<$> recordPatches,
            ("DeviceId" Core..=) Core.<$> deviceId,
            Core.Just
              ("SyncSessionToken" Core..= syncSessionToken)
          ]
      )

instance Core.ToPath UpdateRecords where
  toPath UpdateRecords' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/identities/",
        Core.toBS identityId,
        "/datasets/",
        Core.toBS datasetName
      ]

instance Core.ToQuery UpdateRecords where
  toQuery = Core.const Core.mempty

-- | Returned for a successful UpdateRecordsRequest.
--
-- /See:/ 'newUpdateRecordsResponse' smart constructor.
data UpdateRecordsResponse = UpdateRecordsResponse'
  { -- | A list of records that have been updated.
    records :: Core.Maybe [Record],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'records', 'updateRecordsResponse_records' - A list of records that have been updated.
--
-- 'httpStatus', 'updateRecordsResponse_httpStatus' - The response's http status code.
newUpdateRecordsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRecordsResponse
newUpdateRecordsResponse pHttpStatus_ =
  UpdateRecordsResponse'
    { records = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of records that have been updated.
updateRecordsResponse_records :: Lens.Lens' UpdateRecordsResponse (Core.Maybe [Record])
updateRecordsResponse_records = Lens.lens (\UpdateRecordsResponse' {records} -> records) (\s@UpdateRecordsResponse' {} a -> s {records = a} :: UpdateRecordsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateRecordsResponse_httpStatus :: Lens.Lens' UpdateRecordsResponse Core.Int
updateRecordsResponse_httpStatus = Lens.lens (\UpdateRecordsResponse' {httpStatus} -> httpStatus) (\s@UpdateRecordsResponse' {} a -> s {httpStatus = a} :: UpdateRecordsResponse)

instance Core.NFData UpdateRecordsResponse
