{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts updates to records and adds and deletes records for a dataset and user.
--
-- The sync count in the record patch is your last known sync count for that record. The server will reject an UpdateRecords request with a ResourceConflictException if you try to patch a record with a new value but a stale sync count.
-- For example, if the sync count on the server is 5 for a key called highScore and you try and submit a new highScore with sync count of 4, the request will be rejected. To obtain the current sync count for a record, call ListRecords. On a successful update of the record, the response returns the new sync count for that record. You should present that sync count the next time you try to update that same record. When the record does not exist, specify the sync count as 0.
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.UpdateRecords
  ( -- * Creating a request
    UpdateRecords (..),
    mkUpdateRecords,

    -- ** Request lenses
    urIdentityPoolId,
    urRecordPatches,
    urSyncSessionToken,
    urDatasetName,
    urDeviceId,
    urIdentityId,
    urClientContext,

    -- * Destructuring the response
    UpdateRecordsResponse (..),
    mkUpdateRecordsResponse,

    -- ** Response lenses
    urrsRecords,
    urrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to post updates to records or add and delete records for a dataset and user.
--
-- /See:/ 'mkUpdateRecords' smart constructor.
data UpdateRecords = UpdateRecords'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text,
    -- | A list of patch operations.
    recordPatches :: Lude.Maybe [RecordPatch],
    -- | The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
    syncSessionToken :: Lude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Lude.Text,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Lude.Maybe Lude.Text,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Text,
    -- | Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
    clientContext :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRecords' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'recordPatches' - A list of patch operations.
-- * 'syncSessionToken' - The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
-- * 'datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
-- * 'deviceId' - The unique ID generated for this device by Cognito.
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'clientContext' - Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
mkUpdateRecords ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'syncSessionToken'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  UpdateRecords
mkUpdateRecords
  pIdentityPoolId_
  pSyncSessionToken_
  pDatasetName_
  pIdentityId_ =
    UpdateRecords'
      { identityPoolId = pIdentityPoolId_,
        recordPatches = Lude.Nothing,
        syncSessionToken = pSyncSessionToken_,
        datasetName = pDatasetName_,
        deviceId = Lude.Nothing,
        identityId = pIdentityId_,
        clientContext = Lude.Nothing
      }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urIdentityPoolId :: Lens.Lens' UpdateRecords Lude.Text
urIdentityPoolId = Lens.lens (identityPoolId :: UpdateRecords -> Lude.Text) (\s a -> s {identityPoolId = a} :: UpdateRecords)
{-# DEPRECATED urIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A list of patch operations.
--
-- /Note:/ Consider using 'recordPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRecordPatches :: Lens.Lens' UpdateRecords (Lude.Maybe [RecordPatch])
urRecordPatches = Lens.lens (recordPatches :: UpdateRecords -> Lude.Maybe [RecordPatch]) (\s a -> s {recordPatches = a} :: UpdateRecords)
{-# DEPRECATED urRecordPatches "Use generic-lens or generic-optics with 'recordPatches' instead." #-}

-- | The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSyncSessionToken :: Lens.Lens' UpdateRecords Lude.Text
urSyncSessionToken = Lens.lens (syncSessionToken :: UpdateRecords -> Lude.Text) (\s a -> s {syncSessionToken = a} :: UpdateRecords)
{-# DEPRECATED urSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDatasetName :: Lens.Lens' UpdateRecords Lude.Text
urDatasetName = Lens.lens (datasetName :: UpdateRecords -> Lude.Text) (\s a -> s {datasetName = a} :: UpdateRecords)
{-# DEPRECATED urDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDeviceId :: Lens.Lens' UpdateRecords (Lude.Maybe Lude.Text)
urDeviceId = Lens.lens (deviceId :: UpdateRecords -> Lude.Maybe Lude.Text) (\s a -> s {deviceId = a} :: UpdateRecords)
{-# DEPRECATED urDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urIdentityId :: Lens.Lens' UpdateRecords Lude.Text
urIdentityId = Lens.lens (identityId :: UpdateRecords -> Lude.Text) (\s a -> s {identityId = a} :: UpdateRecords)
{-# DEPRECATED urIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urClientContext :: Lens.Lens' UpdateRecords (Lude.Maybe Lude.Text)
urClientContext = Lens.lens (clientContext :: UpdateRecords -> Lude.Maybe Lude.Text) (\s a -> s {clientContext = a} :: UpdateRecords)
{-# DEPRECATED urClientContext "Use generic-lens or generic-optics with 'clientContext' instead." #-}

instance Lude.AWSRequest UpdateRecords where
  type Rs UpdateRecords = UpdateRecordsResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRecordsResponse'
            Lude.<$> (x Lude..?> "Records" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRecords where
  toHeaders UpdateRecords' {..} =
    Lude.mconcat
      [ "x-amz-Client-Context" Lude.=# clientContext,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON UpdateRecords where
  toJSON UpdateRecords' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RecordPatches" Lude..=) Lude.<$> recordPatches,
            Lude.Just ("SyncSessionToken" Lude..= syncSessionToken),
            ("DeviceId" Lude..=) Lude.<$> deviceId
          ]
      )

instance Lude.ToPath UpdateRecords where
  toPath UpdateRecords' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets/",
        Lude.toBS datasetName
      ]

instance Lude.ToQuery UpdateRecords where
  toQuery = Lude.const Lude.mempty

-- | Returned for a successful UpdateRecordsRequest.
--
-- /See:/ 'mkUpdateRecordsResponse' smart constructor.
data UpdateRecordsResponse = UpdateRecordsResponse'
  { -- | A list of records that have been updated.
    records :: Lude.Maybe [Record],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRecordsResponse' with the minimum fields required to make a request.
--
-- * 'records' - A list of records that have been updated.
-- * 'responseStatus' - The response status code.
mkUpdateRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRecordsResponse
mkUpdateRecordsResponse pResponseStatus_ =
  UpdateRecordsResponse'
    { records = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of records that have been updated.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsRecords :: Lens.Lens' UpdateRecordsResponse (Lude.Maybe [Record])
urrsRecords = Lens.lens (records :: UpdateRecordsResponse -> Lude.Maybe [Record]) (\s a -> s {records = a} :: UpdateRecordsResponse)
{-# DEPRECATED urrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRecordsResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRecordsResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
