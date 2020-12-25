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
    urIdentityId,
    urDatasetName,
    urSyncSessionToken,
    urClientContext,
    urDeviceId,
    urRecordPatches,

    -- * Destructuring the response
    UpdateRecordsResponse (..),
    mkUpdateRecordsResponse,

    -- ** Response lenses
    urrrsRecords,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to post updates to records or add and delete records for a dataset and user.
--
-- /See:/ 'mkUpdateRecords' smart constructor.
data UpdateRecords = UpdateRecords'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Types.IdentityId,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Types.DatasetName,
    -- | The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
    syncSessionToken :: Types.SyncSessionToken,
    -- | Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
    clientContext :: Core.Maybe Types.ClientContext,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Core.Maybe Types.DeviceId,
    -- | A list of patch operations.
    recordPatches :: Core.Maybe [Types.RecordPatch]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateRecords' value with any optional fields omitted.
mkUpdateRecords ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'datasetName'
  Types.DatasetName ->
  -- | 'syncSessionToken'
  Types.SyncSessionToken ->
  UpdateRecords
mkUpdateRecords
  identityPoolId
  identityId
  datasetName
  syncSessionToken =
    UpdateRecords'
      { identityPoolId,
        identityId,
        datasetName,
        syncSessionToken,
        clientContext = Core.Nothing,
        deviceId = Core.Nothing,
        recordPatches = Core.Nothing
      }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urIdentityPoolId :: Lens.Lens' UpdateRecords Types.IdentityPoolId
urIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED urIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urIdentityId :: Lens.Lens' UpdateRecords Types.IdentityId
urIdentityId = Lens.field @"identityId"
{-# DEPRECATED urIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDatasetName :: Lens.Lens' UpdateRecords Types.DatasetName
urDatasetName = Lens.field @"datasetName"
{-# DEPRECATED urDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The SyncSessionToken returned by a previous call to ListRecords for this dataset and identity.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSyncSessionToken :: Lens.Lens' UpdateRecords Types.SyncSessionToken
urSyncSessionToken = Lens.field @"syncSessionToken"
{-# DEPRECATED urSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

-- | Intended to supply a device ID that will populate the lastModifiedBy field referenced in other methods. The ClientContext field is not yet implemented.
--
-- /Note:/ Consider using 'clientContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urClientContext :: Lens.Lens' UpdateRecords (Core.Maybe Types.ClientContext)
urClientContext = Lens.field @"clientContext"
{-# DEPRECATED urClientContext "Use generic-lens or generic-optics with 'clientContext' instead." #-}

-- | The unique ID generated for this device by Cognito.
--
-- /Note:/ Consider using 'deviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDeviceId :: Lens.Lens' UpdateRecords (Core.Maybe Types.DeviceId)
urDeviceId = Lens.field @"deviceId"
{-# DEPRECATED urDeviceId "Use generic-lens or generic-optics with 'deviceId' instead." #-}

-- | A list of patch operations.
--
-- /Note:/ Consider using 'recordPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRecordPatches :: Lens.Lens' UpdateRecords (Core.Maybe [Types.RecordPatch])
urRecordPatches = Lens.field @"recordPatches"
{-# DEPRECATED urRecordPatches "Use generic-lens or generic-optics with 'recordPatches' instead." #-}

instance Core.FromJSON UpdateRecords where
  toJSON UpdateRecords {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SyncSessionToken" Core..= syncSessionToken),
            ("DeviceId" Core..=) Core.<$> deviceId,
            ("RecordPatches" Core..=) Core.<$> recordPatches
          ]
      )

instance Core.AWSRequest UpdateRecords where
  type Rs UpdateRecords = UpdateRecordsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
                Core.<> ("/datasets/")
                Core.<> (Core.toText datasetName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-Client-Context" clientContext
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecordsResponse'
            Core.<$> (x Core..:? "Records") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned for a successful UpdateRecordsRequest.
--
-- /See:/ 'mkUpdateRecordsResponse' smart constructor.
data UpdateRecordsResponse = UpdateRecordsResponse'
  { -- | A list of records that have been updated.
    records :: Core.Maybe [Types.Record],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateRecordsResponse' value with any optional fields omitted.
mkUpdateRecordsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRecordsResponse
mkUpdateRecordsResponse responseStatus =
  UpdateRecordsResponse' {records = Core.Nothing, responseStatus}

-- | A list of records that have been updated.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsRecords :: Lens.Lens' UpdateRecordsResponse (Core.Maybe [Types.Record])
urrrsRecords = Lens.field @"records"
{-# DEPRECATED urrrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRecordsResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
