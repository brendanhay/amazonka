{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets paginated records, optionally changed after a particular sync count for a dataset and identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- ListRecords can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.ListRecords
  ( -- * Creating a request
    ListRecords (..),
    mkListRecords,

    -- ** Request lenses
    lrIdentityPoolId,
    lrIdentityId,
    lrDatasetName,
    lrLastSyncCount,
    lrMaxResults,
    lrNextToken,
    lrSyncSessionToken,

    -- * Destructuring the response
    ListRecordsResponse (..),
    mkListRecordsResponse,

    -- ** Response lenses
    lrrrsCount,
    lrrrsDatasetDeletedAfterRequestedSyncCount,
    lrrrsDatasetExists,
    lrrrsDatasetSyncCount,
    lrrrsLastModifiedBy,
    lrrrsMergedDatasetNames,
    lrrrsNextToken,
    lrrrsRecords,
    lrrrsSyncSessionToken,
    lrrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of records.
--
-- /See:/ 'mkListRecords' smart constructor.
data ListRecords = ListRecords'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Types.IdentityId,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Types.DatasetName,
    -- | The last server sync count for this record.
    lastSyncCount :: Core.Maybe Core.Integer,
    -- | The maximum number of results to be returned.
    maxResults :: Core.Maybe Core.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Core.Maybe Types.SyncSessionToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRecords' value with any optional fields omitted.
mkListRecords ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'datasetName'
  Types.DatasetName ->
  ListRecords
mkListRecords identityPoolId identityId datasetName =
  ListRecords'
    { identityPoolId,
      identityId,
      datasetName,
      lastSyncCount = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      syncSessionToken = Core.Nothing
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityPoolId :: Lens.Lens' ListRecords Types.IdentityPoolId
lrIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED lrIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityId :: Lens.Lens' ListRecords Types.IdentityId
lrIdentityId = Lens.field @"identityId"
{-# DEPRECATED lrIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrDatasetName :: Lens.Lens' ListRecords Types.DatasetName
lrDatasetName = Lens.field @"datasetName"
{-# DEPRECATED lrDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The last server sync count for this record.
--
-- /Note:/ Consider using 'lastSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLastSyncCount :: Lens.Lens' ListRecords (Core.Maybe Core.Integer)
lrLastSyncCount = Lens.field @"lastSyncCount"
{-# DEPRECATED lrLastSyncCount "Use generic-lens or generic-optics with 'lastSyncCount' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRecords (Core.Maybe Core.Int)
lrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRecords (Core.Maybe Types.String)
lrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSyncSessionToken :: Lens.Lens' ListRecords (Core.Maybe Types.SyncSessionToken)
lrSyncSessionToken = Lens.field @"syncSessionToken"
{-# DEPRECATED lrSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

instance Core.AWSRequest ListRecords where
  type Rs ListRecords = ListRecordsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
                Core.<> ("/datasets/")
                Core.<> (Core.toText datasetName)
                Core.<> ("/records")
            ),
        Core._rqQuery =
          Core.toQueryValue "lastSyncCount" Core.<$> lastSyncCount
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "syncSessionToken" Core.<$> syncSessionToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecordsResponse'
            Core.<$> (x Core..:? "Count")
            Core.<*> (x Core..:? "DatasetDeletedAfterRequestedSyncCount")
            Core.<*> (x Core..:? "DatasetExists")
            Core.<*> (x Core..:? "DatasetSyncCount")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "MergedDatasetNames")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Records")
            Core.<*> (x Core..:? "SyncSessionToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'mkListRecordsResponse' smart constructor.
data ListRecordsResponse = ListRecordsResponse'
  { -- | Total number of records.
    count :: Core.Maybe Core.Int,
    -- | A boolean value specifying whether to delete the dataset locally.
    datasetDeletedAfterRequestedSyncCount :: Core.Maybe Core.Bool,
    -- | Indicates whether the dataset exists.
    datasetExists :: Core.Maybe Core.Bool,
    -- | Server sync count for this dataset.
    datasetSyncCount :: Core.Maybe Core.Integer,
    -- | The user/device that made the last change to this record.
    lastModifiedBy :: Core.Maybe Types.String,
    -- | Names of merged datasets.
    mergedDatasetNames :: Core.Maybe [Types.String],
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | A list of all records.
    records :: Core.Maybe [Types.Record],
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListRecordsResponse' value with any optional fields omitted.
mkListRecordsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRecordsResponse
mkListRecordsResponse responseStatus =
  ListRecordsResponse'
    { count = Core.Nothing,
      datasetDeletedAfterRequestedSyncCount = Core.Nothing,
      datasetExists = Core.Nothing,
      datasetSyncCount = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      mergedDatasetNames = Core.Nothing,
      nextToken = Core.Nothing,
      records = Core.Nothing,
      syncSessionToken = Core.Nothing,
      responseStatus
    }

-- | Total number of records.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Int)
lrrrsCount = Lens.field @"count"
{-# DEPRECATED lrrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | A boolean value specifying whether to delete the dataset locally.
--
-- /Note:/ Consider using 'datasetDeletedAfterRequestedSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetDeletedAfterRequestedSyncCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Bool)
lrrrsDatasetDeletedAfterRequestedSyncCount = Lens.field @"datasetDeletedAfterRequestedSyncCount"
{-# DEPRECATED lrrrsDatasetDeletedAfterRequestedSyncCount "Use generic-lens or generic-optics with 'datasetDeletedAfterRequestedSyncCount' instead." #-}

-- | Indicates whether the dataset exists.
--
-- /Note:/ Consider using 'datasetExists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetExists :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Bool)
lrrrsDatasetExists = Lens.field @"datasetExists"
{-# DEPRECATED lrrrsDatasetExists "Use generic-lens or generic-optics with 'datasetExists' instead." #-}

-- | Server sync count for this dataset.
--
-- /Note:/ Consider using 'datasetSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetSyncCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Integer)
lrrrsDatasetSyncCount = Lens.field @"datasetSyncCount"
{-# DEPRECATED lrrrsDatasetSyncCount "Use generic-lens or generic-optics with 'datasetSyncCount' instead." #-}

-- | The user/device that made the last change to this record.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsLastModifiedBy :: Lens.Lens' ListRecordsResponse (Core.Maybe Types.String)
lrrrsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED lrrrsLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | Names of merged datasets.
--
-- /Note:/ Consider using 'mergedDatasetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsMergedDatasetNames :: Lens.Lens' ListRecordsResponse (Core.Maybe [Types.String])
lrrrsMergedDatasetNames = Lens.field @"mergedDatasetNames"
{-# DEPRECATED lrrrsMergedDatasetNames "Use generic-lens or generic-optics with 'mergedDatasetNames' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListRecordsResponse (Core.Maybe Types.String)
lrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of all records.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsRecords :: Lens.Lens' ListRecordsResponse (Core.Maybe [Types.Record])
lrrrsRecords = Lens.field @"records"
{-# DEPRECATED lrrrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsSyncSessionToken :: Lens.Lens' ListRecordsResponse (Core.Maybe Types.String)
lrrrsSyncSessionToken = Lens.field @"syncSessionToken"
{-# DEPRECATED lrrrsSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListRecordsResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
