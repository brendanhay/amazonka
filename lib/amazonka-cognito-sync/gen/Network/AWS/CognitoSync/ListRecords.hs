{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListRecords (..)
    , mkListRecords
    -- ** Request lenses
    , lrIdentityPoolId
    , lrIdentityId
    , lrDatasetName
    , lrLastSyncCount
    , lrMaxResults
    , lrNextToken
    , lrSyncSessionToken

    -- * Destructuring the response
    , ListRecordsResponse (..)
    , mkListRecordsResponse
    -- ** Response lenses
    , lrrrsCount
    , lrrrsDatasetDeletedAfterRequestedSyncCount
    , lrrrsDatasetExists
    , lrrrsDatasetSyncCount
    , lrrrsLastModifiedBy
    , lrrrsMergedDatasetNames
    , lrrrsNextToken
    , lrrrsRecords
    , lrrrsSyncSessionToken
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for a list of records.
--
-- /See:/ 'mkListRecords' smart constructor.
data ListRecords = ListRecords'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , identityId :: Types.IdentityId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , datasetName :: Types.DatasetName
    -- ^ A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
  , lastSyncCount :: Core.Maybe Core.Integer
    -- ^ The last server sync count for this record.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to be returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A pagination token for obtaining the next page of results.
  , syncSessionToken :: Core.Maybe Types.SyncSessionToken
    -- ^ A token containing a session ID, identity ID, and expiration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRecords' value with any optional fields omitted.
mkListRecords
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> Types.IdentityId -- ^ 'identityId'
    -> Types.DatasetName -- ^ 'datasetName'
    -> ListRecords
mkListRecords identityPoolId identityId datasetName
  = ListRecords'{identityPoolId, identityId, datasetName,
                 lastSyncCount = Core.Nothing, maxResults = Core.Nothing,
                 nextToken = Core.Nothing, syncSessionToken = Core.Nothing}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityPoolId :: Lens.Lens' ListRecords Types.IdentityPoolId
lrIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE lrIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityId :: Lens.Lens' ListRecords Types.IdentityId
lrIdentityId = Lens.field @"identityId"
{-# INLINEABLE lrIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrDatasetName :: Lens.Lens' ListRecords Types.DatasetName
lrDatasetName = Lens.field @"datasetName"
{-# INLINEABLE lrDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | The last server sync count for this record.
--
-- /Note:/ Consider using 'lastSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLastSyncCount :: Lens.Lens' ListRecords (Core.Maybe Core.Integer)
lrLastSyncCount = Lens.field @"lastSyncCount"
{-# INLINEABLE lrLastSyncCount #-}
{-# DEPRECATED lastSyncCount "Use generic-lens or generic-optics with 'lastSyncCount' instead"  #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRecords (Core.Maybe Core.Int)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRecords (Core.Maybe Core.Text)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSyncSessionToken :: Lens.Lens' ListRecords (Core.Maybe Types.SyncSessionToken)
lrSyncSessionToken = Lens.field @"syncSessionToken"
{-# INLINEABLE lrSyncSessionToken #-}
{-# DEPRECATED syncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead"  #-}

instance Core.ToQuery ListRecords where
        toQuery ListRecords{..}
          = Core.maybe Core.mempty (Core.toQueryPair "lastSyncCount")
              lastSyncCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "syncSessionToken")
                syncSessionToken

instance Core.ToHeaders ListRecords where
        toHeaders ListRecords{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListRecords where
        type Rs ListRecords = ListRecordsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/identities/"
                             Core.<> Core.toText identityId
                             Core.<> "/datasets/"
                             Core.<> Core.toText datasetName
                             Core.<> "/records",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRecordsResponse' Core.<$>
                   (x Core..:? "Count") Core.<*>
                     x Core..:? "DatasetDeletedAfterRequestedSyncCount"
                     Core.<*> x Core..:? "DatasetExists"
                     Core.<*> x Core..:? "DatasetSyncCount"
                     Core.<*> x Core..:? "LastModifiedBy"
                     Core.<*> x Core..:? "MergedDatasetNames"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "Records"
                     Core.<*> x Core..:? "SyncSessionToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'mkListRecordsResponse' smart constructor.
data ListRecordsResponse = ListRecordsResponse'
  { count :: Core.Maybe Core.Int
    -- ^ Total number of records.
  , datasetDeletedAfterRequestedSyncCount :: Core.Maybe Core.Bool
    -- ^ A boolean value specifying whether to delete the dataset locally.
  , datasetExists :: Core.Maybe Core.Bool
    -- ^ Indicates whether the dataset exists.
  , datasetSyncCount :: Core.Maybe Core.Integer
    -- ^ Server sync count for this dataset.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user/device that made the last change to this record.
  , mergedDatasetNames :: Core.Maybe [Core.Text]
    -- ^ Names of merged datasets.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A pagination token for obtaining the next page of results.
  , records :: Core.Maybe [Types.Record]
    -- ^ A list of all records.
  , syncSessionToken :: Core.Maybe Core.Text
    -- ^ A token containing a session ID, identity ID, and expiration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListRecordsResponse' value with any optional fields omitted.
mkListRecordsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRecordsResponse
mkListRecordsResponse responseStatus
  = ListRecordsResponse'{count = Core.Nothing,
                         datasetDeletedAfterRequestedSyncCount = Core.Nothing,
                         datasetExists = Core.Nothing, datasetSyncCount = Core.Nothing,
                         lastModifiedBy = Core.Nothing, mergedDatasetNames = Core.Nothing,
                         nextToken = Core.Nothing, records = Core.Nothing,
                         syncSessionToken = Core.Nothing, responseStatus}

-- | Total number of records.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Int)
lrrrsCount = Lens.field @"count"
{-# INLINEABLE lrrrsCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | A boolean value specifying whether to delete the dataset locally.
--
-- /Note:/ Consider using 'datasetDeletedAfterRequestedSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetDeletedAfterRequestedSyncCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Bool)
lrrrsDatasetDeletedAfterRequestedSyncCount = Lens.field @"datasetDeletedAfterRequestedSyncCount"
{-# INLINEABLE lrrrsDatasetDeletedAfterRequestedSyncCount #-}
{-# DEPRECATED datasetDeletedAfterRequestedSyncCount "Use generic-lens or generic-optics with 'datasetDeletedAfterRequestedSyncCount' instead"  #-}

-- | Indicates whether the dataset exists.
--
-- /Note:/ Consider using 'datasetExists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetExists :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Bool)
lrrrsDatasetExists = Lens.field @"datasetExists"
{-# INLINEABLE lrrrsDatasetExists #-}
{-# DEPRECATED datasetExists "Use generic-lens or generic-optics with 'datasetExists' instead"  #-}

-- | Server sync count for this dataset.
--
-- /Note:/ Consider using 'datasetSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsDatasetSyncCount :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Integer)
lrrrsDatasetSyncCount = Lens.field @"datasetSyncCount"
{-# INLINEABLE lrrrsDatasetSyncCount #-}
{-# DEPRECATED datasetSyncCount "Use generic-lens or generic-optics with 'datasetSyncCount' instead"  #-}

-- | The user/device that made the last change to this record.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsLastModifiedBy :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Text)
lrrrsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE lrrrsLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | Names of merged datasets.
--
-- /Note:/ Consider using 'mergedDatasetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsMergedDatasetNames :: Lens.Lens' ListRecordsResponse (Core.Maybe [Core.Text])
lrrrsMergedDatasetNames = Lens.field @"mergedDatasetNames"
{-# INLINEABLE lrrrsMergedDatasetNames #-}
{-# DEPRECATED mergedDatasetNames "Use generic-lens or generic-optics with 'mergedDatasetNames' instead"  #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Text)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of all records.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsRecords :: Lens.Lens' ListRecordsResponse (Core.Maybe [Types.Record])
lrrrsRecords = Lens.field @"records"
{-# INLINEABLE lrrrsRecords #-}
{-# DEPRECATED records "Use generic-lens or generic-optics with 'records' instead"  #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsSyncSessionToken :: Lens.Lens' ListRecordsResponse (Core.Maybe Core.Text)
lrrrsSyncSessionToken = Lens.field @"syncSessionToken"
{-# INLINEABLE lrrrsSyncSessionToken #-}
{-# DEPRECATED syncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListRecordsResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
