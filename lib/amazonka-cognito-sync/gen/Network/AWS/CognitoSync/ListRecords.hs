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
    lrLastSyncCount,
    lrNextToken,
    lrSyncSessionToken,
    lrDatasetName,
    lrIdentityId,
    lrMaxResults,

    -- * Destructuring the response
    ListRecordsResponse (..),
    mkListRecordsResponse,

    -- ** Response lenses
    lrrsDatasetDeletedAfterRequestedSyncCount,
    lrrsDatasetExists,
    lrrsCount,
    lrrsRecords,
    lrrsNextToken,
    lrrsMergedDatasetNames,
    lrrsSyncSessionToken,
    lrrsLastModifiedBy,
    lrrsDatasetSyncCount,
    lrrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for a list of records.
--
-- /See:/ 'mkListRecords' smart constructor.
data ListRecords = ListRecords'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text,
    -- | The last server sync count for this record.
    lastSyncCount :: Lude.Maybe Lude.Integer,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Lude.Maybe Lude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Lude.Text,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Text,
    -- | The maximum number of results to be returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecords' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'lastSyncCount' - The last server sync count for this record.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
-- * 'syncSessionToken' - A token containing a session ID, identity ID, and expiration.
-- * 'datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'maxResults' - The maximum number of results to be returned.
mkListRecords ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  ListRecords
mkListRecords pIdentityPoolId_ pDatasetName_ pIdentityId_ =
  ListRecords'
    { identityPoolId = pIdentityPoolId_,
      lastSyncCount = Lude.Nothing,
      nextToken = Lude.Nothing,
      syncSessionToken = Lude.Nothing,
      datasetName = pDatasetName_,
      identityId = pIdentityId_,
      maxResults = Lude.Nothing
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityPoolId :: Lens.Lens' ListRecords Lude.Text
lrIdentityPoolId = Lens.lens (identityPoolId :: ListRecords -> Lude.Text) (\s a -> s {identityPoolId = a} :: ListRecords)
{-# DEPRECATED lrIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The last server sync count for this record.
--
-- /Note:/ Consider using 'lastSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLastSyncCount :: Lens.Lens' ListRecords (Lude.Maybe Lude.Integer)
lrLastSyncCount = Lens.lens (lastSyncCount :: ListRecords -> Lude.Maybe Lude.Integer) (\s a -> s {lastSyncCount = a} :: ListRecords)
{-# DEPRECATED lrLastSyncCount "Use generic-lens or generic-optics with 'lastSyncCount' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRecords (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRecords -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRecords)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSyncSessionToken :: Lens.Lens' ListRecords (Lude.Maybe Lude.Text)
lrSyncSessionToken = Lens.lens (syncSessionToken :: ListRecords -> Lude.Maybe Lude.Text) (\s a -> s {syncSessionToken = a} :: ListRecords)
{-# DEPRECATED lrSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrDatasetName :: Lens.Lens' ListRecords Lude.Text
lrDatasetName = Lens.lens (datasetName :: ListRecords -> Lude.Text) (\s a -> s {datasetName = a} :: ListRecords)
{-# DEPRECATED lrDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrIdentityId :: Lens.Lens' ListRecords Lude.Text
lrIdentityId = Lens.lens (identityId :: ListRecords -> Lude.Text) (\s a -> s {identityId = a} :: ListRecords)
{-# DEPRECATED lrIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRecords (Lude.Maybe Lude.Int)
lrMaxResults = Lens.lens (maxResults :: ListRecords -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListRecords)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListRecords where
  type Rs ListRecords = ListRecordsResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRecordsResponse'
            Lude.<$> (x Lude..?> "DatasetDeletedAfterRequestedSyncCount")
            Lude.<*> (x Lude..?> "DatasetExists")
            Lude.<*> (x Lude..?> "Count")
            Lude.<*> (x Lude..?> "Records" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MergedDatasetNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "SyncSessionToken")
            Lude.<*> (x Lude..?> "LastModifiedBy")
            Lude.<*> (x Lude..?> "DatasetSyncCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRecords where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListRecords where
  toPath ListRecords' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets/",
        Lude.toBS datasetName,
        "/records"
      ]

instance Lude.ToQuery ListRecords where
  toQuery ListRecords' {..} =
    Lude.mconcat
      [ "lastSyncCount" Lude.=: lastSyncCount,
        "nextToken" Lude.=: nextToken,
        "syncSessionToken" Lude.=: syncSessionToken,
        "maxResults" Lude.=: maxResults
      ]

-- | Returned for a successful ListRecordsRequest.
--
-- /See:/ 'mkListRecordsResponse' smart constructor.
data ListRecordsResponse = ListRecordsResponse'
  { -- | A boolean value specifying whether to delete the dataset locally.
    datasetDeletedAfterRequestedSyncCount :: Lude.Maybe Lude.Bool,
    -- | Indicates whether the dataset exists.
    datasetExists :: Lude.Maybe Lude.Bool,
    -- | Total number of records.
    count :: Lude.Maybe Lude.Int,
    -- | A list of all records.
    records :: Lude.Maybe [Record],
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Names of merged datasets.
    mergedDatasetNames :: Lude.Maybe [Lude.Text],
    -- | A token containing a session ID, identity ID, and expiration.
    syncSessionToken :: Lude.Maybe Lude.Text,
    -- | The user/device that made the last change to this record.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | Server sync count for this dataset.
    datasetSyncCount :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecordsResponse' with the minimum fields required to make a request.
--
-- * 'datasetDeletedAfterRequestedSyncCount' - A boolean value specifying whether to delete the dataset locally.
-- * 'datasetExists' - Indicates whether the dataset exists.
-- * 'count' - Total number of records.
-- * 'records' - A list of all records.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
-- * 'mergedDatasetNames' - Names of merged datasets.
-- * 'syncSessionToken' - A token containing a session ID, identity ID, and expiration.
-- * 'lastModifiedBy' - The user/device that made the last change to this record.
-- * 'datasetSyncCount' - Server sync count for this dataset.
-- * 'responseStatus' - The response status code.
mkListRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRecordsResponse
mkListRecordsResponse pResponseStatus_ =
  ListRecordsResponse'
    { datasetDeletedAfterRequestedSyncCount =
        Lude.Nothing,
      datasetExists = Lude.Nothing,
      count = Lude.Nothing,
      records = Lude.Nothing,
      nextToken = Lude.Nothing,
      mergedDatasetNames = Lude.Nothing,
      syncSessionToken = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      datasetSyncCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A boolean value specifying whether to delete the dataset locally.
--
-- /Note:/ Consider using 'datasetDeletedAfterRequestedSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsDatasetDeletedAfterRequestedSyncCount :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Bool)
lrrsDatasetDeletedAfterRequestedSyncCount = Lens.lens (datasetDeletedAfterRequestedSyncCount :: ListRecordsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {datasetDeletedAfterRequestedSyncCount = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsDatasetDeletedAfterRequestedSyncCount "Use generic-lens or generic-optics with 'datasetDeletedAfterRequestedSyncCount' instead." #-}

-- | Indicates whether the dataset exists.
--
-- /Note:/ Consider using 'datasetExists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsDatasetExists :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Bool)
lrrsDatasetExists = Lens.lens (datasetExists :: ListRecordsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {datasetExists = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsDatasetExists "Use generic-lens or generic-optics with 'datasetExists' instead." #-}

-- | Total number of records.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsCount :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Int)
lrrsCount = Lens.lens (count :: ListRecordsResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | A list of all records.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRecords :: Lens.Lens' ListRecordsResponse (Lude.Maybe [Record])
lrrsRecords = Lens.lens (records :: ListRecordsResponse -> Lude.Maybe [Record]) (\s a -> s {records = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Names of merged datasets.
--
-- /Note:/ Consider using 'mergedDatasetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsMergedDatasetNames :: Lens.Lens' ListRecordsResponse (Lude.Maybe [Lude.Text])
lrrsMergedDatasetNames = Lens.lens (mergedDatasetNames :: ListRecordsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {mergedDatasetNames = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsMergedDatasetNames "Use generic-lens or generic-optics with 'mergedDatasetNames' instead." #-}

-- | A token containing a session ID, identity ID, and expiration.
--
-- /Note:/ Consider using 'syncSessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsSyncSessionToken :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Text)
lrrsSyncSessionToken = Lens.lens (syncSessionToken :: ListRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {syncSessionToken = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsSyncSessionToken "Use generic-lens or generic-optics with 'syncSessionToken' instead." #-}

-- | The user/device that made the last change to this record.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsLastModifiedBy :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Text)
lrrsLastModifiedBy = Lens.lens (lastModifiedBy :: ListRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | Server sync count for this dataset.
--
-- /Note:/ Consider using 'datasetSyncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsDatasetSyncCount :: Lens.Lens' ListRecordsResponse (Lude.Maybe Lude.Integer)
lrrsDatasetSyncCount = Lens.lens (datasetSyncCount :: ListRecordsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {datasetSyncCount = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsDatasetSyncCount "Use generic-lens or generic-optics with 'datasetSyncCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRecordsResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRecordsResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
