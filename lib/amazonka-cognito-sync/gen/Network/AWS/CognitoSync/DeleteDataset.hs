{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specific dataset. The dataset will be deleted permanently, and the action can't be undone. Datasets that this dataset was merged with will no longer report the merge. Any subsequent operation on this dataset will result in a ResourceNotFoundException.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DeleteDataset
  ( -- * Creating a request
    DeleteDataset (..),
    mkDeleteDataset,

    -- ** Request lenses
    ddIdentityPoolId,
    ddDatasetName,
    ddIdentityId,

    -- * Destructuring the response
    DeleteDatasetResponse (..),
    mkDeleteDatasetResponse,

    -- ** Response lenses
    drsDataset,
    drsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete the specific dataset.
--
-- /See:/ 'mkDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Lude.Text,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataset' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkDeleteDataset ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  DeleteDataset
mkDeleteDataset pIdentityPoolId_ pDatasetName_ pIdentityId_ =
  DeleteDataset'
    { identityPoolId = pIdentityPoolId_,
      datasetName = pDatasetName_,
      identityId = pIdentityId_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddIdentityPoolId :: Lens.Lens' DeleteDataset Lude.Text
ddIdentityPoolId = Lens.lens (identityPoolId :: DeleteDataset -> Lude.Text) (\s a -> s {identityPoolId = a} :: DeleteDataset)
{-# DEPRECATED ddIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatasetName :: Lens.Lens' DeleteDataset Lude.Text
ddDatasetName = Lens.lens (datasetName :: DeleteDataset -> Lude.Text) (\s a -> s {datasetName = a} :: DeleteDataset)
{-# DEPRECATED ddDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddIdentityId :: Lens.Lens' DeleteDataset Lude.Text
ddIdentityId = Lens.lens (identityId :: DeleteDataset -> Lude.Text) (\s a -> s {identityId = a} :: DeleteDataset)
{-# DEPRECATED ddIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest DeleteDataset where
  type Rs DeleteDataset = DeleteDatasetResponse
  request = Req.delete cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDatasetResponse'
            Lude.<$> (x Lude..?> "Dataset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDataset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets/",
        Lude.toBS datasetName
      ]

instance Lude.ToQuery DeleteDataset where
  toQuery = Lude.const Lude.mempty

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'mkDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
    dataset :: Lude.Maybe Dataset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatasetResponse' with the minimum fields required to make a request.
--
-- * 'dataset' - A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
-- * 'responseStatus' - The response status code.
mkDeleteDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDatasetResponse
mkDeleteDatasetResponse pResponseStatus_ =
  DeleteDatasetResponse'
    { dataset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDataset :: Lens.Lens' DeleteDatasetResponse (Lude.Maybe Dataset)
drsDataset = Lens.lens (dataset :: DeleteDatasetResponse -> Lude.Maybe Dataset) (\s a -> s {dataset = a} :: DeleteDatasetResponse)
{-# DEPRECATED drsDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDatasetResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDatasetResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
