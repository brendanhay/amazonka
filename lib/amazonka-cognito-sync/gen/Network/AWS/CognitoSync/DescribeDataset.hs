{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets meta data about a dataset by identity and dataset name. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.DescribeDataset
  ( -- * Creating a request
    DescribeDataset (..),
    mkDescribeDataset,

    -- ** Request lenses
    ddfIdentityPoolId,
    ddfDatasetName,
    ddfIdentityId,

    -- * Destructuring the response
    DescribeDatasetResponse (..),
    mkDescribeDatasetResponse,

    -- ** Response lenses
    ddrsDataset,
    ddrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for meta data about a dataset (creation date, number of records, size) by owner and dataset name.
--
-- /See:/ 'mkDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Lude.Text,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDataset' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkDescribeDataset ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'datasetName'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  DescribeDataset
mkDescribeDataset pIdentityPoolId_ pDatasetName_ pIdentityId_ =
  DescribeDataset'
    { identityPoolId = pIdentityPoolId_,
      datasetName = pDatasetName_,
      identityId = pIdentityId_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfIdentityPoolId :: Lens.Lens' DescribeDataset Lude.Text
ddfIdentityPoolId = Lens.lens (identityPoolId :: DescribeDataset -> Lude.Text) (\s a -> s {identityPoolId = a} :: DescribeDataset)
{-# DEPRECATED ddfIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDatasetName :: Lens.Lens' DescribeDataset Lude.Text
ddfDatasetName = Lens.lens (datasetName :: DescribeDataset -> Lude.Text) (\s a -> s {datasetName = a} :: DescribeDataset)
{-# DEPRECATED ddfDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfIdentityId :: Lens.Lens' DescribeDataset Lude.Text
ddfIdentityId = Lens.lens (identityId :: DescribeDataset -> Lude.Text) (\s a -> s {identityId = a} :: DescribeDataset)
{-# DEPRECATED ddfIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest DescribeDataset where
  type Rs DescribeDataset = DescribeDatasetResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Lude.<$> (x Lude..?> "Dataset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDataset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeDataset where
  toPath DescribeDataset' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId,
        "/datasets/",
        Lude.toBS datasetName
      ]

instance Lude.ToQuery DescribeDataset where
  toQuery = Lude.const Lude.mempty

-- | Response to a successful DescribeDataset request.
--
-- /See:/ 'mkDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
    dataset :: Lude.Maybe Dataset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDatasetResponse' with the minimum fields required to make a request.
--
-- * 'dataset' - Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
-- * 'responseStatus' - The response status code.
mkDescribeDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDatasetResponse
mkDescribeDatasetResponse pResponseStatus_ =
  DescribeDatasetResponse'
    { dataset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDataset :: Lens.Lens' DescribeDatasetResponse (Lude.Maybe Dataset)
ddrsDataset = Lens.lens (dataset :: DescribeDatasetResponse -> Lude.Maybe Dataset) (\s a -> s {dataset = a} :: DescribeDatasetResponse)
{-# DEPRECATED ddrsDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDatasetResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDatasetResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
