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
-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specific dataset. The dataset will be deleted permanently,
-- and the action can\'t be undone. Datasets that this dataset was merged
-- with will no longer report the merge. Any subsequent operation on this
-- dataset will result in a ResourceNotFoundException.
--
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DeleteDataset
  ( -- * Creating a Request
    DeleteDataset (..),
    newDeleteDataset,

    -- * Request Lenses
    deleteDataset_identityPoolId,
    deleteDataset_identityId,
    deleteDataset_datasetName,

    -- * Destructuring the Response
    DeleteDatasetResponse (..),
    newDeleteDatasetResponse,

    -- * Response Lenses
    deleteDatasetResponse_dataset,
    deleteDatasetResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the specific dataset.
--
-- /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Core.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'deleteDataset_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'identityId', 'deleteDataset_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'datasetName', 'deleteDataset_datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
newDeleteDataset ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityId'
  Core.Text ->
  -- | 'datasetName'
  Core.Text ->
  DeleteDataset
newDeleteDataset
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_ =
    DeleteDataset'
      { identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_
      }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
deleteDataset_identityPoolId :: Lens.Lens' DeleteDataset Core.Text
deleteDataset_identityPoolId = Lens.lens (\DeleteDataset' {identityPoolId} -> identityPoolId) (\s@DeleteDataset' {} a -> s {identityPoolId = a} :: DeleteDataset)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
deleteDataset_identityId :: Lens.Lens' DeleteDataset Core.Text
deleteDataset_identityId = Lens.lens (\DeleteDataset' {identityId} -> identityId) (\s@DeleteDataset' {} a -> s {identityId = a} :: DeleteDataset)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
deleteDataset_datasetName :: Lens.Lens' DeleteDataset Core.Text
deleteDataset_datasetName = Lens.lens (\DeleteDataset' {datasetName} -> datasetName) (\s@DeleteDataset' {} a -> s {datasetName = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDatasetResponse'
            Core.<$> (x Core..?> "Dataset")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDataset

instance Core.NFData DeleteDataset

instance Core.ToHeaders DeleteDataset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/identities/",
        Core.toBS identityId,
        "/datasets/",
        Core.toBS datasetName
      ]

instance Core.ToQuery DeleteDataset where
  toQuery = Core.const Core.mempty

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | A collection of data for an identity pool. An identity pool can have
    -- multiple datasets. A dataset is per identity and can be general or
    -- associated with a particular entity in an application (like a saved
    -- game). Datasets are automatically created if they don\'t exist. Data is
    -- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
    dataset :: Core.Maybe Dataset,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataset', 'deleteDatasetResponse_dataset' - A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- 'httpStatus', 'deleteDatasetResponse_httpStatus' - The response's http status code.
newDeleteDatasetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDatasetResponse
newDeleteDatasetResponse pHttpStatus_ =
  DeleteDatasetResponse'
    { dataset = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
deleteDatasetResponse_dataset :: Lens.Lens' DeleteDatasetResponse (Core.Maybe Dataset)
deleteDatasetResponse_dataset = Lens.lens (\DeleteDatasetResponse' {dataset} -> dataset) (\s@DeleteDatasetResponse' {} a -> s {dataset = a} :: DeleteDatasetResponse)

-- | The response's http status code.
deleteDatasetResponse_httpStatus :: Lens.Lens' DeleteDatasetResponse Core.Int
deleteDatasetResponse_httpStatus = Lens.lens (\DeleteDatasetResponse' {httpStatus} -> httpStatus) (\s@DeleteDatasetResponse' {} a -> s {httpStatus = a} :: DeleteDatasetResponse)

instance Core.NFData DeleteDatasetResponse
