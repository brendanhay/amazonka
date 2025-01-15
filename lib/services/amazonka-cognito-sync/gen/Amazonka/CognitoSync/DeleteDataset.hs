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
-- Module      : Amazonka.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.CognitoSync.DeleteDataset
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

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete the specific dataset.
--
-- /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityPoolId :: Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
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
deleteDataset_identityPoolId :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_identityPoolId = Lens.lens (\DeleteDataset' {identityPoolId} -> identityPoolId) (\s@DeleteDataset' {} a -> s {identityPoolId = a} :: DeleteDataset)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
deleteDataset_identityId :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_identityId = Lens.lens (\DeleteDataset' {identityId} -> identityId) (\s@DeleteDataset' {} a -> s {identityId = a} :: DeleteDataset)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
deleteDataset_datasetName :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetName = Lens.lens (\DeleteDataset' {datasetName} -> datasetName) (\s@DeleteDataset' {} a -> s {datasetName = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDatasetResponse'
            Prelude.<$> (x Data..?> "Dataset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataset where
  hashWithSalt _salt DeleteDataset' {..} =
    _salt
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` datasetName

instance Prelude.NFData DeleteDataset where
  rnf DeleteDataset' {..} =
    Prelude.rnf identityPoolId `Prelude.seq`
      Prelude.rnf identityId `Prelude.seq`
        Prelude.rnf datasetName

instance Data.ToHeaders DeleteDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/identities/",
        Data.toBS identityId,
        "/datasets/",
        Data.toBS datasetName
      ]

instance Data.ToQuery DeleteDataset where
  toQuery = Prelude.const Prelude.mempty

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | A collection of data for an identity pool. An identity pool can have
    -- multiple datasets. A dataset is per identity and can be general or
    -- associated with a particular entity in an application (like a saved
    -- game). Datasets are automatically created if they don\'t exist. Data is
    -- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
    dataset :: Prelude.Maybe Dataset,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDatasetResponse
newDeleteDatasetResponse pHttpStatus_ =
  DeleteDatasetResponse'
    { dataset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
deleteDatasetResponse_dataset :: Lens.Lens' DeleteDatasetResponse (Prelude.Maybe Dataset)
deleteDatasetResponse_dataset = Lens.lens (\DeleteDatasetResponse' {dataset} -> dataset) (\s@DeleteDatasetResponse' {} a -> s {dataset = a} :: DeleteDatasetResponse)

-- | The response's http status code.
deleteDatasetResponse_httpStatus :: Lens.Lens' DeleteDatasetResponse Prelude.Int
deleteDatasetResponse_httpStatus = Lens.lens (\DeleteDatasetResponse' {httpStatus} -> httpStatus) (\s@DeleteDatasetResponse' {} a -> s {httpStatus = a} :: DeleteDatasetResponse)

instance Prelude.NFData DeleteDatasetResponse where
  rnf DeleteDatasetResponse' {..} =
    Prelude.rnf dataset `Prelude.seq`
      Prelude.rnf httpStatus
