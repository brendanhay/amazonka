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
-- Module      : Amazonka.Rekognition.DeleteDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Rekognition Custom Labels dataset. Deleting a
-- dataset might take while. Use DescribeDataset to check the current
-- status. The dataset is still deleting if the value of @Status@ is
-- @DELETE_IN_PROGRESS@. If you try to access the dataset after it is
-- deleted, you get a @ResourceNotFoundException@ exception.
--
-- You can\'t delete a dataset while it is creating (@Status@ =
-- @CREATE_IN_PROGRESS@) or if the dataset is updating (@Status@ =
-- @UPDATE_IN_PROGRESS@).
--
-- This operation requires permissions to perform the
-- @rekognition:DeleteDataset@ action.
module Amazonka.Rekognition.DeleteDataset
  ( -- * Creating a Request
    DeleteDataset (..),
    newDeleteDataset,

    -- * Request Lenses
    deleteDataset_datasetArn,

    -- * Destructuring the Response
    DeleteDatasetResponse (..),
    newDeleteDatasetResponse,

    -- * Response Lenses
    deleteDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | The ARN of the Amazon Rekognition Custom Labels dataset that you want to
    -- delete.
    datasetArn :: Prelude.Text
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
-- 'datasetArn', 'deleteDataset_datasetArn' - The ARN of the Amazon Rekognition Custom Labels dataset that you want to
-- delete.
newDeleteDataset ::
  -- | 'datasetArn'
  Prelude.Text ->
  DeleteDataset
newDeleteDataset pDatasetArn_ =
  DeleteDataset' {datasetArn = pDatasetArn_}

-- | The ARN of the Amazon Rekognition Custom Labels dataset that you want to
-- delete.
deleteDataset_datasetArn :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetArn = Lens.lens (\DeleteDataset' {datasetArn} -> datasetArn) (\s@DeleteDataset' {} a -> s {datasetArn = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataset where
  hashWithSalt _salt DeleteDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData DeleteDataset where
  rnf DeleteDataset' {..} = Prelude.rnf datasetArn

instance Data.ToHeaders DeleteDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DeleteDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataset where
  toJSON DeleteDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatasetArn" Data..= datasetArn)]
      )

instance Data.ToPath DeleteDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'deleteDatasetResponse_httpStatus' - The response's http status code.
newDeleteDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatasetResponse
newDeleteDatasetResponse pHttpStatus_ =
  DeleteDatasetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDatasetResponse_httpStatus :: Lens.Lens' DeleteDatasetResponse Prelude.Int
deleteDatasetResponse_httpStatus = Lens.lens (\DeleteDatasetResponse' {httpStatus} -> httpStatus) (\s@DeleteDatasetResponse' {} a -> s {httpStatus = a} :: DeleteDatasetResponse)

instance Prelude.NFData DeleteDatasetResponse where
  rnf DeleteDatasetResponse' {..} =
    Prelude.rnf httpStatus
