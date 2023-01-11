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
-- Module      : Amazonka.Forecast.DeleteDatasetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dataset import job created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation. You can delete only dataset import jobs that have a status of
-- @ACTIVE@ or @CREATE_FAILED@. To get the status, use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>
-- operation.
module Amazonka.Forecast.DeleteDatasetImportJob
  ( -- * Creating a Request
    DeleteDatasetImportJob (..),
    newDeleteDatasetImportJob,

    -- * Request Lenses
    deleteDatasetImportJob_datasetImportJobArn,

    -- * Destructuring the Response
    DeleteDatasetImportJobResponse (..),
    newDeleteDatasetImportJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDatasetImportJob' smart constructor.
data DeleteDatasetImportJob = DeleteDatasetImportJob'
  { -- | The Amazon Resource Name (ARN) of the dataset import job to delete.
    datasetImportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'deleteDatasetImportJob_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job to delete.
newDeleteDatasetImportJob ::
  -- | 'datasetImportJobArn'
  Prelude.Text ->
  DeleteDatasetImportJob
newDeleteDatasetImportJob pDatasetImportJobArn_ =
  DeleteDatasetImportJob'
    { datasetImportJobArn =
        pDatasetImportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset import job to delete.
deleteDatasetImportJob_datasetImportJobArn :: Lens.Lens' DeleteDatasetImportJob Prelude.Text
deleteDatasetImportJob_datasetImportJobArn = Lens.lens (\DeleteDatasetImportJob' {datasetImportJobArn} -> datasetImportJobArn) (\s@DeleteDatasetImportJob' {} a -> s {datasetImportJobArn = a} :: DeleteDatasetImportJob)

instance Core.AWSRequest DeleteDatasetImportJob where
  type
    AWSResponse DeleteDatasetImportJob =
      DeleteDatasetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDatasetImportJobResponse'

instance Prelude.Hashable DeleteDatasetImportJob where
  hashWithSalt _salt DeleteDatasetImportJob' {..} =
    _salt `Prelude.hashWithSalt` datasetImportJobArn

instance Prelude.NFData DeleteDatasetImportJob where
  rnf DeleteDatasetImportJob' {..} =
    Prelude.rnf datasetImportJobArn

instance Data.ToHeaders DeleteDatasetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteDatasetImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDatasetImportJob where
  toJSON DeleteDatasetImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DatasetImportJobArn" Data..= datasetImportJobArn)
          ]
      )

instance Data.ToPath DeleteDatasetImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDatasetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatasetImportJobResponse' smart constructor.
data DeleteDatasetImportJobResponse = DeleteDatasetImportJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatasetImportJobResponse ::
  DeleteDatasetImportJobResponse
newDeleteDatasetImportJobResponse =
  DeleteDatasetImportJobResponse'

instance
  Prelude.NFData
    DeleteDatasetImportJobResponse
  where
  rnf _ = ()
