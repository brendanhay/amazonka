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
-- Module      : Amazonka.LookoutEquipment.DeleteDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dataset and associated artifacts. The operation will check to
-- see if any inference scheduler or data ingestion job is currently using
-- the dataset, and if there isn\'t, the dataset, its metadata, and any
-- associated data stored in S3 will be deleted. This does not affect any
-- models that used this dataset for training and evaluation, but does
-- prevent it from being used in the future.
module Amazonka.LookoutEquipment.DeleteDataset
  ( -- * Creating a Request
    DeleteDataset (..),
    newDeleteDataset,

    -- * Request Lenses
    deleteDataset_datasetName,

    -- * Destructuring the Response
    DeleteDatasetResponse (..),
    newDeleteDatasetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | The name of the dataset to be deleted.
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
-- 'datasetName', 'deleteDataset_datasetName' - The name of the dataset to be deleted.
newDeleteDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  DeleteDataset
newDeleteDataset pDatasetName_ =
  DeleteDataset' {datasetName = pDatasetName_}

-- | The name of the dataset to be deleted.
deleteDataset_datasetName :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetName = Lens.lens (\DeleteDataset' {datasetName} -> datasetName) (\s@DeleteDataset' {} a -> s {datasetName = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDatasetResponse'

instance Prelude.Hashable DeleteDataset where
  hashWithSalt _salt DeleteDataset' {..} =
    _salt `Prelude.hashWithSalt` datasetName

instance Prelude.NFData DeleteDataset where
  rnf DeleteDataset' {..} = Prelude.rnf datasetName

instance Data.ToHeaders DeleteDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DeleteDataset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataset where
  toJSON DeleteDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatasetName" Data..= datasetName)]
      )

instance Data.ToPath DeleteDataset where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatasetResponse ::
  DeleteDatasetResponse
newDeleteDatasetResponse = DeleteDatasetResponse'

instance Prelude.NFData DeleteDatasetResponse where
  rnf _ = ()
