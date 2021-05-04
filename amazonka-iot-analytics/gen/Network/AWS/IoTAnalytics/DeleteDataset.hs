{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.DeleteDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified dataset.
--
-- You do not have to delete the content of the dataset before you perform
-- this operation.
module Network.AWS.IoTAnalytics.DeleteDataset
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

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | The name of the data set to delete.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetName', 'deleteDataset_datasetName' - The name of the data set to delete.
newDeleteDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  DeleteDataset
newDeleteDataset pDatasetName_ =
  DeleteDataset' {datasetName = pDatasetName_}

-- | The name of the data set to delete.
deleteDataset_datasetName :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetName = Lens.lens (\DeleteDataset' {datasetName} -> datasetName) (\s@DeleteDataset' {} a -> s {datasetName = a} :: DeleteDataset)

instance Prelude.AWSRequest DeleteDataset where
  type Rs DeleteDataset = DeleteDatasetResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteDatasetResponse'

instance Prelude.Hashable DeleteDataset

instance Prelude.NFData DeleteDataset

instance Prelude.ToHeaders DeleteDataset where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Prelude.mconcat
      ["/datasets/", Prelude.toBS datasetName]

instance Prelude.ToQuery DeleteDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatasetResponse ::
  DeleteDatasetResponse
newDeleteDatasetResponse = DeleteDatasetResponse'

instance Prelude.NFData DeleteDatasetResponse
