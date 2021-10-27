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
-- Module      : Network.AWS.Kendra.DeleteDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Kendra data source. An exception is not thrown if the
-- data source is already being deleted. While the data source is being
-- deleted, the @Status@ field returned by a call to the
-- @DescribeDataSource@ operation is set to @DELETING@. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/delete-data-source.html Deleting Data Sources>.
module Network.AWS.Kendra.DeleteDataSource
  ( -- * Creating a Request
    DeleteDataSource (..),
    newDeleteDataSource,

    -- * Request Lenses
    deleteDataSource_id,
    deleteDataSource_indexId,

    -- * Destructuring the Response
    DeleteDataSourceResponse (..),
    newDeleteDataSourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | The unique identifier of the data source to delete.
    id :: Prelude.Text,
    -- | The unique identifier of the index associated with the data source.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteDataSource_id' - The unique identifier of the data source to delete.
--
-- 'indexId', 'deleteDataSource_indexId' - The unique identifier of the index associated with the data source.
newDeleteDataSource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteDataSource
newDeleteDataSource pId_ pIndexId_ =
  DeleteDataSource' {id = pId_, indexId = pIndexId_}

-- | The unique identifier of the data source to delete.
deleteDataSource_id :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_id = Lens.lens (\DeleteDataSource' {id} -> id) (\s@DeleteDataSource' {} a -> s {id = a} :: DeleteDataSource)

-- | The unique identifier of the index associated with the data source.
deleteDataSource_indexId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_indexId = Lens.lens (\DeleteDataSource' {indexId} -> indexId) (\s@DeleteDataSource' {} a -> s {indexId = a} :: DeleteDataSource)

instance Core.AWSRequest DeleteDataSource where
  type
    AWSResponse DeleteDataSource =
      DeleteDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteDataSourceResponse'

instance Prelude.Hashable DeleteDataSource

instance Prelude.NFData DeleteDataSource

instance Core.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DeleteDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDataSource where
  toJSON DeleteDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DeleteDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDataSourceResponse ::
  DeleteDataSourceResponse
newDeleteDataSourceResponse =
  DeleteDataSourceResponse'

instance Prelude.NFData DeleteDataSourceResponse
