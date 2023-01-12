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
-- Module      : Amazonka.Kendra.DeleteDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Kendra data source connector. An exception is not
-- thrown if the data source is already being deleted. While the data
-- source is being deleted, the @Status@ field returned by a call to the
-- @DescribeDataSource@ API is set to @DELETING@. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/delete-data-source.html Deleting Data Sources>.
module Amazonka.Kendra.DeleteDataSource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | The identifier of the data source connector you want to delete.
    id :: Prelude.Text,
    -- | The identifier of the index used with the data source connector.
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
-- 'id', 'deleteDataSource_id' - The identifier of the data source connector you want to delete.
--
-- 'indexId', 'deleteDataSource_indexId' - The identifier of the index used with the data source connector.
newDeleteDataSource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteDataSource
newDeleteDataSource pId_ pIndexId_ =
  DeleteDataSource' {id = pId_, indexId = pIndexId_}

-- | The identifier of the data source connector you want to delete.
deleteDataSource_id :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_id = Lens.lens (\DeleteDataSource' {id} -> id) (\s@DeleteDataSource' {} a -> s {id = a} :: DeleteDataSource)

-- | The identifier of the index used with the data source connector.
deleteDataSource_indexId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_indexId = Lens.lens (\DeleteDataSource' {indexId} -> indexId) (\s@DeleteDataSource' {} a -> s {indexId = a} :: DeleteDataSource)

instance Core.AWSRequest DeleteDataSource where
  type
    AWSResponse DeleteDataSource =
      DeleteDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDataSourceResponse'

instance Prelude.Hashable DeleteDataSource where
  hashWithSalt _salt DeleteDataSource' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DeleteDataSource where
  rnf DeleteDataSource' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataSource where
  toJSON DeleteDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath DeleteDataSource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataSource where
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

instance Prelude.NFData DeleteDataSourceResponse where
  rnf _ = ()
