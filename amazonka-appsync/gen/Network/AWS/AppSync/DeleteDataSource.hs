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
-- Module      : Network.AWS.AppSync.DeleteDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @DataSource@ object.
module Network.AWS.AppSync.DeleteDataSource
  ( -- * Creating a Request
    DeleteDataSource (..),
    newDeleteDataSource,

    -- * Request Lenses
    deleteDataSource_apiId,
    deleteDataSource_name,

    -- * Destructuring the Response
    DeleteDataSourceResponse (..),
    newDeleteDataSourceResponse,

    -- * Response Lenses
    deleteDataSourceResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The name of the data source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteDataSource_apiId' - The API ID.
--
-- 'name', 'deleteDataSource_name' - The name of the data source.
newDeleteDataSource ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteDataSource
newDeleteDataSource pApiId_ pName_ =
  DeleteDataSource' {apiId = pApiId_, name = pName_}

-- | The API ID.
deleteDataSource_apiId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_apiId = Lens.lens (\DeleteDataSource' {apiId} -> apiId) (\s@DeleteDataSource' {} a -> s {apiId = a} :: DeleteDataSource)

-- | The name of the data source.
deleteDataSource_name :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_name = Lens.lens (\DeleteDataSource' {name} -> name) (\s@DeleteDataSource' {} a -> s {name = a} :: DeleteDataSource)

instance Prelude.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataSource

instance Prelude.NFData DeleteDataSource

instance Prelude.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteDataSource where
  toPath DeleteDataSource' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Prelude.toBS apiId,
        "/datasources/",
        Prelude.toBS name
      ]

instance Prelude.ToQuery DeleteDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataSourceResponse_httpStatus' - The response's http status code.
newDeleteDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataSourceResponse
newDeleteDataSourceResponse pHttpStatus_ =
  DeleteDataSourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDataSourceResponse_httpStatus :: Lens.Lens' DeleteDataSourceResponse Prelude.Int
deleteDataSourceResponse_httpStatus = Lens.lens (\DeleteDataSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteDataSourceResponse' {} a -> s {httpStatus = a} :: DeleteDataSourceResponse)

instance Prelude.NFData DeleteDataSourceResponse
