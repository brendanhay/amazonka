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
-- Module      : Network.AWS.Athena.DeleteDataCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data catalog.
module Network.AWS.Athena.DeleteDataCatalog
  ( -- * Creating a Request
    DeleteDataCatalog (..),
    newDeleteDataCatalog,

    -- * Request Lenses
    deleteDataCatalog_name,

    -- * Destructuring the Response
    DeleteDataCatalogResponse (..),
    newDeleteDataCatalogResponse,

    -- * Response Lenses
    deleteDataCatalogResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataCatalog' smart constructor.
data DeleteDataCatalog = DeleteDataCatalog'
  { -- | The name of the data catalog to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteDataCatalog_name' - The name of the data catalog to delete.
newDeleteDataCatalog ::
  -- | 'name'
  Prelude.Text ->
  DeleteDataCatalog
newDeleteDataCatalog pName_ =
  DeleteDataCatalog' {name = pName_}

-- | The name of the data catalog to delete.
deleteDataCatalog_name :: Lens.Lens' DeleteDataCatalog Prelude.Text
deleteDataCatalog_name = Lens.lens (\DeleteDataCatalog' {name} -> name) (\s@DeleteDataCatalog' {} a -> s {name = a} :: DeleteDataCatalog)

instance Prelude.AWSRequest DeleteDataCatalog where
  type Rs DeleteDataCatalog = DeleteDataCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataCatalog

instance Prelude.NFData DeleteDataCatalog

instance Prelude.ToHeaders DeleteDataCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.DeleteDataCatalog" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDataCatalog where
  toJSON DeleteDataCatalog' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteDataCatalog where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDataCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataCatalogResponse' smart constructor.
data DeleteDataCatalogResponse = DeleteDataCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataCatalogResponse_httpStatus' - The response's http status code.
newDeleteDataCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataCatalogResponse
newDeleteDataCatalogResponse pHttpStatus_ =
  DeleteDataCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDataCatalogResponse_httpStatus :: Lens.Lens' DeleteDataCatalogResponse Prelude.Int
deleteDataCatalogResponse_httpStatus = Lens.lens (\DeleteDataCatalogResponse' {httpStatus} -> httpStatus) (\s@DeleteDataCatalogResponse' {} a -> s {httpStatus = a} :: DeleteDataCatalogResponse)

instance Prelude.NFData DeleteDataCatalogResponse
