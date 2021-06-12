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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataCatalog' smart constructor.
data DeleteDataCatalog = DeleteDataCatalog'
  { -- | The name of the data catalog to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteDataCatalog
newDeleteDataCatalog pName_ =
  DeleteDataCatalog' {name = pName_}

-- | The name of the data catalog to delete.
deleteDataCatalog_name :: Lens.Lens' DeleteDataCatalog Core.Text
deleteDataCatalog_name = Lens.lens (\DeleteDataCatalog' {name} -> name) (\s@DeleteDataCatalog' {} a -> s {name = a} :: DeleteDataCatalog)

instance Core.AWSRequest DeleteDataCatalog where
  type
    AWSResponse DeleteDataCatalog =
      DeleteDataCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataCatalogResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDataCatalog

instance Core.NFData DeleteDataCatalog

instance Core.ToHeaders DeleteDataCatalog where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.DeleteDataCatalog" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDataCatalog where
  toJSON DeleteDataCatalog' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteDataCatalog where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDataCatalog where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDataCatalogResponse' smart constructor.
data DeleteDataCatalogResponse = DeleteDataCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteDataCatalogResponse
newDeleteDataCatalogResponse pHttpStatus_ =
  DeleteDataCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDataCatalogResponse_httpStatus :: Lens.Lens' DeleteDataCatalogResponse Core.Int
deleteDataCatalogResponse_httpStatus = Lens.lens (\DeleteDataCatalogResponse' {httpStatus} -> httpStatus) (\s@DeleteDataCatalogResponse' {} a -> s {httpStatus = a} :: DeleteDataCatalogResponse)

instance Core.NFData DeleteDataCatalogResponse
