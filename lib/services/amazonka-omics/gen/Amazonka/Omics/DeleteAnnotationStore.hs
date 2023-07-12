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
-- Module      : Amazonka.Omics.DeleteAnnotationStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an annotation store.
module Amazonka.Omics.DeleteAnnotationStore
  ( -- * Creating a Request
    DeleteAnnotationStore (..),
    newDeleteAnnotationStore,

    -- * Request Lenses
    deleteAnnotationStore_force,
    deleteAnnotationStore_name,

    -- * Destructuring the Response
    DeleteAnnotationStoreResponse (..),
    newDeleteAnnotationStoreResponse,

    -- * Response Lenses
    deleteAnnotationStoreResponse_httpStatus,
    deleteAnnotationStoreResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAnnotationStore' smart constructor.
data DeleteAnnotationStore = DeleteAnnotationStore'
  { -- | Whether to force deletion.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The store\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnnotationStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteAnnotationStore_force' - Whether to force deletion.
--
-- 'name', 'deleteAnnotationStore_name' - The store\'s name.
newDeleteAnnotationStore ::
  -- | 'name'
  Prelude.Text ->
  DeleteAnnotationStore
newDeleteAnnotationStore pName_ =
  DeleteAnnotationStore'
    { force = Prelude.Nothing,
      name = pName_
    }

-- | Whether to force deletion.
deleteAnnotationStore_force :: Lens.Lens' DeleteAnnotationStore (Prelude.Maybe Prelude.Bool)
deleteAnnotationStore_force = Lens.lens (\DeleteAnnotationStore' {force} -> force) (\s@DeleteAnnotationStore' {} a -> s {force = a} :: DeleteAnnotationStore)

-- | The store\'s name.
deleteAnnotationStore_name :: Lens.Lens' DeleteAnnotationStore Prelude.Text
deleteAnnotationStore_name = Lens.lens (\DeleteAnnotationStore' {name} -> name) (\s@DeleteAnnotationStore' {} a -> s {name = a} :: DeleteAnnotationStore)

instance Core.AWSRequest DeleteAnnotationStore where
  type
    AWSResponse DeleteAnnotationStore =
      DeleteAnnotationStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAnnotationStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable DeleteAnnotationStore where
  hashWithSalt _salt DeleteAnnotationStore' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAnnotationStore where
  rnf DeleteAnnotationStore' {..} =
    Prelude.rnf force `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteAnnotationStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAnnotationStore where
  toPath DeleteAnnotationStore' {..} =
    Prelude.mconcat
      ["/annotationStore/", Data.toBS name]

instance Data.ToQuery DeleteAnnotationStore where
  toQuery DeleteAnnotationStore' {..} =
    Prelude.mconcat ["force" Data.=: force]

-- | /See:/ 'newDeleteAnnotationStoreResponse' smart constructor.
data DeleteAnnotationStoreResponse = DeleteAnnotationStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s status.
    status :: StoreStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnnotationStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnnotationStoreResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteAnnotationStoreResponse_status' - The store\'s status.
newDeleteAnnotationStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  StoreStatus ->
  DeleteAnnotationStoreResponse
newDeleteAnnotationStoreResponse
  pHttpStatus_
  pStatus_ =
    DeleteAnnotationStoreResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
deleteAnnotationStoreResponse_httpStatus :: Lens.Lens' DeleteAnnotationStoreResponse Prelude.Int
deleteAnnotationStoreResponse_httpStatus = Lens.lens (\DeleteAnnotationStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteAnnotationStoreResponse' {} a -> s {httpStatus = a} :: DeleteAnnotationStoreResponse)

-- | The store\'s status.
deleteAnnotationStoreResponse_status :: Lens.Lens' DeleteAnnotationStoreResponse StoreStatus
deleteAnnotationStoreResponse_status = Lens.lens (\DeleteAnnotationStoreResponse' {status} -> status) (\s@DeleteAnnotationStoreResponse' {} a -> s {status = a} :: DeleteAnnotationStoreResponse)

instance Prelude.NFData DeleteAnnotationStoreResponse where
  rnf DeleteAnnotationStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
