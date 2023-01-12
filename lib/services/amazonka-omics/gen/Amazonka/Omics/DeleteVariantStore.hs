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
-- Module      : Amazonka.Omics.DeleteVariantStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a variant store.
module Amazonka.Omics.DeleteVariantStore
  ( -- * Creating a Request
    DeleteVariantStore (..),
    newDeleteVariantStore,

    -- * Request Lenses
    deleteVariantStore_force,
    deleteVariantStore_name,

    -- * Destructuring the Response
    DeleteVariantStoreResponse (..),
    newDeleteVariantStoreResponse,

    -- * Response Lenses
    deleteVariantStoreResponse_httpStatus,
    deleteVariantStoreResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVariantStore' smart constructor.
data DeleteVariantStore = DeleteVariantStore'
  { -- | Whether to force deletion.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The store\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVariantStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteVariantStore_force' - Whether to force deletion.
--
-- 'name', 'deleteVariantStore_name' - The store\'s name.
newDeleteVariantStore ::
  -- | 'name'
  Prelude.Text ->
  DeleteVariantStore
newDeleteVariantStore pName_ =
  DeleteVariantStore'
    { force = Prelude.Nothing,
      name = pName_
    }

-- | Whether to force deletion.
deleteVariantStore_force :: Lens.Lens' DeleteVariantStore (Prelude.Maybe Prelude.Bool)
deleteVariantStore_force = Lens.lens (\DeleteVariantStore' {force} -> force) (\s@DeleteVariantStore' {} a -> s {force = a} :: DeleteVariantStore)

-- | The store\'s name.
deleteVariantStore_name :: Lens.Lens' DeleteVariantStore Prelude.Text
deleteVariantStore_name = Lens.lens (\DeleteVariantStore' {name} -> name) (\s@DeleteVariantStore' {} a -> s {name = a} :: DeleteVariantStore)

instance Core.AWSRequest DeleteVariantStore where
  type
    AWSResponse DeleteVariantStore =
      DeleteVariantStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVariantStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable DeleteVariantStore where
  hashWithSalt _salt DeleteVariantStore' {..} =
    _salt `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteVariantStore where
  rnf DeleteVariantStore' {..} =
    Prelude.rnf force `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteVariantStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVariantStore where
  toPath DeleteVariantStore' {..} =
    Prelude.mconcat ["/variantStore/", Data.toBS name]

instance Data.ToQuery DeleteVariantStore where
  toQuery DeleteVariantStore' {..} =
    Prelude.mconcat ["force" Data.=: force]

-- | /See:/ 'newDeleteVariantStoreResponse' smart constructor.
data DeleteVariantStoreResponse = DeleteVariantStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The store\'s status.
    status :: StoreStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVariantStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVariantStoreResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteVariantStoreResponse_status' - The store\'s status.
newDeleteVariantStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  StoreStatus ->
  DeleteVariantStoreResponse
newDeleteVariantStoreResponse pHttpStatus_ pStatus_ =
  DeleteVariantStoreResponse'
    { httpStatus =
        pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteVariantStoreResponse_httpStatus :: Lens.Lens' DeleteVariantStoreResponse Prelude.Int
deleteVariantStoreResponse_httpStatus = Lens.lens (\DeleteVariantStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteVariantStoreResponse' {} a -> s {httpStatus = a} :: DeleteVariantStoreResponse)

-- | The store\'s status.
deleteVariantStoreResponse_status :: Lens.Lens' DeleteVariantStoreResponse StoreStatus
deleteVariantStoreResponse_status = Lens.lens (\DeleteVariantStoreResponse' {status} -> status) (\s@DeleteVariantStoreResponse' {} a -> s {status = a} :: DeleteVariantStoreResponse)

instance Prelude.NFData DeleteVariantStoreResponse where
  rnf DeleteVariantStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
