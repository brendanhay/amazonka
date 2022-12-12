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
-- Module      : Amazonka.Omics.DeleteReferenceStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a genome reference store.
module Amazonka.Omics.DeleteReferenceStore
  ( -- * Creating a Request
    DeleteReferenceStore (..),
    newDeleteReferenceStore,

    -- * Request Lenses
    deleteReferenceStore_id,

    -- * Destructuring the Response
    DeleteReferenceStoreResponse (..),
    newDeleteReferenceStoreResponse,

    -- * Response Lenses
    deleteReferenceStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReferenceStore' smart constructor.
data DeleteReferenceStore = DeleteReferenceStore'
  { -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReferenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteReferenceStore_id' - The store\'s ID.
newDeleteReferenceStore ::
  -- | 'id'
  Prelude.Text ->
  DeleteReferenceStore
newDeleteReferenceStore pId_ =
  DeleteReferenceStore' {id = pId_}

-- | The store\'s ID.
deleteReferenceStore_id :: Lens.Lens' DeleteReferenceStore Prelude.Text
deleteReferenceStore_id = Lens.lens (\DeleteReferenceStore' {id} -> id) (\s@DeleteReferenceStore' {} a -> s {id = a} :: DeleteReferenceStore)

instance Core.AWSRequest DeleteReferenceStore where
  type
    AWSResponse DeleteReferenceStore =
      DeleteReferenceStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReferenceStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReferenceStore where
  hashWithSalt _salt DeleteReferenceStore' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteReferenceStore where
  rnf DeleteReferenceStore' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteReferenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteReferenceStore where
  toPath DeleteReferenceStore' {..} =
    Prelude.mconcat ["/referencestore/", Data.toBS id]

instance Data.ToQuery DeleteReferenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReferenceStoreResponse' smart constructor.
data DeleteReferenceStoreResponse = DeleteReferenceStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReferenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReferenceStoreResponse_httpStatus' - The response's http status code.
newDeleteReferenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReferenceStoreResponse
newDeleteReferenceStoreResponse pHttpStatus_ =
  DeleteReferenceStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReferenceStoreResponse_httpStatus :: Lens.Lens' DeleteReferenceStoreResponse Prelude.Int
deleteReferenceStoreResponse_httpStatus = Lens.lens (\DeleteReferenceStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteReferenceStoreResponse' {} a -> s {httpStatus = a} :: DeleteReferenceStoreResponse)

instance Prelude.NFData DeleteReferenceStoreResponse where
  rnf DeleteReferenceStoreResponse' {..} =
    Prelude.rnf httpStatus
