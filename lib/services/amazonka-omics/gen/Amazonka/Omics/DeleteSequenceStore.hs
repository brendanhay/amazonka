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
-- Module      : Amazonka.Omics.DeleteSequenceStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sequence store.
module Amazonka.Omics.DeleteSequenceStore
  ( -- * Creating a Request
    DeleteSequenceStore (..),
    newDeleteSequenceStore,

    -- * Request Lenses
    deleteSequenceStore_id,

    -- * Destructuring the Response
    DeleteSequenceStoreResponse (..),
    newDeleteSequenceStoreResponse,

    -- * Response Lenses
    deleteSequenceStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSequenceStore' smart constructor.
data DeleteSequenceStore = DeleteSequenceStore'
  { -- | The sequence store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSequenceStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSequenceStore_id' - The sequence store\'s ID.
newDeleteSequenceStore ::
  -- | 'id'
  Prelude.Text ->
  DeleteSequenceStore
newDeleteSequenceStore pId_ =
  DeleteSequenceStore' {id = pId_}

-- | The sequence store\'s ID.
deleteSequenceStore_id :: Lens.Lens' DeleteSequenceStore Prelude.Text
deleteSequenceStore_id = Lens.lens (\DeleteSequenceStore' {id} -> id) (\s@DeleteSequenceStore' {} a -> s {id = a} :: DeleteSequenceStore)

instance Core.AWSRequest DeleteSequenceStore where
  type
    AWSResponse DeleteSequenceStore =
      DeleteSequenceStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSequenceStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSequenceStore where
  hashWithSalt _salt DeleteSequenceStore' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSequenceStore where
  rnf DeleteSequenceStore' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteSequenceStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSequenceStore where
  toPath DeleteSequenceStore' {..} =
    Prelude.mconcat ["/sequencestore/", Data.toBS id]

instance Data.ToQuery DeleteSequenceStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSequenceStoreResponse' smart constructor.
data DeleteSequenceStoreResponse = DeleteSequenceStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSequenceStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSequenceStoreResponse_httpStatus' - The response's http status code.
newDeleteSequenceStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSequenceStoreResponse
newDeleteSequenceStoreResponse pHttpStatus_ =
  DeleteSequenceStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSequenceStoreResponse_httpStatus :: Lens.Lens' DeleteSequenceStoreResponse Prelude.Int
deleteSequenceStoreResponse_httpStatus = Lens.lens (\DeleteSequenceStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteSequenceStoreResponse' {} a -> s {httpStatus = a} :: DeleteSequenceStoreResponse)

instance Prelude.NFData DeleteSequenceStoreResponse where
  rnf DeleteSequenceStoreResponse' {..} =
    Prelude.rnf httpStatus
