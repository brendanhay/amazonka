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
-- Module      : Amazonka.Omics.DeleteReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a genome reference.
module Amazonka.Omics.DeleteReference
  ( -- * Creating a Request
    DeleteReference (..),
    newDeleteReference,

    -- * Request Lenses
    deleteReference_id,
    deleteReference_referenceStoreId,

    -- * Destructuring the Response
    DeleteReferenceResponse (..),
    newDeleteReferenceResponse,

    -- * Response Lenses
    deleteReferenceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReference' smart constructor.
data DeleteReference = DeleteReference'
  { -- | The reference\'s ID.
    id :: Prelude.Text,
    -- | The reference\'s store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteReference_id' - The reference\'s ID.
--
-- 'referenceStoreId', 'deleteReference_referenceStoreId' - The reference\'s store ID.
newDeleteReference ::
  -- | 'id'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  DeleteReference
newDeleteReference pId_ pReferenceStoreId_ =
  DeleteReference'
    { id = pId_,
      referenceStoreId = pReferenceStoreId_
    }

-- | The reference\'s ID.
deleteReference_id :: Lens.Lens' DeleteReference Prelude.Text
deleteReference_id = Lens.lens (\DeleteReference' {id} -> id) (\s@DeleteReference' {} a -> s {id = a} :: DeleteReference)

-- | The reference\'s store ID.
deleteReference_referenceStoreId :: Lens.Lens' DeleteReference Prelude.Text
deleteReference_referenceStoreId = Lens.lens (\DeleteReference' {referenceStoreId} -> referenceStoreId) (\s@DeleteReference' {} a -> s {referenceStoreId = a} :: DeleteReference)

instance Core.AWSRequest DeleteReference where
  type
    AWSResponse DeleteReference =
      DeleteReferenceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReference where
  hashWithSalt _salt DeleteReference' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData DeleteReference where
  rnf DeleteReference' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf referenceStoreId

instance Data.ToHeaders DeleteReference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteReference where
  toPath DeleteReference' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/reference/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteReference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReferenceResponse' smart constructor.
data DeleteReferenceResponse = DeleteReferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReferenceResponse_httpStatus' - The response's http status code.
newDeleteReferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReferenceResponse
newDeleteReferenceResponse pHttpStatus_ =
  DeleteReferenceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteReferenceResponse_httpStatus :: Lens.Lens' DeleteReferenceResponse Prelude.Int
deleteReferenceResponse_httpStatus = Lens.lens (\DeleteReferenceResponse' {httpStatus} -> httpStatus) (\s@DeleteReferenceResponse' {} a -> s {httpStatus = a} :: DeleteReferenceResponse)

instance Prelude.NFData DeleteReferenceResponse where
  rnf DeleteReferenceResponse' {..} =
    Prelude.rnf httpStatus
