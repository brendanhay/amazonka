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
-- Module      : Amazonka.OpenSearchServerless.DeleteCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenSearch Serverless collection. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-manage.html Creating and managing Amazon OpenSearch Serverless collections>.
module Amazonka.OpenSearchServerless.DeleteCollection
  ( -- * Creating a Request
    DeleteCollection (..),
    newDeleteCollection,

    -- * Request Lenses
    deleteCollection_clientToken,
    deleteCollection_id,

    -- * Destructuring the Response
    DeleteCollectionResponse (..),
    newDeleteCollectionResponse,

    -- * Response Lenses
    deleteCollectionResponse_deleteCollectionDetail,
    deleteCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCollection' smart constructor.
data DeleteCollection = DeleteCollection'
  { -- | A unique, case-sensitive identifier to ensure idempotency of the
    -- request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the collection. For example, @1iu5usc406kd@.
    -- The ID is part of the collection endpoint. You can also retrieve it
    -- using the
    -- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
    -- API.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteCollection_clientToken' - A unique, case-sensitive identifier to ensure idempotency of the
-- request.
--
-- 'id', 'deleteCollection_id' - The unique identifier of the collection. For example, @1iu5usc406kd@.
-- The ID is part of the collection endpoint. You can also retrieve it
-- using the
-- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
-- API.
newDeleteCollection ::
  -- | 'id'
  Prelude.Text ->
  DeleteCollection
newDeleteCollection pId_ =
  DeleteCollection'
    { clientToken = Prelude.Nothing,
      id = pId_
    }

-- | A unique, case-sensitive identifier to ensure idempotency of the
-- request.
deleteCollection_clientToken :: Lens.Lens' DeleteCollection (Prelude.Maybe Prelude.Text)
deleteCollection_clientToken = Lens.lens (\DeleteCollection' {clientToken} -> clientToken) (\s@DeleteCollection' {} a -> s {clientToken = a} :: DeleteCollection)

-- | The unique identifier of the collection. For example, @1iu5usc406kd@.
-- The ID is part of the collection endpoint. You can also retrieve it
-- using the
-- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
-- API.
deleteCollection_id :: Lens.Lens' DeleteCollection Prelude.Text
deleteCollection_id = Lens.lens (\DeleteCollection' {id} -> id) (\s@DeleteCollection' {} a -> s {id = a} :: DeleteCollection)

instance Core.AWSRequest DeleteCollection where
  type
    AWSResponse DeleteCollection =
      DeleteCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCollectionResponse'
            Prelude.<$> (x Data..?> "deleteCollectionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCollection where
  hashWithSalt _salt DeleteCollection' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteCollection where
  rnf DeleteCollection' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.DeleteCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCollection where
  toJSON DeleteCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DeleteCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCollectionResponse' smart constructor.
data DeleteCollectionResponse = DeleteCollectionResponse'
  { -- | Details of the deleted collection.
    deleteCollectionDetail :: Prelude.Maybe DeleteCollectionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteCollectionDetail', 'deleteCollectionResponse_deleteCollectionDetail' - Details of the deleted collection.
--
-- 'httpStatus', 'deleteCollectionResponse_httpStatus' - The response's http status code.
newDeleteCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCollectionResponse
newDeleteCollectionResponse pHttpStatus_ =
  DeleteCollectionResponse'
    { deleteCollectionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the deleted collection.
deleteCollectionResponse_deleteCollectionDetail :: Lens.Lens' DeleteCollectionResponse (Prelude.Maybe DeleteCollectionDetail)
deleteCollectionResponse_deleteCollectionDetail = Lens.lens (\DeleteCollectionResponse' {deleteCollectionDetail} -> deleteCollectionDetail) (\s@DeleteCollectionResponse' {} a -> s {deleteCollectionDetail = a} :: DeleteCollectionResponse)

-- | The response's http status code.
deleteCollectionResponse_httpStatus :: Lens.Lens' DeleteCollectionResponse Prelude.Int
deleteCollectionResponse_httpStatus = Lens.lens (\DeleteCollectionResponse' {httpStatus} -> httpStatus) (\s@DeleteCollectionResponse' {} a -> s {httpStatus = a} :: DeleteCollectionResponse)

instance Prelude.NFData DeleteCollectionResponse where
  rnf DeleteCollectionResponse' {..} =
    Prelude.rnf deleteCollectionDetail
      `Prelude.seq` Prelude.rnf httpStatus
