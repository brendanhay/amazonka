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
-- Module      : Amazonka.OpenSearchServerless.UpdateCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an OpenSearch Serverless collection.
module Amazonka.OpenSearchServerless.UpdateCollection
  ( -- * Creating a Request
    UpdateCollection (..),
    newUpdateCollection,

    -- * Request Lenses
    updateCollection_clientToken,
    updateCollection_description,
    updateCollection_id,

    -- * Destructuring the Response
    UpdateCollectionResponse (..),
    newUpdateCollectionResponse,

    -- * Response Lenses
    updateCollectionResponse_updateCollectionDetail,
    updateCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCollection' smart constructor.
data UpdateCollection = UpdateCollection'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the collection.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateCollection_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'updateCollection_description' - A description of the collection.
--
-- 'id', 'updateCollection_id' - The unique identifier of the collection.
newUpdateCollection ::
  -- | 'id'
  Prelude.Text ->
  UpdateCollection
newUpdateCollection pId_ =
  UpdateCollection'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
updateCollection_clientToken :: Lens.Lens' UpdateCollection (Prelude.Maybe Prelude.Text)
updateCollection_clientToken = Lens.lens (\UpdateCollection' {clientToken} -> clientToken) (\s@UpdateCollection' {} a -> s {clientToken = a} :: UpdateCollection)

-- | A description of the collection.
updateCollection_description :: Lens.Lens' UpdateCollection (Prelude.Maybe Prelude.Text)
updateCollection_description = Lens.lens (\UpdateCollection' {description} -> description) (\s@UpdateCollection' {} a -> s {description = a} :: UpdateCollection)

-- | The unique identifier of the collection.
updateCollection_id :: Lens.Lens' UpdateCollection Prelude.Text
updateCollection_id = Lens.lens (\UpdateCollection' {id} -> id) (\s@UpdateCollection' {} a -> s {id = a} :: UpdateCollection)

instance Core.AWSRequest UpdateCollection where
  type
    AWSResponse UpdateCollection =
      UpdateCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCollectionResponse'
            Prelude.<$> (x Data..?> "updateCollectionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCollection where
  hashWithSalt _salt UpdateCollection' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateCollection where
  rnf UpdateCollection' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCollection where
  toJSON UpdateCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCollectionResponse' smart constructor.
data UpdateCollectionResponse = UpdateCollectionResponse'
  { -- | Details about the updated collection.
    updateCollectionDetail :: Prelude.Maybe UpdateCollectionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateCollectionDetail', 'updateCollectionResponse_updateCollectionDetail' - Details about the updated collection.
--
-- 'httpStatus', 'updateCollectionResponse_httpStatus' - The response's http status code.
newUpdateCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCollectionResponse
newUpdateCollectionResponse pHttpStatus_ =
  UpdateCollectionResponse'
    { updateCollectionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated collection.
updateCollectionResponse_updateCollectionDetail :: Lens.Lens' UpdateCollectionResponse (Prelude.Maybe UpdateCollectionDetail)
updateCollectionResponse_updateCollectionDetail = Lens.lens (\UpdateCollectionResponse' {updateCollectionDetail} -> updateCollectionDetail) (\s@UpdateCollectionResponse' {} a -> s {updateCollectionDetail = a} :: UpdateCollectionResponse)

-- | The response's http status code.
updateCollectionResponse_httpStatus :: Lens.Lens' UpdateCollectionResponse Prelude.Int
updateCollectionResponse_httpStatus = Lens.lens (\UpdateCollectionResponse' {httpStatus} -> httpStatus) (\s@UpdateCollectionResponse' {} a -> s {httpStatus = a} :: UpdateCollectionResponse)

instance Prelude.NFData UpdateCollectionResponse where
  rnf UpdateCollectionResponse' {..} =
    Prelude.rnf updateCollectionDetail
      `Prelude.seq` Prelude.rnf httpStatus
