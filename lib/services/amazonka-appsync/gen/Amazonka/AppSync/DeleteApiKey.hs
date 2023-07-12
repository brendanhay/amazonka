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
-- Module      : Amazonka.AppSync.DeleteApiKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API key.
module Amazonka.AppSync.DeleteApiKey
  ( -- * Creating a Request
    DeleteApiKey (..),
    newDeleteApiKey,

    -- * Request Lenses
    deleteApiKey_apiId,
    deleteApiKey_id,

    -- * Destructuring the Response
    DeleteApiKeyResponse (..),
    newDeleteApiKeyResponse,

    -- * Response Lenses
    deleteApiKeyResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApiKey' smart constructor.
data DeleteApiKey = DeleteApiKey'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The ID for the API key.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteApiKey_apiId' - The API ID.
--
-- 'id', 'deleteApiKey_id' - The ID for the API key.
newDeleteApiKey ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteApiKey
newDeleteApiKey pApiId_ pId_ =
  DeleteApiKey' {apiId = pApiId_, id = pId_}

-- | The API ID.
deleteApiKey_apiId :: Lens.Lens' DeleteApiKey Prelude.Text
deleteApiKey_apiId = Lens.lens (\DeleteApiKey' {apiId} -> apiId) (\s@DeleteApiKey' {} a -> s {apiId = a} :: DeleteApiKey)

-- | The ID for the API key.
deleteApiKey_id :: Lens.Lens' DeleteApiKey Prelude.Text
deleteApiKey_id = Lens.lens (\DeleteApiKey' {id} -> id) (\s@DeleteApiKey' {} a -> s {id = a} :: DeleteApiKey)

instance Core.AWSRequest DeleteApiKey where
  type AWSResponse DeleteApiKey = DeleteApiKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApiKey where
  hashWithSalt _salt DeleteApiKey' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteApiKey where
  rnf DeleteApiKey' {..} =
    Prelude.rnf apiId `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApiKey where
  toPath DeleteApiKey' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/apikeys/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiKeyResponse' smart constructor.
data DeleteApiKeyResponse = DeleteApiKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApiKeyResponse_httpStatus' - The response's http status code.
newDeleteApiKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApiKeyResponse
newDeleteApiKeyResponse pHttpStatus_ =
  DeleteApiKeyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteApiKeyResponse_httpStatus :: Lens.Lens' DeleteApiKeyResponse Prelude.Int
deleteApiKeyResponse_httpStatus = Lens.lens (\DeleteApiKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteApiKeyResponse' {} a -> s {httpStatus = a} :: DeleteApiKeyResponse)

instance Prelude.NFData DeleteApiKeyResponse where
  rnf DeleteApiKeyResponse' {..} =
    Prelude.rnf httpStatus
