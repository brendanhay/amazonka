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
-- Module      : Network.AWS.AppSync.DeleteApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API key.
module Network.AWS.AppSync.DeleteApiKey
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApiKey' smart constructor.
data DeleteApiKey = DeleteApiKey'
  { -- | The API ID.
    apiId :: Core.Text,
    -- | The ID for the API key.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'id'
  Core.Text ->
  DeleteApiKey
newDeleteApiKey pApiId_ pId_ =
  DeleteApiKey' {apiId = pApiId_, id = pId_}

-- | The API ID.
deleteApiKey_apiId :: Lens.Lens' DeleteApiKey Core.Text
deleteApiKey_apiId = Lens.lens (\DeleteApiKey' {apiId} -> apiId) (\s@DeleteApiKey' {} a -> s {apiId = a} :: DeleteApiKey)

-- | The ID for the API key.
deleteApiKey_id :: Lens.Lens' DeleteApiKey Core.Text
deleteApiKey_id = Lens.lens (\DeleteApiKey' {id} -> id) (\s@DeleteApiKey' {} a -> s {id = a} :: DeleteApiKey)

instance Core.AWSRequest DeleteApiKey where
  type AWSResponse DeleteApiKey = DeleteApiKeyResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiKeyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteApiKey

instance Core.NFData DeleteApiKey

instance Core.ToHeaders DeleteApiKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApiKey where
  toPath DeleteApiKey' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/apikeys/",
        Core.toBS id
      ]

instance Core.ToQuery DeleteApiKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApiKeyResponse' smart constructor.
data DeleteApiKeyResponse = DeleteApiKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteApiKeyResponse
newDeleteApiKeyResponse pHttpStatus_ =
  DeleteApiKeyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteApiKeyResponse_httpStatus :: Lens.Lens' DeleteApiKeyResponse Core.Int
deleteApiKeyResponse_httpStatus = Lens.lens (\DeleteApiKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteApiKeyResponse' {} a -> s {httpStatus = a} :: DeleteApiKeyResponse)

instance Core.NFData DeleteApiKeyResponse
