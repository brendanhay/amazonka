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
-- Module      : Network.AWS.AppSync.UpdateApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an API key. The key can be updated while it is not deleted.
module Network.AWS.AppSync.UpdateApiKey
  ( -- * Creating a Request
    UpdateApiKey (..),
    newUpdateApiKey,

    -- * Request Lenses
    updateApiKey_description,
    updateApiKey_expires,
    updateApiKey_apiId,
    updateApiKey_id,

    -- * Destructuring the Response
    UpdateApiKeyResponse (..),
    newUpdateApiKeyResponse,

    -- * Response Lenses
    updateApiKeyResponse_apiKey,
    updateApiKeyResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { -- | A description of the purpose of the API key.
    description :: Core.Maybe Core.Text,
    -- | The time from update time after which the API key expires. The date is
    -- represented as seconds since the epoch. For more information, see .
    expires :: Core.Maybe Core.Integer,
    -- | The ID for the GraphQL API.
    apiId :: Core.Text,
    -- | The API key ID.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateApiKey_description' - A description of the purpose of the API key.
--
-- 'expires', 'updateApiKey_expires' - The time from update time after which the API key expires. The date is
-- represented as seconds since the epoch. For more information, see .
--
-- 'apiId', 'updateApiKey_apiId' - The ID for the GraphQL API.
--
-- 'id', 'updateApiKey_id' - The API key ID.
newUpdateApiKey ::
  -- | 'apiId'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  UpdateApiKey
newUpdateApiKey pApiId_ pId_ =
  UpdateApiKey'
    { description = Core.Nothing,
      expires = Core.Nothing,
      apiId = pApiId_,
      id = pId_
    }

-- | A description of the purpose of the API key.
updateApiKey_description :: Lens.Lens' UpdateApiKey (Core.Maybe Core.Text)
updateApiKey_description = Lens.lens (\UpdateApiKey' {description} -> description) (\s@UpdateApiKey' {} a -> s {description = a} :: UpdateApiKey)

-- | The time from update time after which the API key expires. The date is
-- represented as seconds since the epoch. For more information, see .
updateApiKey_expires :: Lens.Lens' UpdateApiKey (Core.Maybe Core.Integer)
updateApiKey_expires = Lens.lens (\UpdateApiKey' {expires} -> expires) (\s@UpdateApiKey' {} a -> s {expires = a} :: UpdateApiKey)

-- | The ID for the GraphQL API.
updateApiKey_apiId :: Lens.Lens' UpdateApiKey Core.Text
updateApiKey_apiId = Lens.lens (\UpdateApiKey' {apiId} -> apiId) (\s@UpdateApiKey' {} a -> s {apiId = a} :: UpdateApiKey)

-- | The API key ID.
updateApiKey_id :: Lens.Lens' UpdateApiKey Core.Text
updateApiKey_id = Lens.lens (\UpdateApiKey' {id} -> id) (\s@UpdateApiKey' {} a -> s {id = a} :: UpdateApiKey)

instance Core.AWSRequest UpdateApiKey where
  type AWSResponse UpdateApiKey = UpdateApiKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiKeyResponse'
            Core.<$> (x Core..?> "apiKey")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateApiKey

instance Core.NFData UpdateApiKey

instance Core.ToHeaders UpdateApiKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApiKey where
  toJSON UpdateApiKey' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            ("expires" Core..=) Core.<$> expires
          ]
      )

instance Core.ToPath UpdateApiKey where
  toPath UpdateApiKey' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/apikeys/",
        Core.toBS id
      ]

instance Core.ToQuery UpdateApiKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApiKeyResponse' smart constructor.
data UpdateApiKeyResponse = UpdateApiKeyResponse'
  { -- | The API key.
    apiKey :: Core.Maybe ApiKey,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'updateApiKeyResponse_apiKey' - The API key.
--
-- 'httpStatus', 'updateApiKeyResponse_httpStatus' - The response's http status code.
newUpdateApiKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateApiKeyResponse
newUpdateApiKeyResponse pHttpStatus_ =
  UpdateApiKeyResponse'
    { apiKey = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API key.
updateApiKeyResponse_apiKey :: Lens.Lens' UpdateApiKeyResponse (Core.Maybe ApiKey)
updateApiKeyResponse_apiKey = Lens.lens (\UpdateApiKeyResponse' {apiKey} -> apiKey) (\s@UpdateApiKeyResponse' {} a -> s {apiKey = a} :: UpdateApiKeyResponse)

-- | The response's http status code.
updateApiKeyResponse_httpStatus :: Lens.Lens' UpdateApiKeyResponse Core.Int
updateApiKeyResponse_httpStatus = Lens.lens (\UpdateApiKeyResponse' {httpStatus} -> httpStatus) (\s@UpdateApiKeyResponse' {} a -> s {httpStatus = a} :: UpdateApiKeyResponse)

instance Core.NFData UpdateApiKeyResponse
