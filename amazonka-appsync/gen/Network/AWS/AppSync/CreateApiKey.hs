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
-- Module      : Network.AWS.AppSync.CreateApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique key that you can distribute to clients who are
-- executing your API.
module Network.AWS.AppSync.CreateApiKey
  ( -- * Creating a Request
    CreateApiKey (..),
    newCreateApiKey,

    -- * Request Lenses
    createApiKey_description,
    createApiKey_expires,
    createApiKey_apiId,

    -- * Destructuring the Response
    CreateApiKeyResponse (..),
    newCreateApiKeyResponse,

    -- * Response Lenses
    createApiKeyResponse_apiKey,
    createApiKeyResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | A description of the purpose of the API key.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time from creation time after which the API key expires. The date is
    -- represented as seconds since the epoch, rounded down to the nearest
    -- hour. The default value for this parameter is 7 days from creation time.
    -- For more information, see .
    expires :: Prelude.Maybe Prelude.Integer,
    -- | The ID for your GraphQL API.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createApiKey_description' - A description of the purpose of the API key.
--
-- 'expires', 'createApiKey_expires' - The time from creation time after which the API key expires. The date is
-- represented as seconds since the epoch, rounded down to the nearest
-- hour. The default value for this parameter is 7 days from creation time.
-- For more information, see .
--
-- 'apiId', 'createApiKey_apiId' - The ID for your GraphQL API.
newCreateApiKey ::
  -- | 'apiId'
  Prelude.Text ->
  CreateApiKey
newCreateApiKey pApiId_ =
  CreateApiKey'
    { description = Prelude.Nothing,
      expires = Prelude.Nothing,
      apiId = pApiId_
    }

-- | A description of the purpose of the API key.
createApiKey_description :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_description = Lens.lens (\CreateApiKey' {description} -> description) (\s@CreateApiKey' {} a -> s {description = a} :: CreateApiKey)

-- | The time from creation time after which the API key expires. The date is
-- represented as seconds since the epoch, rounded down to the nearest
-- hour. The default value for this parameter is 7 days from creation time.
-- For more information, see .
createApiKey_expires :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Integer)
createApiKey_expires = Lens.lens (\CreateApiKey' {expires} -> expires) (\s@CreateApiKey' {} a -> s {expires = a} :: CreateApiKey)

-- | The ID for your GraphQL API.
createApiKey_apiId :: Lens.Lens' CreateApiKey Prelude.Text
createApiKey_apiId = Lens.lens (\CreateApiKey' {apiId} -> apiId) (\s@CreateApiKey' {} a -> s {apiId = a} :: CreateApiKey)

instance Core.AWSRequest CreateApiKey where
  type AWSResponse CreateApiKey = CreateApiKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiKeyResponse'
            Prelude.<$> (x Core..?> "apiKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApiKey

instance Prelude.NFData CreateApiKey

instance Core.ToHeaders CreateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("expires" Core..=) Prelude.<$> expires
          ]
      )

instance Core.ToPath CreateApiKey where
  toPath CreateApiKey' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/apikeys"]

instance Core.ToQuery CreateApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApiKeyResponse' smart constructor.
data CreateApiKeyResponse = CreateApiKeyResponse'
  { -- | The API key.
    apiKey :: Prelude.Maybe ApiKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'createApiKeyResponse_apiKey' - The API key.
--
-- 'httpStatus', 'createApiKeyResponse_httpStatus' - The response's http status code.
newCreateApiKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApiKeyResponse
newCreateApiKeyResponse pHttpStatus_ =
  CreateApiKeyResponse'
    { apiKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API key.
createApiKeyResponse_apiKey :: Lens.Lens' CreateApiKeyResponse (Prelude.Maybe ApiKey)
createApiKeyResponse_apiKey = Lens.lens (\CreateApiKeyResponse' {apiKey} -> apiKey) (\s@CreateApiKeyResponse' {} a -> s {apiKey = a} :: CreateApiKeyResponse)

-- | The response's http status code.
createApiKeyResponse_httpStatus :: Lens.Lens' CreateApiKeyResponse Prelude.Int
createApiKeyResponse_httpStatus = Lens.lens (\CreateApiKeyResponse' {httpStatus} -> httpStatus) (\s@CreateApiKeyResponse' {} a -> s {httpStatus = a} :: CreateApiKeyResponse)

instance Prelude.NFData CreateApiKeyResponse
