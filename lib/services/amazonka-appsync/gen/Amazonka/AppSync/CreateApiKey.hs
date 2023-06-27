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
-- Module      : Amazonka.AppSync.CreateApiKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique key that you can distribute to clients who invoke your
-- API.
module Amazonka.AppSync.CreateApiKey
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | A description of the purpose of the API key.
    description :: Prelude.Maybe Prelude.Text,
    -- | From the creation time, the time after which the API key expires. The
    -- date is represented as seconds since the epoch, rounded down to the
    -- nearest hour. The default value for this parameter is 7 days from
    -- creation time. For more information, see .
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
-- 'expires', 'createApiKey_expires' - From the creation time, the time after which the API key expires. The
-- date is represented as seconds since the epoch, rounded down to the
-- nearest hour. The default value for this parameter is 7 days from
-- creation time. For more information, see .
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

-- | From the creation time, the time after which the API key expires. The
-- date is represented as seconds since the epoch, rounded down to the
-- nearest hour. The default value for this parameter is 7 days from
-- creation time. For more information, see .
createApiKey_expires :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Integer)
createApiKey_expires = Lens.lens (\CreateApiKey' {expires} -> expires) (\s@CreateApiKey' {} a -> s {expires = a} :: CreateApiKey)

-- | The ID for your GraphQL API.
createApiKey_apiId :: Lens.Lens' CreateApiKey Prelude.Text
createApiKey_apiId = Lens.lens (\CreateApiKey' {apiId} -> apiId) (\s@CreateApiKey' {} a -> s {apiId = a} :: CreateApiKey)

instance Core.AWSRequest CreateApiKey where
  type AWSResponse CreateApiKey = CreateApiKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiKeyResponse'
            Prelude.<$> (x Data..?> "apiKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApiKey where
  hashWithSalt _salt CreateApiKey' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expires
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData CreateApiKey where
  rnf CreateApiKey' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders CreateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("expires" Data..=) Prelude.<$> expires
          ]
      )

instance Data.ToPath CreateApiKey where
  toPath CreateApiKey' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/apikeys"]

instance Data.ToQuery CreateApiKey where
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

instance Prelude.NFData CreateApiKeyResponse where
  rnf CreateApiKeyResponse' {..} =
    Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf httpStatus
