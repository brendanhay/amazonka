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
-- Module      : Amazonka.ApiGatewayV2.CreateApiMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an API mapping.
module Amazonka.ApiGatewayV2.CreateApiMapping
  ( -- * Creating a Request
    CreateApiMapping (..),
    newCreateApiMapping,

    -- * Request Lenses
    createApiMapping_apiMappingKey,
    createApiMapping_domainName,
    createApiMapping_stage,
    createApiMapping_apiId,

    -- * Destructuring the Response
    CreateApiMappingResponse (..),
    newCreateApiMappingResponse,

    -- * Response Lenses
    createApiMappingResponse_apiId,
    createApiMappingResponse_apiMappingId,
    createApiMappingResponse_apiMappingKey,
    createApiMappingResponse_stage,
    createApiMappingResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new ApiMapping resource to represent an API mapping.
--
-- /See:/ 'newCreateApiMapping' smart constructor.
data CreateApiMapping = CreateApiMapping'
  { -- | The API mapping key.
    apiMappingKey :: Prelude.Maybe Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text,
    -- | The API stage.
    stage :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingKey', 'createApiMapping_apiMappingKey' - The API mapping key.
--
-- 'domainName', 'createApiMapping_domainName' - The domain name.
--
-- 'stage', 'createApiMapping_stage' - The API stage.
--
-- 'apiId', 'createApiMapping_apiId' - The API identifier.
newCreateApiMapping ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'stage'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  CreateApiMapping
newCreateApiMapping pDomainName_ pStage_ pApiId_ =
  CreateApiMapping'
    { apiMappingKey = Prelude.Nothing,
      domainName = pDomainName_,
      stage = pStage_,
      apiId = pApiId_
    }

-- | The API mapping key.
createApiMapping_apiMappingKey :: Lens.Lens' CreateApiMapping (Prelude.Maybe Prelude.Text)
createApiMapping_apiMappingKey = Lens.lens (\CreateApiMapping' {apiMappingKey} -> apiMappingKey) (\s@CreateApiMapping' {} a -> s {apiMappingKey = a} :: CreateApiMapping)

-- | The domain name.
createApiMapping_domainName :: Lens.Lens' CreateApiMapping Prelude.Text
createApiMapping_domainName = Lens.lens (\CreateApiMapping' {domainName} -> domainName) (\s@CreateApiMapping' {} a -> s {domainName = a} :: CreateApiMapping)

-- | The API stage.
createApiMapping_stage :: Lens.Lens' CreateApiMapping Prelude.Text
createApiMapping_stage = Lens.lens (\CreateApiMapping' {stage} -> stage) (\s@CreateApiMapping' {} a -> s {stage = a} :: CreateApiMapping)

-- | The API identifier.
createApiMapping_apiId :: Lens.Lens' CreateApiMapping Prelude.Text
createApiMapping_apiId = Lens.lens (\CreateApiMapping' {apiId} -> apiId) (\s@CreateApiMapping' {} a -> s {apiId = a} :: CreateApiMapping)

instance Core.AWSRequest CreateApiMapping where
  type
    AWSResponse CreateApiMapping =
      CreateApiMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiMappingResponse'
            Prelude.<$> (x Data..?> "apiId")
            Prelude.<*> (x Data..?> "apiMappingId")
            Prelude.<*> (x Data..?> "apiMappingKey")
            Prelude.<*> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApiMapping where
  hashWithSalt _salt CreateApiMapping' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingKey
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData CreateApiMapping where
  rnf CreateApiMapping' {..} =
    Prelude.rnf apiMappingKey
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders CreateApiMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApiMapping where
  toJSON CreateApiMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiMappingKey" Data..=) Prelude.<$> apiMappingKey,
            Prelude.Just ("stage" Data..= stage),
            Prelude.Just ("apiId" Data..= apiId)
          ]
      )

instance Data.ToPath CreateApiMapping where
  toPath CreateApiMapping' {..} =
    Prelude.mconcat
      [ "/v2/domainnames/",
        Data.toBS domainName,
        "/apimappings"
      ]

instance Data.ToQuery CreateApiMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApiMappingResponse' smart constructor.
data CreateApiMappingResponse = CreateApiMappingResponse'
  { -- | The API identifier.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | The API mapping identifier.
    apiMappingId :: Prelude.Maybe Prelude.Text,
    -- | The API mapping key.
    apiMappingKey :: Prelude.Maybe Prelude.Text,
    -- | The API stage.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'createApiMappingResponse_apiId' - The API identifier.
--
-- 'apiMappingId', 'createApiMappingResponse_apiMappingId' - The API mapping identifier.
--
-- 'apiMappingKey', 'createApiMappingResponse_apiMappingKey' - The API mapping key.
--
-- 'stage', 'createApiMappingResponse_stage' - The API stage.
--
-- 'httpStatus', 'createApiMappingResponse_httpStatus' - The response's http status code.
newCreateApiMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApiMappingResponse
newCreateApiMappingResponse pHttpStatus_ =
  CreateApiMappingResponse'
    { apiId = Prelude.Nothing,
      apiMappingId = Prelude.Nothing,
      apiMappingKey = Prelude.Nothing,
      stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API identifier.
createApiMappingResponse_apiId :: Lens.Lens' CreateApiMappingResponse (Prelude.Maybe Prelude.Text)
createApiMappingResponse_apiId = Lens.lens (\CreateApiMappingResponse' {apiId} -> apiId) (\s@CreateApiMappingResponse' {} a -> s {apiId = a} :: CreateApiMappingResponse)

-- | The API mapping identifier.
createApiMappingResponse_apiMappingId :: Lens.Lens' CreateApiMappingResponse (Prelude.Maybe Prelude.Text)
createApiMappingResponse_apiMappingId = Lens.lens (\CreateApiMappingResponse' {apiMappingId} -> apiMappingId) (\s@CreateApiMappingResponse' {} a -> s {apiMappingId = a} :: CreateApiMappingResponse)

-- | The API mapping key.
createApiMappingResponse_apiMappingKey :: Lens.Lens' CreateApiMappingResponse (Prelude.Maybe Prelude.Text)
createApiMappingResponse_apiMappingKey = Lens.lens (\CreateApiMappingResponse' {apiMappingKey} -> apiMappingKey) (\s@CreateApiMappingResponse' {} a -> s {apiMappingKey = a} :: CreateApiMappingResponse)

-- | The API stage.
createApiMappingResponse_stage :: Lens.Lens' CreateApiMappingResponse (Prelude.Maybe Prelude.Text)
createApiMappingResponse_stage = Lens.lens (\CreateApiMappingResponse' {stage} -> stage) (\s@CreateApiMappingResponse' {} a -> s {stage = a} :: CreateApiMappingResponse)

-- | The response's http status code.
createApiMappingResponse_httpStatus :: Lens.Lens' CreateApiMappingResponse Prelude.Int
createApiMappingResponse_httpStatus = Lens.lens (\CreateApiMappingResponse' {httpStatus} -> httpStatus) (\s@CreateApiMappingResponse' {} a -> s {httpStatus = a} :: CreateApiMappingResponse)

instance Prelude.NFData CreateApiMappingResponse where
  rnf CreateApiMappingResponse' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiMappingId
      `Prelude.seq` Prelude.rnf apiMappingKey
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
