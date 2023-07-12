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
-- Module      : Amazonka.ApiGatewayV2.UpdateApiMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The API mapping.
module Amazonka.ApiGatewayV2.UpdateApiMapping
  ( -- * Creating a Request
    UpdateApiMapping (..),
    newUpdateApiMapping,

    -- * Request Lenses
    updateApiMapping_apiMappingKey,
    updateApiMapping_stage,
    updateApiMapping_apiMappingId,
    updateApiMapping_apiId,
    updateApiMapping_domainName,

    -- * Destructuring the Response
    UpdateApiMappingResponse (..),
    newUpdateApiMappingResponse,

    -- * Response Lenses
    updateApiMappingResponse_apiId,
    updateApiMappingResponse_apiMappingId,
    updateApiMappingResponse_apiMappingKey,
    updateApiMappingResponse_stage,
    updateApiMappingResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an ApiMapping.
--
-- /See:/ 'newUpdateApiMapping' smart constructor.
data UpdateApiMapping = UpdateApiMapping'
  { -- | The API mapping key.
    apiMappingKey :: Prelude.Maybe Prelude.Text,
    -- | The API stage.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The API mapping identifier.
    apiMappingId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingKey', 'updateApiMapping_apiMappingKey' - The API mapping key.
--
-- 'stage', 'updateApiMapping_stage' - The API stage.
--
-- 'apiMappingId', 'updateApiMapping_apiMappingId' - The API mapping identifier.
--
-- 'apiId', 'updateApiMapping_apiId' - The API identifier.
--
-- 'domainName', 'updateApiMapping_domainName' - The domain name.
newUpdateApiMapping ::
  -- | 'apiMappingId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  UpdateApiMapping
newUpdateApiMapping
  pApiMappingId_
  pApiId_
  pDomainName_ =
    UpdateApiMapping'
      { apiMappingKey = Prelude.Nothing,
        stage = Prelude.Nothing,
        apiMappingId = pApiMappingId_,
        apiId = pApiId_,
        domainName = pDomainName_
      }

-- | The API mapping key.
updateApiMapping_apiMappingKey :: Lens.Lens' UpdateApiMapping (Prelude.Maybe Prelude.Text)
updateApiMapping_apiMappingKey = Lens.lens (\UpdateApiMapping' {apiMappingKey} -> apiMappingKey) (\s@UpdateApiMapping' {} a -> s {apiMappingKey = a} :: UpdateApiMapping)

-- | The API stage.
updateApiMapping_stage :: Lens.Lens' UpdateApiMapping (Prelude.Maybe Prelude.Text)
updateApiMapping_stage = Lens.lens (\UpdateApiMapping' {stage} -> stage) (\s@UpdateApiMapping' {} a -> s {stage = a} :: UpdateApiMapping)

-- | The API mapping identifier.
updateApiMapping_apiMappingId :: Lens.Lens' UpdateApiMapping Prelude.Text
updateApiMapping_apiMappingId = Lens.lens (\UpdateApiMapping' {apiMappingId} -> apiMappingId) (\s@UpdateApiMapping' {} a -> s {apiMappingId = a} :: UpdateApiMapping)

-- | The API identifier.
updateApiMapping_apiId :: Lens.Lens' UpdateApiMapping Prelude.Text
updateApiMapping_apiId = Lens.lens (\UpdateApiMapping' {apiId} -> apiId) (\s@UpdateApiMapping' {} a -> s {apiId = a} :: UpdateApiMapping)

-- | The domain name.
updateApiMapping_domainName :: Lens.Lens' UpdateApiMapping Prelude.Text
updateApiMapping_domainName = Lens.lens (\UpdateApiMapping' {domainName} -> domainName) (\s@UpdateApiMapping' {} a -> s {domainName = a} :: UpdateApiMapping)

instance Core.AWSRequest UpdateApiMapping where
  type
    AWSResponse UpdateApiMapping =
      UpdateApiMappingResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiMappingResponse'
            Prelude.<$> (x Data..?> "apiId")
            Prelude.<*> (x Data..?> "apiMappingId")
            Prelude.<*> (x Data..?> "apiMappingKey")
            Prelude.<*> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApiMapping where
  hashWithSalt _salt UpdateApiMapping' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingKey
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` apiMappingId
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateApiMapping where
  rnf UpdateApiMapping' {..} =
    Prelude.rnf apiMappingKey
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf apiMappingId
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateApiMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApiMapping where
  toJSON UpdateApiMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiMappingKey" Data..=) Prelude.<$> apiMappingKey,
            ("stage" Data..=) Prelude.<$> stage,
            Prelude.Just ("apiId" Data..= apiId)
          ]
      )

instance Data.ToPath UpdateApiMapping where
  toPath UpdateApiMapping' {..} =
    Prelude.mconcat
      [ "/v2/domainnames/",
        Data.toBS domainName,
        "/apimappings/",
        Data.toBS apiMappingId
      ]

instance Data.ToQuery UpdateApiMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApiMappingResponse' smart constructor.
data UpdateApiMappingResponse = UpdateApiMappingResponse'
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
-- Create a value of 'UpdateApiMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'updateApiMappingResponse_apiId' - The API identifier.
--
-- 'apiMappingId', 'updateApiMappingResponse_apiMappingId' - The API mapping identifier.
--
-- 'apiMappingKey', 'updateApiMappingResponse_apiMappingKey' - The API mapping key.
--
-- 'stage', 'updateApiMappingResponse_stage' - The API stage.
--
-- 'httpStatus', 'updateApiMappingResponse_httpStatus' - The response's http status code.
newUpdateApiMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApiMappingResponse
newUpdateApiMappingResponse pHttpStatus_ =
  UpdateApiMappingResponse'
    { apiId = Prelude.Nothing,
      apiMappingId = Prelude.Nothing,
      apiMappingKey = Prelude.Nothing,
      stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API identifier.
updateApiMappingResponse_apiId :: Lens.Lens' UpdateApiMappingResponse (Prelude.Maybe Prelude.Text)
updateApiMappingResponse_apiId = Lens.lens (\UpdateApiMappingResponse' {apiId} -> apiId) (\s@UpdateApiMappingResponse' {} a -> s {apiId = a} :: UpdateApiMappingResponse)

-- | The API mapping identifier.
updateApiMappingResponse_apiMappingId :: Lens.Lens' UpdateApiMappingResponse (Prelude.Maybe Prelude.Text)
updateApiMappingResponse_apiMappingId = Lens.lens (\UpdateApiMappingResponse' {apiMappingId} -> apiMappingId) (\s@UpdateApiMappingResponse' {} a -> s {apiMappingId = a} :: UpdateApiMappingResponse)

-- | The API mapping key.
updateApiMappingResponse_apiMappingKey :: Lens.Lens' UpdateApiMappingResponse (Prelude.Maybe Prelude.Text)
updateApiMappingResponse_apiMappingKey = Lens.lens (\UpdateApiMappingResponse' {apiMappingKey} -> apiMappingKey) (\s@UpdateApiMappingResponse' {} a -> s {apiMappingKey = a} :: UpdateApiMappingResponse)

-- | The API stage.
updateApiMappingResponse_stage :: Lens.Lens' UpdateApiMappingResponse (Prelude.Maybe Prelude.Text)
updateApiMappingResponse_stage = Lens.lens (\UpdateApiMappingResponse' {stage} -> stage) (\s@UpdateApiMappingResponse' {} a -> s {stage = a} :: UpdateApiMappingResponse)

-- | The response's http status code.
updateApiMappingResponse_httpStatus :: Lens.Lens' UpdateApiMappingResponse Prelude.Int
updateApiMappingResponse_httpStatus = Lens.lens (\UpdateApiMappingResponse' {httpStatus} -> httpStatus) (\s@UpdateApiMappingResponse' {} a -> s {httpStatus = a} :: UpdateApiMappingResponse)

instance Prelude.NFData UpdateApiMappingResponse where
  rnf UpdateApiMappingResponse' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiMappingId
      `Prelude.seq` Prelude.rnf apiMappingKey
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
