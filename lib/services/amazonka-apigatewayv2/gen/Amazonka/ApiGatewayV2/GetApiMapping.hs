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
-- Module      : Amazonka.ApiGatewayV2.GetApiMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an API mapping.
module Amazonka.ApiGatewayV2.GetApiMapping
  ( -- * Creating a Request
    GetApiMapping (..),
    newGetApiMapping,

    -- * Request Lenses
    getApiMapping_apiMappingId,
    getApiMapping_domainName,

    -- * Destructuring the Response
    GetApiMappingResponse (..),
    newGetApiMappingResponse,

    -- * Response Lenses
    getApiMappingResponse_apiId,
    getApiMappingResponse_apiMappingId,
    getApiMappingResponse_apiMappingKey,
    getApiMappingResponse_stage,
    getApiMappingResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApiMapping' smart constructor.
data GetApiMapping = GetApiMapping'
  { -- | The API mapping identifier.
    apiMappingId :: Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingId', 'getApiMapping_apiMappingId' - The API mapping identifier.
--
-- 'domainName', 'getApiMapping_domainName' - The domain name.
newGetApiMapping ::
  -- | 'apiMappingId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  GetApiMapping
newGetApiMapping pApiMappingId_ pDomainName_ =
  GetApiMapping'
    { apiMappingId = pApiMappingId_,
      domainName = pDomainName_
    }

-- | The API mapping identifier.
getApiMapping_apiMappingId :: Lens.Lens' GetApiMapping Prelude.Text
getApiMapping_apiMappingId = Lens.lens (\GetApiMapping' {apiMappingId} -> apiMappingId) (\s@GetApiMapping' {} a -> s {apiMappingId = a} :: GetApiMapping)

-- | The domain name.
getApiMapping_domainName :: Lens.Lens' GetApiMapping Prelude.Text
getApiMapping_domainName = Lens.lens (\GetApiMapping' {domainName} -> domainName) (\s@GetApiMapping' {} a -> s {domainName = a} :: GetApiMapping)

instance Core.AWSRequest GetApiMapping where
  type
    AWSResponse GetApiMapping =
      GetApiMappingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiMappingResponse'
            Prelude.<$> (x Data..?> "apiId")
            Prelude.<*> (x Data..?> "apiMappingId")
            Prelude.<*> (x Data..?> "apiMappingKey")
            Prelude.<*> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApiMapping where
  hashWithSalt _salt GetApiMapping' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetApiMapping where
  rnf GetApiMapping' {..} =
    Prelude.rnf apiMappingId `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders GetApiMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApiMapping where
  toPath GetApiMapping' {..} =
    Prelude.mconcat
      [ "/v2/domainnames/",
        Data.toBS domainName,
        "/apimappings/",
        Data.toBS apiMappingId
      ]

instance Data.ToQuery GetApiMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApiMappingResponse' smart constructor.
data GetApiMappingResponse = GetApiMappingResponse'
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
-- Create a value of 'GetApiMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getApiMappingResponse_apiId' - The API identifier.
--
-- 'apiMappingId', 'getApiMappingResponse_apiMappingId' - The API mapping identifier.
--
-- 'apiMappingKey', 'getApiMappingResponse_apiMappingKey' - The API mapping key.
--
-- 'stage', 'getApiMappingResponse_stage' - The API stage.
--
-- 'httpStatus', 'getApiMappingResponse_httpStatus' - The response's http status code.
newGetApiMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiMappingResponse
newGetApiMappingResponse pHttpStatus_ =
  GetApiMappingResponse'
    { apiId = Prelude.Nothing,
      apiMappingId = Prelude.Nothing,
      apiMappingKey = Prelude.Nothing,
      stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API identifier.
getApiMappingResponse_apiId :: Lens.Lens' GetApiMappingResponse (Prelude.Maybe Prelude.Text)
getApiMappingResponse_apiId = Lens.lens (\GetApiMappingResponse' {apiId} -> apiId) (\s@GetApiMappingResponse' {} a -> s {apiId = a} :: GetApiMappingResponse)

-- | The API mapping identifier.
getApiMappingResponse_apiMappingId :: Lens.Lens' GetApiMappingResponse (Prelude.Maybe Prelude.Text)
getApiMappingResponse_apiMappingId = Lens.lens (\GetApiMappingResponse' {apiMappingId} -> apiMappingId) (\s@GetApiMappingResponse' {} a -> s {apiMappingId = a} :: GetApiMappingResponse)

-- | The API mapping key.
getApiMappingResponse_apiMappingKey :: Lens.Lens' GetApiMappingResponse (Prelude.Maybe Prelude.Text)
getApiMappingResponse_apiMappingKey = Lens.lens (\GetApiMappingResponse' {apiMappingKey} -> apiMappingKey) (\s@GetApiMappingResponse' {} a -> s {apiMappingKey = a} :: GetApiMappingResponse)

-- | The API stage.
getApiMappingResponse_stage :: Lens.Lens' GetApiMappingResponse (Prelude.Maybe Prelude.Text)
getApiMappingResponse_stage = Lens.lens (\GetApiMappingResponse' {stage} -> stage) (\s@GetApiMappingResponse' {} a -> s {stage = a} :: GetApiMappingResponse)

-- | The response's http status code.
getApiMappingResponse_httpStatus :: Lens.Lens' GetApiMappingResponse Prelude.Int
getApiMappingResponse_httpStatus = Lens.lens (\GetApiMappingResponse' {httpStatus} -> httpStatus) (\s@GetApiMappingResponse' {} a -> s {httpStatus = a} :: GetApiMappingResponse)

instance Prelude.NFData GetApiMappingResponse where
  rnf GetApiMappingResponse' {..} =
    Prelude.rnf apiId `Prelude.seq`
      Prelude.rnf apiMappingId `Prelude.seq`
        Prelude.rnf apiMappingKey `Prelude.seq`
          Prelude.rnf stage `Prelude.seq`
            Prelude.rnf httpStatus
