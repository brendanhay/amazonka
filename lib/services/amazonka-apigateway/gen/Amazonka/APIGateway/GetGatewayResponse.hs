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
-- Module      : Amazonka.APIGateway.GetGatewayResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a GatewayResponse of a specified response type on the given
-- RestApi.
module Amazonka.APIGateway.GetGatewayResponse
  ( -- * Creating a Request
    GetGatewayResponse (..),
    newGetGatewayResponse,

    -- * Request Lenses
    getGatewayResponse_restApiId,
    getGatewayResponse_responseType,

    -- * Destructuring the Response
    GatewayResponse (..),
    newGatewayResponse,

    -- * Response Lenses
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_responseType,
    gatewayResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets a GatewayResponse of a specified response type on the given
-- RestApi.
--
-- /See:/ 'newGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The response type of the associated GatewayResponse.
    responseType :: GatewayResponseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getGatewayResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'responseType', 'getGatewayResponse_responseType' - The response type of the associated GatewayResponse.
newGetGatewayResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  GetGatewayResponse
newGetGatewayResponse pRestApiId_ pResponseType_ =
  GetGatewayResponse'
    { restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | The string identifier of the associated RestApi.
getGatewayResponse_restApiId :: Lens.Lens' GetGatewayResponse Prelude.Text
getGatewayResponse_restApiId = Lens.lens (\GetGatewayResponse' {restApiId} -> restApiId) (\s@GetGatewayResponse' {} a -> s {restApiId = a} :: GetGatewayResponse)

-- | The response type of the associated GatewayResponse.
getGatewayResponse_responseType :: Lens.Lens' GetGatewayResponse GatewayResponseType
getGatewayResponse_responseType = Lens.lens (\GetGatewayResponse' {responseType} -> responseType) (\s@GetGatewayResponse' {} a -> s {responseType = a} :: GetGatewayResponse)

instance Core.AWSRequest GetGatewayResponse where
  type AWSResponse GetGatewayResponse = GatewayResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetGatewayResponse where
  hashWithSalt _salt GetGatewayResponse' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` responseType

instance Prelude.NFData GetGatewayResponse where
  rnf GetGatewayResponse' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf responseType

instance Data.ToHeaders GetGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetGatewayResponse where
  toPath GetGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/gatewayresponses/",
        Data.toBS responseType
      ]

instance Data.ToQuery GetGatewayResponse where
  toQuery = Prelude.const Prelude.mempty
