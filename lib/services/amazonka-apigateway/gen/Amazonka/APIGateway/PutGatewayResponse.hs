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
-- Module      : Amazonka.APIGateway.PutGatewayResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customization of a GatewayResponse of a specified response
-- type and status code on the given RestApi.
module Amazonka.APIGateway.PutGatewayResponse
  ( -- * Creating a Request
    PutGatewayResponse (..),
    newPutGatewayResponse,

    -- * Request Lenses
    putGatewayResponse_responseParameters,
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,

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

-- | Creates a customization of a GatewayResponse of a specified response
-- type and status code on the given RestApi.
--
-- /See:/ 'newPutGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { -- | Response parameters (paths, query strings and headers) of the
    -- GatewayResponse as a string-to-string map of key-value pairs.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Response templates of the GatewayResponse as a string-to-string map of
    -- key-value pairs.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code of the GatewayResponse.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The response type of the associated GatewayResponse
    responseType :: GatewayResponseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseParameters', 'putGatewayResponse_responseParameters' - Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
--
-- 'responseTemplates', 'putGatewayResponse_responseTemplates' - Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
--
-- 'statusCode', 'putGatewayResponse_statusCode' - The HTTP status code of the GatewayResponse.
--
-- 'restApiId', 'putGatewayResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'responseType', 'putGatewayResponse_responseType' - The response type of the associated GatewayResponse
newPutGatewayResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  PutGatewayResponse
newPutGatewayResponse pRestApiId_ pResponseType_ =
  PutGatewayResponse'
    { responseParameters =
        Prelude.Nothing,
      responseTemplates = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
putGatewayResponse_responseParameters :: Lens.Lens' PutGatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putGatewayResponse_responseParameters = Lens.lens (\PutGatewayResponse' {responseParameters} -> responseParameters) (\s@PutGatewayResponse' {} a -> s {responseParameters = a} :: PutGatewayResponse) Prelude.. Lens.mapping Lens.coerced

-- | Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
putGatewayResponse_responseTemplates :: Lens.Lens' PutGatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putGatewayResponse_responseTemplates = Lens.lens (\PutGatewayResponse' {responseTemplates} -> responseTemplates) (\s@PutGatewayResponse' {} a -> s {responseTemplates = a} :: PutGatewayResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status code of the GatewayResponse.
putGatewayResponse_statusCode :: Lens.Lens' PutGatewayResponse (Prelude.Maybe Prelude.Text)
putGatewayResponse_statusCode = Lens.lens (\PutGatewayResponse' {statusCode} -> statusCode) (\s@PutGatewayResponse' {} a -> s {statusCode = a} :: PutGatewayResponse)

-- | The string identifier of the associated RestApi.
putGatewayResponse_restApiId :: Lens.Lens' PutGatewayResponse Prelude.Text
putGatewayResponse_restApiId = Lens.lens (\PutGatewayResponse' {restApiId} -> restApiId) (\s@PutGatewayResponse' {} a -> s {restApiId = a} :: PutGatewayResponse)

-- | The response type of the associated GatewayResponse
putGatewayResponse_responseType :: Lens.Lens' PutGatewayResponse GatewayResponseType
putGatewayResponse_responseType = Lens.lens (\PutGatewayResponse' {responseType} -> responseType) (\s@PutGatewayResponse' {} a -> s {responseType = a} :: PutGatewayResponse)

instance Core.AWSRequest PutGatewayResponse where
  type AWSResponse PutGatewayResponse = GatewayResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutGatewayResponse where
  hashWithSalt _salt PutGatewayResponse' {..} =
    _salt `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` responseType

instance Prelude.NFData PutGatewayResponse where
  rnf PutGatewayResponse' {..} =
    Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf responseType

instance Data.ToHeaders PutGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON PutGatewayResponse where
  toJSON PutGatewayResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("responseParameters" Data..=)
              Prelude.<$> responseParameters,
            ("responseTemplates" Data..=)
              Prelude.<$> responseTemplates,
            ("statusCode" Data..=) Prelude.<$> statusCode
          ]
      )

instance Data.ToPath PutGatewayResponse where
  toPath PutGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/gatewayresponses/",
        Data.toBS responseType
      ]

instance Data.ToQuery PutGatewayResponse where
  toQuery = Prelude.const Prelude.mempty
