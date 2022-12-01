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
-- Module      : Amazonka.APIGateway.GetRequestValidator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a RequestValidator of a given RestApi.
module Amazonka.APIGateway.GetRequestValidator
  ( -- * Creating a Request
    GetRequestValidator (..),
    newGetRequestValidator,

    -- * Request Lenses
    getRequestValidator_restApiId,
    getRequestValidator_requestValidatorId,

    -- * Destructuring the Response
    RequestValidator (..),
    newRequestValidator,

    -- * Response Lenses
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets a RequestValidator of a given RestApi.
--
-- /See:/ 'newGetRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the RequestValidator to be retrieved.
    requestValidatorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getRequestValidator_restApiId' - The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'getRequestValidator_requestValidatorId' - The identifier of the RequestValidator to be retrieved.
newGetRequestValidator ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'requestValidatorId'
  Prelude.Text ->
  GetRequestValidator
newGetRequestValidator
  pRestApiId_
  pRequestValidatorId_ =
    GetRequestValidator'
      { restApiId = pRestApiId_,
        requestValidatorId = pRequestValidatorId_
      }

-- | The string identifier of the associated RestApi.
getRequestValidator_restApiId :: Lens.Lens' GetRequestValidator Prelude.Text
getRequestValidator_restApiId = Lens.lens (\GetRequestValidator' {restApiId} -> restApiId) (\s@GetRequestValidator' {} a -> s {restApiId = a} :: GetRequestValidator)

-- | The identifier of the RequestValidator to be retrieved.
getRequestValidator_requestValidatorId :: Lens.Lens' GetRequestValidator Prelude.Text
getRequestValidator_requestValidatorId = Lens.lens (\GetRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@GetRequestValidator' {} a -> s {requestValidatorId = a} :: GetRequestValidator)

instance Core.AWSRequest GetRequestValidator where
  type
    AWSResponse GetRequestValidator =
      RequestValidator
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetRequestValidator where
  hashWithSalt _salt GetRequestValidator' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` requestValidatorId

instance Prelude.NFData GetRequestValidator where
  rnf GetRequestValidator' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf requestValidatorId

instance Core.ToHeaders GetRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetRequestValidator where
  toPath GetRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/requestvalidators/",
        Core.toBS requestValidatorId
      ]

instance Core.ToQuery GetRequestValidator where
  toQuery = Prelude.const Prelude.mempty
