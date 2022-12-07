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
-- Module      : Amazonka.APIGateway.CreateRequestValidator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a RequestValidator of a given RestApi.
module Amazonka.APIGateway.CreateRequestValidator
  ( -- * Creating a Request
    CreateRequestValidator (..),
    newCreateRequestValidator,

    -- * Request Lenses
    createRequestValidator_validateRequestBody,
    createRequestValidator_name,
    createRequestValidator_validateRequestParameters,
    createRequestValidator_restApiId,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a RequestValidator of a given RestApi.
--
-- /See:/ 'newCreateRequestValidator' smart constructor.
data CreateRequestValidator = CreateRequestValidator'
  { -- | A Boolean flag to indicate whether to validate request body according to
    -- the configured model schema for the method (@true@) or not (@false@).
    validateRequestBody :: Prelude.Maybe Prelude.Bool,
    -- | The name of the to-be-created RequestValidator.
    name :: Prelude.Maybe Prelude.Text,
    -- | A Boolean flag to indicate whether to validate request parameters,
    -- @true@, or not @false@.
    validateRequestParameters :: Prelude.Maybe Prelude.Bool,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validateRequestBody', 'createRequestValidator_validateRequestBody' - A Boolean flag to indicate whether to validate request body according to
-- the configured model schema for the method (@true@) or not (@false@).
--
-- 'name', 'createRequestValidator_name' - The name of the to-be-created RequestValidator.
--
-- 'validateRequestParameters', 'createRequestValidator_validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters,
-- @true@, or not @false@.
--
-- 'restApiId', 'createRequestValidator_restApiId' - The string identifier of the associated RestApi.
newCreateRequestValidator ::
  -- | 'restApiId'
  Prelude.Text ->
  CreateRequestValidator
newCreateRequestValidator pRestApiId_ =
  CreateRequestValidator'
    { validateRequestBody =
        Prelude.Nothing,
      name = Prelude.Nothing,
      validateRequestParameters = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | A Boolean flag to indicate whether to validate request body according to
-- the configured model schema for the method (@true@) or not (@false@).
createRequestValidator_validateRequestBody :: Lens.Lens' CreateRequestValidator (Prelude.Maybe Prelude.Bool)
createRequestValidator_validateRequestBody = Lens.lens (\CreateRequestValidator' {validateRequestBody} -> validateRequestBody) (\s@CreateRequestValidator' {} a -> s {validateRequestBody = a} :: CreateRequestValidator)

-- | The name of the to-be-created RequestValidator.
createRequestValidator_name :: Lens.Lens' CreateRequestValidator (Prelude.Maybe Prelude.Text)
createRequestValidator_name = Lens.lens (\CreateRequestValidator' {name} -> name) (\s@CreateRequestValidator' {} a -> s {name = a} :: CreateRequestValidator)

-- | A Boolean flag to indicate whether to validate request parameters,
-- @true@, or not @false@.
createRequestValidator_validateRequestParameters :: Lens.Lens' CreateRequestValidator (Prelude.Maybe Prelude.Bool)
createRequestValidator_validateRequestParameters = Lens.lens (\CreateRequestValidator' {validateRequestParameters} -> validateRequestParameters) (\s@CreateRequestValidator' {} a -> s {validateRequestParameters = a} :: CreateRequestValidator)

-- | The string identifier of the associated RestApi.
createRequestValidator_restApiId :: Lens.Lens' CreateRequestValidator Prelude.Text
createRequestValidator_restApiId = Lens.lens (\CreateRequestValidator' {restApiId} -> restApiId) (\s@CreateRequestValidator' {} a -> s {restApiId = a} :: CreateRequestValidator)

instance Core.AWSRequest CreateRequestValidator where
  type
    AWSResponse CreateRequestValidator =
      RequestValidator
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateRequestValidator where
  hashWithSalt _salt CreateRequestValidator' {..} =
    _salt `Prelude.hashWithSalt` validateRequestBody
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` validateRequestParameters
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData CreateRequestValidator where
  rnf CreateRequestValidator' {..} =
    Prelude.rnf validateRequestBody
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf validateRequestParameters
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders CreateRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateRequestValidator where
  toJSON CreateRequestValidator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("validateRequestBody" Data..=)
              Prelude.<$> validateRequestBody,
            ("name" Data..=) Prelude.<$> name,
            ("validateRequestParameters" Data..=)
              Prelude.<$> validateRequestParameters
          ]
      )

instance Data.ToPath CreateRequestValidator where
  toPath CreateRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/requestvalidators"
      ]

instance Data.ToQuery CreateRequestValidator where
  toQuery = Prelude.const Prelude.mempty
