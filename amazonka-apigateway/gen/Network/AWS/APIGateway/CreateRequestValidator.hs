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
-- Module      : Network.AWS.APIGateway.CreateRequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a ReqeustValidator of a given RestApi.
module Network.AWS.APIGateway.CreateRequestValidator
  ( -- * Creating a Request
    CreateRequestValidator (..),
    newCreateRequestValidator,

    -- * Request Lenses
    createRequestValidator_validateRequestBody,
    createRequestValidator_validateRequestParameters,
    createRequestValidator_name,
    createRequestValidator_restApiId,

    -- * Destructuring the Response
    RequestValidator (..),
    newRequestValidator,

    -- * Response Lenses
    requestValidator_validateRequestBody,
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a RequestValidator of a given RestApi.
--
-- /See:/ 'newCreateRequestValidator' smart constructor.
data CreateRequestValidator = CreateRequestValidator'
  { -- | A Boolean flag to indicate whether to validate request body according to
    -- the configured model schema for the method (@true@) or not (@false@).
    validateRequestBody :: Core.Maybe Core.Bool,
    -- | A Boolean flag to indicate whether to validate request parameters,
    -- @true@, or not @false@.
    validateRequestParameters :: Core.Maybe Core.Bool,
    -- | The name of the to-be-created RequestValidator.
    name :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'validateRequestParameters', 'createRequestValidator_validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters,
-- @true@, or not @false@.
--
-- 'name', 'createRequestValidator_name' - The name of the to-be-created RequestValidator.
--
-- 'restApiId', 'createRequestValidator_restApiId' - [Required] The string identifier of the associated RestApi.
newCreateRequestValidator ::
  -- | 'restApiId'
  Core.Text ->
  CreateRequestValidator
newCreateRequestValidator pRestApiId_ =
  CreateRequestValidator'
    { validateRequestBody =
        Core.Nothing,
      validateRequestParameters = Core.Nothing,
      name = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | A Boolean flag to indicate whether to validate request body according to
-- the configured model schema for the method (@true@) or not (@false@).
createRequestValidator_validateRequestBody :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Bool)
createRequestValidator_validateRequestBody = Lens.lens (\CreateRequestValidator' {validateRequestBody} -> validateRequestBody) (\s@CreateRequestValidator' {} a -> s {validateRequestBody = a} :: CreateRequestValidator)

-- | A Boolean flag to indicate whether to validate request parameters,
-- @true@, or not @false@.
createRequestValidator_validateRequestParameters :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Bool)
createRequestValidator_validateRequestParameters = Lens.lens (\CreateRequestValidator' {validateRequestParameters} -> validateRequestParameters) (\s@CreateRequestValidator' {} a -> s {validateRequestParameters = a} :: CreateRequestValidator)

-- | The name of the to-be-created RequestValidator.
createRequestValidator_name :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Text)
createRequestValidator_name = Lens.lens (\CreateRequestValidator' {name} -> name) (\s@CreateRequestValidator' {} a -> s {name = a} :: CreateRequestValidator)

-- | [Required] The string identifier of the associated RestApi.
createRequestValidator_restApiId :: Lens.Lens' CreateRequestValidator Core.Text
createRequestValidator_restApiId = Lens.lens (\CreateRequestValidator' {restApiId} -> restApiId) (\s@CreateRequestValidator' {} a -> s {restApiId = a} :: CreateRequestValidator)

instance Core.AWSRequest CreateRequestValidator where
  type
    AWSResponse CreateRequestValidator =
      RequestValidator
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateRequestValidator

instance Core.NFData CreateRequestValidator

instance Core.ToHeaders CreateRequestValidator where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRequestValidator where
  toJSON CreateRequestValidator' {..} =
    Core.object
      ( Core.catMaybes
          [ ("validateRequestBody" Core..=)
              Core.<$> validateRequestBody,
            ("validateRequestParameters" Core..=)
              Core.<$> validateRequestParameters,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.ToPath CreateRequestValidator where
  toPath CreateRequestValidator' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/requestvalidators"
      ]

instance Core.ToQuery CreateRequestValidator where
  toQuery = Core.const Core.mempty
