{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.Types.RequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.RequestValidator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of validation rules for incoming Method requests.
--
-- In OpenAPI, a RequestValidator of an API is defined by the
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html x-amazon-apigateway-request-validators.requestValidator>
-- object. It the referenced using the
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator x-amazon-apigateway-request-validator>
-- property.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'newRequestValidator' smart constructor.
data RequestValidator = RequestValidator'
  { -- | A Boolean flag to indicate whether to validate request parameters
    -- (@true@) or not (@false@).
    validateRequestParameters :: Prelude.Maybe Prelude.Bool,
    -- | The name of this RequestValidator
    name :: Prelude.Maybe Prelude.Text,
    -- | A Boolean flag to indicate whether to validate a request body according
    -- to the configured Model schema.
    validateRequestBody :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of this RequestValidator.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validateRequestParameters', 'requestValidator_validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters
-- (@true@) or not (@false@).
--
-- 'name', 'requestValidator_name' - The name of this RequestValidator
--
-- 'validateRequestBody', 'requestValidator_validateRequestBody' - A Boolean flag to indicate whether to validate a request body according
-- to the configured Model schema.
--
-- 'id', 'requestValidator_id' - The identifier of this RequestValidator.
newRequestValidator ::
  RequestValidator
newRequestValidator =
  RequestValidator'
    { validateRequestParameters =
        Prelude.Nothing,
      name = Prelude.Nothing,
      validateRequestBody = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A Boolean flag to indicate whether to validate request parameters
-- (@true@) or not (@false@).
requestValidator_validateRequestParameters :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Bool)
requestValidator_validateRequestParameters = Lens.lens (\RequestValidator' {validateRequestParameters} -> validateRequestParameters) (\s@RequestValidator' {} a -> s {validateRequestParameters = a} :: RequestValidator)

-- | The name of this RequestValidator
requestValidator_name :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Text)
requestValidator_name = Lens.lens (\RequestValidator' {name} -> name) (\s@RequestValidator' {} a -> s {name = a} :: RequestValidator)

-- | A Boolean flag to indicate whether to validate a request body according
-- to the configured Model schema.
requestValidator_validateRequestBody :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Bool)
requestValidator_validateRequestBody = Lens.lens (\RequestValidator' {validateRequestBody} -> validateRequestBody) (\s@RequestValidator' {} a -> s {validateRequestBody = a} :: RequestValidator)

-- | The identifier of this RequestValidator.
requestValidator_id :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Text)
requestValidator_id = Lens.lens (\RequestValidator' {id} -> id) (\s@RequestValidator' {} a -> s {id = a} :: RequestValidator)

instance Core.FromJSON RequestValidator where
  parseJSON =
    Core.withObject
      "RequestValidator"
      ( \x ->
          RequestValidator'
            Prelude.<$> (x Core..:? "validateRequestParameters")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "validateRequestBody")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable RequestValidator where
  hashWithSalt salt' RequestValidator' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` validateRequestBody
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` validateRequestParameters

instance Prelude.NFData RequestValidator where
  rnf RequestValidator' {..} =
    Prelude.rnf validateRequestParameters
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf validateRequestBody
      `Prelude.seq` Prelude.rnf name
