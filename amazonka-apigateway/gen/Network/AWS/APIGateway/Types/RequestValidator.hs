{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.Types.RequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.RequestValidator where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | A Boolean flag to indicate whether to validate a request body according
    -- to the configured Model schema.
    validateRequestBody :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of this RequestValidator.
    id :: Prelude.Maybe Prelude.Text,
    -- | A Boolean flag to indicate whether to validate request parameters
    -- (@true@) or not (@false@).
    validateRequestParameters :: Prelude.Maybe Prelude.Bool,
    -- | The name of this RequestValidator
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validateRequestBody', 'requestValidator_validateRequestBody' - A Boolean flag to indicate whether to validate a request body according
-- to the configured Model schema.
--
-- 'id', 'requestValidator_id' - The identifier of this RequestValidator.
--
-- 'validateRequestParameters', 'requestValidator_validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters
-- (@true@) or not (@false@).
--
-- 'name', 'requestValidator_name' - The name of this RequestValidator
newRequestValidator ::
  RequestValidator
newRequestValidator =
  RequestValidator'
    { validateRequestBody =
        Prelude.Nothing,
      id = Prelude.Nothing,
      validateRequestParameters = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A Boolean flag to indicate whether to validate a request body according
-- to the configured Model schema.
requestValidator_validateRequestBody :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Bool)
requestValidator_validateRequestBody = Lens.lens (\RequestValidator' {validateRequestBody} -> validateRequestBody) (\s@RequestValidator' {} a -> s {validateRequestBody = a} :: RequestValidator)

-- | The identifier of this RequestValidator.
requestValidator_id :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Text)
requestValidator_id = Lens.lens (\RequestValidator' {id} -> id) (\s@RequestValidator' {} a -> s {id = a} :: RequestValidator)

-- | A Boolean flag to indicate whether to validate request parameters
-- (@true@) or not (@false@).
requestValidator_validateRequestParameters :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Bool)
requestValidator_validateRequestParameters = Lens.lens (\RequestValidator' {validateRequestParameters} -> validateRequestParameters) (\s@RequestValidator' {} a -> s {validateRequestParameters = a} :: RequestValidator)

-- | The name of this RequestValidator
requestValidator_name :: Lens.Lens' RequestValidator (Prelude.Maybe Prelude.Text)
requestValidator_name = Lens.lens (\RequestValidator' {name} -> name) (\s@RequestValidator' {} a -> s {name = a} :: RequestValidator)

instance Prelude.FromJSON RequestValidator where
  parseJSON =
    Prelude.withObject
      "RequestValidator"
      ( \x ->
          RequestValidator'
            Prelude.<$> (x Prelude..:? "validateRequestBody")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "validateRequestParameters")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable RequestValidator

instance Prelude.NFData RequestValidator
