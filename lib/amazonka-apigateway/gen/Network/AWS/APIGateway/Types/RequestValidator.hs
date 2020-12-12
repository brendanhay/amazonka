{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.RequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.RequestValidator
  ( RequestValidator (..),

    -- * Smart constructor
    mkRequestValidator,

    -- * Lenses
    rvValidateRequestParameters,
    rvName,
    rvValidateRequestBody,
    rvId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of validation rules for incoming 'Method' requests.
--
-- In OpenAPI, a 'RequestValidator' of an API is defined by the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html x-amazon-apigateway-request-validators.requestValidator> object. It the referenced using the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator x-amazon-apigateway-request-validator> property.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'mkRequestValidator' smart constructor.
data RequestValidator = RequestValidator'
  { validateRequestParameters ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    validateRequestBody :: Lude.Maybe Lude.Bool,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestValidator' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of this 'RequestValidator' .
-- * 'name' - The name of this 'RequestValidator'
-- * 'validateRequestBody' - A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
-- * 'validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
mkRequestValidator ::
  RequestValidator
mkRequestValidator =
  RequestValidator'
    { validateRequestParameters = Lude.Nothing,
      name = Lude.Nothing,
      validateRequestBody = Lude.Nothing,
      id = Lude.Nothing
    }

-- | A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'validateRequestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValidateRequestParameters :: Lens.Lens' RequestValidator (Lude.Maybe Lude.Bool)
rvValidateRequestParameters = Lens.lens (validateRequestParameters :: RequestValidator -> Lude.Maybe Lude.Bool) (\s a -> s {validateRequestParameters = a} :: RequestValidator)
{-# DEPRECATED rvValidateRequestParameters "Use generic-lens or generic-optics with 'validateRequestParameters' instead." #-}

-- | The name of this 'RequestValidator'
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvName :: Lens.Lens' RequestValidator (Lude.Maybe Lude.Text)
rvName = Lens.lens (name :: RequestValidator -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RequestValidator)
{-# DEPRECATED rvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
--
-- /Note:/ Consider using 'validateRequestBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValidateRequestBody :: Lens.Lens' RequestValidator (Lude.Maybe Lude.Bool)
rvValidateRequestBody = Lens.lens (validateRequestBody :: RequestValidator -> Lude.Maybe Lude.Bool) (\s a -> s {validateRequestBody = a} :: RequestValidator)
{-# DEPRECATED rvValidateRequestBody "Use generic-lens or generic-optics with 'validateRequestBody' instead." #-}

-- | The identifier of this 'RequestValidator' .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvId :: Lens.Lens' RequestValidator (Lude.Maybe Lude.Text)
rvId = Lens.lens (id :: RequestValidator -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RequestValidator)
{-# DEPRECATED rvId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON RequestValidator where
  parseJSON =
    Lude.withObject
      "RequestValidator"
      ( \x ->
          RequestValidator'
            Lude.<$> (x Lude..:? "validateRequestParameters")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "validateRequestBody")
            Lude.<*> (x Lude..:? "id")
      )
