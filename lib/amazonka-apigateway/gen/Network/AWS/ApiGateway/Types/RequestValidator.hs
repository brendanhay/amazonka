{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.RequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.RequestValidator
  ( RequestValidator (..)
  -- * Smart constructor
  , mkRequestValidator
  -- * Lenses
  , rvId
  , rvName
  , rvValidateRequestBody
  , rvValidateRequestParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of validation rules for incoming 'Method' requests.
--
-- In OpenAPI, a 'RequestValidator' of an API is defined by the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.requestValidator.html x-amazon-apigateway-request-validators.requestValidator> object. It the referenced using the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validator x-amazon-apigateway-request-validator> property.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway> 
--
-- /See:/ 'mkRequestValidator' smart constructor.
data RequestValidator = RequestValidator'
  { id :: Core.Maybe Core.Text
    -- ^ The identifier of this 'RequestValidator' .
  , name :: Core.Maybe Core.Text
    -- ^ The name of this 'RequestValidator' 
  , validateRequestBody :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
  , validateRequestParameters :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestValidator' value with any optional fields omitted.
mkRequestValidator
    :: RequestValidator
mkRequestValidator
  = RequestValidator'{id = Core.Nothing, name = Core.Nothing,
                      validateRequestBody = Core.Nothing,
                      validateRequestParameters = Core.Nothing}

-- | The identifier of this 'RequestValidator' .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvId :: Lens.Lens' RequestValidator (Core.Maybe Core.Text)
rvId = Lens.field @"id"
{-# INLINEABLE rvId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of this 'RequestValidator' 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvName :: Lens.Lens' RequestValidator (Core.Maybe Core.Text)
rvName = Lens.field @"name"
{-# INLINEABLE rvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A Boolean flag to indicate whether to validate a request body according to the configured 'Model' schema.
--
-- /Note:/ Consider using 'validateRequestBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValidateRequestBody :: Lens.Lens' RequestValidator (Core.Maybe Core.Bool)
rvValidateRequestBody = Lens.field @"validateRequestBody"
{-# INLINEABLE rvValidateRequestBody #-}
{-# DEPRECATED validateRequestBody "Use generic-lens or generic-optics with 'validateRequestBody' instead"  #-}

-- | A Boolean flag to indicate whether to validate request parameters (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'validateRequestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValidateRequestParameters :: Lens.Lens' RequestValidator (Core.Maybe Core.Bool)
rvValidateRequestParameters = Lens.field @"validateRequestParameters"
{-# INLINEABLE rvValidateRequestParameters #-}
{-# DEPRECATED validateRequestParameters "Use generic-lens or generic-optics with 'validateRequestParameters' instead"  #-}

instance Core.FromJSON RequestValidator where
        parseJSON
          = Core.withObject "RequestValidator" Core.$
              \ x ->
                RequestValidator' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "validateRequestBody"
                    Core.<*> x Core..:? "validateRequestParameters"
