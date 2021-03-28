{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'ReqeustValidator' of a given 'RestApi' .
module Network.AWS.ApiGateway.CreateRequestValidator
    (
    -- * Creating a request
      CreateRequestValidator (..)
    , mkCreateRequestValidator
    -- ** Request lenses
    , crvRestApiId
    , crvName
    , crvValidateRequestBody
    , crvValidateRequestParameters

     -- * Destructuring the response
    , Types.RequestValidator (..)
    , Types.mkRequestValidator
    -- ** Response lenses
    , Types.rvId
    , Types.rvName
    , Types.rvValidateRequestBody
    , Types.rvValidateRequestParameters
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkCreateRequestValidator' smart constructor.
data CreateRequestValidator = CreateRequestValidator'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , name :: Core.Maybe Core.Text
    -- ^ The name of the to-be-created 'RequestValidator' .
  , validateRequestBody :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
  , validateRequestParameters :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRequestValidator' value with any optional fields omitted.
mkCreateRequestValidator
    :: Core.Text -- ^ 'restApiId'
    -> CreateRequestValidator
mkCreateRequestValidator restApiId
  = CreateRequestValidator'{restApiId, name = Core.Nothing,
                            validateRequestBody = Core.Nothing,
                            validateRequestParameters = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvRestApiId :: Lens.Lens' CreateRequestValidator Core.Text
crvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE crvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The name of the to-be-created 'RequestValidator' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvName :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Text)
crvName = Lens.field @"name"
{-# INLINEABLE crvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'validateRequestBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvValidateRequestBody :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Bool)
crvValidateRequestBody = Lens.field @"validateRequestBody"
{-# INLINEABLE crvValidateRequestBody #-}
{-# DEPRECATED validateRequestBody "Use generic-lens or generic-optics with 'validateRequestBody' instead"  #-}

-- | A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
--
-- /Note:/ Consider using 'validateRequestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvValidateRequestParameters :: Lens.Lens' CreateRequestValidator (Core.Maybe Core.Bool)
crvValidateRequestParameters = Lens.field @"validateRequestParameters"
{-# INLINEABLE crvValidateRequestParameters #-}
{-# DEPRECATED validateRequestParameters "Use generic-lens or generic-optics with 'validateRequestParameters' instead"  #-}

instance Core.ToQuery CreateRequestValidator where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRequestValidator where
        toHeaders CreateRequestValidator{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateRequestValidator where
        toJSON CreateRequestValidator{..}
          = Core.object
              (Core.catMaybes
                 [("name" Core..=) Core.<$> name,
                  ("validateRequestBody" Core..=) Core.<$> validateRequestBody,
                  ("validateRequestParameters" Core..=) Core.<$>
                    validateRequestParameters])

instance Core.AWSRequest CreateRequestValidator where
        type Rs CreateRequestValidator = Types.RequestValidator
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/requestvalidators",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
