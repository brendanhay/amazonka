{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetRequestValidators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'RequestValidators' collection of a given 'RestApi' .
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetRequestValidators
    (
    -- * Creating a request
      GetRequestValidators (..)
    , mkGetRequestValidators
    -- ** Request lenses
    , grvRestApiId
    , grvLimit
    , grvPosition

    -- * Destructuring the response
    , GetRequestValidatorsResponse (..)
    , mkGetRequestValidatorsResponse
    -- ** Response lenses
    , grvrrsItems
    , grvrrsPosition
    , grvrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the 'RequestValidators' collection of a given 'RestApi' .
--
-- /See:/ 'mkGetRequestValidators' smart constructor.
data GetRequestValidators = GetRequestValidators'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRequestValidators' value with any optional fields omitted.
mkGetRequestValidators
    :: Core.Text -- ^ 'restApiId'
    -> GetRequestValidators
mkGetRequestValidators restApiId
  = GetRequestValidators'{restApiId, limit = Core.Nothing,
                          position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvRestApiId :: Lens.Lens' GetRequestValidators Core.Text
grvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE grvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvLimit :: Lens.Lens' GetRequestValidators (Core.Maybe Core.Int)
grvLimit = Lens.field @"limit"
{-# INLINEABLE grvLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvPosition :: Lens.Lens' GetRequestValidators (Core.Maybe Core.Text)
grvPosition = Lens.field @"position"
{-# INLINEABLE grvPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetRequestValidators where
        toQuery GetRequestValidators{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetRequestValidators where
        toHeaders GetRequestValidators{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetRequestValidators where
        type Rs GetRequestValidators = GetRequestValidatorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/requestvalidators",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRequestValidatorsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRequestValidators where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | A collection of 'RequestValidator' resources of a given 'RestApi' .
--
-- In OpenAPI, the 'RequestValidators' of an API is defined by the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html x-amazon-apigateway-request-validators> extension.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway> 
--
-- /See:/ 'mkGetRequestValidatorsResponse' smart constructor.
data GetRequestValidatorsResponse = GetRequestValidatorsResponse'
  { items :: Core.Maybe [Types.RequestValidator]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRequestValidatorsResponse' value with any optional fields omitted.
mkGetRequestValidatorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRequestValidatorsResponse
mkGetRequestValidatorsResponse responseStatus
  = GetRequestValidatorsResponse'{items = Core.Nothing,
                                  position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrrsItems :: Lens.Lens' GetRequestValidatorsResponse (Core.Maybe [Types.RequestValidator])
grvrrsItems = Lens.field @"items"
{-# INLINEABLE grvrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrrsPosition :: Lens.Lens' GetRequestValidatorsResponse (Core.Maybe Core.Text)
grvrrsPosition = Lens.field @"position"
{-# INLINEABLE grvrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrrsResponseStatus :: Lens.Lens' GetRequestValidatorsResponse Core.Int
grvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
