{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.PutMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a 'MethodResponse' to an existing 'Method' resource.
module Network.AWS.ApiGateway.PutMethodResponse
    (
    -- * Creating a request
      PutMethodResponse (..)
    , mkPutMethodResponse
    -- ** Request lenses
    , pmrRestApiId
    , pmrResourceId
    , pmrHttpMethod
    , pmrStatusCode
    , pmrResponseModels
    , pmrResponseParameters

     -- * Destructuring the response
    , Types.MethodResponse (..)
    , Types.mkMethodResponse
    -- ** Response lenses
    , Types.mrResponseModels
    , Types.mrResponseParameters
    , Types.mrStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add a 'MethodResponse' to an existing 'Method' resource.
--
-- /See:/ 'mkPutMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'Method' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  , statusCode :: Types.StatusCode
    -- ^ [Required] The method response's status code.
  , responseModels :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
  , responseParameters :: Core.Maybe (Core.HashMap Core.Text Core.Bool)
    -- ^ A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMethodResponse' value with any optional fields omitted.
mkPutMethodResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> PutMethodResponse
mkPutMethodResponse restApiId resourceId httpMethod statusCode
  = PutMethodResponse'{restApiId, resourceId, httpMethod, statusCode,
                       responseModels = Core.Nothing, responseParameters = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrRestApiId :: Lens.Lens' PutMethodResponse Core.Text
pmrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE pmrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrResourceId :: Lens.Lens' PutMethodResponse Core.Text
pmrResourceId = Lens.field @"resourceId"
{-# INLINEABLE pmrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrHttpMethod :: Lens.Lens' PutMethodResponse Core.Text
pmrHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE pmrHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] The method response's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrStatusCode :: Lens.Lens' PutMethodResponse Types.StatusCode
pmrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE pmrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
--
-- /Note:/ Consider using 'responseModels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrResponseModels :: Lens.Lens' PutMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pmrResponseModels = Lens.field @"responseModels"
{-# INLINEABLE pmrResponseModels #-}
{-# DEPRECATED responseModels "Use generic-lens or generic-optics with 'responseModels' instead"  #-}

-- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrResponseParameters :: Lens.Lens' PutMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Bool))
pmrResponseParameters = Lens.field @"responseParameters"
{-# INLINEABLE pmrResponseParameters #-}
{-# DEPRECATED responseParameters "Use generic-lens or generic-optics with 'responseParameters' instead"  #-}

instance Core.ToQuery PutMethodResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutMethodResponse where
        toHeaders PutMethodResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON PutMethodResponse where
        toJSON PutMethodResponse{..}
          = Core.object
              (Core.catMaybes
                 [("responseModels" Core..=) Core.<$> responseModels,
                  ("responseParameters" Core..=) Core.<$> responseParameters])

instance Core.AWSRequest PutMethodResponse where
        type Rs PutMethodResponse = Types.MethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
