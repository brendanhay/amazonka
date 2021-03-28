{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.PutGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
module Network.AWS.ApiGateway.PutGatewayResponse
    (
    -- * Creating a request
      PutGatewayResponse (..)
    , mkPutGatewayResponse
    -- ** Request lenses
    , pgrRestApiId
    , pgrResponseType
    , pgrResponseParameters
    , pgrResponseTemplates
    , pgrStatusCode

     -- * Destructuring the response
    , Types.GatewayResponse (..)
    , Types.mkGatewayResponse
    -- ** Response lenses
    , Types.grDefaultResponse
    , Types.grResponseParameters
    , Types.grResponseTemplates
    , Types.grResponseType
    , Types.grStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
--
-- /See:/ 'mkPutGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , responseType :: Types.GatewayResponseType
    -- ^ [Required] The response type of the associated 'GatewayResponse' . Valid values are 
--
--     * ACCESS_DENIED
--
--     * API_CONFIGURATION_ERROR
--
--     * AUTHORIZER_FAILURE
--
--     * AUTHORIZER_CONFIGURATION_ERROR
--
--     * BAD_REQUEST_PARAMETERS
--
--     * BAD_REQUEST_BODY
--
--     * DEFAULT_4XX
--
--     * DEFAULT_5XX
--
--     * EXPIRED_TOKEN
--
--     * INVALID_SIGNATURE
--
--     * INTEGRATION_FAILURE
--
--     * INTEGRATION_TIMEOUT
--
--     * INVALID_API_KEY
--
--     * MISSING_AUTHENTICATION_TOKEN
--
--     * QUOTA_EXCEEDED
--
--     * REQUEST_TOO_LARGE
--
--     * RESOURCE_NOT_FOUND
--
--     * THROTTLED
--
--     * UNAUTHORIZED
--
--     * UNSUPPORTED_MEDIA_TYPE
--
--
--
  , responseParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
  , responseTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
  , statusCode :: Core.Maybe Types.StatusCode
    -- ^ 'GatewayResponse' 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutGatewayResponse' value with any optional fields omitted.
mkPutGatewayResponse
    :: Core.Text -- ^ 'restApiId'
    -> Types.GatewayResponseType -- ^ 'responseType'
    -> PutGatewayResponse
mkPutGatewayResponse restApiId responseType
  = PutGatewayResponse'{restApiId, responseType,
                        responseParameters = Core.Nothing,
                        responseTemplates = Core.Nothing, statusCode = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgrRestApiId :: Lens.Lens' PutGatewayResponse Core.Text
pgrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE pgrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The response type of the associated 'GatewayResponse' . Valid values are 
--
--     * ACCESS_DENIED
--
--     * API_CONFIGURATION_ERROR
--
--     * AUTHORIZER_FAILURE
--
--     * AUTHORIZER_CONFIGURATION_ERROR
--
--     * BAD_REQUEST_PARAMETERS
--
--     * BAD_REQUEST_BODY
--
--     * DEFAULT_4XX
--
--     * DEFAULT_5XX
--
--     * EXPIRED_TOKEN
--
--     * INVALID_SIGNATURE
--
--     * INTEGRATION_FAILURE
--
--     * INTEGRATION_TIMEOUT
--
--     * INVALID_API_KEY
--
--     * MISSING_AUTHENTICATION_TOKEN
--
--     * QUOTA_EXCEEDED
--
--     * REQUEST_TOO_LARGE
--
--     * RESOURCE_NOT_FOUND
--
--     * THROTTLED
--
--     * UNAUTHORIZED
--
--     * UNSUPPORTED_MEDIA_TYPE
--
--
--
--
-- /Note:/ Consider using 'responseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgrResponseType :: Lens.Lens' PutGatewayResponse Types.GatewayResponseType
pgrResponseType = Lens.field @"responseType"
{-# INLINEABLE pgrResponseType #-}
{-# DEPRECATED responseType "Use generic-lens or generic-optics with 'responseType' instead"  #-}

-- | Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgrResponseParameters :: Lens.Lens' PutGatewayResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pgrResponseParameters = Lens.field @"responseParameters"
{-# INLINEABLE pgrResponseParameters #-}
{-# DEPRECATED responseParameters "Use generic-lens or generic-optics with 'responseParameters' instead"  #-}

-- | Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
--
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgrResponseTemplates :: Lens.Lens' PutGatewayResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
pgrResponseTemplates = Lens.field @"responseTemplates"
{-# INLINEABLE pgrResponseTemplates #-}
{-# DEPRECATED responseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead"  #-}

-- | 'GatewayResponse' 
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgrStatusCode :: Lens.Lens' PutGatewayResponse (Core.Maybe Types.StatusCode)
pgrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE pgrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.ToQuery PutGatewayResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutGatewayResponse where
        toHeaders PutGatewayResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON PutGatewayResponse where
        toJSON PutGatewayResponse{..}
          = Core.object
              (Core.catMaybes
                 [("responseParameters" Core..=) Core.<$> responseParameters,
                  ("responseTemplates" Core..=) Core.<$> responseTemplates,
                  ("statusCode" Core..=) Core.<$> statusCode])

instance Core.AWSRequest PutGatewayResponse where
        type Rs PutGatewayResponse = Types.GatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/gatewayresponses/"
                             Core.<> Core.toText responseType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
