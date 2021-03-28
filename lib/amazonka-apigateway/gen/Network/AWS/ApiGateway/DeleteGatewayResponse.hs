{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
module Network.AWS.ApiGateway.DeleteGatewayResponse
    (
    -- * Creating a request
      DeleteGatewayResponse (..)
    , mkDeleteGatewayResponse
    -- ** Request lenses
    , dgrRestApiId
    , dgrResponseType

    -- * Destructuring the response
    , DeleteGatewayResponseResponse (..)
    , mkDeleteGatewayResponseResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
--
-- /See:/ 'mkDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGatewayResponse' value with any optional fields omitted.
mkDeleteGatewayResponse
    :: Core.Text -- ^ 'restApiId'
    -> Types.GatewayResponseType -- ^ 'responseType'
    -> DeleteGatewayResponse
mkDeleteGatewayResponse restApiId responseType
  = DeleteGatewayResponse'{restApiId, responseType}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrRestApiId :: Lens.Lens' DeleteGatewayResponse Core.Text
dgrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dgrRestApiId #-}
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
dgrResponseType :: Lens.Lens' DeleteGatewayResponse Types.GatewayResponseType
dgrResponseType = Lens.field @"responseType"
{-# INLINEABLE dgrResponseType #-}
{-# DEPRECATED responseType "Use generic-lens or generic-optics with 'responseType' instead"  #-}

instance Core.ToQuery DeleteGatewayResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGatewayResponse where
        toHeaders DeleteGatewayResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteGatewayResponse where
        type Rs DeleteGatewayResponse = DeleteGatewayResponseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/gatewayresponses/"
                             Core.<> Core.toText responseType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteGatewayResponseResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGatewayResponseResponse' value with any optional fields omitted.
mkDeleteGatewayResponseResponse
    :: DeleteGatewayResponseResponse
mkDeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
