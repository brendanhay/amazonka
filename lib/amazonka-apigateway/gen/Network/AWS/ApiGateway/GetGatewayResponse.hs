{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetGatewayResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
module Network.AWS.ApiGateway.GetGatewayResponse
    (
    -- * Creating a request
      GetGatewayResponse (..)
    , mkGetGatewayResponse
    -- ** Request lenses
    , ggrfRestApiId
    , ggrfResponseType

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

-- | Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
--
-- /See:/ 'mkGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
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

-- | Creates a 'GetGatewayResponse' value with any optional fields omitted.
mkGetGatewayResponse
    :: Core.Text -- ^ 'restApiId'
    -> Types.GatewayResponseType -- ^ 'responseType'
    -> GetGatewayResponse
mkGetGatewayResponse restApiId responseType
  = GetGatewayResponse'{restApiId, responseType}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrfRestApiId :: Lens.Lens' GetGatewayResponse Core.Text
ggrfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE ggrfRestApiId #-}
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
ggrfResponseType :: Lens.Lens' GetGatewayResponse Types.GatewayResponseType
ggrfResponseType = Lens.field @"responseType"
{-# INLINEABLE ggrfResponseType #-}
{-# DEPRECATED responseType "Use generic-lens or generic-optics with 'responseType' instead"  #-}

instance Core.ToQuery GetGatewayResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGatewayResponse where
        toHeaders GetGatewayResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetGatewayResponse where
        type Rs GetGatewayResponse = Types.GatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/gatewayresponses/"
                             Core.<> Core.toText responseType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
