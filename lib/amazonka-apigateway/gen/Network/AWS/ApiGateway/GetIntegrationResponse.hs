{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
module Network.AWS.ApiGateway.GetIntegrationResponse
    (
    -- * Creating a request
      GetIntegrationResponse (..)
    , mkGetIntegrationResponse
    -- ** Request lenses
    , girRestApiId
    , girResourceId
    , girHttpMethod
    , girStatusCode

     -- * Destructuring the response
    , Types.IntegrationResponse (..)
    , Types.mkIntegrationResponse
    -- ** Response lenses
    , Types.irContentHandling
    , Types.irResponseParameters
    , Types.irResponseTemplates
    , Types.irSelectionPattern
    , Types.irStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a get integration response request.
--
-- /See:/ 'mkGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a get integration response request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a get integration response request's HTTP method.
  , statusCode :: Types.StatusCode
    -- ^ [Required] Specifies a get integration response request's status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntegrationResponse' value with any optional fields omitted.
mkGetIntegrationResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> GetIntegrationResponse
mkGetIntegrationResponse restApiId resourceId httpMethod statusCode
  = GetIntegrationResponse'{restApiId, resourceId, httpMethod,
                            statusCode}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girRestApiId :: Lens.Lens' GetIntegrationResponse Core.Text
girRestApiId = Lens.field @"restApiId"
{-# INLINEABLE girRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a get integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girResourceId :: Lens.Lens' GetIntegrationResponse Core.Text
girResourceId = Lens.field @"resourceId"
{-# INLINEABLE girResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a get integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girHttpMethod :: Lens.Lens' GetIntegrationResponse Core.Text
girHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE girHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] Specifies a get integration response request's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girStatusCode :: Lens.Lens' GetIntegrationResponse Types.StatusCode
girStatusCode = Lens.field @"statusCode"
{-# INLINEABLE girStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.ToQuery GetIntegrationResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIntegrationResponse where
        toHeaders GetIntegrationResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetIntegrationResponse where
        type Rs GetIntegrationResponse = Types.IntegrationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
