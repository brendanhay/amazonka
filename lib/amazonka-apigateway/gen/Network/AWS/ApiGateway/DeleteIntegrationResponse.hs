{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration response.
module Network.AWS.ApiGateway.DeleteIntegrationResponse
    (
    -- * Creating a request
      DeleteIntegrationResponse (..)
    , mkDeleteIntegrationResponse
    -- ** Request lenses
    , dirRestApiId
    , dirResourceId
    , dirHttpMethod
    , dirStatusCode

    -- * Destructuring the response
    , DeleteIntegrationResponseResponse (..)
    , mkDeleteIntegrationResponseResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a delete integration response request.
--
-- /See:/ 'mkDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a delete integration response request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a delete integration response request's HTTP method.
  , statusCode :: Types.StatusCode
    -- ^ [Required] Specifies a delete integration response request's status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntegrationResponse' value with any optional fields omitted.
mkDeleteIntegrationResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> DeleteIntegrationResponse
mkDeleteIntegrationResponse restApiId resourceId httpMethod
  statusCode
  = DeleteIntegrationResponse'{restApiId, resourceId, httpMethod,
                               statusCode}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirRestApiId :: Lens.Lens' DeleteIntegrationResponse Core.Text
dirRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dirRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a delete integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirResourceId :: Lens.Lens' DeleteIntegrationResponse Core.Text
dirResourceId = Lens.field @"resourceId"
{-# INLINEABLE dirResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a delete integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirHttpMethod :: Lens.Lens' DeleteIntegrationResponse Core.Text
dirHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE dirHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] Specifies a delete integration response request's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirStatusCode :: Lens.Lens' DeleteIntegrationResponse Types.StatusCode
dirStatusCode = Lens.field @"statusCode"
{-# INLINEABLE dirStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.ToQuery DeleteIntegrationResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIntegrationResponse where
        toHeaders DeleteIntegrationResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteIntegrationResponse where
        type Rs DeleteIntegrationResponse =
             DeleteIntegrationResponseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
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
          = Response.receiveNull DeleteIntegrationResponseResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntegrationResponseResponse' value with any optional fields omitted.
mkDeleteIntegrationResponseResponse
    :: DeleteIntegrationResponseResponse
mkDeleteIntegrationResponseResponse
  = DeleteIntegrationResponseResponse'
