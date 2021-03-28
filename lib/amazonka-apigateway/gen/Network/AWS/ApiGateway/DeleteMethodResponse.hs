{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'MethodResponse' resource.
module Network.AWS.ApiGateway.DeleteMethodResponse
    (
    -- * Creating a request
      DeleteMethodResponse (..)
    , mkDeleteMethodResponse
    -- ** Request lenses
    , dmrRestApiId
    , dmrResourceId
    , dmrHttpMethod
    , dmrStatusCode

    -- * Destructuring the response
    , DeleteMethodResponseResponse (..)
    , mkDeleteMethodResponseResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete an existing 'MethodResponse' resource.
--
-- /See:/ 'mkDeleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'MethodResponse' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  , statusCode :: Types.StatusCode
    -- ^ [Required] The status code identifier for the 'MethodResponse' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMethodResponse' value with any optional fields omitted.
mkDeleteMethodResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> DeleteMethodResponse
mkDeleteMethodResponse restApiId resourceId httpMethod statusCode
  = DeleteMethodResponse'{restApiId, resourceId, httpMethod,
                          statusCode}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrRestApiId :: Lens.Lens' DeleteMethodResponse Core.Text
dmrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dmrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrResourceId :: Lens.Lens' DeleteMethodResponse Core.Text
dmrResourceId = Lens.field @"resourceId"
{-# INLINEABLE dmrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrHttpMethod :: Lens.Lens' DeleteMethodResponse Core.Text
dmrHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE dmrHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] The status code identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrStatusCode :: Lens.Lens' DeleteMethodResponse Types.StatusCode
dmrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE dmrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.ToQuery DeleteMethodResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMethodResponse where
        toHeaders DeleteMethodResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteMethodResponse where
        type Rs DeleteMethodResponse = DeleteMethodResponseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteMethodResponseResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse = DeleteMethodResponseResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMethodResponseResponse' value with any optional fields omitted.
mkDeleteMethodResponseResponse
    :: DeleteMethodResponseResponse
mkDeleteMethodResponseResponse = DeleteMethodResponseResponse'
