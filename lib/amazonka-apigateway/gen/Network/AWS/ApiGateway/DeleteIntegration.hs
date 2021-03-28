{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration.
module Network.AWS.ApiGateway.DeleteIntegration
    (
    -- * Creating a request
      DeleteIntegration (..)
    , mkDeleteIntegration
    -- ** Request lenses
    , diRestApiId
    , diResourceId
    , diHttpMethod

    -- * Destructuring the response
    , DeleteIntegrationResponse' (..)
    , mkDeleteIntegrationResponse'
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a delete integration request.
--
-- /See:/ 'mkDeleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a delete integration request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a delete integration request's HTTP method.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntegration' value with any optional fields omitted.
mkDeleteIntegration
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> DeleteIntegration
mkDeleteIntegration restApiId resourceId httpMethod
  = DeleteIntegration'{restApiId, resourceId, httpMethod}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRestApiId :: Lens.Lens' DeleteIntegration Core.Text
diRestApiId = Lens.field @"restApiId"
{-# INLINEABLE diRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a delete integration request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResourceId :: Lens.Lens' DeleteIntegration Core.Text
diResourceId = Lens.field @"resourceId"
{-# INLINEABLE diResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a delete integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diHttpMethod :: Lens.Lens' DeleteIntegration Core.Text
diHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE diHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

instance Core.ToQuery DeleteIntegration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIntegration where
        toHeaders DeleteIntegration{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteIntegration where
        type Rs DeleteIntegration = DeleteIntegrationResponse'
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteIntegrationResponse''
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' = DeleteIntegrationResponse''
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntegrationResponse'' value with any optional fields omitted.
mkDeleteIntegrationResponse'
    :: DeleteIntegrationResponse'
mkDeleteIntegrationResponse' = DeleteIntegrationResponse''
