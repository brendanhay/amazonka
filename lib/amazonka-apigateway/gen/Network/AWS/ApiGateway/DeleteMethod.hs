{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Method' resource.
module Network.AWS.ApiGateway.DeleteMethod
    (
    -- * Creating a request
      DeleteMethod (..)
    , mkDeleteMethod
    -- ** Request lenses
    , dRestApiId
    , dResourceId
    , dHttpMethod

    -- * Destructuring the response
    , DeleteMethodResponse' (..)
    , mkDeleteMethodResponse'
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing 'Method' resource.
--
-- /See:/ 'mkDeleteMethod' smart constructor.
data DeleteMethod = DeleteMethod'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'Method' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMethod' value with any optional fields omitted.
mkDeleteMethod
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> DeleteMethod
mkDeleteMethod restApiId resourceId httpMethod
  = DeleteMethod'{restApiId, resourceId, httpMethod}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRestApiId :: Lens.Lens' DeleteMethod Core.Text
dRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceId :: Lens.Lens' DeleteMethod Core.Text
dResourceId = Lens.field @"resourceId"
{-# INLINEABLE dResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHttpMethod :: Lens.Lens' DeleteMethod Core.Text
dHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE dHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

instance Core.ToQuery DeleteMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMethod where
        toHeaders DeleteMethod{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteMethod where
        type Rs DeleteMethod = DeleteMethodResponse'
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteMethodResponse''
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMethodResponse'' smart constructor.
data DeleteMethodResponse' = DeleteMethodResponse''
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMethodResponse'' value with any optional fields omitted.
mkDeleteMethodResponse'
    :: DeleteMethodResponse'
mkDeleteMethodResponse' = DeleteMethodResponse''
