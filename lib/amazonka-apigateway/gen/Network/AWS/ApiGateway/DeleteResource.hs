{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Resource' resource.
module Network.AWS.ApiGateway.DeleteResource
    (
    -- * Creating a request
      DeleteResource (..)
    , mkDeleteResource
    -- ** Request lenses
    , drRestApiId
    , drResourceId

    -- * Destructuring the response
    , DeleteResourceResponse (..)
    , mkDeleteResourceResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a 'Resource' .
--
-- /See:/ 'mkDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The identifier of the 'Resource' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResource' value with any optional fields omitted.
mkDeleteResource
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> DeleteResource
mkDeleteResource restApiId resourceId
  = DeleteResource'{restApiId, resourceId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRestApiId :: Lens.Lens' DeleteResource Core.Text
drRestApiId = Lens.field @"restApiId"
{-# INLINEABLE drRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drResourceId :: Lens.Lens' DeleteResource Core.Text
drResourceId = Lens.field @"resourceId"
{-# INLINEABLE drResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery DeleteResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResource where
        toHeaders DeleteResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteResource where
        type Rs DeleteResource = DeleteResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteResourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceResponse' value with any optional fields omitted.
mkDeleteResourceResponse
    :: DeleteResourceResponse
mkDeleteResourceResponse = DeleteResourceResponse'
