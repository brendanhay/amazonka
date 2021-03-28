{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Resource' resource.
module Network.AWS.ApiGateway.CreateResource
    (
    -- * Creating a request
      CreateResource (..)
    , mkCreateResource
    -- ** Request lenses
    , crRestApiId
    , crParentId
    , crPathPart

     -- * Destructuring the response
    , Types.Resource (..)
    , Types.mkResource
    -- ** Response lenses
    , Types.rId
    , Types.rParentId
    , Types.rPath
    , Types.rPathPart
    , Types.rResourceMethods
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a 'Resource' resource.
--
-- /See:/ 'mkCreateResource' smart constructor.
data CreateResource = CreateResource'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , parentId :: Core.Text
    -- ^ [Required] The parent resource's identifier.
  , pathPart :: Core.Text
    -- ^ The last path segment for this resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResource' value with any optional fields omitted.
mkCreateResource
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'parentId'
    -> Core.Text -- ^ 'pathPart'
    -> CreateResource
mkCreateResource restApiId parentId pathPart
  = CreateResource'{restApiId, parentId, pathPart}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRestApiId :: Lens.Lens' CreateResource Core.Text
crRestApiId = Lens.field @"restApiId"
{-# INLINEABLE crRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The parent resource's identifier.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crParentId :: Lens.Lens' CreateResource Core.Text
crParentId = Lens.field @"parentId"
{-# INLINEABLE crParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The last path segment for this resource.
--
-- /Note:/ Consider using 'pathPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPathPart :: Lens.Lens' CreateResource Core.Text
crPathPart = Lens.field @"pathPart"
{-# INLINEABLE crPathPart #-}
{-# DEPRECATED pathPart "Use generic-lens or generic-optics with 'pathPart' instead"  #-}

instance Core.ToQuery CreateResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateResource where
        toHeaders CreateResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateResource where
        toJSON CreateResource{..}
          = Core.object
              (Core.catMaybes [Core.Just ("pathPart" Core..= pathPart)])

instance Core.AWSRequest CreateResource where
        type Rs CreateResource = Types.Resource
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText parentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
