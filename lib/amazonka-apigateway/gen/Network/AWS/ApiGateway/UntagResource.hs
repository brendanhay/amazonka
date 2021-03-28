{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a tag from a given resource.
module Network.AWS.ApiGateway.UntagResource
    (
    -- * Creating a request
      UntagResource (..)
    , mkUntagResource
    -- ** Request lenses
    , urResourceArn
    , urTagKeys

    -- * Destructuring the response
    , UntagResourceResponse (..)
    , mkUntagResourceResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes a tag from a given resource.
--
-- /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { resourceArn :: Core.Text
    -- ^ [Required] The ARN of a resource that can be tagged.
  , tagKeys :: [Core.Text]
    -- ^ [Required] The Tag keys to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource
    :: Core.Text -- ^ 'resourceArn'
    -> UntagResource
mkUntagResource resourceArn
  = UntagResource'{resourceArn, tagKeys = Core.mempty}

-- | [Required] The ARN of a resource that can be tagged.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceArn :: Lens.Lens' UntagResource Core.Text
urResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE urResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | [Required] The Tag keys to delete.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Core.Text]
urTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE urTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagResource where
        toQuery UntagResource{..}
          = Core.toQueryPair "tagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders UntagResource where
        toHeaders UntagResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/tags/" Core.<> Core.toText resourceArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UntagResourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourceResponse' value with any optional fields omitted.
mkUntagResourceResponse
    :: UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
