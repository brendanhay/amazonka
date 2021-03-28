{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an origin request policy.
--
-- You cannot delete an origin request policy if it’s attached to any cache behaviors. First update your distributions to remove the origin request policy from all cache behaviors, then delete the origin request policy.
-- To delete an origin request policy, you must provide the policy’s identifier and version. To get the identifier, you can use @ListOriginRequestPolicies@ or @GetOriginRequestPolicy@ .
module Network.AWS.CloudFront.DeleteOriginRequestPolicy
    (
    -- * Creating a request
      DeleteOriginRequestPolicy (..)
    , mkDeleteOriginRequestPolicy
    -- ** Request lenses
    , dorpId
    , dorpIfMatch

    -- * Destructuring the response
    , DeleteOriginRequestPolicyResponse (..)
    , mkDeleteOriginRequestPolicyResponse
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOriginRequestPolicy' smart constructor.
data DeleteOriginRequestPolicy = DeleteOriginRequestPolicy'
  { id :: Core.Text
    -- ^ The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOriginRequestPolicy' value with any optional fields omitted.
mkDeleteOriginRequestPolicy
    :: Core.Text -- ^ 'id'
    -> DeleteOriginRequestPolicy
mkDeleteOriginRequestPolicy id
  = DeleteOriginRequestPolicy'{id, ifMatch = Core.Nothing}

-- | The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorpId :: Lens.Lens' DeleteOriginRequestPolicy Core.Text
dorpId = Lens.field @"id"
{-# INLINEABLE dorpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorpIfMatch :: Lens.Lens' DeleteOriginRequestPolicy (Core.Maybe Core.Text)
dorpIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE dorpIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery DeleteOriginRequestPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOriginRequestPolicy where
        toHeaders DeleteOriginRequestPolicy{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest DeleteOriginRequestPolicy where
        type Rs DeleteOriginRequestPolicy =
             DeleteOriginRequestPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2020-05-31/origin-request-policy/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteOriginRequestPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOriginRequestPolicyResponse' smart constructor.
data DeleteOriginRequestPolicyResponse = DeleteOriginRequestPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOriginRequestPolicyResponse' value with any optional fields omitted.
mkDeleteOriginRequestPolicyResponse
    :: DeleteOriginRequestPolicyResponse
mkDeleteOriginRequestPolicyResponse
  = DeleteOriginRequestPolicyResponse'
