{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified traffic policy version.
module Network.AWS.Route53.UpdateTrafficPolicyComment
    (
    -- * Creating a request
      UpdateTrafficPolicyComment (..)
    , mkUpdateTrafficPolicyComment
    -- ** Request lenses
    , utpcId
    , utpcVersion
    , utpcComment

    -- * Destructuring the response
    , UpdateTrafficPolicyCommentResponse (..)
    , mkUpdateTrafficPolicyCommentResponse
    -- ** Response lenses
    , utpcrrsTrafficPolicy
    , utpcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the traffic policy that you want to update the comment for.
--
-- /See:/ 'mkUpdateTrafficPolicyComment' smart constructor.
data UpdateTrafficPolicyComment = UpdateTrafficPolicyComment'
  { id :: Types.Id
    -- ^ The value of @Id@ for the traffic policy that you want to update the comment for.
  , version :: Core.Natural
    -- ^ The value of @Version@ for the traffic policy that you want to update the comment for.
  , comment :: Types.Comment
    -- ^ The new comment for the specified traffic policy and version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrafficPolicyComment' value with any optional fields omitted.
mkUpdateTrafficPolicyComment
    :: Types.Id -- ^ 'id'
    -> Core.Natural -- ^ 'version'
    -> Types.Comment -- ^ 'comment'
    -> UpdateTrafficPolicyComment
mkUpdateTrafficPolicyComment id version comment
  = UpdateTrafficPolicyComment'{id, version, comment}

-- | The value of @Id@ for the traffic policy that you want to update the comment for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcId :: Lens.Lens' UpdateTrafficPolicyComment Types.Id
utpcId = Lens.field @"id"
{-# INLINEABLE utpcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of @Version@ for the traffic policy that you want to update the comment for.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcVersion :: Lens.Lens' UpdateTrafficPolicyComment Core.Natural
utpcVersion = Lens.field @"version"
{-# INLINEABLE utpcVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The new comment for the specified traffic policy and version.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcComment :: Lens.Lens' UpdateTrafficPolicyComment Types.Comment
utpcComment = Lens.field @"comment"
{-# INLINEABLE utpcComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToQuery UpdateTrafficPolicyComment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTrafficPolicyComment where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML UpdateTrafficPolicyComment where
        toXML UpdateTrafficPolicyComment{..}
          = Core.toXMLElement "Comment" comment
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyCommentRequest"

instance Core.AWSRequest UpdateTrafficPolicyComment where
        type Rs UpdateTrafficPolicyComment =
             UpdateTrafficPolicyCommentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2013-04-01/trafficpolicy/" Core.<> Core.toText id Core.<> "/"
                             Core.<> Core.toText version,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateTrafficPolicyCommentResponse' Core.<$>
                   (x Core..@ "TrafficPolicy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the traffic policy.
--
-- /See:/ 'mkUpdateTrafficPolicyCommentResponse' smart constructor.
data UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse'
  { trafficPolicy :: Types.TrafficPolicy
    -- ^ A complex type that contains settings for the specified traffic policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrafficPolicyCommentResponse' value with any optional fields omitted.
mkUpdateTrafficPolicyCommentResponse
    :: Types.TrafficPolicy -- ^ 'trafficPolicy'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateTrafficPolicyCommentResponse
mkUpdateTrafficPolicyCommentResponse trafficPolicy responseStatus
  = UpdateTrafficPolicyCommentResponse'{trafficPolicy,
                                        responseStatus}

-- | A complex type that contains settings for the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcrrsTrafficPolicy :: Lens.Lens' UpdateTrafficPolicyCommentResponse Types.TrafficPolicy
utpcrrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# INLINEABLE utpcrrsTrafficPolicy #-}
{-# DEPRECATED trafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcrrsResponseStatus :: Lens.Lens' UpdateTrafficPolicyCommentResponse Core.Int
utpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
