{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateHostedZoneComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified hosted zone.
module Network.AWS.Route53.UpdateHostedZoneComment
    (
    -- * Creating a request
      UpdateHostedZoneComment (..)
    , mkUpdateHostedZoneComment
    -- ** Request lenses
    , uhzcId
    , uhzcComment

    -- * Destructuring the response
    , UpdateHostedZoneCommentResponse (..)
    , mkUpdateHostedZoneCommentResponse
    -- ** Response lenses
    , uhzcrrsHostedZone
    , uhzcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to update the comment for a hosted zone.
--
-- /See:/ 'mkUpdateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
  { id :: Types.ResourceId
    -- ^ The ID for the hosted zone that you want to update the comment for.
  , comment :: Core.Maybe Types.ResourceDescription
    -- ^ The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHostedZoneComment' value with any optional fields omitted.
mkUpdateHostedZoneComment
    :: Types.ResourceId -- ^ 'id'
    -> UpdateHostedZoneComment
mkUpdateHostedZoneComment id
  = UpdateHostedZoneComment'{id, comment = Core.Nothing}

-- | The ID for the hosted zone that you want to update the comment for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcId :: Lens.Lens' UpdateHostedZoneComment Types.ResourceId
uhzcId = Lens.field @"id"
{-# INLINEABLE uhzcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcComment :: Lens.Lens' UpdateHostedZoneComment (Core.Maybe Types.ResourceDescription)
uhzcComment = Lens.field @"comment"
{-# INLINEABLE uhzcComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToQuery UpdateHostedZoneComment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateHostedZoneComment where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML UpdateHostedZoneComment where
        toXML UpdateHostedZoneComment{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance Core.AWSRequest UpdateHostedZoneComment where
        type Rs UpdateHostedZoneComment = UpdateHostedZoneCommentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2013-04-01/hostedzone/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateHostedZoneCommentResponse' Core.<$>
                   (x Core..@ "HostedZone") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /See:/ 'mkUpdateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
  { hostedZone :: Types.HostedZone
    -- ^ A complex type that contains the response to the @UpdateHostedZoneComment@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHostedZoneCommentResponse' value with any optional fields omitted.
mkUpdateHostedZoneCommentResponse
    :: Types.HostedZone -- ^ 'hostedZone'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateHostedZoneCommentResponse
mkUpdateHostedZoneCommentResponse hostedZone responseStatus
  = UpdateHostedZoneCommentResponse'{hostedZone, responseStatus}

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrrsHostedZone :: Lens.Lens' UpdateHostedZoneCommentResponse Types.HostedZone
uhzcrrsHostedZone = Lens.field @"hostedZone"
{-# INLINEABLE uhzcrrsHostedZone #-}
{-# DEPRECATED hostedZone "Use generic-lens or generic-optics with 'hostedZone' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrrsResponseStatus :: Lens.Lens' UpdateHostedZoneCommentResponse Core.Int
uhzcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uhzcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
