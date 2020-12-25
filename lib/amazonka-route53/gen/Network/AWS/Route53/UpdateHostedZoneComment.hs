{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateHostedZoneComment (..),
    mkUpdateHostedZoneComment,

    -- ** Request lenses
    uhzcId,
    uhzcComment,

    -- * Destructuring the response
    UpdateHostedZoneCommentResponse (..),
    mkUpdateHostedZoneCommentResponse,

    -- ** Response lenses
    uhzcrrsHostedZone,
    uhzcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to update the comment for a hosted zone.
--
-- /See:/ 'mkUpdateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
  { -- | The ID for the hosted zone that you want to update the comment for.
    id :: Types.ResourceId,
    -- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
    comment :: Core.Maybe Types.ResourceDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHostedZoneComment' value with any optional fields omitted.
mkUpdateHostedZoneComment ::
  -- | 'id'
  Types.ResourceId ->
  UpdateHostedZoneComment
mkUpdateHostedZoneComment id =
  UpdateHostedZoneComment' {id, comment = Core.Nothing}

-- | The ID for the hosted zone that you want to update the comment for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcId :: Lens.Lens' UpdateHostedZoneComment Types.ResourceId
uhzcId = Lens.field @"id"
{-# DEPRECATED uhzcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcComment :: Lens.Lens' UpdateHostedZoneComment (Core.Maybe Types.ResourceDescription)
uhzcComment = Lens.field @"comment"
{-# DEPRECATED uhzcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML UpdateHostedZoneComment where
  toXML UpdateHostedZoneComment {..} =
    Core.toXMLNode "Comment" Core.<$> comment
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance Core.AWSRequest UpdateHostedZoneComment where
  type Rs UpdateHostedZoneComment = UpdateHostedZoneCommentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/2013-04-01/hostedzone/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateHostedZoneCommentResponse'
            Core.<$> (x Core..@ "HostedZone") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /See:/ 'mkUpdateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
  { -- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
    hostedZone :: Types.HostedZone,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHostedZoneCommentResponse' value with any optional fields omitted.
mkUpdateHostedZoneCommentResponse ::
  -- | 'hostedZone'
  Types.HostedZone ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateHostedZoneCommentResponse
mkUpdateHostedZoneCommentResponse hostedZone responseStatus =
  UpdateHostedZoneCommentResponse' {hostedZone, responseStatus}

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrrsHostedZone :: Lens.Lens' UpdateHostedZoneCommentResponse Types.HostedZone
uhzcrrsHostedZone = Lens.field @"hostedZone"
{-# DEPRECATED uhzcrrsHostedZone "Use generic-lens or generic-optics with 'hostedZone' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrrsResponseStatus :: Lens.Lens' UpdateHostedZoneCommentResponse Core.Int
uhzcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uhzcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
