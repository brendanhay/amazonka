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
    uhzcrsHostedZone,
    uhzcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to update the comment for a hosted zone.
--
-- /See:/ 'mkUpdateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
  { -- | The ID for the hosted zone that you want to update the comment for.
    id :: ResourceId,
    -- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHostedZoneComment' with the minimum fields required to make a request.
--
-- * 'id' - The ID for the hosted zone that you want to update the comment for.
-- * 'comment' - The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
mkUpdateHostedZoneComment ::
  -- | 'id'
  ResourceId ->
  UpdateHostedZoneComment
mkUpdateHostedZoneComment pId_ =
  UpdateHostedZoneComment' {id = pId_, comment = Lude.Nothing}

-- | The ID for the hosted zone that you want to update the comment for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcId :: Lens.Lens' UpdateHostedZoneComment ResourceId
uhzcId = Lens.lens (id :: UpdateHostedZoneComment -> ResourceId) (\s a -> s {id = a} :: UpdateHostedZoneComment)
{-# DEPRECATED uhzcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcComment :: Lens.Lens' UpdateHostedZoneComment (Lude.Maybe Lude.Text)
uhzcComment = Lens.lens (comment :: UpdateHostedZoneComment -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: UpdateHostedZoneComment)
{-# DEPRECATED uhzcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.AWSRequest UpdateHostedZoneComment where
  type Rs UpdateHostedZoneComment = UpdateHostedZoneCommentResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateHostedZoneCommentResponse'
            Lude.<$> (x Lude..@ "HostedZone") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateHostedZoneComment where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance Lude.ToHeaders UpdateHostedZoneComment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateHostedZoneComment where
  toPath UpdateHostedZoneComment' {..} =
    Lude.mconcat ["/2013-04-01/hostedzone/", Lude.toBS id]

instance Lude.ToQuery UpdateHostedZoneComment where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML UpdateHostedZoneComment where
  toXML UpdateHostedZoneComment' {..} =
    Lude.mconcat ["Comment" Lude.@= comment]

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /See:/ 'mkUpdateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
  { -- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
    hostedZone :: HostedZone,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHostedZoneCommentResponse' with the minimum fields required to make a request.
--
-- * 'hostedZone' - A complex type that contains the response to the @UpdateHostedZoneComment@ request.
-- * 'responseStatus' - The response status code.
mkUpdateHostedZoneCommentResponse ::
  -- | 'hostedZone'
  HostedZone ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateHostedZoneCommentResponse
mkUpdateHostedZoneCommentResponse pHostedZone_ pResponseStatus_ =
  UpdateHostedZoneCommentResponse'
    { hostedZone = pHostedZone_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrsHostedZone :: Lens.Lens' UpdateHostedZoneCommentResponse HostedZone
uhzcrsHostedZone = Lens.lens (hostedZone :: UpdateHostedZoneCommentResponse -> HostedZone) (\s a -> s {hostedZone = a} :: UpdateHostedZoneCommentResponse)
{-# DEPRECATED uhzcrsHostedZone "Use generic-lens or generic-optics with 'hostedZone' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhzcrsResponseStatus :: Lens.Lens' UpdateHostedZoneCommentResponse Lude.Int
uhzcrsResponseStatus = Lens.lens (responseStatus :: UpdateHostedZoneCommentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateHostedZoneCommentResponse)
{-# DEPRECATED uhzcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
