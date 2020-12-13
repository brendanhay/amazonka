{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateTrafficPolicyComment (..),
    mkUpdateTrafficPolicyComment,

    -- ** Request lenses
    utpcVersion,
    utpcId,
    utpcComment,

    -- * Destructuring the response
    UpdateTrafficPolicyCommentResponse (..),
    mkUpdateTrafficPolicyCommentResponse,

    -- ** Response lenses
    utpcrsTrafficPolicy,
    utpcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the traffic policy that you want to update the comment for.
--
-- /See:/ 'mkUpdateTrafficPolicyComment' smart constructor.
data UpdateTrafficPolicyComment = UpdateTrafficPolicyComment'
  { -- | The value of @Version@ for the traffic policy that you want to update the comment for.
    version :: Lude.Natural,
    -- | The value of @Id@ for the traffic policy that you want to update the comment for.
    id :: Lude.Text,
    -- | The new comment for the specified traffic policy and version.
    comment :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrafficPolicyComment' with the minimum fields required to make a request.
--
-- * 'version' - The value of @Version@ for the traffic policy that you want to update the comment for.
-- * 'id' - The value of @Id@ for the traffic policy that you want to update the comment for.
-- * 'comment' - The new comment for the specified traffic policy and version.
mkUpdateTrafficPolicyComment ::
  -- | 'version'
  Lude.Natural ->
  -- | 'id'
  Lude.Text ->
  -- | 'comment'
  Lude.Text ->
  UpdateTrafficPolicyComment
mkUpdateTrafficPolicyComment pVersion_ pId_ pComment_ =
  UpdateTrafficPolicyComment'
    { version = pVersion_,
      id = pId_,
      comment = pComment_
    }

-- | The value of @Version@ for the traffic policy that you want to update the comment for.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcVersion :: Lens.Lens' UpdateTrafficPolicyComment Lude.Natural
utpcVersion = Lens.lens (version :: UpdateTrafficPolicyComment -> Lude.Natural) (\s a -> s {version = a} :: UpdateTrafficPolicyComment)
{-# DEPRECATED utpcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The value of @Id@ for the traffic policy that you want to update the comment for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcId :: Lens.Lens' UpdateTrafficPolicyComment Lude.Text
utpcId = Lens.lens (id :: UpdateTrafficPolicyComment -> Lude.Text) (\s a -> s {id = a} :: UpdateTrafficPolicyComment)
{-# DEPRECATED utpcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The new comment for the specified traffic policy and version.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcComment :: Lens.Lens' UpdateTrafficPolicyComment Lude.Text
utpcComment = Lens.lens (comment :: UpdateTrafficPolicyComment -> Lude.Text) (\s a -> s {comment = a} :: UpdateTrafficPolicyComment)
{-# DEPRECATED utpcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.AWSRequest UpdateTrafficPolicyComment where
  type
    Rs UpdateTrafficPolicyComment =
      UpdateTrafficPolicyCommentResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateTrafficPolicyCommentResponse'
            Lude.<$> (x Lude..@ "TrafficPolicy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateTrafficPolicyComment where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyCommentRequest"

instance Lude.ToHeaders UpdateTrafficPolicyComment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateTrafficPolicyComment where
  toPath UpdateTrafficPolicyComment' {..} =
    Lude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Lude.toBS id,
        "/",
        Lude.toBS version
      ]

instance Lude.ToQuery UpdateTrafficPolicyComment where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML UpdateTrafficPolicyComment where
  toXML UpdateTrafficPolicyComment' {..} =
    Lude.mconcat ["Comment" Lude.@= comment]

-- | A complex type that contains the response information for the traffic policy.
--
-- /See:/ 'mkUpdateTrafficPolicyCommentResponse' smart constructor.
data UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse'
  { -- | A complex type that contains settings for the specified traffic policy.
    trafficPolicy :: TrafficPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrafficPolicyCommentResponse' with the minimum fields required to make a request.
--
-- * 'trafficPolicy' - A complex type that contains settings for the specified traffic policy.
-- * 'responseStatus' - The response status code.
mkUpdateTrafficPolicyCommentResponse ::
  -- | 'trafficPolicy'
  TrafficPolicy ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTrafficPolicyCommentResponse
mkUpdateTrafficPolicyCommentResponse
  pTrafficPolicy_
  pResponseStatus_ =
    UpdateTrafficPolicyCommentResponse'
      { trafficPolicy =
          pTrafficPolicy_,
        responseStatus = pResponseStatus_
      }

-- | A complex type that contains settings for the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcrsTrafficPolicy :: Lens.Lens' UpdateTrafficPolicyCommentResponse TrafficPolicy
utpcrsTrafficPolicy = Lens.lens (trafficPolicy :: UpdateTrafficPolicyCommentResponse -> TrafficPolicy) (\s a -> s {trafficPolicy = a} :: UpdateTrafficPolicyCommentResponse)
{-# DEPRECATED utpcrsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpcrsResponseStatus :: Lens.Lens' UpdateTrafficPolicyCommentResponse Lude.Int
utpcrsResponseStatus = Lens.lens (responseStatus :: UpdateTrafficPolicyCommentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrafficPolicyCommentResponse)
{-# DEPRECATED utpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
