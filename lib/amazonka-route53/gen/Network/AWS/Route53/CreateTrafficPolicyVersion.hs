{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateTrafficPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an existing traffic policy. When you create a new version of a traffic policy, you specify the ID of the traffic policy that you want to update and a JSON-formatted document that describes the new version. You use traffic policies to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com). You can create a maximum of 1000 versions of a traffic policy. If you reach the limit and need to create another version, you'll need to start a new traffic policy.
module Network.AWS.Route53.CreateTrafficPolicyVersion
  ( -- * Creating a request
    CreateTrafficPolicyVersion (..),
    mkCreateTrafficPolicyVersion,

    -- ** Request lenses
    ctpvDocument,
    ctpvId,
    ctpvComment,

    -- * Destructuring the response
    CreateTrafficPolicyVersionResponse (..),
    mkCreateTrafficPolicyVersionResponse,

    -- ** Response lenses
    ctpvrsLocation,
    ctpvrsTrafficPolicy,
    ctpvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the traffic policy that you want to create a new version for.
--
-- /See:/ 'mkCreateTrafficPolicyVersion' smart constructor.
data CreateTrafficPolicyVersion = CreateTrafficPolicyVersion'
  { -- | The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
    document :: Lude.Text,
    -- | The ID of the traffic policy for which you want to create a new version.
    id :: Lude.Text,
    -- | The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficPolicyVersion' with the minimum fields required to make a request.
--
-- * 'document' - The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
-- * 'id' - The ID of the traffic policy for which you want to create a new version.
-- * 'comment' - The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
mkCreateTrafficPolicyVersion ::
  -- | 'document'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  CreateTrafficPolicyVersion
mkCreateTrafficPolicyVersion pDocument_ pId_ =
  CreateTrafficPolicyVersion'
    { document = pDocument_,
      id = pId_,
      comment = Lude.Nothing
    }

-- | The definition of this version of the traffic policy, in JSON format. You specified the JSON in the @CreateTrafficPolicyVersion@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateTrafficPolicy.html CreateTrafficPolicy> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvDocument :: Lens.Lens' CreateTrafficPolicyVersion Lude.Text
ctpvDocument = Lens.lens (document :: CreateTrafficPolicyVersion -> Lude.Text) (\s a -> s {document = a} :: CreateTrafficPolicyVersion)
{-# DEPRECATED ctpvDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The ID of the traffic policy for which you want to create a new version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvId :: Lens.Lens' CreateTrafficPolicyVersion Lude.Text
ctpvId = Lens.lens (id :: CreateTrafficPolicyVersion -> Lude.Text) (\s a -> s {id = a} :: CreateTrafficPolicyVersion)
{-# DEPRECATED ctpvId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The comment that you specified in the @CreateTrafficPolicyVersion@ request, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvComment :: Lens.Lens' CreateTrafficPolicyVersion (Lude.Maybe Lude.Text)
ctpvComment = Lens.lens (comment :: CreateTrafficPolicyVersion -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CreateTrafficPolicyVersion)
{-# DEPRECATED ctpvComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.AWSRequest CreateTrafficPolicyVersion where
  type
    Rs CreateTrafficPolicyVersion =
      CreateTrafficPolicyVersionResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficPolicyVersionResponse'
            Lude.<$> (h Lude..# "Location")
            Lude.<*> (x Lude..@ "TrafficPolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateTrafficPolicyVersion where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyVersionRequest"

instance Lude.ToHeaders CreateTrafficPolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficPolicyVersion where
  toPath CreateTrafficPolicyVersion' {..} =
    Lude.mconcat ["/2013-04-01/trafficpolicy/", Lude.toBS id]

instance Lude.ToQuery CreateTrafficPolicyVersion where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateTrafficPolicyVersion where
  toXML CreateTrafficPolicyVersion' {..} =
    Lude.mconcat
      ["Document" Lude.@= document, "Comment" Lude.@= comment]

-- | A complex type that contains the response information for the @CreateTrafficPolicyVersion@ request.
--
-- /See:/ 'mkCreateTrafficPolicyVersionResponse' smart constructor.
data CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse'
  { -- | A unique URL that represents a new traffic policy version.
    location :: Lude.Text,
    -- | A complex type that contains settings for the new version of the traffic policy.
    trafficPolicy :: TrafficPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficPolicyVersionResponse' with the minimum fields required to make a request.
--
-- * 'location' - A unique URL that represents a new traffic policy version.
-- * 'trafficPolicy' - A complex type that contains settings for the new version of the traffic policy.
-- * 'responseStatus' - The response status code.
mkCreateTrafficPolicyVersionResponse ::
  -- | 'location'
  Lude.Text ->
  -- | 'trafficPolicy'
  TrafficPolicy ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrafficPolicyVersionResponse
mkCreateTrafficPolicyVersionResponse
  pLocation_
  pTrafficPolicy_
  pResponseStatus_ =
    CreateTrafficPolicyVersionResponse'
      { location = pLocation_,
        trafficPolicy = pTrafficPolicy_,
        responseStatus = pResponseStatus_
      }

-- | A unique URL that represents a new traffic policy version.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrsLocation :: Lens.Lens' CreateTrafficPolicyVersionResponse Lude.Text
ctpvrsLocation = Lens.lens (location :: CreateTrafficPolicyVersionResponse -> Lude.Text) (\s a -> s {location = a} :: CreateTrafficPolicyVersionResponse)
{-# DEPRECATED ctpvrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A complex type that contains settings for the new version of the traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrsTrafficPolicy :: Lens.Lens' CreateTrafficPolicyVersionResponse TrafficPolicy
ctpvrsTrafficPolicy = Lens.lens (trafficPolicy :: CreateTrafficPolicyVersionResponse -> TrafficPolicy) (\s a -> s {trafficPolicy = a} :: CreateTrafficPolicyVersionResponse)
{-# DEPRECATED ctpvrsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpvrsResponseStatus :: Lens.Lens' CreateTrafficPolicyVersionResponse Lude.Int
ctpvrsResponseStatus = Lens.lens (responseStatus :: CreateTrafficPolicyVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficPolicyVersionResponse)
{-# DEPRECATED ctpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
