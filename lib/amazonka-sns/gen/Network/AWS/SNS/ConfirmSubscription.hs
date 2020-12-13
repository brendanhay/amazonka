{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ConfirmSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an endpoint owner's intent to receive messages by validating the token sent to the endpoint by an earlier @Subscribe@ action. If the token is valid, the action creates a new subscription and returns its Amazon Resource Name (ARN). This call requires an AWS signature only when the @AuthenticateOnUnsubscribe@ flag is set to "true".
module Network.AWS.SNS.ConfirmSubscription
  ( -- * Creating a request
    ConfirmSubscription (..),
    mkConfirmSubscription,

    -- ** Request lenses
    csToken,
    csTopicARN,
    csAuthenticateOnUnsubscribe,

    -- * Destructuring the response
    ConfirmSubscriptionResponse (..),
    mkConfirmSubscriptionResponse,

    -- ** Response lenses
    csrsSubscriptionARN,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for ConfirmSubscription action.
--
-- /See:/ 'mkConfirmSubscription' smart constructor.
data ConfirmSubscription = ConfirmSubscription'
  { -- | Short-lived token sent to an endpoint during the @Subscribe@ action.
    token :: Lude.Text,
    -- | The ARN of the topic for which you wish to confirm a subscription.
    topicARN :: Lude.Text,
    -- | Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
    authenticateOnUnsubscribe :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmSubscription' with the minimum fields required to make a request.
--
-- * 'token' - Short-lived token sent to an endpoint during the @Subscribe@ action.
-- * 'topicARN' - The ARN of the topic for which you wish to confirm a subscription.
-- * 'authenticateOnUnsubscribe' - Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
mkConfirmSubscription ::
  -- | 'token'
  Lude.Text ->
  -- | 'topicARN'
  Lude.Text ->
  ConfirmSubscription
mkConfirmSubscription pToken_ pTopicARN_ =
  ConfirmSubscription'
    { token = pToken_,
      topicARN = pTopicARN_,
      authenticateOnUnsubscribe = Lude.Nothing
    }

-- | Short-lived token sent to an endpoint during the @Subscribe@ action.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csToken :: Lens.Lens' ConfirmSubscription Lude.Text
csToken = Lens.lens (token :: ConfirmSubscription -> Lude.Text) (\s a -> s {token = a} :: ConfirmSubscription)
{-# DEPRECATED csToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The ARN of the topic for which you wish to confirm a subscription.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTopicARN :: Lens.Lens' ConfirmSubscription Lude.Text
csTopicARN = Lens.lens (topicARN :: ConfirmSubscription -> Lude.Text) (\s a -> s {topicARN = a} :: ConfirmSubscription)
{-# DEPRECATED csTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
--
-- /Note:/ Consider using 'authenticateOnUnsubscribe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAuthenticateOnUnsubscribe :: Lens.Lens' ConfirmSubscription (Lude.Maybe Lude.Text)
csAuthenticateOnUnsubscribe = Lens.lens (authenticateOnUnsubscribe :: ConfirmSubscription -> Lude.Maybe Lude.Text) (\s a -> s {authenticateOnUnsubscribe = a} :: ConfirmSubscription)
{-# DEPRECATED csAuthenticateOnUnsubscribe "Use generic-lens or generic-optics with 'authenticateOnUnsubscribe' instead." #-}

instance Lude.AWSRequest ConfirmSubscription where
  type Rs ConfirmSubscription = ConfirmSubscriptionResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ConfirmSubscriptionResult"
      ( \s h x ->
          ConfirmSubscriptionResponse'
            Lude.<$> (x Lude..@? "SubscriptionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ConfirmSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmSubscription where
  toQuery ConfirmSubscription' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ConfirmSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "Token" Lude.=: token,
        "TopicArn" Lude.=: topicARN,
        "AuthenticateOnUnsubscribe" Lude.=: authenticateOnUnsubscribe
      ]

-- | Response for ConfirmSubscriptions action.
--
-- /See:/ 'mkConfirmSubscriptionResponse' smart constructor.
data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse'
  { -- | The ARN of the created subscription.
    subscriptionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'subscriptionARN' - The ARN of the created subscription.
-- * 'responseStatus' - The response status code.
mkConfirmSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmSubscriptionResponse
mkConfirmSubscriptionResponse pResponseStatus_ =
  ConfirmSubscriptionResponse'
    { subscriptionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the created subscription.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSubscriptionARN :: Lens.Lens' ConfirmSubscriptionResponse (Lude.Maybe Lude.Text)
csrsSubscriptionARN = Lens.lens (subscriptionARN :: ConfirmSubscriptionResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionARN = a} :: ConfirmSubscriptionResponse)
{-# DEPRECATED csrsSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' ConfirmSubscriptionResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: ConfirmSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmSubscriptionResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
