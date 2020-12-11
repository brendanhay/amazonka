{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription. If the subscription requires authentication for deletion, only the owner of the subscription or the topic's owner can unsubscribe, and an AWS signature is required. If the @Unsubscribe@ call does not require authentication and the requester is not the subscription owner, a final cancellation message is delivered to the endpoint, so that the endpoint owner can easily resubscribe to the topic if the @Unsubscribe@ request was unintended.
--
-- This action is throttled at 100 transactions per second (TPS).
module Network.AWS.SNS.Unsubscribe
  ( -- * Creating a request
    Unsubscribe (..),
    mkUnsubscribe,

    -- ** Request lenses
    uSubscriptionARN,

    -- * Destructuring the response
    UnsubscribeResponse (..),
    mkUnsubscribeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for Unsubscribe action.
--
-- /See:/ 'mkUnsubscribe' smart constructor.
newtype Unsubscribe = Unsubscribe' {subscriptionARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Unsubscribe' with the minimum fields required to make a request.
--
-- * 'subscriptionARN' - The ARN of the subscription to be deleted.
mkUnsubscribe ::
  -- | 'subscriptionARN'
  Lude.Text ->
  Unsubscribe
mkUnsubscribe pSubscriptionARN_ =
  Unsubscribe' {subscriptionARN = pSubscriptionARN_}

-- | The ARN of the subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSubscriptionARN :: Lens.Lens' Unsubscribe Lude.Text
uSubscriptionARN = Lens.lens (subscriptionARN :: Unsubscribe -> Lude.Text) (\s a -> s {subscriptionARN = a} :: Unsubscribe)
{-# DEPRECATED uSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

instance Lude.AWSRequest Unsubscribe where
  type Rs Unsubscribe = UnsubscribeResponse
  request = Req.postQuery snsService
  response = Res.receiveNull UnsubscribeResponse'

instance Lude.ToHeaders Unsubscribe where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath Unsubscribe where
  toPath = Lude.const "/"

instance Lude.ToQuery Unsubscribe where
  toQuery Unsubscribe' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("Unsubscribe" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "SubscriptionArn" Lude.=: subscriptionARN
      ]

-- | /See:/ 'mkUnsubscribeResponse' smart constructor.
data UnsubscribeResponse = UnsubscribeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsubscribeResponse' with the minimum fields required to make a request.
mkUnsubscribeResponse ::
  UnsubscribeResponse
mkUnsubscribeResponse = UnsubscribeResponse'
