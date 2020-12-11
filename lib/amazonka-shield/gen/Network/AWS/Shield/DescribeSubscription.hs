{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about the AWS Shield Advanced subscription for an account.
module Network.AWS.Shield.DescribeSubscription
  ( -- * Creating a request
    DescribeSubscription (..),
    mkDescribeSubscription,

    -- * Destructuring the response
    DescribeSubscriptionResponse (..),
    mkDescribeSubscriptionResponse,

    -- ** Response lenses
    dsrsSubscription,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeSubscription' smart constructor.
data DescribeSubscription = DescribeSubscription'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscription' with the minimum fields required to make a request.
mkDescribeSubscription ::
  DescribeSubscription
mkDescribeSubscription = DescribeSubscription'

instance Lude.AWSRequest DescribeSubscription where
  type Rs DescribeSubscription = DescribeSubscriptionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSubscriptionResponse'
            Lude.<$> (x Lude..?> "Subscription") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeSubscription" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSubscription where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSubscriptionResponse' smart constructor.
data DescribeSubscriptionResponse = DescribeSubscriptionResponse'
  { subscription ::
      Lude.Maybe Subscription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subscription' - The AWS Shield Advanced subscription details for an account.
mkDescribeSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubscriptionResponse
mkDescribeSubscriptionResponse pResponseStatus_ =
  DescribeSubscriptionResponse'
    { subscription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS Shield Advanced subscription details for an account.
--
-- /Note:/ Consider using 'subscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSubscription :: Lens.Lens' DescribeSubscriptionResponse (Lude.Maybe Subscription)
dsrsSubscription = Lens.lens (subscription :: DescribeSubscriptionResponse -> Lude.Maybe Subscription) (\s a -> s {subscription = a} :: DescribeSubscriptionResponse)
{-# DEPRECATED dsrsSubscription "Use generic-lens or generic-optics with 'subscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSubscriptionResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubscriptionResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
