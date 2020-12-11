{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates AWS Shield Advanced for an account.
--
-- When you initally create a subscription, your subscription is set to be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request.
module Network.AWS.Shield.CreateSubscription
  ( -- * Creating a request
    CreateSubscription (..),
    mkCreateSubscription,

    -- * Destructuring the response
    CreateSubscriptionResponse (..),
    mkCreateSubscriptionResponse,

    -- ** Response lenses
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkCreateSubscription' smart constructor.
data CreateSubscription = CreateSubscription'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscription' with the minimum fields required to make a request.
mkCreateSubscription ::
  CreateSubscription
mkCreateSubscription = CreateSubscription'

instance Lude.AWSRequest CreateSubscription where
  type Rs CreateSubscription = CreateSubscriptionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateSubscriptionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.CreateSubscription" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSubscription where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSubscriptionResponse' smart constructor.
newtype CreateSubscriptionResponse = CreateSubscriptionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubscriptionResponse
mkCreateSubscriptionResponse pResponseStatus_ =
  CreateSubscriptionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSubscriptionResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubscriptionResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
