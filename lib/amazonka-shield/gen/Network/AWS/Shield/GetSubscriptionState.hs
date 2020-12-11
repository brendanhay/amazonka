{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.GetSubscriptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @SubscriptionState@ , either @Active@ or @Inactive@ .
module Network.AWS.Shield.GetSubscriptionState
  ( -- * Creating a request
    GetSubscriptionState (..),
    mkGetSubscriptionState,

    -- * Destructuring the response
    GetSubscriptionStateResponse (..),
    mkGetSubscriptionStateResponse,

    -- ** Response lenses
    gssrsResponseStatus,
    gssrsSubscriptionState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkGetSubscriptionState' smart constructor.
data GetSubscriptionState = GetSubscriptionState'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionState' with the minimum fields required to make a request.
mkGetSubscriptionState ::
  GetSubscriptionState
mkGetSubscriptionState = GetSubscriptionState'

instance Lude.AWSRequest GetSubscriptionState where
  type Rs GetSubscriptionState = GetSubscriptionStateResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSubscriptionStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "SubscriptionState")
      )

instance Lude.ToHeaders GetSubscriptionState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.GetSubscriptionState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSubscriptionState where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetSubscriptionState where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSubscriptionState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSubscriptionStateResponse' smart constructor.
data GetSubscriptionStateResponse = GetSubscriptionStateResponse'
  { responseStatus ::
      Lude.Int,
    subscriptionState ::
      SubscriptionState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subscriptionState' - The status of the subscription.
mkGetSubscriptionStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'subscriptionState'
  SubscriptionState ->
  GetSubscriptionStateResponse
mkGetSubscriptionStateResponse pResponseStatus_ pSubscriptionState_ =
  GetSubscriptionStateResponse'
    { responseStatus = pResponseStatus_,
      subscriptionState = pSubscriptionState_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsResponseStatus :: Lens.Lens' GetSubscriptionStateResponse Lude.Int
gssrsResponseStatus = Lens.lens (responseStatus :: GetSubscriptionStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSubscriptionStateResponse)
{-# DEPRECATED gssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The status of the subscription.
--
-- /Note:/ Consider using 'subscriptionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsSubscriptionState :: Lens.Lens' GetSubscriptionStateResponse SubscriptionState
gssrsSubscriptionState = Lens.lens (subscriptionState :: GetSubscriptionStateResponse -> SubscriptionState) (\s a -> s {subscriptionState = a} :: GetSubscriptionStateResponse)
{-# DEPRECATED gssrsSubscriptionState "Use generic-lens or generic-optics with 'subscriptionState' instead." #-}
