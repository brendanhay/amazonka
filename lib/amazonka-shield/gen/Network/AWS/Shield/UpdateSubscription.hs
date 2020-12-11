{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an existing subscription. Only enter values for parameters you want to change. Empty parameters are not updated.
module Network.AWS.Shield.UpdateSubscription
  ( -- * Creating a request
    UpdateSubscription (..),
    mkUpdateSubscription,

    -- ** Request lenses
    usAutoRenew,

    -- * Destructuring the response
    UpdateSubscriptionResponse (..),
    mkUpdateSubscriptionResponse,

    -- ** Response lenses
    usrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkUpdateSubscription' smart constructor.
newtype UpdateSubscription = UpdateSubscription'
  { autoRenew ::
      Lude.Maybe AutoRenew
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubscription' with the minimum fields required to make a request.
--
-- * 'autoRenew' - When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
mkUpdateSubscription ::
  UpdateSubscription
mkUpdateSubscription =
  UpdateSubscription' {autoRenew = Lude.Nothing}

-- | When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAutoRenew :: Lens.Lens' UpdateSubscription (Lude.Maybe AutoRenew)
usAutoRenew = Lens.lens (autoRenew :: UpdateSubscription -> Lude.Maybe AutoRenew) (\s a -> s {autoRenew = a} :: UpdateSubscription)
{-# DEPRECATED usAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

instance Lude.AWSRequest UpdateSubscription where
  type Rs UpdateSubscription = UpdateSubscriptionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.UpdateSubscription" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSubscription where
  toJSON UpdateSubscription' {..} =
    Lude.object
      (Lude.catMaybes [("AutoRenew" Lude..=) Lude.<$> autoRenew])

instance Lude.ToPath UpdateSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSubscriptionResponse' smart constructor.
newtype UpdateSubscriptionResponse = UpdateSubscriptionResponse'
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

-- | Creates a value of 'UpdateSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSubscriptionResponse
mkUpdateSubscriptionResponse pResponseStatus_ =
  UpdateSubscriptionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateSubscriptionResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSubscriptionResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
