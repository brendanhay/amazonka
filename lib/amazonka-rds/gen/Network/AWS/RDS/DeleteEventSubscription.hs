{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an RDS event notification subscription.
module Network.AWS.RDS.DeleteEventSubscription
  ( -- * Creating a request
    DeleteEventSubscription (..),
    mkDeleteEventSubscription,

    -- ** Request lenses
    desSubscriptionName,

    -- * Destructuring the response
    DeleteEventSubscriptionResponse (..),
    mkDeleteEventSubscriptionResponse,

    -- ** Response lenses
    delrsEventSubscription,
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { subscriptionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name of the RDS event notification subscription you want to delete.
mkDeleteEventSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  DeleteEventSubscription
mkDeleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription' {subscriptionName = pSubscriptionName_}

-- | The name of the RDS event notification subscription you want to delete.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desSubscriptionName :: Lens.Lens' DeleteEventSubscription Lude.Text
desSubscriptionName = Lens.lens (subscriptionName :: DeleteEventSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: DeleteEventSubscription)
{-# DEPRECATED desSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Lude.AWSRequest DeleteEventSubscription where
  type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteEventSubscriptionResult"
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Lude.<$> (x Lude..@? "EventSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEventSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEventSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEventSubscription where
  toQuery DeleteEventSubscription' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteEventSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName
      ]

-- | /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { eventSubscription ::
      Lude.Maybe
        EventSubscription,
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

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscription' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteEventSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEventSubscriptionResponse
mkDeleteEventSubscriptionResponse pResponseStatus_ =
  DeleteEventSubscriptionResponse'
    { eventSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsEventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Lude.Maybe EventSubscription)
delrsEventSubscription = Lens.lens (eventSubscription :: DeleteEventSubscriptionResponse -> Lude.Maybe EventSubscription) (\s a -> s {eventSubscription = a} :: DeleteEventSubscriptionResponse)
{-# DEPRECATED delrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteEventSubscriptionResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteEventSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEventSubscriptionResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
