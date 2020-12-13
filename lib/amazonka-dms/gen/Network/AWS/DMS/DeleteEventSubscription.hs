{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS DMS event subscription.
module Network.AWS.DMS.DeleteEventSubscription
  ( -- * Creating a request
    DeleteEventSubscription (..),
    mkDeleteEventSubscription,

    -- ** Request lenses
    dSubscriptionName,

    -- * Destructuring the response
    DeleteEventSubscriptionResponse (..),
    mkDeleteEventSubscriptionResponse,

    -- ** Response lenses
    desfrsEventSubscription,
    desfrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the DMS event notification subscription to be deleted.
    subscriptionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name of the DMS event notification subscription to be deleted.
mkDeleteEventSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  DeleteEventSubscription
mkDeleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription' {subscriptionName = pSubscriptionName_}

-- | The name of the DMS event notification subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubscriptionName :: Lens.Lens' DeleteEventSubscription Lude.Text
dSubscriptionName = Lens.lens (subscriptionName :: DeleteEventSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: DeleteEventSubscription)
{-# DEPRECATED dSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Lude.AWSRequest DeleteEventSubscription where
  type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Lude.<$> (x Lude..?> "EventSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEventSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DeleteEventSubscription" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEventSubscription where
  toJSON DeleteEventSubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SubscriptionName" Lude..= subscriptionName)]
      )

instance Lude.ToPath DeleteEventSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEventSubscription where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { -- | The event subscription that was deleted.
    eventSubscription :: Lude.Maybe EventSubscription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscription' - The event subscription that was deleted.
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

-- | The event subscription that was deleted.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desfrsEventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Lude.Maybe EventSubscription)
desfrsEventSubscription = Lens.lens (eventSubscription :: DeleteEventSubscriptionResponse -> Lude.Maybe EventSubscription) (\s a -> s {eventSubscription = a} :: DeleteEventSubscriptionResponse)
{-# DEPRECATED desfrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desfrsResponseStatus :: Lens.Lens' DeleteEventSubscriptionResponse Lude.Int
desfrsResponseStatus = Lens.lens (responseStatus :: DeleteEventSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEventSubscriptionResponse)
{-# DEPRECATED desfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
