{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a source identifier from an existing RDS event notification subscription.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
  ( -- * Creating a request
    RemoveSourceIdentifierFromSubscription (..),
    mkRemoveSourceIdentifierFromSubscription,

    -- ** Request lenses
    rsifsSubscriptionName,
    rsifsSourceIdentifier,

    -- * Destructuring the response
    RemoveSourceIdentifierFromSubscriptionResponse (..),
    mkRemoveSourceIdentifierFromSubscriptionResponse,

    -- ** Response lenses
    rsifsrsEventSubscription,
    rsifsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRemoveSourceIdentifierFromSubscription' smart constructor.
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription'
  { -- | The name of the RDS event notification subscription you want to remove a source identifier from.
    subscriptionName :: Lude.Text,
    -- | The source identifier to be removed from the subscription, such as the __DB instance identifier__ for a DB instance or the name of a security group.
    sourceIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveSourceIdentifierFromSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name of the RDS event notification subscription you want to remove a source identifier from.
-- * 'sourceIdentifier' - The source identifier to be removed from the subscription, such as the __DB instance identifier__ for a DB instance or the name of a security group.
mkRemoveSourceIdentifierFromSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  -- | 'sourceIdentifier'
  Lude.Text ->
  RemoveSourceIdentifierFromSubscription
mkRemoveSourceIdentifierFromSubscription
  pSubscriptionName_
  pSourceIdentifier_ =
    RemoveSourceIdentifierFromSubscription'
      { subscriptionName =
          pSubscriptionName_,
        sourceIdentifier = pSourceIdentifier_
      }

-- | The name of the RDS event notification subscription you want to remove a source identifier from.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsSubscriptionName :: Lens.Lens' RemoveSourceIdentifierFromSubscription Lude.Text
rsifsSubscriptionName = Lens.lens (subscriptionName :: RemoveSourceIdentifierFromSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: RemoveSourceIdentifierFromSubscription)
{-# DEPRECATED rsifsSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The source identifier to be removed from the subscription, such as the __DB instance identifier__ for a DB instance or the name of a security group.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsSourceIdentifier :: Lens.Lens' RemoveSourceIdentifierFromSubscription Lude.Text
rsifsSourceIdentifier = Lens.lens (sourceIdentifier :: RemoveSourceIdentifierFromSubscription -> Lude.Text) (\s a -> s {sourceIdentifier = a} :: RemoveSourceIdentifierFromSubscription)
{-# DEPRECATED rsifsSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Lude.AWSRequest RemoveSourceIdentifierFromSubscription where
  type
    Rs RemoveSourceIdentifierFromSubscription =
      RemoveSourceIdentifierFromSubscriptionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RemoveSourceIdentifierFromSubscriptionResult"
      ( \s h x ->
          RemoveSourceIdentifierFromSubscriptionResponse'
            Lude.<$> (x Lude..@? "EventSubscription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveSourceIdentifierFromSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveSourceIdentifierFromSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveSourceIdentifierFromSubscription where
  toQuery RemoveSourceIdentifierFromSubscription' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RemoveSourceIdentifierFromSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName,
        "SourceIdentifier" Lude.=: sourceIdentifier
      ]

-- | /See:/ 'mkRemoveSourceIdentifierFromSubscriptionResponse' smart constructor.
data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse'
  { eventSubscription :: Lude.Maybe EventSubscription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveSourceIdentifierFromSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscription' -
-- * 'responseStatus' - The response status code.
mkRemoveSourceIdentifierFromSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveSourceIdentifierFromSubscriptionResponse
mkRemoveSourceIdentifierFromSubscriptionResponse pResponseStatus_ =
  RemoveSourceIdentifierFromSubscriptionResponse'
    { eventSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsrsEventSubscription :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse (Lude.Maybe EventSubscription)
rsifsrsEventSubscription = Lens.lens (eventSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> Lude.Maybe EventSubscription) (\s a -> s {eventSubscription = a} :: RemoveSourceIdentifierFromSubscriptionResponse)
{-# DEPRECATED rsifsrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsrsResponseStatus :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse Lude.Int
rsifsrsResponseStatus = Lens.lens (responseStatus :: RemoveSourceIdentifierFromSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveSourceIdentifierFromSubscriptionResponse)
{-# DEPRECATED rsifsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
