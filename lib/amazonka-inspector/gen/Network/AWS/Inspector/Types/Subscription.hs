{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Subscription
  ( Subscription (..),

    -- * Smart constructor
    mkSubscription,

    -- * Lenses
    sResourceARN,
    sTopicARN,
    sEventSubscriptions,
  )
where

import Network.AWS.Inspector.Types.EventSubscription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a response element in the 'ListEventSubscriptions' action.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { resourceARN :: Lude.Text,
    topicARN :: Lude.Text,
    eventSubscriptions :: Lude.NonEmpty EventSubscription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- * 'eventSubscriptions' - The list of existing event subscriptions.
-- * 'resourceARN' - The ARN of the assessment template that is used during the event for which the SNS notification is sent.
-- * 'topicARN' - The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
mkSubscription ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'topicARN'
  Lude.Text ->
  -- | 'eventSubscriptions'
  Lude.NonEmpty EventSubscription ->
  Subscription
mkSubscription pResourceARN_ pTopicARN_ pEventSubscriptions_ =
  Subscription'
    { resourceARN = pResourceARN_,
      topicARN = pTopicARN_,
      eventSubscriptions = pEventSubscriptions_
    }

-- | The ARN of the assessment template that is used during the event for which the SNS notification is sent.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceARN :: Lens.Lens' Subscription Lude.Text
sResourceARN = Lens.lens (resourceARN :: Subscription -> Lude.Text) (\s a -> s {resourceARN = a} :: Subscription)
{-# DEPRECATED sResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicARN :: Lens.Lens' Subscription Lude.Text
sTopicARN = Lens.lens (topicARN :: Subscription -> Lude.Text) (\s a -> s {topicARN = a} :: Subscription)
{-# DEPRECATED sTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The list of existing event subscriptions.
--
-- /Note:/ Consider using 'eventSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEventSubscriptions :: Lens.Lens' Subscription (Lude.NonEmpty EventSubscription)
sEventSubscriptions = Lens.lens (eventSubscriptions :: Subscription -> Lude.NonEmpty EventSubscription) (\s a -> s {eventSubscriptions = a} :: Subscription)
{-# DEPRECATED sEventSubscriptions "Use generic-lens or generic-optics with 'eventSubscriptions' instead." #-}

instance Lude.FromJSON Subscription where
  parseJSON =
    Lude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Lude.<$> (x Lude..: "resourceArn")
            Lude.<*> (x Lude..: "topicArn")
            Lude.<*> (x Lude..: "eventSubscriptions")
      )
