{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Subscription
  ( Subscription (..)
  -- * Smart constructor
  , mkSubscription
  -- * Lenses
  , sResourceArn
  , sTopicArn
  , sEventSubscriptions
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.EventSubscription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a response element in the 'ListEventSubscriptions' action.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { resourceArn :: Types.Arn
    -- ^ The ARN of the assessment template that is used during the event for which the SNS notification is sent.
  , topicArn :: Types.Arn
    -- ^ The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
  , eventSubscriptions :: Core.NonEmpty Types.EventSubscription
    -- ^ The list of existing event subscriptions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Subscription' value with any optional fields omitted.
mkSubscription
    :: Types.Arn -- ^ 'resourceArn'
    -> Types.Arn -- ^ 'topicArn'
    -> Core.NonEmpty Types.EventSubscription -- ^ 'eventSubscriptions'
    -> Subscription
mkSubscription resourceArn topicArn eventSubscriptions
  = Subscription'{resourceArn, topicArn, eventSubscriptions}

-- | The ARN of the assessment template that is used during the event for which the SNS notification is sent.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceArn :: Lens.Lens' Subscription Types.Arn
sResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE sResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The ARN of the Amazon Simple Notification Service (SNS) topic to which the SNS notifications are sent.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicArn :: Lens.Lens' Subscription Types.Arn
sTopicArn = Lens.field @"topicArn"
{-# INLINEABLE sTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | The list of existing event subscriptions.
--
-- /Note:/ Consider using 'eventSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEventSubscriptions :: Lens.Lens' Subscription (Core.NonEmpty Types.EventSubscription)
sEventSubscriptions = Lens.field @"eventSubscriptions"
{-# INLINEABLE sEventSubscriptions #-}
{-# DEPRECATED eventSubscriptions "Use generic-lens or generic-optics with 'eventSubscriptions' instead"  #-}

instance Core.FromJSON Subscription where
        parseJSON
          = Core.withObject "Subscription" Core.$
              \ x ->
                Subscription' Core.<$>
                  (x Core..: "resourceArn") Core.<*> x Core..: "topicArn" Core.<*>
                    x Core..: "eventSubscriptions"
