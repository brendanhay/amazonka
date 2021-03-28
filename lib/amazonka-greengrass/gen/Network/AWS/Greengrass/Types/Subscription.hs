{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Subscription
  ( Subscription (..)
  -- * Smart constructor
  , mkSubscription
  -- * Lenses
  , sTarget
  , sId
  , sSubject
  , sSource
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { target :: Core.Text
    -- ^ Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
  , id :: Core.Text
    -- ^ A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
  , subject :: Core.Text
    -- ^ The MQTT topic used to route the message.
  , source :: Core.Text
    -- ^ The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscription' value with any optional fields omitted.
mkSubscription
    :: Core.Text -- ^ 'target'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'subject'
    -> Core.Text -- ^ 'source'
    -> Subscription
mkSubscription target id subject source
  = Subscription'{target, id, subject, source}

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTarget :: Lens.Lens' Subscription Core.Text
sTarget = Lens.field @"target"
{-# INLINEABLE sTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Subscription Core.Text
sId = Lens.field @"id"
{-# INLINEABLE sId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The MQTT topic used to route the message.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubject :: Lens.Lens' Subscription Core.Text
sSubject = Lens.field @"subject"
{-# INLINEABLE sSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSource :: Lens.Lens' Subscription Core.Text
sSource = Lens.field @"source"
{-# INLINEABLE sSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.FromJSON Subscription where
        toJSON Subscription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Target" Core..= target), Core.Just ("Id" Core..= id),
                  Core.Just ("Subject" Core..= subject),
                  Core.Just ("Source" Core..= source)])

instance Core.FromJSON Subscription where
        parseJSON
          = Core.withObject "Subscription" Core.$
              \ x ->
                Subscription' Core.<$>
                  (x Core..: "Target") Core.<*> x Core..: "Id" Core.<*>
                    x Core..: "Subject"
                    Core.<*> x Core..: "Source"
