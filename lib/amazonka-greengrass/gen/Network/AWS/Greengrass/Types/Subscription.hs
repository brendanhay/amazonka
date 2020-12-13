{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Subscription
  ( Subscription (..),

    -- * Smart constructor
    mkSubscription,

    -- * Lenses
    sSubject,
    sSource,
    sId,
    sTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The MQTT topic used to route the message.
    subject :: Lude.Text,
    -- | The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
    source :: Lude.Text,
    -- | A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Lude.Text,
    -- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- * 'subject' - The MQTT topic used to route the message.
-- * 'source' - The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
-- * 'id' - A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
-- * 'target' - Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
mkSubscription ::
  -- | 'subject'
  Lude.Text ->
  -- | 'source'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'target'
  Lude.Text ->
  Subscription
mkSubscription pSubject_ pSource_ pId_ pTarget_ =
  Subscription'
    { subject = pSubject_,
      source = pSource_,
      id = pId_,
      target = pTarget_
    }

-- | The MQTT topic used to route the message.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubject :: Lens.Lens' Subscription Lude.Text
sSubject = Lens.lens (subject :: Subscription -> Lude.Text) (\s a -> s {subject = a} :: Subscription)
{-# DEPRECATED sSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSource :: Lens.Lens' Subscription Lude.Text
sSource = Lens.lens (source :: Subscription -> Lude.Text) (\s a -> s {source = a} :: Subscription)
{-# DEPRECATED sSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Subscription Lude.Text
sId = Lens.lens (id :: Subscription -> Lude.Text) (\s a -> s {id = a} :: Subscription)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTarget :: Lens.Lens' Subscription Lude.Text
sTarget = Lens.lens (target :: Subscription -> Lude.Text) (\s a -> s {target = a} :: Subscription)
{-# DEPRECATED sTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON Subscription where
  parseJSON =
    Lude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Lude.<$> (x Lude..: "Subject")
            Lude.<*> (x Lude..: "Source")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Target")
      )

instance Lude.ToJSON Subscription where
  toJSON Subscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Subject" Lude..= subject),
            Lude.Just ("Source" Lude..= source),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Target" Lude..= target)
          ]
      )
