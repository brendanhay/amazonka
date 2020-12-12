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
    sTarget,
    sId,
    sSubject,
    sSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { target :: Lude.Text,
    id :: Lude.Text,
    subject :: Lude.Text,
    source :: Lude.Text
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
-- * 'id' - A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
-- * 'source' - The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
-- * 'subject' - The MQTT topic used to route the message.
-- * 'target' - Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
mkSubscription ::
  -- | 'target'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'subject'
  Lude.Text ->
  -- | 'source'
  Lude.Text ->
  Subscription
mkSubscription pTarget_ pId_ pSubject_ pSource_ =
  Subscription'
    { target = pTarget_,
      id = pId_,
      subject = pSubject_,
      source = pSource_
    }

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTarget :: Lens.Lens' Subscription Lude.Text
sTarget = Lens.lens (target :: Subscription -> Lude.Text) (\s a -> s {target = a} :: Subscription)
{-# DEPRECATED sTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Subscription Lude.Text
sId = Lens.lens (id :: Subscription -> Lude.Text) (\s a -> s {id = a} :: Subscription)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

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

instance Lude.FromJSON Subscription where
  parseJSON =
    Lude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Lude.<$> (x Lude..: "Target")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Subject")
            Lude.<*> (x Lude..: "Source")
      )

instance Lude.ToJSON Subscription where
  toJSON Subscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Target" Lude..= target),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Subject" Lude..= subject),
            Lude.Just ("Source" Lude..= source)
          ]
      )
