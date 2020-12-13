{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventBus
  ( EventBus (..),

    -- * Smart constructor
    mkEventBus,

    -- * Lenses
    ebARN,
    ebName,
    ebPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An event bus receives events from a source and routes them to rules associated with that event bus. Your account's default event bus receives rules from AWS services. A custom event bus can receive rules from AWS services as well as your custom applications and services. A partner event bus receives events from an event source created by an SaaS partner. These events come from the partners services or applications.
--
-- /See:/ 'mkEventBus' smart constructor.
data EventBus = EventBus'
  { -- | The ARN of the event bus.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the event bus.
    name :: Lude.Maybe Lude.Text,
    -- | The permissions policy of the event bus, describing which other AWS accounts can write events to this event bus.
    policy :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventBus' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the event bus.
-- * 'name' - The name of the event bus.
-- * 'policy' - The permissions policy of the event bus, describing which other AWS accounts can write events to this event bus.
mkEventBus ::
  EventBus
mkEventBus =
  EventBus'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      policy = Lude.Nothing
    }

-- | The ARN of the event bus.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebARN :: Lens.Lens' EventBus (Lude.Maybe Lude.Text)
ebARN = Lens.lens (arn :: EventBus -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: EventBus)
{-# DEPRECATED ebARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the event bus.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebName :: Lens.Lens' EventBus (Lude.Maybe Lude.Text)
ebName = Lens.lens (name :: EventBus -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EventBus)
{-# DEPRECATED ebName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The permissions policy of the event bus, describing which other AWS accounts can write events to this event bus.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebPolicy :: Lens.Lens' EventBus (Lude.Maybe Lude.Text)
ebPolicy = Lens.lens (policy :: EventBus -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: EventBus)
{-# DEPRECATED ebPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.FromJSON EventBus where
  parseJSON =
    Lude.withObject
      "EventBus"
      ( \x ->
          EventBus'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Policy")
      )
