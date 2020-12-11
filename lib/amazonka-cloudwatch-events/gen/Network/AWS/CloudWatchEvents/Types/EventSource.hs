-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.EventSource
  ( EventSource (..),

    -- * Smart constructor
    mkEventSource,

    -- * Lenses
    esCreationTime,
    esState,
    esARN,
    esCreatedBy,
    esName,
    esExpirationTime,
  )
where

import Network.AWS.CloudWatchEvents.Types.EventSourceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A partner event source is created by an SaaS partner. If a customer creates a partner event bus that matches this event source, that AWS account can receive events from the partner's applications or services.
--
-- /See:/ 'mkEventSource' smart constructor.
data EventSource = EventSource'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    state :: Lude.Maybe EventSourceState,
    arn :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    expirationTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventSource' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the event source.
-- * 'createdBy' - The name of the partner that created the event source.
-- * 'creationTime' - The date and time the event source was created.
-- * 'expirationTime' - The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
-- * 'name' - The name of the event source.
-- * 'state' - The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
mkEventSource ::
  EventSource
mkEventSource =
  EventSource'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      arn = Lude.Nothing,
      createdBy = Lude.Nothing,
      name = Lude.Nothing,
      expirationTime = Lude.Nothing
    }

-- | The date and time the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreationTime :: Lens.Lens' EventSource (Lude.Maybe Lude.Timestamp)
esCreationTime = Lens.lens (creationTime :: EventSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: EventSource)
{-# DEPRECATED esCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esState :: Lens.Lens' EventSource (Lude.Maybe EventSourceState)
esState = Lens.lens (state :: EventSource -> Lude.Maybe EventSourceState) (\s a -> s {state = a} :: EventSource)
{-# DEPRECATED esState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esARN :: Lens.Lens' EventSource (Lude.Maybe Lude.Text)
esARN = Lens.lens (arn :: EventSource -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: EventSource)
{-# DEPRECATED esARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the partner that created the event source.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreatedBy :: Lens.Lens' EventSource (Lude.Maybe Lude.Text)
esCreatedBy = Lens.lens (createdBy :: EventSource -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: EventSource)
{-# DEPRECATED esCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esName :: Lens.Lens' EventSource (Lude.Maybe Lude.Text)
esName = Lens.lens (name :: EventSource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EventSource)
{-# DEPRECATED esName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExpirationTime :: Lens.Lens' EventSource (Lude.Maybe Lude.Timestamp)
esExpirationTime = Lens.lens (expirationTime :: EventSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: EventSource)
{-# DEPRECATED esExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

instance Lude.FromJSON EventSource where
  parseJSON =
    Lude.withObject
      "EventSource"
      ( \x ->
          EventSource'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ExpirationTime")
      )
