{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eSourceType,
    eSourceIdentifier,
    eDate,
    eMessage,
  )
where

import Network.AWS.ElastiCache.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { sourceType :: Lude.Maybe SourceType,
    sourceIdentifier :: Lude.Maybe Lude.Text,
    date :: Lude.Maybe Lude.DateTime,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'date' - The date and time when the event occurred.
-- * 'message' - The text of the event.
-- * 'sourceIdentifier' - The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
-- * 'sourceType' - Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
mkEvent ::
  Event
mkEvent =
  Event'
    { sourceType = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      date = Lude.Nothing,
      message = Lude.Nothing
    }

-- | Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Lude.Maybe SourceType)
eSourceType = Lens.lens (sourceType :: Event -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: Event)
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceIdentifier = Lens.lens (sourceIdentifier :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: Event)
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Lude.Maybe Lude.DateTime)
eDate = Lens.lens (date :: Event -> Lude.Maybe Lude.DateTime) (\s a -> s {date = a} :: Event)
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The text of the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Event -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Event)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML Event where
  parseXML x =
    Event'
      Lude.<$> (x Lude..@? "SourceType")
      Lude.<*> (x Lude..@? "SourceIdentifier")
      Lude.<*> (x Lude..@? "Date")
      Lude.<*> (x Lude..@? "Message")
