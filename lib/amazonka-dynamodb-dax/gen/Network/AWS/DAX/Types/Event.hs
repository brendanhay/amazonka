{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eSourceName,
    eSourceType,
    eDate,
    eMessage,
  )
where

import Network.AWS.DAX.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a DAX cluster, adding or removing a node, or rebooting a node.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { sourceName :: Lude.Maybe Lude.Text,
    sourceType :: Lude.Maybe SourceType,
    date :: Lude.Maybe Lude.Timestamp,
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
-- * 'message' - A user-defined message associated with the event.
-- * 'sourceName' - The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
-- * 'sourceType' - Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
mkEvent ::
  Event
mkEvent =
  Event'
    { sourceName = Lude.Nothing,
      sourceType = Lude.Nothing,
      date = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
--
-- /Note:/ Consider using 'sourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceName :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceName = Lens.lens (sourceName :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceName = a} :: Event)
{-# DEPRECATED eSourceName "Use generic-lens or generic-optics with 'sourceName' instead." #-}

-- | Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Lude.Maybe SourceType)
eSourceType = Lens.lens (sourceType :: Event -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: Event)
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eDate = Lens.lens (date :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {date = a} :: Event)
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | A user-defined message associated with the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Event -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Event)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON Event where
  parseJSON =
    Lude.withObject
      "Event"
      ( \x ->
          Event'
            Lude.<$> (x Lude..:? "SourceName")
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "Date")
            Lude.<*> (x Lude..:? "Message")
      )
