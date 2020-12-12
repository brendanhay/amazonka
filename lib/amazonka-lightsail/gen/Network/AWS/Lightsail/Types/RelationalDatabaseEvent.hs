{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEvent
  ( RelationalDatabaseEvent (..),

    -- * Smart constructor
    mkRelationalDatabaseEvent,

    -- * Lenses
    rdeCreatedAt,
    rdeEventCategories,
    rdeResource,
    rdeMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an event for a database.
--
-- /See:/ 'mkRelationalDatabaseEvent' smart constructor.
data RelationalDatabaseEvent = RelationalDatabaseEvent'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
    eventCategories :: Lude.Maybe [Lude.Text],
    resource :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RelationalDatabaseEvent' with the minimum fields required to make a request.
--
-- * 'createdAt' - The timestamp when the database event was created.
-- * 'eventCategories' - The category that the database event belongs to.
-- * 'message' - The message of the database event.
-- * 'resource' - The database that the database event relates to.
mkRelationalDatabaseEvent ::
  RelationalDatabaseEvent
mkRelationalDatabaseEvent =
  RelationalDatabaseEvent'
    { createdAt = Lude.Nothing,
      eventCategories = Lude.Nothing,
      resource = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The timestamp when the database event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeCreatedAt :: Lens.Lens' RelationalDatabaseEvent (Lude.Maybe Lude.Timestamp)
rdeCreatedAt = Lens.lens (createdAt :: RelationalDatabaseEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: RelationalDatabaseEvent)
{-# DEPRECATED rdeCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The category that the database event belongs to.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeEventCategories :: Lens.Lens' RelationalDatabaseEvent (Lude.Maybe [Lude.Text])
rdeEventCategories = Lens.lens (eventCategories :: RelationalDatabaseEvent -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: RelationalDatabaseEvent)
{-# DEPRECATED rdeEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The database that the database event relates to.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeResource :: Lens.Lens' RelationalDatabaseEvent (Lude.Maybe Lude.Text)
rdeResource = Lens.lens (resource :: RelationalDatabaseEvent -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: RelationalDatabaseEvent)
{-# DEPRECATED rdeResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The message of the database event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeMessage :: Lens.Lens' RelationalDatabaseEvent (Lude.Maybe Lude.Text)
rdeMessage = Lens.lens (message :: RelationalDatabaseEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: RelationalDatabaseEvent)
{-# DEPRECATED rdeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON RelationalDatabaseEvent where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseEvent"
      ( \x ->
          RelationalDatabaseEvent'
            Lude.<$> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "eventCategories" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "resource")
            Lude.<*> (x Lude..:? "message")
      )
