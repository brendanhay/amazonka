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
    rdeMessage,
    rdeResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Message as Types
import qualified Network.AWS.Lightsail.Types.Resource as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an event for a database.
--
-- /See:/ 'mkRelationalDatabaseEvent' smart constructor.
data RelationalDatabaseEvent = RelationalDatabaseEvent'
  { -- | The timestamp when the database event was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The category that the database event belongs to.
    eventCategories :: Core.Maybe [Types.String],
    -- | The message of the database event.
    message :: Core.Maybe Types.Message,
    -- | The database that the database event relates to.
    resource :: Core.Maybe Types.Resource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RelationalDatabaseEvent' value with any optional fields omitted.
mkRelationalDatabaseEvent ::
  RelationalDatabaseEvent
mkRelationalDatabaseEvent =
  RelationalDatabaseEvent'
    { createdAt = Core.Nothing,
      eventCategories = Core.Nothing,
      message = Core.Nothing,
      resource = Core.Nothing
    }

-- | The timestamp when the database event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeCreatedAt :: Lens.Lens' RelationalDatabaseEvent (Core.Maybe Core.NominalDiffTime)
rdeCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED rdeCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The category that the database event belongs to.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeEventCategories :: Lens.Lens' RelationalDatabaseEvent (Core.Maybe [Types.String])
rdeEventCategories = Lens.field @"eventCategories"
{-# DEPRECATED rdeEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The message of the database event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeMessage :: Lens.Lens' RelationalDatabaseEvent (Core.Maybe Types.Message)
rdeMessage = Lens.field @"message"
{-# DEPRECATED rdeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The database that the database event relates to.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeResource :: Lens.Lens' RelationalDatabaseEvent (Core.Maybe Types.Resource)
rdeResource = Lens.field @"resource"
{-# DEPRECATED rdeResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Core.FromJSON RelationalDatabaseEvent where
  parseJSON =
    Core.withObject "RelationalDatabaseEvent" Core.$
      \x ->
        RelationalDatabaseEvent'
          Core.<$> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "eventCategories")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "resource")
