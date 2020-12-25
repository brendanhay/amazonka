{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Dimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Dimensions
  ( Dimensions (..),

    -- * Smart constructor
    mkDimensions,

    -- * Lenses
    dChannel,
    dQueue,
  )
where

import qualified Network.AWS.Connect.Types.Channel as Types
import qualified Network.AWS.Connect.Types.QueueReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the dimensions for a set of metrics.
--
-- /See:/ 'mkDimensions' smart constructor.
data Dimensions = Dimensions'
  { -- | The channel used for grouping and filters.
    channel :: Core.Maybe Types.Channel,
    -- | Information about the queue for which metrics are returned.
    queue :: Core.Maybe Types.QueueReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Dimensions' value with any optional fields omitted.
mkDimensions ::
  Dimensions
mkDimensions =
  Dimensions' {channel = Core.Nothing, queue = Core.Nothing}

-- | The channel used for grouping and filters.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannel :: Lens.Lens' Dimensions (Core.Maybe Types.Channel)
dChannel = Lens.field @"channel"
{-# DEPRECATED dChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | Information about the queue for which metrics are returned.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dQueue :: Lens.Lens' Dimensions (Core.Maybe Types.QueueReference)
dQueue = Lens.field @"queue"
{-# DEPRECATED dQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

instance Core.FromJSON Dimensions where
  parseJSON =
    Core.withObject "Dimensions" Core.$
      \x ->
        Dimensions'
          Core.<$> (x Core..:? "Channel") Core.<*> (x Core..:? "Queue")
