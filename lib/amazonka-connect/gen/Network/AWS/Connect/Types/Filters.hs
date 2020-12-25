{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Filters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Filters
  ( Filters (..),

    -- * Smart constructor
    mkFilters,

    -- * Lenses
    fChannels,
    fQueues,
  )
where

import qualified Network.AWS.Connect.Types.Channel as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'mkFilters' smart constructor.
data Filters = Filters'
  { -- | The channel to use to filter the metrics.
    channels :: Core.Maybe [Types.Channel],
    -- | The queues to use to filter the metrics. You can specify up to 100 queues per request.
    queues :: Core.Maybe (Core.NonEmpty Types.QueueId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filters' value with any optional fields omitted.
mkFilters ::
  Filters
mkFilters =
  Filters' {channels = Core.Nothing, queues = Core.Nothing}

-- | The channel to use to filter the metrics.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fChannels :: Lens.Lens' Filters (Core.Maybe [Types.Channel])
fChannels = Lens.field @"channels"
{-# DEPRECATED fChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | The queues to use to filter the metrics. You can specify up to 100 queues per request.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fQueues :: Lens.Lens' Filters (Core.Maybe (Core.NonEmpty Types.QueueId))
fQueues = Lens.field @"queues"
{-# DEPRECATED fQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

instance Core.FromJSON Filters where
  toJSON Filters {..} =
    Core.object
      ( Core.catMaybes
          [ ("Channels" Core..=) Core.<$> channels,
            ("Queues" Core..=) Core.<$> queues
          ]
      )
