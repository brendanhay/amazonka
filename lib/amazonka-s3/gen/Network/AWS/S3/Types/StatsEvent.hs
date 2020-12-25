{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StatsEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StatsEvent
  ( StatsEvent (..),

    -- * Smart constructor
    mkStatsEvent,

    -- * Lenses
    seDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Stats as Types

-- | Container for the Stats Event.
--
-- /See:/ 'mkStatsEvent' smart constructor.
newtype StatsEvent = StatsEvent'
  { -- | The Stats event details.
    details :: Core.Maybe Types.Stats
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StatsEvent' value with any optional fields omitted.
mkStatsEvent ::
  StatsEvent
mkStatsEvent = StatsEvent' {details = Core.Nothing}

-- | The Stats event details.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seDetails :: Lens.Lens' StatsEvent (Core.Maybe Types.Stats)
seDetails = Lens.field @"details"
{-# DEPRECATED seDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Core.FromXML StatsEvent where
  parseXML x = StatsEvent' Core.<$> (x Core..@? "Details")
