{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.QueryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.QueryFilter
  ( QueryFilter (..),

    -- * Smart constructor
    mkQueryFilter,

    -- * Lenses
    qfDeltaTime,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DeltaTime as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information that is used to filter message data, to segregate it according to the timeframe in which it arrives.
--
-- /See:/ 'mkQueryFilter' smart constructor.
newtype QueryFilter = QueryFilter'
  { -- | Used to limit data to that which has arrived since the last execution of the action.
    deltaTime :: Core.Maybe Types.DeltaTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'QueryFilter' value with any optional fields omitted.
mkQueryFilter ::
  QueryFilter
mkQueryFilter = QueryFilter' {deltaTime = Core.Nothing}

-- | Used to limit data to that which has arrived since the last execution of the action.
--
-- /Note:/ Consider using 'deltaTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qfDeltaTime :: Lens.Lens' QueryFilter (Core.Maybe Types.DeltaTime)
qfDeltaTime = Lens.field @"deltaTime"
{-# DEPRECATED qfDeltaTime "Use generic-lens or generic-optics with 'deltaTime' instead." #-}

instance Core.FromJSON QueryFilter where
  toJSON QueryFilter {..} =
    Core.object
      (Core.catMaybes [("deltaTime" Core..=) Core.<$> deltaTime])

instance Core.FromJSON QueryFilter where
  parseJSON =
    Core.withObject "QueryFilter" Core.$
      \x -> QueryFilter' Core.<$> (x Core..:? "deltaTime")
