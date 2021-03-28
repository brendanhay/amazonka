{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EventFilter
  ( EventFilter (..)
  -- * Smart constructor
  , mkEventFilter
  -- * Lenses
  , efFilterType
  , efDimensions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventDimensions as Types
import qualified Network.AWS.Pinpoint.Types.FilterType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for an event that causes a campaign to be sent or a journey activity to be performed.
--
-- /See:/ 'mkEventFilter' smart constructor.
data EventFilter = EventFilter'
  { filterType :: Types.FilterType
    -- ^ The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
  , dimensions :: Types.EventDimensions
    -- ^ The dimensions for the event filter to use for the campaign or the journey activity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventFilter' value with any optional fields omitted.
mkEventFilter
    :: Types.FilterType -- ^ 'filterType'
    -> Types.EventDimensions -- ^ 'dimensions'
    -> EventFilter
mkEventFilter filterType dimensions
  = EventFilter'{filterType, dimensions}

-- | The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
--
-- /Note:/ Consider using 'filterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efFilterType :: Lens.Lens' EventFilter Types.FilterType
efFilterType = Lens.field @"filterType"
{-# INLINEABLE efFilterType #-}
{-# DEPRECATED filterType "Use generic-lens or generic-optics with 'filterType' instead"  #-}

-- | The dimensions for the event filter to use for the campaign or the journey activity.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efDimensions :: Lens.Lens' EventFilter Types.EventDimensions
efDimensions = Lens.field @"dimensions"
{-# INLINEABLE efDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

instance Core.FromJSON EventFilter where
        toJSON EventFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FilterType" Core..= filterType),
                  Core.Just ("Dimensions" Core..= dimensions)])

instance Core.FromJSON EventFilter where
        parseJSON
          = Core.withObject "EventFilter" Core.$
              \ x ->
                EventFilter' Core.<$>
                  (x Core..: "FilterType") Core.<*> x Core..: "Dimensions"
