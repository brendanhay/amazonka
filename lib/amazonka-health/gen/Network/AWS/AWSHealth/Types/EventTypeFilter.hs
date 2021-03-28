{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventTypeFilter
  ( EventTypeFilter (..)
  -- * Smart constructor
  , mkEventTypeFilter
  -- * Lenses
  , etfEventTypeCategories
  , etfEventTypeCodes
  , etfServices
  ) where

import qualified Network.AWS.AWSHealth.Types.EventTypeCategory as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCode as Types
import qualified Network.AWS.AWSHealth.Types.Service as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventTypes.html DescribeEventTypes> operation.
--
-- /See:/ 'mkEventTypeFilter' smart constructor.
data EventTypeFilter = EventTypeFilter'
  { eventTypeCategories :: Core.Maybe (Core.NonEmpty Types.EventTypeCategory)
    -- ^ A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
  , eventTypeCodes :: Core.Maybe (Core.NonEmpty Types.EventTypeCode)
    -- ^ A list of event type codes.
  , services :: Core.Maybe (Core.NonEmpty Types.Service)
    -- ^ The AWS services associated with the event. For example, @EC2@ , @RDS@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventTypeFilter' value with any optional fields omitted.
mkEventTypeFilter
    :: EventTypeFilter
mkEventTypeFilter
  = EventTypeFilter'{eventTypeCategories = Core.Nothing,
                     eventTypeCodes = Core.Nothing, services = Core.Nothing}

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'eventTypeCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfEventTypeCategories :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty Types.EventTypeCategory))
etfEventTypeCategories = Lens.field @"eventTypeCategories"
{-# INLINEABLE etfEventTypeCategories #-}
{-# DEPRECATED eventTypeCategories "Use generic-lens or generic-optics with 'eventTypeCategories' instead"  #-}

-- | A list of event type codes.
--
-- /Note:/ Consider using 'eventTypeCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfEventTypeCodes :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty Types.EventTypeCode))
etfEventTypeCodes = Lens.field @"eventTypeCodes"
{-# INLINEABLE etfEventTypeCodes #-}
{-# DEPRECATED eventTypeCodes "Use generic-lens or generic-optics with 'eventTypeCodes' instead"  #-}

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfServices :: Lens.Lens' EventTypeFilter (Core.Maybe (Core.NonEmpty Types.Service))
etfServices = Lens.field @"services"
{-# INLINEABLE etfServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

instance Core.FromJSON EventTypeFilter where
        toJSON EventTypeFilter{..}
          = Core.object
              (Core.catMaybes
                 [("eventTypeCategories" Core..=) Core.<$> eventTypeCategories,
                  ("eventTypeCodes" Core..=) Core.<$> eventTypeCodes,
                  ("services" Core..=) Core.<$> services])
