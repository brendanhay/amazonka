{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventType
  ( EventType (..)
  -- * Smart constructor
  , mkEventType
  -- * Lenses
  , etCategory
  , etCode
  , etService
  ) where

import qualified Network.AWS.AWSHealth.Types.EventTypeCategory as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCode as Types
import qualified Network.AWS.AWSHealth.Types.Service as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, @issue@ ), the service (for example, @EC2@ ), and the event type code (for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
-- /See:/ 'mkEventType' smart constructor.
data EventType = EventType'
  { category :: Core.Maybe Types.EventTypeCategory
    -- ^ A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
  , code :: Core.Maybe Types.EventTypeCode
    -- ^ The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
  , service :: Core.Maybe Types.Service
    -- ^ The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventType' value with any optional fields omitted.
mkEventType
    :: EventType
mkEventType
  = EventType'{category = Core.Nothing, code = Core.Nothing,
               service = Core.Nothing}

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCategory :: Lens.Lens' EventType (Core.Maybe Types.EventTypeCategory)
etCategory = Lens.field @"category"
{-# INLINEABLE etCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCode :: Lens.Lens' EventType (Core.Maybe Types.EventTypeCode)
etCode = Lens.field @"code"
{-# INLINEABLE etCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etService :: Lens.Lens' EventType (Core.Maybe Types.Service)
etService = Lens.field @"service"
{-# INLINEABLE etService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

instance Core.FromJSON EventType where
        parseJSON
          = Core.withObject "EventType" Core.$
              \ x ->
                EventType' Core.<$>
                  (x Core..:? "category") Core.<*> x Core..:? "code" Core.<*>
                    x Core..:? "service"
