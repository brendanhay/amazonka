{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
  ( ActivityTaskTimedOutEventAttributes (..)
  -- * Smart constructor
  , mkActivityTaskTimedOutEventAttributes
  -- * Lenses
  , attoeaTimeoutType
  , attoeaScheduledEventId
  , attoeaStartedEventId
  , attoeaDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityTaskTimeoutType as Types
import qualified Network.AWS.SWF.Types.LimitedData as Types

-- | Provides the details of the @ActivityTaskTimedOut@ event.
--
-- /See:/ 'mkActivityTaskTimedOutEventAttributes' smart constructor.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
  { timeoutType :: Types.ActivityTaskTimeoutType
    -- ^ The type of the timeout that caused this event.
  , scheduledEventId :: Core.Integer
    -- ^ The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , details :: Core.Maybe Types.LimitedData
    -- ^ Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskTimedOutEventAttributes' value with any optional fields omitted.
mkActivityTaskTimedOutEventAttributes
    :: Types.ActivityTaskTimeoutType -- ^ 'timeoutType'
    -> Core.Integer -- ^ 'scheduledEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> ActivityTaskTimedOutEventAttributes
mkActivityTaskTimedOutEventAttributes timeoutType scheduledEventId
  startedEventId
  = ActivityTaskTimedOutEventAttributes'{timeoutType,
                                         scheduledEventId, startedEventId, details = Core.Nothing}

-- | The type of the timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaTimeoutType :: Lens.Lens' ActivityTaskTimedOutEventAttributes Types.ActivityTaskTimeoutType
attoeaTimeoutType = Lens.field @"timeoutType"
{-# INLINEABLE attoeaTimeoutType #-}
{-# DEPRECATED timeoutType "Use generic-lens or generic-optics with 'timeoutType' instead"  #-}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaScheduledEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Core.Integer
attoeaScheduledEventId = Lens.field @"scheduledEventId"
{-# INLINEABLE attoeaScheduledEventId #-}
{-# DEPRECATED scheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead"  #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaStartedEventId :: Lens.Lens' ActivityTaskTimedOutEventAttributes Core.Integer
attoeaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE attoeaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attoeaDetails :: Lens.Lens' ActivityTaskTimedOutEventAttributes (Core.Maybe Types.LimitedData)
attoeaDetails = Lens.field @"details"
{-# INLINEABLE attoeaDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

instance Core.FromJSON ActivityTaskTimedOutEventAttributes where
        parseJSON
          = Core.withObject "ActivityTaskTimedOutEventAttributes" Core.$
              \ x ->
                ActivityTaskTimedOutEventAttributes' Core.<$>
                  (x Core..: "timeoutType") Core.<*> x Core..: "scheduledEventId"
                    Core.<*> x Core..: "startedEventId"
                    Core.<*> x Core..:? "details"
