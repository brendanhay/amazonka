{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
  ( ActivityTaskCompletedEventAttributes (..)
  -- * Smart constructor
  , mkActivityTaskCompletedEventAttributes
  -- * Lenses
  , aScheduledEventId
  , aStartedEventId
  , aResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types

-- | Provides the details of the @ActivityTaskCompleted@ event.
--
-- /See:/ 'mkActivityTaskCompletedEventAttributes' smart constructor.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
  { scheduledEventId :: Core.Integer
    -- ^ The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , result :: Core.Maybe Types.Data
    -- ^ The results of the activity task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskCompletedEventAttributes' value with any optional fields omitted.
mkActivityTaskCompletedEventAttributes
    :: Core.Integer -- ^ 'scheduledEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> ActivityTaskCompletedEventAttributes
mkActivityTaskCompletedEventAttributes scheduledEventId
  startedEventId
  = ActivityTaskCompletedEventAttributes'{scheduledEventId,
                                          startedEventId, result = Core.Nothing}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aScheduledEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Core.Integer
aScheduledEventId = Lens.field @"scheduledEventId"
{-# INLINEABLE aScheduledEventId #-}
{-# DEPRECATED scheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead"  #-}

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStartedEventId :: Lens.Lens' ActivityTaskCompletedEventAttributes Core.Integer
aStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE aStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | The results of the activity task.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResult :: Lens.Lens' ActivityTaskCompletedEventAttributes (Core.Maybe Types.Data)
aResult = Lens.field @"result"
{-# INLINEABLE aResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON ActivityTaskCompletedEventAttributes where
        parseJSON
          = Core.withObject "ActivityTaskCompletedEventAttributes" Core.$
              \ x ->
                ActivityTaskCompletedEventAttributes' Core.<$>
                  (x Core..: "scheduledEventId") Core.<*> x Core..: "startedEventId"
                    Core.<*> x Core..:? "result"
