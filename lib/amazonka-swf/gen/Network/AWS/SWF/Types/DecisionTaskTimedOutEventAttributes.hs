{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
  ( DecisionTaskTimedOutEventAttributes (..)
  -- * Smart constructor
  , mkDecisionTaskTimedOutEventAttributes
  -- * Lenses
  , dttoeaTimeoutType
  , dttoeaScheduledEventId
  , dttoeaStartedEventId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.DecisionTaskTimeoutType as Types

-- | Provides the details of the @DecisionTaskTimedOut@ event.
--
-- /See:/ 'mkDecisionTaskTimedOutEventAttributes' smart constructor.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes'
  { timeoutType :: Types.DecisionTaskTimeoutType
    -- ^ The type of timeout that expired before the decision task could be completed.
  , scheduledEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecisionTaskTimedOutEventAttributes' value with any optional fields omitted.
mkDecisionTaskTimedOutEventAttributes
    :: Types.DecisionTaskTimeoutType -- ^ 'timeoutType'
    -> Core.Integer -- ^ 'scheduledEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> DecisionTaskTimedOutEventAttributes
mkDecisionTaskTimedOutEventAttributes timeoutType scheduledEventId
  startedEventId
  = DecisionTaskTimedOutEventAttributes'{timeoutType,
                                         scheduledEventId, startedEventId}

-- | The type of timeout that expired before the decision task could be completed.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaTimeoutType :: Lens.Lens' DecisionTaskTimedOutEventAttributes Types.DecisionTaskTimeoutType
dttoeaTimeoutType = Lens.field @"timeoutType"
{-# INLINEABLE dttoeaTimeoutType #-}
{-# DEPRECATED timeoutType "Use generic-lens or generic-optics with 'timeoutType' instead"  #-}

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaScheduledEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Core.Integer
dttoeaScheduledEventId = Lens.field @"scheduledEventId"
{-# INLINEABLE dttoeaScheduledEventId #-}
{-# DEPRECATED scheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead"  #-}

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttoeaStartedEventId :: Lens.Lens' DecisionTaskTimedOutEventAttributes Core.Integer
dttoeaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE dttoeaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

instance Core.FromJSON DecisionTaskTimedOutEventAttributes where
        parseJSON
          = Core.withObject "DecisionTaskTimedOutEventAttributes" Core.$
              \ x ->
                DecisionTaskTimedOutEventAttributes' Core.<$>
                  (x Core..: "timeoutType") Core.<*> x Core..: "scheduledEventId"
                    Core.<*> x Core..: "startedEventId"
