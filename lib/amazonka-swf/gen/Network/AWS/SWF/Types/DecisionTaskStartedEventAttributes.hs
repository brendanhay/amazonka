{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
  ( DecisionTaskStartedEventAttributes (..)
  -- * Smart constructor
  , mkDecisionTaskStartedEventAttributes
  -- * Lenses
  , dtseaScheduledEventId
  , dtseaIdentity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Identity as Types

-- | Provides the details of the @DecisionTaskStarted@ event.
--
-- /See:/ 'mkDecisionTaskStartedEventAttributes' smart constructor.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes'
  { scheduledEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , identity :: Core.Maybe Types.Identity
    -- ^ Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecisionTaskStartedEventAttributes' value with any optional fields omitted.
mkDecisionTaskStartedEventAttributes
    :: Core.Integer -- ^ 'scheduledEventId'
    -> DecisionTaskStartedEventAttributes
mkDecisionTaskStartedEventAttributes scheduledEventId
  = DecisionTaskStartedEventAttributes'{scheduledEventId,
                                        identity = Core.Nothing}

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaScheduledEventId :: Lens.Lens' DecisionTaskStartedEventAttributes Core.Integer
dtseaScheduledEventId = Lens.field @"scheduledEventId"
{-# INLINEABLE dtseaScheduledEventId #-}
{-# DEPRECATED scheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead"  #-}

-- | Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaIdentity :: Lens.Lens' DecisionTaskStartedEventAttributes (Core.Maybe Types.Identity)
dtseaIdentity = Lens.field @"identity"
{-# INLINEABLE dtseaIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

instance Core.FromJSON DecisionTaskStartedEventAttributes where
        parseJSON
          = Core.withObject "DecisionTaskStartedEventAttributes" Core.$
              \ x ->
                DecisionTaskStartedEventAttributes' Core.<$>
                  (x Core..: "scheduledEventId") Core.<*> x Core..:? "identity"
