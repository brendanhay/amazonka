{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
  ( ActivityTaskStartedEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskStartedEventAttributes,

    -- * Lenses
    atseaScheduledEventId,
    atseaIdentity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Identity as Types

-- | Provides the details of the @ActivityTaskStarted@ event.
--
-- /See:/ 'mkActivityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
    identity :: Core.Maybe Types.Identity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskStartedEventAttributes' value with any optional fields omitted.
mkActivityTaskStartedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  ActivityTaskStartedEventAttributes
mkActivityTaskStartedEventAttributes scheduledEventId =
  ActivityTaskStartedEventAttributes'
    { scheduledEventId,
      identity = Core.Nothing
    }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduledEventId :: Lens.Lens' ActivityTaskStartedEventAttributes Core.Integer
atseaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED atseaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

-- | Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaIdentity :: Lens.Lens' ActivityTaskStartedEventAttributes (Core.Maybe Types.Identity)
atseaIdentity = Lens.field @"identity"
{-# DEPRECATED atseaIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Core.FromJSON ActivityTaskStartedEventAttributes where
  parseJSON =
    Core.withObject "ActivityTaskStartedEventAttributes" Core.$
      \x ->
        ActivityTaskStartedEventAttributes'
          Core.<$> (x Core..: "scheduledEventId") Core.<*> (x Core..:? "identity")
