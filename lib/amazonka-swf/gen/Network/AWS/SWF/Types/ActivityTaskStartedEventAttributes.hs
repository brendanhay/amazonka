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
    atseaIdentity,
    atseaScheduledEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @ActivityTaskStarted@ event.
--
-- /See:/ 'mkActivityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { identity ::
      Lude.Maybe Lude.Text,
    scheduledEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'identity' - Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
-- * 'scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkActivityTaskStartedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  ActivityTaskStartedEventAttributes
mkActivityTaskStartedEventAttributes pScheduledEventId_ =
  ActivityTaskStartedEventAttributes'
    { identity = Lude.Nothing,
      scheduledEventId = pScheduledEventId_
    }

-- | Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaIdentity :: Lens.Lens' ActivityTaskStartedEventAttributes (Lude.Maybe Lude.Text)
atseaIdentity = Lens.lens (identity :: ActivityTaskStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {identity = a} :: ActivityTaskStartedEventAttributes)
{-# DEPRECATED atseaIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduledEventId :: Lens.Lens' ActivityTaskStartedEventAttributes Lude.Integer
atseaScheduledEventId = Lens.lens (scheduledEventId :: ActivityTaskStartedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: ActivityTaskStartedEventAttributes)
{-# DEPRECATED atseaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

instance Lude.FromJSON ActivityTaskStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskStartedEventAttributes"
      ( \x ->
          ActivityTaskStartedEventAttributes'
            Lude.<$> (x Lude..:? "identity") Lude.<*> (x Lude..: "scheduledEventId")
      )
