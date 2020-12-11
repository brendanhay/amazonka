-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
  ( DecisionTaskStartedEventAttributes (..),

    -- * Smart constructor
    mkDecisionTaskStartedEventAttributes,

    -- * Lenses
    dtseaIdentity,
    dtseaScheduledEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @DecisionTaskStarted@ event.
--
-- /See:/ 'mkDecisionTaskStartedEventAttributes' smart constructor.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes'
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

-- | Creates a value of 'DecisionTaskStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'identity' - Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
-- * 'scheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkDecisionTaskStartedEventAttributes ::
  -- | 'scheduledEventId'
  Lude.Integer ->
  DecisionTaskStartedEventAttributes
mkDecisionTaskStartedEventAttributes pScheduledEventId_ =
  DecisionTaskStartedEventAttributes'
    { identity = Lude.Nothing,
      scheduledEventId = pScheduledEventId_
    }

-- | Identity of the decider making the request. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaIdentity :: Lens.Lens' DecisionTaskStartedEventAttributes (Lude.Maybe Lude.Text)
dtseaIdentity = Lens.lens (identity :: DecisionTaskStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {identity = a} :: DecisionTaskStartedEventAttributes)
{-# DEPRECATED dtseaIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtseaScheduledEventId :: Lens.Lens' DecisionTaskStartedEventAttributes Lude.Integer
dtseaScheduledEventId = Lens.lens (scheduledEventId :: DecisionTaskStartedEventAttributes -> Lude.Integer) (\s a -> s {scheduledEventId = a} :: DecisionTaskStartedEventAttributes)
{-# DEPRECATED dtseaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

instance Lude.FromJSON DecisionTaskStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "DecisionTaskStartedEventAttributes"
      ( \x ->
          DecisionTaskStartedEventAttributes'
            Lude.<$> (x Lude..:? "identity") Lude.<*> (x Lude..: "scheduledEventId")
      )
