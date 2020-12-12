{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Trigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Trigger
  ( Trigger (..),

    -- * Smart constructor
    mkTrigger,

    -- * Lenses
    triWorkflowName,
    triState,
    triActions,
    triSchedule,
    triPredicate,
    triName,
    triId,
    triType,
    triDescription,
  )
where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a specific trigger.
--
-- /See:/ 'mkTrigger' smart constructor.
data Trigger = Trigger'
  { workflowName :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe TriggerState,
    actions :: Lude.Maybe [Action],
    schedule :: Lude.Maybe Lude.Text,
    predicate :: Lude.Maybe Predicate,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe TriggerType,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- * 'actions' - The actions initiated by this trigger.
-- * 'description' - A description of this trigger.
-- * 'id' - Reserved for future use.
-- * 'name' - The name of the trigger.
-- * 'predicate' - The predicate of this trigger, which defines when it will fire.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'state' - The current state of the trigger.
-- * 'type'' - The type of trigger that this is.
-- * 'workflowName' - The name of the workflow associated with the trigger.
mkTrigger ::
  Trigger
mkTrigger =
  Trigger'
    { workflowName = Lude.Nothing,
      state = Lude.Nothing,
      actions = Lude.Nothing,
      schedule = Lude.Nothing,
      predicate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the workflow associated with the trigger.
--
-- /Note:/ Consider using 'workflowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triWorkflowName :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
triWorkflowName = Lens.lens (workflowName :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {workflowName = a} :: Trigger)
{-# DEPRECATED triWorkflowName "Use generic-lens or generic-optics with 'workflowName' instead." #-}

-- | The current state of the trigger.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triState :: Lens.Lens' Trigger (Lude.Maybe TriggerState)
triState = Lens.lens (state :: Trigger -> Lude.Maybe TriggerState) (\s a -> s {state = a} :: Trigger)
{-# DEPRECATED triState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triActions :: Lens.Lens' Trigger (Lude.Maybe [Action])
triActions = Lens.lens (actions :: Trigger -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: Trigger)
{-# DEPRECATED triActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triSchedule :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
triSchedule = Lens.lens (schedule :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: Trigger)
{-# DEPRECATED triSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triPredicate :: Lens.Lens' Trigger (Lude.Maybe Predicate)
triPredicate = Lens.lens (predicate :: Trigger -> Lude.Maybe Predicate) (\s a -> s {predicate = a} :: Trigger)
{-# DEPRECATED triPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triName :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
triName = Lens.lens (name :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Trigger)
{-# DEPRECATED triName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triId :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
triId = Lens.lens (id :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Trigger)
{-# DEPRECATED triId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of trigger that this is.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triType :: Lens.Lens' Trigger (Lude.Maybe TriggerType)
triType = Lens.lens (type' :: Trigger -> Lude.Maybe TriggerType) (\s a -> s {type' = a} :: Trigger)
{-# DEPRECATED triType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
triDescription :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
triDescription = Lens.lens (description :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Trigger)
{-# DEPRECATED triDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Trigger where
  parseJSON =
    Lude.withObject
      "Trigger"
      ( \x ->
          Trigger'
            Lude.<$> (x Lude..:? "WorkflowName")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Actions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "Predicate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
      )
