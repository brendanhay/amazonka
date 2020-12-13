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
    tfWorkflowName,
    tfState,
    tfActions,
    tfSchedule,
    tfPredicate,
    tfName,
    tfId,
    tfType,
    tfDescription,
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
  { -- | The name of the workflow associated with the trigger.
    workflowName :: Lude.Maybe Lude.Text,
    -- | The current state of the trigger.
    state :: Lude.Maybe TriggerState,
    -- | The actions initiated by this trigger.
    actions :: Lude.Maybe [Action],
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Lude.Maybe Lude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Lude.Maybe Predicate,
    -- | The name of the trigger.
    name :: Lude.Maybe Lude.Text,
    -- | Reserved for future use.
    id :: Lude.Maybe Lude.Text,
    -- | The type of trigger that this is.
    type' :: Lude.Maybe TriggerType,
    -- | A description of this trigger.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- * 'workflowName' - The name of the workflow associated with the trigger.
-- * 'state' - The current state of the trigger.
-- * 'actions' - The actions initiated by this trigger.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'predicate' - The predicate of this trigger, which defines when it will fire.
-- * 'name' - The name of the trigger.
-- * 'id' - Reserved for future use.
-- * 'type'' - The type of trigger that this is.
-- * 'description' - A description of this trigger.
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
tfWorkflowName :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tfWorkflowName = Lens.lens (workflowName :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {workflowName = a} :: Trigger)
{-# DEPRECATED tfWorkflowName "Use generic-lens or generic-optics with 'workflowName' instead." #-}

-- | The current state of the trigger.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfState :: Lens.Lens' Trigger (Lude.Maybe TriggerState)
tfState = Lens.lens (state :: Trigger -> Lude.Maybe TriggerState) (\s a -> s {state = a} :: Trigger)
{-# DEPRECATED tfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfActions :: Lens.Lens' Trigger (Lude.Maybe [Action])
tfActions = Lens.lens (actions :: Trigger -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: Trigger)
{-# DEPRECATED tfActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfSchedule :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tfSchedule = Lens.lens (schedule :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: Trigger)
{-# DEPRECATED tfSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfPredicate :: Lens.Lens' Trigger (Lude.Maybe Predicate)
tfPredicate = Lens.lens (predicate :: Trigger -> Lude.Maybe Predicate) (\s a -> s {predicate = a} :: Trigger)
{-# DEPRECATED tfPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfName :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tfName = Lens.lens (name :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Trigger)
{-# DEPRECATED tfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfId :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tfId = Lens.lens (id :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Trigger)
{-# DEPRECATED tfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of trigger that this is.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfType :: Lens.Lens' Trigger (Lude.Maybe TriggerType)
tfType = Lens.lens (type' :: Trigger -> Lude.Maybe TriggerType) (\s a -> s {type' = a} :: Trigger)
{-# DEPRECATED tfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDescription :: Lens.Lens' Trigger (Lude.Maybe Lude.Text)
tfDescription = Lens.lens (description :: Trigger -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Trigger)
{-# DEPRECATED tfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
