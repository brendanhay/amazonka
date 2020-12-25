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
    tfActions,
    tfDescription,
    tfId,
    tfName,
    tfPredicate,
    tfSchedule,
    tfState,
    tfType,
    tfWorkflowName,
  )
where

import qualified Network.AWS.Glue.Types.Action as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.Id as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.Predicate as Types
import qualified Network.AWS.Glue.Types.TriggerState as Types
import qualified Network.AWS.Glue.Types.TriggerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a specific trigger.
--
-- /See:/ 'mkTrigger' smart constructor.
data Trigger = Trigger'
  { -- | The actions initiated by this trigger.
    actions :: Core.Maybe [Types.Action],
    -- | A description of this trigger.
    description :: Core.Maybe Types.DescriptionString,
    -- | Reserved for future use.
    id :: Core.Maybe Types.Id,
    -- | The name of the trigger.
    name :: Core.Maybe Types.NameString,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Core.Maybe Types.Predicate,
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Core.Maybe Types.GenericString,
    -- | The current state of the trigger.
    state :: Core.Maybe Types.TriggerState,
    -- | The type of trigger that this is.
    type' :: Core.Maybe Types.TriggerType,
    -- | The name of the workflow associated with the trigger.
    workflowName :: Core.Maybe Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Trigger' value with any optional fields omitted.
mkTrigger ::
  Trigger
mkTrigger =
  Trigger'
    { actions = Core.Nothing,
      description = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      predicate = Core.Nothing,
      schedule = Core.Nothing,
      state = Core.Nothing,
      type' = Core.Nothing,
      workflowName = Core.Nothing
    }

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfActions :: Lens.Lens' Trigger (Core.Maybe [Types.Action])
tfActions = Lens.field @"actions"
{-# DEPRECATED tfActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDescription :: Lens.Lens' Trigger (Core.Maybe Types.DescriptionString)
tfDescription = Lens.field @"description"
{-# DEPRECATED tfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfId :: Lens.Lens' Trigger (Core.Maybe Types.Id)
tfId = Lens.field @"id"
{-# DEPRECATED tfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfName :: Lens.Lens' Trigger (Core.Maybe Types.NameString)
tfName = Lens.field @"name"
{-# DEPRECATED tfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfPredicate :: Lens.Lens' Trigger (Core.Maybe Types.Predicate)
tfPredicate = Lens.field @"predicate"
{-# DEPRECATED tfPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfSchedule :: Lens.Lens' Trigger (Core.Maybe Types.GenericString)
tfSchedule = Lens.field @"schedule"
{-# DEPRECATED tfSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The current state of the trigger.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfState :: Lens.Lens' Trigger (Core.Maybe Types.TriggerState)
tfState = Lens.field @"state"
{-# DEPRECATED tfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The type of trigger that this is.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfType :: Lens.Lens' Trigger (Core.Maybe Types.TriggerType)
tfType = Lens.field @"type'"
{-# DEPRECATED tfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the workflow associated with the trigger.
--
-- /Note:/ Consider using 'workflowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfWorkflowName :: Lens.Lens' Trigger (Core.Maybe Types.NameString)
tfWorkflowName = Lens.field @"workflowName"
{-# DEPRECATED tfWorkflowName "Use generic-lens or generic-optics with 'workflowName' instead." #-}

instance Core.FromJSON Trigger where
  parseJSON =
    Core.withObject "Trigger" Core.$
      \x ->
        Trigger'
          Core.<$> (x Core..:? "Actions")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Predicate")
          Core.<*> (x Core..:? "Schedule")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "WorkflowName")
