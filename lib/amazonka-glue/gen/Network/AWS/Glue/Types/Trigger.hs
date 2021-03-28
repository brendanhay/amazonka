{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Trigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Trigger
  ( Trigger (..)
  -- * Smart constructor
  , mkTrigger
  -- * Lenses
  , tfActions
  , tfDescription
  , tfId
  , tfName
  , tfPredicate
  , tfSchedule
  , tfState
  , tfType
  , tfWorkflowName
  ) where

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
  { actions :: Core.Maybe [Types.Action]
    -- ^ The actions initiated by this trigger.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of this trigger.
  , id :: Core.Maybe Types.Id
    -- ^ Reserved for future use.
  , name :: Core.Maybe Types.NameString
    -- ^ The name of the trigger.
  , predicate :: Core.Maybe Types.Predicate
    -- ^ The predicate of this trigger, which defines when it will fire.
  , schedule :: Core.Maybe Types.GenericString
    -- ^ A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
  , state :: Core.Maybe Types.TriggerState
    -- ^ The current state of the trigger.
  , type' :: Core.Maybe Types.TriggerType
    -- ^ The type of trigger that this is.
  , workflowName :: Core.Maybe Types.NameString
    -- ^ The name of the workflow associated with the trigger.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Trigger' value with any optional fields omitted.
mkTrigger
    :: Trigger
mkTrigger
  = Trigger'{actions = Core.Nothing, description = Core.Nothing,
             id = Core.Nothing, name = Core.Nothing, predicate = Core.Nothing,
             schedule = Core.Nothing, state = Core.Nothing,
             type' = Core.Nothing, workflowName = Core.Nothing}

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfActions :: Lens.Lens' Trigger (Core.Maybe [Types.Action])
tfActions = Lens.field @"actions"
{-# INLINEABLE tfActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfDescription :: Lens.Lens' Trigger (Core.Maybe Types.DescriptionString)
tfDescription = Lens.field @"description"
{-# INLINEABLE tfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfId :: Lens.Lens' Trigger (Core.Maybe Types.Id)
tfId = Lens.field @"id"
{-# INLINEABLE tfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfName :: Lens.Lens' Trigger (Core.Maybe Types.NameString)
tfName = Lens.field @"name"
{-# INLINEABLE tfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfPredicate :: Lens.Lens' Trigger (Core.Maybe Types.Predicate)
tfPredicate = Lens.field @"predicate"
{-# INLINEABLE tfPredicate #-}
{-# DEPRECATED predicate "Use generic-lens or generic-optics with 'predicate' instead"  #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfSchedule :: Lens.Lens' Trigger (Core.Maybe Types.GenericString)
tfSchedule = Lens.field @"schedule"
{-# INLINEABLE tfSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The current state of the trigger.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfState :: Lens.Lens' Trigger (Core.Maybe Types.TriggerState)
tfState = Lens.field @"state"
{-# INLINEABLE tfState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The type of trigger that this is.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfType :: Lens.Lens' Trigger (Core.Maybe Types.TriggerType)
tfType = Lens.field @"type'"
{-# INLINEABLE tfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The name of the workflow associated with the trigger.
--
-- /Note:/ Consider using 'workflowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfWorkflowName :: Lens.Lens' Trigger (Core.Maybe Types.NameString)
tfWorkflowName = Lens.field @"workflowName"
{-# INLINEABLE tfWorkflowName #-}
{-# DEPRECATED workflowName "Use generic-lens or generic-optics with 'workflowName' instead"  #-}

instance Core.FromJSON Trigger where
        parseJSON
          = Core.withObject "Trigger" Core.$
              \ x ->
                Trigger' Core.<$>
                  (x Core..:? "Actions") Core.<*> x Core..:? "Description" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Predicate"
                    Core.<*> x Core..:? "Schedule"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "WorkflowName"
