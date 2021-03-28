{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TriggerUpdate
  ( TriggerUpdate (..)
  -- * Smart constructor
  , mkTriggerUpdate
  -- * Lenses
  , tuActions
  , tuDescription
  , tuName
  , tuPredicate
  , tuSchedule
  ) where

import qualified Network.AWS.Glue.Types.Action as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.Predicate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure used to provide information used to update a trigger. This object updates the previous trigger definition by overwriting it completely.
--
-- /See:/ 'mkTriggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { actions :: Core.Maybe [Types.Action]
    -- ^ The actions initiated by this trigger.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of this trigger.
  , name :: Core.Maybe Types.Name
    -- ^ Reserved for future use.
  , predicate :: Core.Maybe Types.Predicate
    -- ^ The predicate of this trigger, which defines when it will fire.
  , schedule :: Core.Maybe Types.GenericString
    -- ^ A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TriggerUpdate' value with any optional fields omitted.
mkTriggerUpdate
    :: TriggerUpdate
mkTriggerUpdate
  = TriggerUpdate'{actions = Core.Nothing,
                   description = Core.Nothing, name = Core.Nothing,
                   predicate = Core.Nothing, schedule = Core.Nothing}

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuActions :: Lens.Lens' TriggerUpdate (Core.Maybe [Types.Action])
tuActions = Lens.field @"actions"
{-# INLINEABLE tuActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuDescription :: Lens.Lens' TriggerUpdate (Core.Maybe Types.DescriptionString)
tuDescription = Lens.field @"description"
{-# INLINEABLE tuDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuName :: Lens.Lens' TriggerUpdate (Core.Maybe Types.Name)
tuName = Lens.field @"name"
{-# INLINEABLE tuName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuPredicate :: Lens.Lens' TriggerUpdate (Core.Maybe Types.Predicate)
tuPredicate = Lens.field @"predicate"
{-# INLINEABLE tuPredicate #-}
{-# DEPRECATED predicate "Use generic-lens or generic-optics with 'predicate' instead"  #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuSchedule :: Lens.Lens' TriggerUpdate (Core.Maybe Types.GenericString)
tuSchedule = Lens.field @"schedule"
{-# INLINEABLE tuSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

instance Core.FromJSON TriggerUpdate where
        toJSON TriggerUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("Actions" Core..=) Core.<$> actions,
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name,
                  ("Predicate" Core..=) Core.<$> predicate,
                  ("Schedule" Core..=) Core.<$> schedule])
