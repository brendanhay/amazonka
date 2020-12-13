{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerUpdate
  ( TriggerUpdate (..),

    -- * Smart constructor
    mkTriggerUpdate,

    -- * Lenses
    tuActions,
    tuSchedule,
    tuPredicate,
    tuName,
    tuDescription,
  )
where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure used to provide information used to update a trigger. This object updates the previous trigger definition by overwriting it completely.
--
-- /See:/ 'mkTriggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { -- | The actions initiated by this trigger.
    actions :: Lude.Maybe [Action],
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Lude.Maybe Lude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Lude.Maybe Predicate,
    -- | Reserved for future use.
    name :: Lude.Maybe Lude.Text,
    -- | A description of this trigger.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TriggerUpdate' with the minimum fields required to make a request.
--
-- * 'actions' - The actions initiated by this trigger.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'predicate' - The predicate of this trigger, which defines when it will fire.
-- * 'name' - Reserved for future use.
-- * 'description' - A description of this trigger.
mkTriggerUpdate ::
  TriggerUpdate
mkTriggerUpdate =
  TriggerUpdate'
    { actions = Lude.Nothing,
      schedule = Lude.Nothing,
      predicate = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The actions initiated by this trigger.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuActions :: Lens.Lens' TriggerUpdate (Lude.Maybe [Action])
tuActions = Lens.lens (actions :: TriggerUpdate -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: TriggerUpdate)
{-# DEPRECATED tuActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuSchedule :: Lens.Lens' TriggerUpdate (Lude.Maybe Lude.Text)
tuSchedule = Lens.lens (schedule :: TriggerUpdate -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: TriggerUpdate)
{-# DEPRECATED tuSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The predicate of this trigger, which defines when it will fire.
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuPredicate :: Lens.Lens' TriggerUpdate (Lude.Maybe Predicate)
tuPredicate = Lens.lens (predicate :: TriggerUpdate -> Lude.Maybe Predicate) (\s a -> s {predicate = a} :: TriggerUpdate)
{-# DEPRECATED tuPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuName :: Lens.Lens' TriggerUpdate (Lude.Maybe Lude.Text)
tuName = Lens.lens (name :: TriggerUpdate -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TriggerUpdate)
{-# DEPRECATED tuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description of this trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuDescription :: Lens.Lens' TriggerUpdate (Lude.Maybe Lude.Text)
tuDescription = Lens.lens (description :: TriggerUpdate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TriggerUpdate)
{-# DEPRECATED tuDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON TriggerUpdate where
  toJSON TriggerUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Actions" Lude..=) Lude.<$> actions,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("Predicate" Lude..=) Lude.<$> predicate,
            ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
