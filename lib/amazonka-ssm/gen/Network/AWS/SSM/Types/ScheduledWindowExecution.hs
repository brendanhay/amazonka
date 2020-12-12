{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ScheduledWindowExecution
  ( ScheduledWindowExecution (..),

    -- * Smart constructor
    mkScheduledWindowExecution,

    -- * Lenses
    sweExecutionTime,
    sweName,
    sweWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a scheduled execution for a maintenance window.
--
-- /See:/ 'mkScheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { executionTime ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledWindowExecution' with the minimum fields required to make a request.
--
-- * 'executionTime' - The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
-- * 'name' - The name of the maintenance window to be run.
-- * 'windowId' - The ID of the maintenance window to be run.
mkScheduledWindowExecution ::
  ScheduledWindowExecution
mkScheduledWindowExecution =
  ScheduledWindowExecution'
    { executionTime = Lude.Nothing,
      name = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweExecutionTime :: Lens.Lens' ScheduledWindowExecution (Lude.Maybe Lude.Text)
sweExecutionTime = Lens.lens (executionTime :: ScheduledWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {executionTime = a} :: ScheduledWindowExecution)
{-# DEPRECATED sweExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | The name of the maintenance window to be run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweName :: Lens.Lens' ScheduledWindowExecution (Lude.Maybe Lude.Text)
sweName = Lens.lens (name :: ScheduledWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ScheduledWindowExecution)
{-# DEPRECATED sweName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the maintenance window to be run.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweWindowId :: Lens.Lens' ScheduledWindowExecution (Lude.Maybe Lude.Text)
sweWindowId = Lens.lens (windowId :: ScheduledWindowExecution -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: ScheduledWindowExecution)
{-# DEPRECATED sweWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON ScheduledWindowExecution where
  parseJSON =
    Lude.withObject
      "ScheduledWindowExecution"
      ( \x ->
          ScheduledWindowExecution'
            Lude.<$> (x Lude..:? "ExecutionTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "WindowId")
      )
