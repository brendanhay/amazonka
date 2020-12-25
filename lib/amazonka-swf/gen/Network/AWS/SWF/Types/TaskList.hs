{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TaskList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TaskList
  ( TaskList (..),

    -- * Smart constructor
    mkTaskList,

    -- * Lenses
    tlName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Name as Types

-- | Represents a task list.
--
-- /See:/ 'mkTaskList' smart constructor.
newtype TaskList = TaskList'
  { -- | The name of the task list.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TaskList' value with any optional fields omitted.
mkTaskList ::
  -- | 'name'
  Types.Name ->
  TaskList
mkTaskList name = TaskList' {name}

-- | The name of the task list.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlName :: Lens.Lens' TaskList Types.Name
tlName = Lens.field @"name"
{-# DEPRECATED tlName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON TaskList where
  toJSON TaskList {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.FromJSON TaskList where
  parseJSON =
    Core.withObject "TaskList" Core.$
      \x -> TaskList' Core.<$> (x Core..: "name")
