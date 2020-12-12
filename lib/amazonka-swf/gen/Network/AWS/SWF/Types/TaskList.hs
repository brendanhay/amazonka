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
import qualified Network.AWS.Prelude as Lude

-- | Represents a task list.
--
-- /See:/ 'mkTaskList' smart constructor.
newtype TaskList = TaskList' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskList' with the minimum fields required to make a request.
--
-- * 'name' - The name of the task list.
mkTaskList ::
  -- | 'name'
  Lude.Text ->
  TaskList
mkTaskList pName_ = TaskList' {name = pName_}

-- | The name of the task list.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlName :: Lens.Lens' TaskList Lude.Text
tlName = Lens.lens (name :: TaskList -> Lude.Text) (\s a -> s {name = a} :: TaskList)
{-# DEPRECATED tlName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON TaskList where
  parseJSON =
    Lude.withObject
      "TaskList"
      (\x -> TaskList' Lude.<$> (x Lude..: "name"))

instance Lude.ToJSON TaskList where
  toJSON TaskList' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])
