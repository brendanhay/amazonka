{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionContext
  ( ActionContext (..),

    -- * Smart constructor
    mkActionContext,

    -- * Lenses
    acName,
    acActionExecutionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the context of an action in the stage of a pipeline to a job worker.
--
-- /See:/ 'mkActionContext' smart constructor.
data ActionContext = ActionContext'
  { name :: Lude.Maybe Lude.Text,
    actionExecutionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionContext' with the minimum fields required to make a request.
--
-- * 'actionExecutionId' - The system-generated unique ID that corresponds to an action's execution.
-- * 'name' - The name of the action in the context of a job.
mkActionContext ::
  ActionContext
mkActionContext =
  ActionContext'
    { name = Lude.Nothing,
      actionExecutionId = Lude.Nothing
    }

-- | The name of the action in the context of a job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' ActionContext (Lude.Maybe Lude.Text)
acName = Lens.lens (name :: ActionContext -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ActionContext)
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The system-generated unique ID that corresponds to an action's execution.
--
-- /Note:/ Consider using 'actionExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acActionExecutionId :: Lens.Lens' ActionContext (Lude.Maybe Lude.Text)
acActionExecutionId = Lens.lens (actionExecutionId :: ActionContext -> Lude.Maybe Lude.Text) (\s a -> s {actionExecutionId = a} :: ActionContext)
{-# DEPRECATED acActionExecutionId "Use generic-lens or generic-optics with 'actionExecutionId' instead." #-}

instance Lude.FromJSON ActionContext where
  parseJSON =
    Lude.withObject
      "ActionContext"
      ( \x ->
          ActionContext'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "actionExecutionId")
      )
