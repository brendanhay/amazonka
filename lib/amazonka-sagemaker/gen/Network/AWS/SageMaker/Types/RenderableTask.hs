{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderableTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderableTask
  ( RenderableTask (..),

    -- * Smart constructor
    mkRenderableTask,

    -- * Lenses
    rtInput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Input as Types

-- | Contains input values for a task.
--
-- /See:/ 'mkRenderableTask' smart constructor.
newtype RenderableTask = RenderableTask'
  { -- | A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
    input :: Types.Input
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RenderableTask' value with any optional fields omitted.
mkRenderableTask ::
  -- | 'input'
  Types.Input ->
  RenderableTask
mkRenderableTask input = RenderableTask' {input}

-- | A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtInput :: Lens.Lens' RenderableTask Types.Input
rtInput = Lens.field @"input"
{-# DEPRECATED rtInput "Use generic-lens or generic-optics with 'input' instead." #-}

instance Core.FromJSON RenderableTask where
  toJSON RenderableTask {..} =
    Core.object (Core.catMaybes [Core.Just ("Input" Core..= input)])
