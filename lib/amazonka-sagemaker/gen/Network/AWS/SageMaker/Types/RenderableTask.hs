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
import qualified Network.AWS.Prelude as Lude

-- | Contains input values for a task.
--
-- /See:/ 'mkRenderableTask' smart constructor.
newtype RenderableTask = RenderableTask' {input :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenderableTask' with the minimum fields required to make a request.
--
-- * 'input' - A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
mkRenderableTask ::
  -- | 'input'
  Lude.Text ->
  RenderableTask
mkRenderableTask pInput_ = RenderableTask' {input = pInput_}

-- | A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtInput :: Lens.Lens' RenderableTask Lude.Text
rtInput = Lens.lens (input :: RenderableTask -> Lude.Text) (\s a -> s {input = a} :: RenderableTask)
{-# DEPRECATED rtInput "Use generic-lens or generic-optics with 'input' instead." #-}

instance Lude.ToJSON RenderableTask where
  toJSON RenderableTask' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Input" Lude..= input)])
