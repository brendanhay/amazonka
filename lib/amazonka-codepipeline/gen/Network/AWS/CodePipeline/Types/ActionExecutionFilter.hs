-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionFilter
  ( ActionExecutionFilter (..),

    -- * Smart constructor
    mkActionExecutionFilter,

    -- * Lenses
    aefPipelineExecutionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filter values for the action execution.
--
-- /See:/ 'mkActionExecutionFilter' smart constructor.
newtype ActionExecutionFilter = ActionExecutionFilter'
  { pipelineExecutionId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecutionFilter' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The pipeline execution ID used to filter action execution history.
mkActionExecutionFilter ::
  ActionExecutionFilter
mkActionExecutionFilter =
  ActionExecutionFilter' {pipelineExecutionId = Lude.Nothing}

-- | The pipeline execution ID used to filter action execution history.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefPipelineExecutionId :: Lens.Lens' ActionExecutionFilter (Lude.Maybe Lude.Text)
aefPipelineExecutionId = Lens.lens (pipelineExecutionId :: ActionExecutionFilter -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: ActionExecutionFilter)
{-# DEPRECATED aefPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

instance Lude.ToJSON ActionExecutionFilter where
  toJSON ActionExecutionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("pipelineExecutionId" Lude..=) Lude.<$> pipelineExecutionId]
      )
