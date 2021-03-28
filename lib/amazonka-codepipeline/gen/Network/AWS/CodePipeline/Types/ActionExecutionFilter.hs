{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionExecutionFilter
  ( ActionExecutionFilter (..)
  -- * Smart constructor
  , mkActionExecutionFilter
  -- * Lenses
  , aefPipelineExecutionId
  ) where

import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filter values for the action execution.
--
-- /See:/ 'mkActionExecutionFilter' smart constructor.
newtype ActionExecutionFilter = ActionExecutionFilter'
  { pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId
    -- ^ The pipeline execution ID used to filter action execution history.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActionExecutionFilter' value with any optional fields omitted.
mkActionExecutionFilter
    :: ActionExecutionFilter
mkActionExecutionFilter
  = ActionExecutionFilter'{pipelineExecutionId = Core.Nothing}

-- | The pipeline execution ID used to filter action execution history.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aefPipelineExecutionId :: Lens.Lens' ActionExecutionFilter (Core.Maybe Types.PipelineExecutionId)
aefPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE aefPipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

instance Core.FromJSON ActionExecutionFilter where
        toJSON ActionExecutionFilter{..}
          = Core.object
              (Core.catMaybes
                 [("pipelineExecutionId" Core..=) Core.<$> pipelineExecutionId])
