{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StageExecution
  ( StageExecution (..)
  -- * Smart constructor
  , mkStageExecution
  -- * Lenses
  , sePipelineExecutionId
  , seStatus
  ) where

import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.StageExecutionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the run of a stage.
--
-- /See:/ 'mkStageExecution' smart constructor.
data StageExecution = StageExecution'
  { pipelineExecutionId :: Types.PipelineExecutionId
    -- ^ The ID of the pipeline execution associated with the stage.
  , status :: Types.StageExecutionStatus
    -- ^ The status of the stage, or for a completed stage, the last status of the stage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StageExecution' value with any optional fields omitted.
mkStageExecution
    :: Types.PipelineExecutionId -- ^ 'pipelineExecutionId'
    -> Types.StageExecutionStatus -- ^ 'status'
    -> StageExecution
mkStageExecution pipelineExecutionId status
  = StageExecution'{pipelineExecutionId, status}

-- | The ID of the pipeline execution associated with the stage.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sePipelineExecutionId :: Lens.Lens' StageExecution Types.PipelineExecutionId
sePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE sePipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

-- | The status of the stage, or for a completed stage, the last status of the stage.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStatus :: Lens.Lens' StageExecution Types.StageExecutionStatus
seStatus = Lens.field @"status"
{-# INLINEABLE seStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON StageExecution where
        parseJSON
          = Core.withObject "StageExecution" Core.$
              \ x ->
                StageExecution' Core.<$>
                  (x Core..: "pipelineExecutionId") Core.<*> x Core..: "status"
