{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StopExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StopExecutionTrigger
  ( StopExecutionTrigger (..)
  -- * Smart constructor
  , mkStopExecutionTrigger
  -- * Lenses
  , setReason
  ) where

import qualified Network.AWS.CodePipeline.Types.StopPipelineExecutionReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The interaction that stopped a pipeline execution.
--
-- /See:/ 'mkStopExecutionTrigger' smart constructor.
newtype StopExecutionTrigger = StopExecutionTrigger'
  { reason :: Core.Maybe Types.StopPipelineExecutionReason
    -- ^ The user-specified reason the pipeline was stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopExecutionTrigger' value with any optional fields omitted.
mkStopExecutionTrigger
    :: StopExecutionTrigger
mkStopExecutionTrigger
  = StopExecutionTrigger'{reason = Core.Nothing}

-- | The user-specified reason the pipeline was stopped.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setReason :: Lens.Lens' StopExecutionTrigger (Core.Maybe Types.StopPipelineExecutionReason)
setReason = Lens.field @"reason"
{-# INLINEABLE setReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.FromJSON StopExecutionTrigger where
        parseJSON
          = Core.withObject "StopExecutionTrigger" Core.$
              \ x -> StopExecutionTrigger' Core.<$> (x Core..:? "reason")
