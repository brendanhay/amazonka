{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
  ( ExecutionSucceededEventDetails (..)
  -- * Smart constructor
  , mkExecutionSucceededEventDetails
  -- * Lenses
  , esedOutput
  , esedOutputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about the successful termination of the execution.
--
-- /See:/ 'mkExecutionSucceededEventDetails' smart constructor.
data ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'
  { output :: Core.Maybe Types.SensitiveData
    -- ^ The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
    -- ^ Contains details about the output of an execution history event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionSucceededEventDetails' value with any optional fields omitted.
mkExecutionSucceededEventDetails
    :: ExecutionSucceededEventDetails
mkExecutionSucceededEventDetails
  = ExecutionSucceededEventDetails'{output = Core.Nothing,
                                    outputDetails = Core.Nothing}

-- | The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedOutput :: Lens.Lens' ExecutionSucceededEventDetails (Core.Maybe Types.SensitiveData)
esedOutput = Lens.field @"output"
{-# INLINEABLE esedOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedOutputDetails :: Lens.Lens' ExecutionSucceededEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
esedOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE esedOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

instance Core.FromJSON ExecutionSucceededEventDetails where
        parseJSON
          = Core.withObject "ExecutionSucceededEventDetails" Core.$
              \ x ->
                ExecutionSucceededEventDetails' Core.<$>
                  (x Core..:? "output") Core.<*> x Core..:? "outputDetails"
