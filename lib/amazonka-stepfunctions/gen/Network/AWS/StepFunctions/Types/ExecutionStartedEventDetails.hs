{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
  ( ExecutionStartedEventDetails (..),

    -- * Smart constructor
    mkExecutionStartedEventDetails,

    -- * Lenses
    esedInput,
    esedInputDetails,
    esedRoleArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Arn as Types
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about the start of the execution.
--
-- /See:/ 'mkExecutionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { -- | The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    -- | Contains details about the input for an execution history event.
    inputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
    roleArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionStartedEventDetails' value with any optional fields omitted.
mkExecutionStartedEventDetails ::
  ExecutionStartedEventDetails
mkExecutionStartedEventDetails =
  ExecutionStartedEventDetails'
    { input = Core.Nothing,
      inputDetails = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedInput :: Lens.Lens' ExecutionStartedEventDetails (Core.Maybe Types.SensitiveData)
esedInput = Lens.field @"input"
{-# DEPRECATED esedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedInputDetails :: Lens.Lens' ExecutionStartedEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
esedInputDetails = Lens.field @"inputDetails"
{-# DEPRECATED esedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedRoleArn :: Lens.Lens' ExecutionStartedEventDetails (Core.Maybe Types.Arn)
esedRoleArn = Lens.field @"roleArn"
{-# DEPRECATED esedRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON ExecutionStartedEventDetails where
  parseJSON =
    Core.withObject "ExecutionStartedEventDetails" Core.$
      \x ->
        ExecutionStartedEventDetails'
          Core.<$> (x Core..:? "input")
          Core.<*> (x Core..:? "inputDetails")
          Core.<*> (x Core..:? "roleArn")
