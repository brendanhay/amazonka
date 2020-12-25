{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
  ( LambdaFunctionScheduledEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionScheduledEventDetails,

    -- * Lenses
    lfsedResource,
    lfsedInput,
    lfsedInputDetails,
    lfsedTimeoutInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Arn as Types
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about a lambda function scheduled during an execution.
--
-- /See:/ 'mkLambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'
  { -- | The Amazon Resource Name (ARN) of the scheduled lambda function.
    resource :: Types.Arn,
    -- | The JSON data input to the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    -- | Contains details about input for an execution history event.
    inputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails,
    -- | The maximum allowed duration of the lambda function.
    timeoutInSeconds :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionScheduledEventDetails' value with any optional fields omitted.
mkLambdaFunctionScheduledEventDetails ::
  -- | 'resource'
  Types.Arn ->
  LambdaFunctionScheduledEventDetails
mkLambdaFunctionScheduledEventDetails resource =
  LambdaFunctionScheduledEventDetails'
    { resource,
      input = Core.Nothing,
      inputDetails = Core.Nothing,
      timeoutInSeconds = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled lambda function.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedResource :: Lens.Lens' LambdaFunctionScheduledEventDetails Types.Arn
lfsedResource = Lens.field @"resource"
{-# DEPRECATED lfsedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The JSON data input to the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedInput :: Lens.Lens' LambdaFunctionScheduledEventDetails (Core.Maybe Types.SensitiveData)
lfsedInput = Lens.field @"input"
{-# DEPRECATED lfsedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Contains details about input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedInputDetails :: Lens.Lens' LambdaFunctionScheduledEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
lfsedInputDetails = Lens.field @"inputDetails"
{-# DEPRECATED lfsedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The maximum allowed duration of the lambda function.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedTimeoutInSeconds :: Lens.Lens' LambdaFunctionScheduledEventDetails (Core.Maybe Core.Integer)
lfsedTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# DEPRECATED lfsedTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

instance Core.FromJSON LambdaFunctionScheduledEventDetails where
  parseJSON =
    Core.withObject "LambdaFunctionScheduledEventDetails" Core.$
      \x ->
        LambdaFunctionScheduledEventDetails'
          Core.<$> (x Core..: "resource")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "inputDetails")
          Core.<*> (x Core..:? "timeoutInSeconds")
