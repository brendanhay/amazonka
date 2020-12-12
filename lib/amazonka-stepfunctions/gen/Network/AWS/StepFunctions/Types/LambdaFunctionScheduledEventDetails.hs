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
    lfsedInputDetails,
    lfsedInput,
    lfsedTimeoutInSeconds,
    lfsedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a lambda function scheduled during an execution.
--
-- /See:/ 'mkLambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'
  { inputDetails ::
      Lude.Maybe
        HistoryEventExecutionDataDetails,
    input ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    timeoutInSeconds ::
      Lude.Maybe
        Lude.Integer,
    resource ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionScheduledEventDetails' with the minimum fields required to make a request.
--
-- * 'input' - The JSON data input to the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'inputDetails' - Contains details about input for an execution history event.
-- * 'resource' - The Amazon Resource Name (ARN) of the scheduled lambda function.
-- * 'timeoutInSeconds' - The maximum allowed duration of the lambda function.
mkLambdaFunctionScheduledEventDetails ::
  -- | 'resource'
  Lude.Text ->
  LambdaFunctionScheduledEventDetails
mkLambdaFunctionScheduledEventDetails pResource_ =
  LambdaFunctionScheduledEventDetails'
    { inputDetails = Lude.Nothing,
      input = Lude.Nothing,
      timeoutInSeconds = Lude.Nothing,
      resource = pResource_
    }

-- | Contains details about input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedInputDetails :: Lens.Lens' LambdaFunctionScheduledEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
lfsedInputDetails = Lens.lens (inputDetails :: LambdaFunctionScheduledEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {inputDetails = a} :: LambdaFunctionScheduledEventDetails)
{-# DEPRECATED lfsedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The JSON data input to the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedInput :: Lens.Lens' LambdaFunctionScheduledEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lfsedInput = Lens.lens (input :: LambdaFunctionScheduledEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: LambdaFunctionScheduledEventDetails)
{-# DEPRECATED lfsedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum allowed duration of the lambda function.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedTimeoutInSeconds :: Lens.Lens' LambdaFunctionScheduledEventDetails (Lude.Maybe Lude.Integer)
lfsedTimeoutInSeconds = Lens.lens (timeoutInSeconds :: LambdaFunctionScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {timeoutInSeconds = a} :: LambdaFunctionScheduledEventDetails)
{-# DEPRECATED lfsedTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | The Amazon Resource Name (ARN) of the scheduled lambda function.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedResource :: Lens.Lens' LambdaFunctionScheduledEventDetails Lude.Text
lfsedResource = Lens.lens (resource :: LambdaFunctionScheduledEventDetails -> Lude.Text) (\s a -> s {resource = a} :: LambdaFunctionScheduledEventDetails)
{-# DEPRECATED lfsedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON LambdaFunctionScheduledEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionScheduledEventDetails"
      ( \x ->
          LambdaFunctionScheduledEventDetails'
            Lude.<$> (x Lude..:? "inputDetails")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "timeoutInSeconds")
            Lude.<*> (x Lude..: "resource")
      )
