{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
  ( LambdaFunctionSucceededEventDetails (..),

    -- * Smart constructor
    mkLambdaFunctionSucceededEventDetails,

    -- * Lenses
    lfsedOutput,
    lfsedOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a lambda function that successfully terminated during an execution.
--
-- /See:/ 'mkLambdaFunctionSucceededEventDetails' smart constructor.
data LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { -- | The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Lude.Maybe HistoryEventExecutionDataDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionSucceededEventDetails' with the minimum fields required to make a request.
--
-- * 'output' - The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Contains details about the output of an execution history event.
mkLambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
mkLambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { output = Lude.Nothing,
      outputDetails = Lude.Nothing
    }

-- | The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedOutput :: Lens.Lens' LambdaFunctionSucceededEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
lfsedOutput = Lens.lens (output :: LambdaFunctionSucceededEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: LambdaFunctionSucceededEventDetails)
{-# DEPRECATED lfsedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsedOutputDetails :: Lens.Lens' LambdaFunctionSucceededEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
lfsedOutputDetails = Lens.lens (outputDetails :: LambdaFunctionSucceededEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {outputDetails = a} :: LambdaFunctionSucceededEventDetails)
{-# DEPRECATED lfsedOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Lude.FromJSON LambdaFunctionSucceededEventDetails where
  parseJSON =
    Lude.withObject
      "LambdaFunctionSucceededEventDetails"
      ( \x ->
          LambdaFunctionSucceededEventDetails'
            Lude.<$> (x Lude..:? "output") Lude.<*> (x Lude..:? "outputDetails")
      )
