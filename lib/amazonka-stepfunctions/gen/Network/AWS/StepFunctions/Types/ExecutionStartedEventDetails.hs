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
    esedInputDetails,
    esedInput,
    esedRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the start of the execution.
--
-- /See:/ 'mkExecutionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { inputDetails ::
      Lude.Maybe
        HistoryEventExecutionDataDetails,
    input ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionStartedEventDetails' with the minimum fields required to make a request.
--
-- * 'input' - The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'inputDetails' - Contains details about the input for an execution history event.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
mkExecutionStartedEventDetails ::
  ExecutionStartedEventDetails
mkExecutionStartedEventDetails =
  ExecutionStartedEventDetails'
    { inputDetails = Lude.Nothing,
      input = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedInputDetails :: Lens.Lens' ExecutionStartedEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
esedInputDetails = Lens.lens (inputDetails :: ExecutionStartedEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {inputDetails = a} :: ExecutionStartedEventDetails)
{-# DEPRECATED esedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedInput :: Lens.Lens' ExecutionStartedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
esedInput = Lens.lens (input :: ExecutionStartedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: ExecutionStartedEventDetails)
{-# DEPRECATED esedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedRoleARN :: Lens.Lens' ExecutionStartedEventDetails (Lude.Maybe Lude.Text)
esedRoleARN = Lens.lens (roleARN :: ExecutionStartedEventDetails -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ExecutionStartedEventDetails)
{-# DEPRECATED esedRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ExecutionStartedEventDetails where
  parseJSON =
    Lude.withObject
      "ExecutionStartedEventDetails"
      ( \x ->
          ExecutionStartedEventDetails'
            Lude.<$> (x Lude..:? "inputDetails")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "roleArn")
      )
