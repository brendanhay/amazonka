{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
  ( ExecutionSucceededEventDetails (..),

    -- * Smart constructor
    mkExecutionSucceededEventDetails,

    -- * Lenses
    esedOutput,
    esedOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful termination of the execution.
--
-- /See:/ 'mkExecutionSucceededEventDetails' smart constructor.
data ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'
  { -- | The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Lude.Maybe HistoryEventExecutionDataDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionSucceededEventDetails' with the minimum fields required to make a request.
--
-- * 'output' - The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Contains details about the output of an execution history event.
mkExecutionSucceededEventDetails ::
  ExecutionSucceededEventDetails
mkExecutionSucceededEventDetails =
  ExecutionSucceededEventDetails'
    { output = Lude.Nothing,
      outputDetails = Lude.Nothing
    }

-- | The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedOutput :: Lens.Lens' ExecutionSucceededEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
esedOutput = Lens.lens (output :: ExecutionSucceededEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: ExecutionSucceededEventDetails)
{-# DEPRECATED esedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esedOutputDetails :: Lens.Lens' ExecutionSucceededEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
esedOutputDetails = Lens.lens (outputDetails :: ExecutionSucceededEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {outputDetails = a} :: ExecutionSucceededEventDetails)
{-# DEPRECATED esedOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Lude.FromJSON ExecutionSucceededEventDetails where
  parseJSON =
    Lude.withObject
      "ExecutionSucceededEventDetails"
      ( \x ->
          ExecutionSucceededEventDetails'
            Lude.<$> (x Lude..:? "output") Lude.<*> (x Lude..:? "outputDetails")
      )
