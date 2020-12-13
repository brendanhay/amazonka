{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
  ( ActivitySucceededEventDetails (..),

    -- * Smart constructor
    mkActivitySucceededEventDetails,

    -- * Lenses
    asedOutput,
    asedOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity that successfully terminated during an execution.
--
-- /See:/ 'mkActivitySucceededEventDetails' smart constructor.
data ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { -- | The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Lude.Maybe HistoryEventExecutionDataDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivitySucceededEventDetails' with the minimum fields required to make a request.
--
-- * 'output' - The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Contains details about the output of an execution history event.
mkActivitySucceededEventDetails ::
  ActivitySucceededEventDetails
mkActivitySucceededEventDetails =
  ActivitySucceededEventDetails'
    { output = Lude.Nothing,
      outputDetails = Lude.Nothing
    }

-- | The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedOutput :: Lens.Lens' ActivitySucceededEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
asedOutput = Lens.lens (output :: ActivitySucceededEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: ActivitySucceededEventDetails)
{-# DEPRECATED asedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedOutputDetails :: Lens.Lens' ActivitySucceededEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
asedOutputDetails = Lens.lens (outputDetails :: ActivitySucceededEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {outputDetails = a} :: ActivitySucceededEventDetails)
{-# DEPRECATED asedOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Lude.FromJSON ActivitySucceededEventDetails where
  parseJSON =
    Lude.withObject
      "ActivitySucceededEventDetails"
      ( \x ->
          ActivitySucceededEventDetails'
            Lude.<$> (x Lude..:? "output") Lude.<*> (x Lude..:? "outputDetails")
      )
