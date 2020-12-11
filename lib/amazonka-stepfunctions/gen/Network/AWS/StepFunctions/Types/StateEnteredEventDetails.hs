-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateEnteredEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateEnteredEventDetails
  ( StateEnteredEventDetails (..),

    -- * Smart constructor
    mkStateEnteredEventDetails,

    -- * Lenses
    sInputDetails,
    sInput,
    sName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a state entered during an execution.
--
-- /See:/ 'mkStateEnteredEventDetails' smart constructor.
data StateEnteredEventDetails = StateEnteredEventDetails'
  { inputDetails ::
      Lude.Maybe
        HistoryEventExecutionDataDetails,
    input ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StateEnteredEventDetails' with the minimum fields required to make a request.
--
-- * 'input' - The string that contains the JSON input data for the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'inputDetails' - Contains details about the input for an execution history event.
-- * 'name' - The name of the state.
mkStateEnteredEventDetails ::
  -- | 'name'
  Lude.Text ->
  StateEnteredEventDetails
mkStateEnteredEventDetails pName_ =
  StateEnteredEventDetails'
    { inputDetails = Lude.Nothing,
      input = Lude.Nothing,
      name = pName_
    }

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInputDetails :: Lens.Lens' StateEnteredEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
sInputDetails = Lens.lens (inputDetails :: StateEnteredEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {inputDetails = a} :: StateEnteredEventDetails)
{-# DEPRECATED sInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The string that contains the JSON input data for the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInput :: Lens.Lens' StateEnteredEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
sInput = Lens.lens (input :: StateEnteredEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: StateEnteredEventDetails)
{-# DEPRECATED sInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the state.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StateEnteredEventDetails Lude.Text
sName = Lens.lens (name :: StateEnteredEventDetails -> Lude.Text) (\s a -> s {name = a} :: StateEnteredEventDetails)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON StateEnteredEventDetails where
  parseJSON =
    Lude.withObject
      "StateEnteredEventDetails"
      ( \x ->
          StateEnteredEventDetails'
            Lude.<$> (x Lude..:? "inputDetails")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..: "name")
      )
