-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateExitedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateExitedEventDetails
  ( StateExitedEventDetails (..),

    -- * Smart constructor
    mkStateExitedEventDetails,

    -- * Lenses
    seedOutput,
    seedOutputDetails,
    seedName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an exit from a state during an execution.
--
-- /See:/ 'mkStateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'
  { output ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    outputDetails ::
      Lude.Maybe HistoryEventExecutionDataDetails,
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StateExitedEventDetails' with the minimum fields required to make a request.
--
-- * 'name' - The name of the state.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
-- * 'output' - The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Contains details about the output of an execution history event.
mkStateExitedEventDetails ::
  -- | 'name'
  Lude.Text ->
  StateExitedEventDetails
mkStateExitedEventDetails pName_ =
  StateExitedEventDetails'
    { output = Lude.Nothing,
      outputDetails = Lude.Nothing,
      name = pName_
    }

-- | The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedOutput :: Lens.Lens' StateExitedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
seedOutput = Lens.lens (output :: StateExitedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: StateExitedEventDetails)
{-# DEPRECATED seedOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedOutputDetails :: Lens.Lens' StateExitedEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
seedOutputDetails = Lens.lens (outputDetails :: StateExitedEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {outputDetails = a} :: StateExitedEventDetails)
{-# DEPRECATED seedOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | The name of the state.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedName :: Lens.Lens' StateExitedEventDetails Lude.Text
seedName = Lens.lens (name :: StateExitedEventDetails -> Lude.Text) (\s a -> s {name = a} :: StateExitedEventDetails)
{-# DEPRECATED seedName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON StateExitedEventDetails where
  parseJSON =
    Lude.withObject
      "StateExitedEventDetails"
      ( \x ->
          StateExitedEventDetails'
            Lude.<$> (x Lude..:? "output")
            Lude.<*> (x Lude..:? "outputDetails")
            Lude.<*> (x Lude..: "name")
      )
