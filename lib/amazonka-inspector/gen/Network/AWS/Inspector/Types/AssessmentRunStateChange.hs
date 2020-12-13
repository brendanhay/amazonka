{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunStateChange
  ( AssessmentRunStateChange (..),

    -- * Smart constructor
    mkAssessmentRunStateChange,

    -- * Lenses
    arscState,
    arscStateChangedAt,
  )
where

import Network.AWS.Inspector.Types.AssessmentRunState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
-- /See:/ 'mkAssessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { -- | The assessment run state.
    state :: AssessmentRunState,
    -- | The last time the assessment run state changed.
    stateChangedAt :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentRunStateChange' with the minimum fields required to make a request.
--
-- * 'state' - The assessment run state.
-- * 'stateChangedAt' - The last time the assessment run state changed.
mkAssessmentRunStateChange ::
  -- | 'state'
  AssessmentRunState ->
  -- | 'stateChangedAt'
  Lude.Timestamp ->
  AssessmentRunStateChange
mkAssessmentRunStateChange pState_ pStateChangedAt_ =
  AssessmentRunStateChange'
    { state = pState_,
      stateChangedAt = pStateChangedAt_
    }

-- | The assessment run state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscState :: Lens.Lens' AssessmentRunStateChange AssessmentRunState
arscState = Lens.lens (state :: AssessmentRunStateChange -> AssessmentRunState) (\s a -> s {state = a} :: AssessmentRunStateChange)
{-# DEPRECATED arscState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The last time the assessment run state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscStateChangedAt :: Lens.Lens' AssessmentRunStateChange Lude.Timestamp
arscStateChangedAt = Lens.lens (stateChangedAt :: AssessmentRunStateChange -> Lude.Timestamp) (\s a -> s {stateChangedAt = a} :: AssessmentRunStateChange)
{-# DEPRECATED arscStateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead." #-}

instance Lude.FromJSON AssessmentRunStateChange where
  parseJSON =
    Lude.withObject
      "AssessmentRunStateChange"
      ( \x ->
          AssessmentRunStateChange'
            Lude.<$> (x Lude..: "state") Lude.<*> (x Lude..: "stateChangedAt")
      )
