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
    arscStateChangedAt,
    arscState,
  )
where

import qualified Network.AWS.Inspector.Types.AssessmentRunState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
-- /See:/ 'mkAssessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { -- | The last time the assessment run state changed.
    stateChangedAt :: Core.NominalDiffTime,
    -- | The assessment run state.
    state :: Types.AssessmentRunState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssessmentRunStateChange' value with any optional fields omitted.
mkAssessmentRunStateChange ::
  -- | 'stateChangedAt'
  Core.NominalDiffTime ->
  -- | 'state'
  Types.AssessmentRunState ->
  AssessmentRunStateChange
mkAssessmentRunStateChange stateChangedAt state =
  AssessmentRunStateChange' {stateChangedAt, state}

-- | The last time the assessment run state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscStateChangedAt :: Lens.Lens' AssessmentRunStateChange Core.NominalDiffTime
arscStateChangedAt = Lens.field @"stateChangedAt"
{-# DEPRECATED arscStateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead." #-}

-- | The assessment run state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscState :: Lens.Lens' AssessmentRunStateChange Types.AssessmentRunState
arscState = Lens.field @"state"
{-# DEPRECATED arscState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON AssessmentRunStateChange where
  parseJSON =
    Core.withObject "AssessmentRunStateChange" Core.$
      \x ->
        AssessmentRunStateChange'
          Core.<$> (x Core..: "stateChangedAt") Core.<*> (x Core..: "state")
