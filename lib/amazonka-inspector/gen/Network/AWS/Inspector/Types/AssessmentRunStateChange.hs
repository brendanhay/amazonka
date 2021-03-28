{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRunStateChange
  ( AssessmentRunStateChange (..)
  -- * Smart constructor
  , mkAssessmentRunStateChange
  -- * Lenses
  , arscStateChangedAt
  , arscState
  ) where

import qualified Network.AWS.Inspector.Types.AssessmentRunState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
-- /See:/ 'mkAssessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { stateChangedAt :: Core.NominalDiffTime
    -- ^ The last time the assessment run state changed.
  , state :: Types.AssessmentRunState
    -- ^ The assessment run state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssessmentRunStateChange' value with any optional fields omitted.
mkAssessmentRunStateChange
    :: Core.NominalDiffTime -- ^ 'stateChangedAt'
    -> Types.AssessmentRunState -- ^ 'state'
    -> AssessmentRunStateChange
mkAssessmentRunStateChange stateChangedAt state
  = AssessmentRunStateChange'{stateChangedAt, state}

-- | The last time the assessment run state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscStateChangedAt :: Lens.Lens' AssessmentRunStateChange Core.NominalDiffTime
arscStateChangedAt = Lens.field @"stateChangedAt"
{-# INLINEABLE arscStateChangedAt #-}
{-# DEPRECATED stateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead"  #-}

-- | The assessment run state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arscState :: Lens.Lens' AssessmentRunStateChange Types.AssessmentRunState
arscState = Lens.field @"state"
{-# INLINEABLE arscState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON AssessmentRunStateChange where
        parseJSON
          = Core.withObject "AssessmentRunStateChange" Core.$
              \ x ->
                AssessmentRunStateChange' Core.<$>
                  (x Core..: "stateChangedAt") Core.<*> x Core..: "state"
