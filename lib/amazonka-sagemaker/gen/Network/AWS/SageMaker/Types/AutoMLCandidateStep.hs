{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidateStep
  ( AutoMLCandidateStep (..),

    -- * Smart constructor
    mkAutoMLCandidateStep,

    -- * Lenses
    amlcsCandidateStepType,
    amlcsCandidateStepArn,
    amlcsCandidateStepName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CandidateStepArn as Types
import qualified Network.AWS.SageMaker.Types.CandidateStepName as Types
import qualified Network.AWS.SageMaker.Types.CandidateStepType as Types

-- | Information about the steps for a Candidate, and what step it is working on.
--
-- /See:/ 'mkAutoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { -- | Whether the Candidate is at the transform, training, or processing step.
    candidateStepType :: Types.CandidateStepType,
    -- | The ARN for the Candidate's step.
    candidateStepArn :: Types.CandidateStepArn,
    -- | The name for the Candidate's step.
    candidateStepName :: Types.CandidateStepName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLCandidateStep' value with any optional fields omitted.
mkAutoMLCandidateStep ::
  -- | 'candidateStepType'
  Types.CandidateStepType ->
  -- | 'candidateStepArn'
  Types.CandidateStepArn ->
  -- | 'candidateStepName'
  Types.CandidateStepName ->
  AutoMLCandidateStep
mkAutoMLCandidateStep
  candidateStepType
  candidateStepArn
  candidateStepName =
    AutoMLCandidateStep'
      { candidateStepType,
        candidateStepArn,
        candidateStepName
      }

-- | Whether the Candidate is at the transform, training, or processing step.
--
-- /Note:/ Consider using 'candidateStepType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepType :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepType
amlcsCandidateStepType = Lens.field @"candidateStepType"
{-# DEPRECATED amlcsCandidateStepType "Use generic-lens or generic-optics with 'candidateStepType' instead." #-}

-- | The ARN for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepArn :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepArn
amlcsCandidateStepArn = Lens.field @"candidateStepArn"
{-# DEPRECATED amlcsCandidateStepArn "Use generic-lens or generic-optics with 'candidateStepArn' instead." #-}

-- | The name for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepName :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepName
amlcsCandidateStepName = Lens.field @"candidateStepName"
{-# DEPRECATED amlcsCandidateStepName "Use generic-lens or generic-optics with 'candidateStepName' instead." #-}

instance Core.FromJSON AutoMLCandidateStep where
  parseJSON =
    Core.withObject "AutoMLCandidateStep" Core.$
      \x ->
        AutoMLCandidateStep'
          Core.<$> (x Core..: "CandidateStepType")
          Core.<*> (x Core..: "CandidateStepArn")
          Core.<*> (x Core..: "CandidateStepName")
