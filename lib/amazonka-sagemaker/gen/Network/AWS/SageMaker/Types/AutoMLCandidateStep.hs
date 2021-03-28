{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLCandidateStep
  ( AutoMLCandidateStep (..)
  -- * Smart constructor
  , mkAutoMLCandidateStep
  -- * Lenses
  , amlcsCandidateStepType
  , amlcsCandidateStepArn
  , amlcsCandidateStepName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CandidateStepArn as Types
import qualified Network.AWS.SageMaker.Types.CandidateStepName as Types
import qualified Network.AWS.SageMaker.Types.CandidateStepType as Types

-- | Information about the steps for a Candidate, and what step it is working on.
--
-- /See:/ 'mkAutoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { candidateStepType :: Types.CandidateStepType
    -- ^ Whether the Candidate is at the transform, training, or processing step.
  , candidateStepArn :: Types.CandidateStepArn
    -- ^ The ARN for the Candidate's step.
  , candidateStepName :: Types.CandidateStepName
    -- ^ The name for the Candidate's step.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLCandidateStep' value with any optional fields omitted.
mkAutoMLCandidateStep
    :: Types.CandidateStepType -- ^ 'candidateStepType'
    -> Types.CandidateStepArn -- ^ 'candidateStepArn'
    -> Types.CandidateStepName -- ^ 'candidateStepName'
    -> AutoMLCandidateStep
mkAutoMLCandidateStep candidateStepType candidateStepArn
  candidateStepName
  = AutoMLCandidateStep'{candidateStepType, candidateStepArn,
                         candidateStepName}

-- | Whether the Candidate is at the transform, training, or processing step.
--
-- /Note:/ Consider using 'candidateStepType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepType :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepType
amlcsCandidateStepType = Lens.field @"candidateStepType"
{-# INLINEABLE amlcsCandidateStepType #-}
{-# DEPRECATED candidateStepType "Use generic-lens or generic-optics with 'candidateStepType' instead"  #-}

-- | The ARN for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepArn :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepArn
amlcsCandidateStepArn = Lens.field @"candidateStepArn"
{-# INLINEABLE amlcsCandidateStepArn #-}
{-# DEPRECATED candidateStepArn "Use generic-lens or generic-optics with 'candidateStepArn' instead"  #-}

-- | The name for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepName :: Lens.Lens' AutoMLCandidateStep Types.CandidateStepName
amlcsCandidateStepName = Lens.field @"candidateStepName"
{-# INLINEABLE amlcsCandidateStepName #-}
{-# DEPRECATED candidateStepName "Use generic-lens or generic-optics with 'candidateStepName' instead"  #-}

instance Core.FromJSON AutoMLCandidateStep where
        parseJSON
          = Core.withObject "AutoMLCandidateStep" Core.$
              \ x ->
                AutoMLCandidateStep' Core.<$>
                  (x Core..: "CandidateStepType") Core.<*>
                    x Core..: "CandidateStepArn"
                    Core.<*> x Core..: "CandidateStepName"
