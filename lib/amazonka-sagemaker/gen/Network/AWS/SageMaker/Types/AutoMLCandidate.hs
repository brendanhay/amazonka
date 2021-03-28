{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLCandidate
  ( AutoMLCandidate (..)
  -- * Smart constructor
  , mkAutoMLCandidate
  -- * Lenses
  , amlcCandidateName
  , amlcObjectiveStatus
  , amlcCandidateSteps
  , amlcCandidateStatus
  , amlcCreationTime
  , amlcLastModifiedTime
  , amlcEndTime
  , amlcFailureReason
  , amlcFinalAutoMLJobObjectiveMetric
  , amlcInferenceContainers
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLCandidateStep as Types
import qualified Network.AWS.SageMaker.Types.AutoMLContainerDefinition as Types
import qualified Network.AWS.SageMaker.Types.AutoMLFailureReason as Types
import qualified Network.AWS.SageMaker.Types.CandidateName as Types
import qualified Network.AWS.SageMaker.Types.CandidateStatus as Types
import qualified Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric as Types
import qualified Network.AWS.SageMaker.Types.ObjectiveStatus as Types

-- | An Autopilot job returns recommendations, or candidates. Each candidate has futher details about the steps involed, and the status.
--
-- /See:/ 'mkAutoMLCandidate' smart constructor.
data AutoMLCandidate = AutoMLCandidate'
  { candidateName :: Types.CandidateName
    -- ^ The candidate name.
  , objectiveStatus :: Types.ObjectiveStatus
    -- ^ The objective status.
  , candidateSteps :: [Types.AutoMLCandidateStep]
    -- ^ The candidate's steps.
  , candidateStatus :: Types.CandidateStatus
    -- ^ The candidate's status.
  , creationTime :: Core.NominalDiffTime
    -- ^ The creation time.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ The last modified time.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time.
  , failureReason :: Core.Maybe Types.AutoMLFailureReason
    -- ^ The failure reason.
  , finalAutoMLJobObjectiveMetric :: Core.Maybe Types.FinalAutoMLJobObjectiveMetric
  , inferenceContainers :: Core.Maybe [Types.AutoMLContainerDefinition]
    -- ^ The inference containers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AutoMLCandidate' value with any optional fields omitted.
mkAutoMLCandidate
    :: Types.CandidateName -- ^ 'candidateName'
    -> Types.ObjectiveStatus -- ^ 'objectiveStatus'
    -> Types.CandidateStatus -- ^ 'candidateStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> AutoMLCandidate
mkAutoMLCandidate candidateName objectiveStatus candidateStatus
  creationTime lastModifiedTime
  = AutoMLCandidate'{candidateName, objectiveStatus,
                     candidateSteps = Core.mempty, candidateStatus, creationTime,
                     lastModifiedTime, endTime = Core.Nothing,
                     failureReason = Core.Nothing,
                     finalAutoMLJobObjectiveMetric = Core.Nothing,
                     inferenceContainers = Core.Nothing}

-- | The candidate name.
--
-- /Note:/ Consider using 'candidateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateName :: Lens.Lens' AutoMLCandidate Types.CandidateName
amlcCandidateName = Lens.field @"candidateName"
{-# INLINEABLE amlcCandidateName #-}
{-# DEPRECATED candidateName "Use generic-lens or generic-optics with 'candidateName' instead"  #-}

-- | The objective status.
--
-- /Note:/ Consider using 'objectiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcObjectiveStatus :: Lens.Lens' AutoMLCandidate Types.ObjectiveStatus
amlcObjectiveStatus = Lens.field @"objectiveStatus"
{-# INLINEABLE amlcObjectiveStatus #-}
{-# DEPRECATED objectiveStatus "Use generic-lens or generic-optics with 'objectiveStatus' instead"  #-}

-- | The candidate's steps.
--
-- /Note:/ Consider using 'candidateSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateSteps :: Lens.Lens' AutoMLCandidate [Types.AutoMLCandidateStep]
amlcCandidateSteps = Lens.field @"candidateSteps"
{-# INLINEABLE amlcCandidateSteps #-}
{-# DEPRECATED candidateSteps "Use generic-lens or generic-optics with 'candidateSteps' instead"  #-}

-- | The candidate's status.
--
-- /Note:/ Consider using 'candidateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateStatus :: Lens.Lens' AutoMLCandidate Types.CandidateStatus
amlcCandidateStatus = Lens.field @"candidateStatus"
{-# INLINEABLE amlcCandidateStatus #-}
{-# DEPRECATED candidateStatus "Use generic-lens or generic-optics with 'candidateStatus' instead"  #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCreationTime :: Lens.Lens' AutoMLCandidate Core.NominalDiffTime
amlcCreationTime = Lens.field @"creationTime"
{-# INLINEABLE amlcCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcLastModifiedTime :: Lens.Lens' AutoMLCandidate Core.NominalDiffTime
amlcLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE amlcLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The end time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcEndTime :: Lens.Lens' AutoMLCandidate (Core.Maybe Core.NominalDiffTime)
amlcEndTime = Lens.field @"endTime"
{-# INLINEABLE amlcEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcFailureReason :: Lens.Lens' AutoMLCandidate (Core.Maybe Types.AutoMLFailureReason)
amlcFailureReason = Lens.field @"failureReason"
{-# INLINEABLE amlcFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finalAutoMLJobObjectiveMetric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcFinalAutoMLJobObjectiveMetric :: Lens.Lens' AutoMLCandidate (Core.Maybe Types.FinalAutoMLJobObjectiveMetric)
amlcFinalAutoMLJobObjectiveMetric = Lens.field @"finalAutoMLJobObjectiveMetric"
{-# INLINEABLE amlcFinalAutoMLJobObjectiveMetric #-}
{-# DEPRECATED finalAutoMLJobObjectiveMetric "Use generic-lens or generic-optics with 'finalAutoMLJobObjectiveMetric' instead"  #-}

-- | The inference containers.
--
-- /Note:/ Consider using 'inferenceContainers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcInferenceContainers :: Lens.Lens' AutoMLCandidate (Core.Maybe [Types.AutoMLContainerDefinition])
amlcInferenceContainers = Lens.field @"inferenceContainers"
{-# INLINEABLE amlcInferenceContainers #-}
{-# DEPRECATED inferenceContainers "Use generic-lens or generic-optics with 'inferenceContainers' instead"  #-}

instance Core.FromJSON AutoMLCandidate where
        parseJSON
          = Core.withObject "AutoMLCandidate" Core.$
              \ x ->
                AutoMLCandidate' Core.<$>
                  (x Core..: "CandidateName") Core.<*> x Core..: "ObjectiveStatus"
                    Core.<*> x Core..:? "CandidateSteps" Core..!= Core.mempty
                    Core.<*> x Core..: "CandidateStatus"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "LastModifiedTime"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "FinalAutoMLJobObjectiveMetric"
                    Core.<*> x Core..:? "InferenceContainers"
