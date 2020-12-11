-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidate
  ( AutoMLCandidate (..),

    -- * Smart constructor
    mkAutoMLCandidate,

    -- * Lenses
    amlcFailureReason,
    amlcInferenceContainers,
    amlcEndTime,
    amlcFinalAutoMLJobObjectiveMetric,
    amlcCandidateName,
    amlcObjectiveStatus,
    amlcCandidateSteps,
    amlcCandidateStatus,
    amlcCreationTime,
    amlcLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
import Network.AWS.SageMaker.Types.CandidateStatus
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Network.AWS.SageMaker.Types.ObjectiveStatus

-- | An Autopilot job returns recommendations, or candidates. Each candidate has futher details about the steps involed, and the status.
--
-- /See:/ 'mkAutoMLCandidate' smart constructor.
data AutoMLCandidate = AutoMLCandidate'
  { failureReason ::
      Lude.Maybe Lude.Text,
    inferenceContainers ::
      Lude.Maybe [AutoMLContainerDefinition],
    endTime :: Lude.Maybe Lude.Timestamp,
    finalAutoMLJobObjectiveMetric ::
      Lude.Maybe FinalAutoMLJobObjectiveMetric,
    candidateName :: Lude.Text,
    objectiveStatus :: ObjectiveStatus,
    candidateSteps :: [AutoMLCandidateStep],
    candidateStatus :: CandidateStatus,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLCandidate' with the minimum fields required to make a request.
--
-- * 'candidateName' - The candidate name.
-- * 'candidateStatus' - The candidate's status.
-- * 'candidateSteps' - The candidate's steps.
-- * 'creationTime' - The creation time.
-- * 'endTime' - The end time.
-- * 'failureReason' - The failure reason.
-- * 'finalAutoMLJobObjectiveMetric' - Undocumented field.
-- * 'inferenceContainers' - The inference containers.
-- * 'lastModifiedTime' - The last modified time.
-- * 'objectiveStatus' - The objective status.
mkAutoMLCandidate ::
  -- | 'candidateName'
  Lude.Text ->
  -- | 'objectiveStatus'
  ObjectiveStatus ->
  -- | 'candidateStatus'
  CandidateStatus ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  AutoMLCandidate
mkAutoMLCandidate
  pCandidateName_
  pObjectiveStatus_
  pCandidateStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLCandidate'
      { failureReason = Lude.Nothing,
        inferenceContainers = Lude.Nothing,
        endTime = Lude.Nothing,
        finalAutoMLJobObjectiveMetric = Lude.Nothing,
        candidateName = pCandidateName_,
        objectiveStatus = pObjectiveStatus_,
        candidateSteps = Lude.mempty,
        candidateStatus = pCandidateStatus_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcFailureReason :: Lens.Lens' AutoMLCandidate (Lude.Maybe Lude.Text)
amlcFailureReason = Lens.lens (failureReason :: AutoMLCandidate -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: AutoMLCandidate)
{-# DEPRECATED amlcFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The inference containers.
--
-- /Note:/ Consider using 'inferenceContainers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcInferenceContainers :: Lens.Lens' AutoMLCandidate (Lude.Maybe [AutoMLContainerDefinition])
amlcInferenceContainers = Lens.lens (inferenceContainers :: AutoMLCandidate -> Lude.Maybe [AutoMLContainerDefinition]) (\s a -> s {inferenceContainers = a} :: AutoMLCandidate)
{-# DEPRECATED amlcInferenceContainers "Use generic-lens or generic-optics with 'inferenceContainers' instead." #-}

-- | The end time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcEndTime :: Lens.Lens' AutoMLCandidate (Lude.Maybe Lude.Timestamp)
amlcEndTime = Lens.lens (endTime :: AutoMLCandidate -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: AutoMLCandidate)
{-# DEPRECATED amlcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finalAutoMLJobObjectiveMetric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcFinalAutoMLJobObjectiveMetric :: Lens.Lens' AutoMLCandidate (Lude.Maybe FinalAutoMLJobObjectiveMetric)
amlcFinalAutoMLJobObjectiveMetric = Lens.lens (finalAutoMLJobObjectiveMetric :: AutoMLCandidate -> Lude.Maybe FinalAutoMLJobObjectiveMetric) (\s a -> s {finalAutoMLJobObjectiveMetric = a} :: AutoMLCandidate)
{-# DEPRECATED amlcFinalAutoMLJobObjectiveMetric "Use generic-lens or generic-optics with 'finalAutoMLJobObjectiveMetric' instead." #-}

-- | The candidate name.
--
-- /Note:/ Consider using 'candidateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateName :: Lens.Lens' AutoMLCandidate Lude.Text
amlcCandidateName = Lens.lens (candidateName :: AutoMLCandidate -> Lude.Text) (\s a -> s {candidateName = a} :: AutoMLCandidate)
{-# DEPRECATED amlcCandidateName "Use generic-lens or generic-optics with 'candidateName' instead." #-}

-- | The objective status.
--
-- /Note:/ Consider using 'objectiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcObjectiveStatus :: Lens.Lens' AutoMLCandidate ObjectiveStatus
amlcObjectiveStatus = Lens.lens (objectiveStatus :: AutoMLCandidate -> ObjectiveStatus) (\s a -> s {objectiveStatus = a} :: AutoMLCandidate)
{-# DEPRECATED amlcObjectiveStatus "Use generic-lens or generic-optics with 'objectiveStatus' instead." #-}

-- | The candidate's steps.
--
-- /Note:/ Consider using 'candidateSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateSteps :: Lens.Lens' AutoMLCandidate [AutoMLCandidateStep]
amlcCandidateSteps = Lens.lens (candidateSteps :: AutoMLCandidate -> [AutoMLCandidateStep]) (\s a -> s {candidateSteps = a} :: AutoMLCandidate)
{-# DEPRECATED amlcCandidateSteps "Use generic-lens or generic-optics with 'candidateSteps' instead." #-}

-- | The candidate's status.
--
-- /Note:/ Consider using 'candidateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCandidateStatus :: Lens.Lens' AutoMLCandidate CandidateStatus
amlcCandidateStatus = Lens.lens (candidateStatus :: AutoMLCandidate -> CandidateStatus) (\s a -> s {candidateStatus = a} :: AutoMLCandidate)
{-# DEPRECATED amlcCandidateStatus "Use generic-lens or generic-optics with 'candidateStatus' instead." #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcCreationTime :: Lens.Lens' AutoMLCandidate Lude.Timestamp
amlcCreationTime = Lens.lens (creationTime :: AutoMLCandidate -> Lude.Timestamp) (\s a -> s {creationTime = a} :: AutoMLCandidate)
{-# DEPRECATED amlcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcLastModifiedTime :: Lens.Lens' AutoMLCandidate Lude.Timestamp
amlcLastModifiedTime = Lens.lens (lastModifiedTime :: AutoMLCandidate -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: AutoMLCandidate)
{-# DEPRECATED amlcLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Lude.FromJSON AutoMLCandidate where
  parseJSON =
    Lude.withObject
      "AutoMLCandidate"
      ( \x ->
          AutoMLCandidate'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "InferenceContainers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "FinalAutoMLJobObjectiveMetric")
            Lude.<*> (x Lude..: "CandidateName")
            Lude.<*> (x Lude..: "ObjectiveStatus")
            Lude.<*> (x Lude..:? "CandidateSteps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "CandidateStatus")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
      )
