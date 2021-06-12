{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
import Network.AWS.SageMaker.Types.CandidateStatus
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Network.AWS.SageMaker.Types.ObjectiveStatus

-- | An Autopilot job returns recommendations, or candidates. Each candidate
-- has futher details about the steps involed, and the status.
--
-- /See:/ 'newAutoMLCandidate' smart constructor.
data AutoMLCandidate = AutoMLCandidate'
  { -- | The end time.
    endTime :: Core.Maybe Core.POSIX,
    -- | The inference containers.
    inferenceContainers :: Core.Maybe [AutoMLContainerDefinition],
    -- | The failure reason.
    failureReason :: Core.Maybe Core.Text,
    finalAutoMLJobObjectiveMetric :: Core.Maybe FinalAutoMLJobObjectiveMetric,
    -- | The candidate name.
    candidateName :: Core.Text,
    -- | The objective status.
    objectiveStatus :: ObjectiveStatus,
    -- | The candidate\'s steps.
    candidateSteps :: [AutoMLCandidateStep],
    -- | The candidate\'s status.
    candidateStatus :: CandidateStatus,
    -- | The creation time.
    creationTime :: Core.POSIX,
    -- | The last modified time.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLCandidate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'autoMLCandidate_endTime' - The end time.
--
-- 'inferenceContainers', 'autoMLCandidate_inferenceContainers' - The inference containers.
--
-- 'failureReason', 'autoMLCandidate_failureReason' - The failure reason.
--
-- 'finalAutoMLJobObjectiveMetric', 'autoMLCandidate_finalAutoMLJobObjectiveMetric' - Undocumented member.
--
-- 'candidateName', 'autoMLCandidate_candidateName' - The candidate name.
--
-- 'objectiveStatus', 'autoMLCandidate_objectiveStatus' - The objective status.
--
-- 'candidateSteps', 'autoMLCandidate_candidateSteps' - The candidate\'s steps.
--
-- 'candidateStatus', 'autoMLCandidate_candidateStatus' - The candidate\'s status.
--
-- 'creationTime', 'autoMLCandidate_creationTime' - The creation time.
--
-- 'lastModifiedTime', 'autoMLCandidate_lastModifiedTime' - The last modified time.
newAutoMLCandidate ::
  -- | 'candidateName'
  Core.Text ->
  -- | 'objectiveStatus'
  ObjectiveStatus ->
  -- | 'candidateStatus'
  CandidateStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  AutoMLCandidate
newAutoMLCandidate
  pCandidateName_
  pObjectiveStatus_
  pCandidateStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLCandidate'
      { endTime = Core.Nothing,
        inferenceContainers = Core.Nothing,
        failureReason = Core.Nothing,
        finalAutoMLJobObjectiveMetric = Core.Nothing,
        candidateName = pCandidateName_,
        objectiveStatus = pObjectiveStatus_,
        candidateSteps = Core.mempty,
        candidateStatus = pCandidateStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | The end time.
autoMLCandidate_endTime :: Lens.Lens' AutoMLCandidate (Core.Maybe Core.UTCTime)
autoMLCandidate_endTime = Lens.lens (\AutoMLCandidate' {endTime} -> endTime) (\s@AutoMLCandidate' {} a -> s {endTime = a} :: AutoMLCandidate) Core.. Lens.mapping Core._Time

-- | The inference containers.
autoMLCandidate_inferenceContainers :: Lens.Lens' AutoMLCandidate (Core.Maybe [AutoMLContainerDefinition])
autoMLCandidate_inferenceContainers = Lens.lens (\AutoMLCandidate' {inferenceContainers} -> inferenceContainers) (\s@AutoMLCandidate' {} a -> s {inferenceContainers = a} :: AutoMLCandidate) Core.. Lens.mapping Lens._Coerce

-- | The failure reason.
autoMLCandidate_failureReason :: Lens.Lens' AutoMLCandidate (Core.Maybe Core.Text)
autoMLCandidate_failureReason = Lens.lens (\AutoMLCandidate' {failureReason} -> failureReason) (\s@AutoMLCandidate' {} a -> s {failureReason = a} :: AutoMLCandidate)

-- | Undocumented member.
autoMLCandidate_finalAutoMLJobObjectiveMetric :: Lens.Lens' AutoMLCandidate (Core.Maybe FinalAutoMLJobObjectiveMetric)
autoMLCandidate_finalAutoMLJobObjectiveMetric = Lens.lens (\AutoMLCandidate' {finalAutoMLJobObjectiveMetric} -> finalAutoMLJobObjectiveMetric) (\s@AutoMLCandidate' {} a -> s {finalAutoMLJobObjectiveMetric = a} :: AutoMLCandidate)

-- | The candidate name.
autoMLCandidate_candidateName :: Lens.Lens' AutoMLCandidate Core.Text
autoMLCandidate_candidateName = Lens.lens (\AutoMLCandidate' {candidateName} -> candidateName) (\s@AutoMLCandidate' {} a -> s {candidateName = a} :: AutoMLCandidate)

-- | The objective status.
autoMLCandidate_objectiveStatus :: Lens.Lens' AutoMLCandidate ObjectiveStatus
autoMLCandidate_objectiveStatus = Lens.lens (\AutoMLCandidate' {objectiveStatus} -> objectiveStatus) (\s@AutoMLCandidate' {} a -> s {objectiveStatus = a} :: AutoMLCandidate)

-- | The candidate\'s steps.
autoMLCandidate_candidateSteps :: Lens.Lens' AutoMLCandidate [AutoMLCandidateStep]
autoMLCandidate_candidateSteps = Lens.lens (\AutoMLCandidate' {candidateSteps} -> candidateSteps) (\s@AutoMLCandidate' {} a -> s {candidateSteps = a} :: AutoMLCandidate) Core.. Lens._Coerce

-- | The candidate\'s status.
autoMLCandidate_candidateStatus :: Lens.Lens' AutoMLCandidate CandidateStatus
autoMLCandidate_candidateStatus = Lens.lens (\AutoMLCandidate' {candidateStatus} -> candidateStatus) (\s@AutoMLCandidate' {} a -> s {candidateStatus = a} :: AutoMLCandidate)

-- | The creation time.
autoMLCandidate_creationTime :: Lens.Lens' AutoMLCandidate Core.UTCTime
autoMLCandidate_creationTime = Lens.lens (\AutoMLCandidate' {creationTime} -> creationTime) (\s@AutoMLCandidate' {} a -> s {creationTime = a} :: AutoMLCandidate) Core.. Core._Time

-- | The last modified time.
autoMLCandidate_lastModifiedTime :: Lens.Lens' AutoMLCandidate Core.UTCTime
autoMLCandidate_lastModifiedTime = Lens.lens (\AutoMLCandidate' {lastModifiedTime} -> lastModifiedTime) (\s@AutoMLCandidate' {} a -> s {lastModifiedTime = a} :: AutoMLCandidate) Core.. Core._Time

instance Core.FromJSON AutoMLCandidate where
  parseJSON =
    Core.withObject
      "AutoMLCandidate"
      ( \x ->
          AutoMLCandidate'
            Core.<$> (x Core..:? "EndTime")
            Core.<*> ( x Core..:? "InferenceContainers"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "FinalAutoMLJobObjectiveMetric")
            Core.<*> (x Core..: "CandidateName")
            Core.<*> (x Core..: "ObjectiveStatus")
            Core.<*> (x Core..:? "CandidateSteps" Core..!= Core.mempty)
            Core.<*> (x Core..: "CandidateStatus")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "LastModifiedTime")
      )

instance Core.Hashable AutoMLCandidate

instance Core.NFData AutoMLCandidate
