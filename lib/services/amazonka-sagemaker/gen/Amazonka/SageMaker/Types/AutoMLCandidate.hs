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
-- Module      : Amazonka.SageMaker.Types.AutoMLCandidate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLCandidate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLCandidateStep
import Amazonka.SageMaker.Types.AutoMLContainerDefinition
import Amazonka.SageMaker.Types.CandidateProperties
import Amazonka.SageMaker.Types.CandidateStatus
import Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Amazonka.SageMaker.Types.ObjectiveStatus

-- | Information about a candidate produced by an AutoML training job,
-- including its status, steps, and other properties.
--
-- /See:/ 'newAutoMLCandidate' smart constructor.
data AutoMLCandidate = AutoMLCandidate'
  { -- | The properties of an AutoML candidate job.
    candidateProperties :: Prelude.Maybe CandidateProperties,
    -- | The end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    finalAutoMLJobObjectiveMetric :: Prelude.Maybe FinalAutoMLJobObjectiveMetric,
    -- | Information about the inference container definitions.
    inferenceContainers :: Prelude.Maybe [AutoMLContainerDefinition],
    -- | The name of the candidate.
    candidateName :: Prelude.Text,
    -- | The objective\'s status.
    objectiveStatus :: ObjectiveStatus,
    -- | Information about the candidate\'s steps.
    candidateSteps :: [AutoMLCandidateStep],
    -- | The candidate\'s status.
    candidateStatus :: CandidateStatus,
    -- | The creation time.
    creationTime :: Data.POSIX,
    -- | The last modified time.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLCandidate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateProperties', 'autoMLCandidate_candidateProperties' - The properties of an AutoML candidate job.
--
-- 'endTime', 'autoMLCandidate_endTime' - The end time.
--
-- 'failureReason', 'autoMLCandidate_failureReason' - The failure reason.
--
-- 'finalAutoMLJobObjectiveMetric', 'autoMLCandidate_finalAutoMLJobObjectiveMetric' - Undocumented member.
--
-- 'inferenceContainers', 'autoMLCandidate_inferenceContainers' - Information about the inference container definitions.
--
-- 'candidateName', 'autoMLCandidate_candidateName' - The name of the candidate.
--
-- 'objectiveStatus', 'autoMLCandidate_objectiveStatus' - The objective\'s status.
--
-- 'candidateSteps', 'autoMLCandidate_candidateSteps' - Information about the candidate\'s steps.
--
-- 'candidateStatus', 'autoMLCandidate_candidateStatus' - The candidate\'s status.
--
-- 'creationTime', 'autoMLCandidate_creationTime' - The creation time.
--
-- 'lastModifiedTime', 'autoMLCandidate_lastModifiedTime' - The last modified time.
newAutoMLCandidate ::
  -- | 'candidateName'
  Prelude.Text ->
  -- | 'objectiveStatus'
  ObjectiveStatus ->
  -- | 'candidateStatus'
  CandidateStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  AutoMLCandidate
newAutoMLCandidate
  pCandidateName_
  pObjectiveStatus_
  pCandidateStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLCandidate'
      { candidateProperties =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        finalAutoMLJobObjectiveMetric = Prelude.Nothing,
        inferenceContainers = Prelude.Nothing,
        candidateName = pCandidateName_,
        objectiveStatus = pObjectiveStatus_,
        candidateSteps = Prelude.mempty,
        candidateStatus = pCandidateStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The properties of an AutoML candidate job.
autoMLCandidate_candidateProperties :: Lens.Lens' AutoMLCandidate (Prelude.Maybe CandidateProperties)
autoMLCandidate_candidateProperties = Lens.lens (\AutoMLCandidate' {candidateProperties} -> candidateProperties) (\s@AutoMLCandidate' {} a -> s {candidateProperties = a} :: AutoMLCandidate)

-- | The end time.
autoMLCandidate_endTime :: Lens.Lens' AutoMLCandidate (Prelude.Maybe Prelude.UTCTime)
autoMLCandidate_endTime = Lens.lens (\AutoMLCandidate' {endTime} -> endTime) (\s@AutoMLCandidate' {} a -> s {endTime = a} :: AutoMLCandidate) Prelude.. Lens.mapping Data._Time

-- | The failure reason.
autoMLCandidate_failureReason :: Lens.Lens' AutoMLCandidate (Prelude.Maybe Prelude.Text)
autoMLCandidate_failureReason = Lens.lens (\AutoMLCandidate' {failureReason} -> failureReason) (\s@AutoMLCandidate' {} a -> s {failureReason = a} :: AutoMLCandidate)

-- | Undocumented member.
autoMLCandidate_finalAutoMLJobObjectiveMetric :: Lens.Lens' AutoMLCandidate (Prelude.Maybe FinalAutoMLJobObjectiveMetric)
autoMLCandidate_finalAutoMLJobObjectiveMetric = Lens.lens (\AutoMLCandidate' {finalAutoMLJobObjectiveMetric} -> finalAutoMLJobObjectiveMetric) (\s@AutoMLCandidate' {} a -> s {finalAutoMLJobObjectiveMetric = a} :: AutoMLCandidate)

-- | Information about the inference container definitions.
autoMLCandidate_inferenceContainers :: Lens.Lens' AutoMLCandidate (Prelude.Maybe [AutoMLContainerDefinition])
autoMLCandidate_inferenceContainers = Lens.lens (\AutoMLCandidate' {inferenceContainers} -> inferenceContainers) (\s@AutoMLCandidate' {} a -> s {inferenceContainers = a} :: AutoMLCandidate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the candidate.
autoMLCandidate_candidateName :: Lens.Lens' AutoMLCandidate Prelude.Text
autoMLCandidate_candidateName = Lens.lens (\AutoMLCandidate' {candidateName} -> candidateName) (\s@AutoMLCandidate' {} a -> s {candidateName = a} :: AutoMLCandidate)

-- | The objective\'s status.
autoMLCandidate_objectiveStatus :: Lens.Lens' AutoMLCandidate ObjectiveStatus
autoMLCandidate_objectiveStatus = Lens.lens (\AutoMLCandidate' {objectiveStatus} -> objectiveStatus) (\s@AutoMLCandidate' {} a -> s {objectiveStatus = a} :: AutoMLCandidate)

-- | Information about the candidate\'s steps.
autoMLCandidate_candidateSteps :: Lens.Lens' AutoMLCandidate [AutoMLCandidateStep]
autoMLCandidate_candidateSteps = Lens.lens (\AutoMLCandidate' {candidateSteps} -> candidateSteps) (\s@AutoMLCandidate' {} a -> s {candidateSteps = a} :: AutoMLCandidate) Prelude.. Lens.coerced

-- | The candidate\'s status.
autoMLCandidate_candidateStatus :: Lens.Lens' AutoMLCandidate CandidateStatus
autoMLCandidate_candidateStatus = Lens.lens (\AutoMLCandidate' {candidateStatus} -> candidateStatus) (\s@AutoMLCandidate' {} a -> s {candidateStatus = a} :: AutoMLCandidate)

-- | The creation time.
autoMLCandidate_creationTime :: Lens.Lens' AutoMLCandidate Prelude.UTCTime
autoMLCandidate_creationTime = Lens.lens (\AutoMLCandidate' {creationTime} -> creationTime) (\s@AutoMLCandidate' {} a -> s {creationTime = a} :: AutoMLCandidate) Prelude.. Data._Time

-- | The last modified time.
autoMLCandidate_lastModifiedTime :: Lens.Lens' AutoMLCandidate Prelude.UTCTime
autoMLCandidate_lastModifiedTime = Lens.lens (\AutoMLCandidate' {lastModifiedTime} -> lastModifiedTime) (\s@AutoMLCandidate' {} a -> s {lastModifiedTime = a} :: AutoMLCandidate) Prelude.. Data._Time

instance Data.FromJSON AutoMLCandidate where
  parseJSON =
    Data.withObject
      "AutoMLCandidate"
      ( \x ->
          AutoMLCandidate'
            Prelude.<$> (x Data..:? "CandidateProperties")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "FinalAutoMLJobObjectiveMetric")
            Prelude.<*> ( x
                            Data..:? "InferenceContainers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "CandidateName")
            Prelude.<*> (x Data..: "ObjectiveStatus")
            Prelude.<*> (x Data..:? "CandidateSteps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "CandidateStatus")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable AutoMLCandidate where
  hashWithSalt _salt AutoMLCandidate' {..} =
    _salt
      `Prelude.hashWithSalt` candidateProperties
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` finalAutoMLJobObjectiveMetric
      `Prelude.hashWithSalt` inferenceContainers
      `Prelude.hashWithSalt` candidateName
      `Prelude.hashWithSalt` objectiveStatus
      `Prelude.hashWithSalt` candidateSteps
      `Prelude.hashWithSalt` candidateStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData AutoMLCandidate where
  rnf AutoMLCandidate' {..} =
    Prelude.rnf candidateProperties
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf finalAutoMLJobObjectiveMetric
      `Prelude.seq` Prelude.rnf inferenceContainers
      `Prelude.seq` Prelude.rnf candidateName
      `Prelude.seq` Prelude.rnf objectiveStatus
      `Prelude.seq` Prelude.rnf candidateSteps
      `Prelude.seq` Prelude.rnf candidateStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
