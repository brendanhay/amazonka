{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
import Network.AWS.SageMaker.Types.CandidateStatus
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Network.AWS.SageMaker.Types.ObjectiveStatus

-- | An Autopilot job returns recommendations, or candidates. Each candidate has futher details about the steps involed, and the status.
--
--
--
-- /See:/ 'autoMLCandidate' smart constructor.
data AutoMLCandidate = AutoMLCandidate'
  { _amlcFailureReason ::
      !(Maybe Text),
    _amlcInferenceContainers ::
      !(Maybe [AutoMLContainerDefinition]),
    _amlcEndTime :: !(Maybe POSIX),
    _amlcFinalAutoMLJobObjectiveMetric ::
      !(Maybe FinalAutoMLJobObjectiveMetric),
    _amlcCandidateName :: !Text,
    _amlcObjectiveStatus :: !ObjectiveStatus,
    _amlcCandidateSteps :: ![AutoMLCandidateStep],
    _amlcCandidateStatus :: !CandidateStatus,
    _amlcCreationTime :: !POSIX,
    _amlcLastModifiedTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLCandidate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlcFailureReason' - The failure reason.
--
-- * 'amlcInferenceContainers' - The inference containers.
--
-- * 'amlcEndTime' - The end time.
--
-- * 'amlcFinalAutoMLJobObjectiveMetric' - Undocumented member.
--
-- * 'amlcCandidateName' - The candidate name.
--
-- * 'amlcObjectiveStatus' - The objective status.
--
-- * 'amlcCandidateSteps' - The candidate's steps.
--
-- * 'amlcCandidateStatus' - The candidate's status.
--
-- * 'amlcCreationTime' - The creation time.
--
-- * 'amlcLastModifiedTime' - The last modified time.
autoMLCandidate ::
  -- | 'amlcCandidateName'
  Text ->
  -- | 'amlcObjectiveStatus'
  ObjectiveStatus ->
  -- | 'amlcCandidateStatus'
  CandidateStatus ->
  -- | 'amlcCreationTime'
  UTCTime ->
  -- | 'amlcLastModifiedTime'
  UTCTime ->
  AutoMLCandidate
autoMLCandidate
  pCandidateName_
  pObjectiveStatus_
  pCandidateStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLCandidate'
      { _amlcFailureReason = Nothing,
        _amlcInferenceContainers = Nothing,
        _amlcEndTime = Nothing,
        _amlcFinalAutoMLJobObjectiveMetric = Nothing,
        _amlcCandidateName = pCandidateName_,
        _amlcObjectiveStatus = pObjectiveStatus_,
        _amlcCandidateSteps = mempty,
        _amlcCandidateStatus = pCandidateStatus_,
        _amlcCreationTime = _Time # pCreationTime_,
        _amlcLastModifiedTime = _Time # pLastModifiedTime_
      }

-- | The failure reason.
amlcFailureReason :: Lens' AutoMLCandidate (Maybe Text)
amlcFailureReason = lens _amlcFailureReason (\s a -> s {_amlcFailureReason = a})

-- | The inference containers.
amlcInferenceContainers :: Lens' AutoMLCandidate [AutoMLContainerDefinition]
amlcInferenceContainers = lens _amlcInferenceContainers (\s a -> s {_amlcInferenceContainers = a}) . _Default . _Coerce

-- | The end time.
amlcEndTime :: Lens' AutoMLCandidate (Maybe UTCTime)
amlcEndTime = lens _amlcEndTime (\s a -> s {_amlcEndTime = a}) . mapping _Time

-- | Undocumented member.
amlcFinalAutoMLJobObjectiveMetric :: Lens' AutoMLCandidate (Maybe FinalAutoMLJobObjectiveMetric)
amlcFinalAutoMLJobObjectiveMetric = lens _amlcFinalAutoMLJobObjectiveMetric (\s a -> s {_amlcFinalAutoMLJobObjectiveMetric = a})

-- | The candidate name.
amlcCandidateName :: Lens' AutoMLCandidate Text
amlcCandidateName = lens _amlcCandidateName (\s a -> s {_amlcCandidateName = a})

-- | The objective status.
amlcObjectiveStatus :: Lens' AutoMLCandidate ObjectiveStatus
amlcObjectiveStatus = lens _amlcObjectiveStatus (\s a -> s {_amlcObjectiveStatus = a})

-- | The candidate's steps.
amlcCandidateSteps :: Lens' AutoMLCandidate [AutoMLCandidateStep]
amlcCandidateSteps = lens _amlcCandidateSteps (\s a -> s {_amlcCandidateSteps = a}) . _Coerce

-- | The candidate's status.
amlcCandidateStatus :: Lens' AutoMLCandidate CandidateStatus
amlcCandidateStatus = lens _amlcCandidateStatus (\s a -> s {_amlcCandidateStatus = a})

-- | The creation time.
amlcCreationTime :: Lens' AutoMLCandidate UTCTime
amlcCreationTime = lens _amlcCreationTime (\s a -> s {_amlcCreationTime = a}) . _Time

-- | The last modified time.
amlcLastModifiedTime :: Lens' AutoMLCandidate UTCTime
amlcLastModifiedTime = lens _amlcLastModifiedTime (\s a -> s {_amlcLastModifiedTime = a}) . _Time

instance FromJSON AutoMLCandidate where
  parseJSON =
    withObject
      "AutoMLCandidate"
      ( \x ->
          AutoMLCandidate'
            <$> (x .:? "FailureReason")
            <*> (x .:? "InferenceContainers" .!= mempty)
            <*> (x .:? "EndTime")
            <*> (x .:? "FinalAutoMLJobObjectiveMetric")
            <*> (x .: "CandidateName")
            <*> (x .: "ObjectiveStatus")
            <*> (x .:? "CandidateSteps" .!= mempty)
            <*> (x .: "CandidateStatus")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
      )

instance Hashable AutoMLCandidate

instance NFData AutoMLCandidate
