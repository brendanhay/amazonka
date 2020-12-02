{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRun where

import Network.AWS.Inspector.Types.AssessmentRunNotification
import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.AssessmentRunStateChange
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A snapshot of an Amazon Inspector assessment run that contains the findings of the assessment run .
--
--
-- Used as the response element in the 'DescribeAssessmentRuns' action.
--
--
-- /See:/ 'assessmentRun' smart constructor.
data AssessmentRun = AssessmentRun'
  { _arStartedAt :: !(Maybe POSIX),
    _arCompletedAt :: !(Maybe POSIX),
    _arArn :: !Text,
    _arName :: !Text,
    _arAssessmentTemplateARN :: !Text,
    _arState :: !AssessmentRunState,
    _arDurationInSeconds :: !Nat,
    _arRulesPackageARNs :: !(List1 Text),
    _arUserAttributesForFindings :: ![Attribute],
    _arCreatedAt :: !POSIX,
    _arStateChangedAt :: !POSIX,
    _arDataCollected :: !Bool,
    _arStateChanges :: ![AssessmentRunStateChange],
    _arNotifications :: ![AssessmentRunNotification],
    _arFindingCounts :: !(Map Severity (Int))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arStartedAt' - The time when 'StartAssessmentRun' was called.
--
-- * 'arCompletedAt' - The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
--
-- * 'arArn' - The ARN of the assessment run.
--
-- * 'arName' - The auto-generated name for the assessment run.
--
-- * 'arAssessmentTemplateARN' - The ARN of the assessment template that is associated with the assessment run.
--
-- * 'arState' - The state of the assessment run.
--
-- * 'arDurationInSeconds' - The duration of the assessment run.
--
-- * 'arRulesPackageARNs' - The rules packages selected for the assessment run.
--
-- * 'arUserAttributesForFindings' - The user-defined attributes that are assigned to every generated finding.
--
-- * 'arCreatedAt' - The time when 'StartAssessmentRun' was called.
--
-- * 'arStateChangedAt' - The last time when the assessment run's state changed.
--
-- * 'arDataCollected' - A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
--
-- * 'arStateChanges' - A list of the assessment run state changes.
--
-- * 'arNotifications' - A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
--
-- * 'arFindingCounts' - Provides a total count of generated findings per severity.
assessmentRun ::
  -- | 'arArn'
  Text ->
  -- | 'arName'
  Text ->
  -- | 'arAssessmentTemplateARN'
  Text ->
  -- | 'arState'
  AssessmentRunState ->
  -- | 'arDurationInSeconds'
  Natural ->
  -- | 'arRulesPackageARNs'
  NonEmpty Text ->
  -- | 'arCreatedAt'
  UTCTime ->
  -- | 'arStateChangedAt'
  UTCTime ->
  -- | 'arDataCollected'
  Bool ->
  AssessmentRun
assessmentRun
  pArn_
  pName_
  pAssessmentTemplateARN_
  pState_
  pDurationInSeconds_
  pRulesPackageARNs_
  pCreatedAt_
  pStateChangedAt_
  pDataCollected_ =
    AssessmentRun'
      { _arStartedAt = Nothing,
        _arCompletedAt = Nothing,
        _arArn = pArn_,
        _arName = pName_,
        _arAssessmentTemplateARN = pAssessmentTemplateARN_,
        _arState = pState_,
        _arDurationInSeconds = _Nat # pDurationInSeconds_,
        _arRulesPackageARNs = _List1 # pRulesPackageARNs_,
        _arUserAttributesForFindings = mempty,
        _arCreatedAt = _Time # pCreatedAt_,
        _arStateChangedAt = _Time # pStateChangedAt_,
        _arDataCollected = pDataCollected_,
        _arStateChanges = mempty,
        _arNotifications = mempty,
        _arFindingCounts = mempty
      }

-- | The time when 'StartAssessmentRun' was called.
arStartedAt :: Lens' AssessmentRun (Maybe UTCTime)
arStartedAt = lens _arStartedAt (\s a -> s {_arStartedAt = a}) . mapping _Time

-- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
arCompletedAt :: Lens' AssessmentRun (Maybe UTCTime)
arCompletedAt = lens _arCompletedAt (\s a -> s {_arCompletedAt = a}) . mapping _Time

-- | The ARN of the assessment run.
arArn :: Lens' AssessmentRun Text
arArn = lens _arArn (\s a -> s {_arArn = a})

-- | The auto-generated name for the assessment run.
arName :: Lens' AssessmentRun Text
arName = lens _arName (\s a -> s {_arName = a})

-- | The ARN of the assessment template that is associated with the assessment run.
arAssessmentTemplateARN :: Lens' AssessmentRun Text
arAssessmentTemplateARN = lens _arAssessmentTemplateARN (\s a -> s {_arAssessmentTemplateARN = a})

-- | The state of the assessment run.
arState :: Lens' AssessmentRun AssessmentRunState
arState = lens _arState (\s a -> s {_arState = a})

-- | The duration of the assessment run.
arDurationInSeconds :: Lens' AssessmentRun Natural
arDurationInSeconds = lens _arDurationInSeconds (\s a -> s {_arDurationInSeconds = a}) . _Nat

-- | The rules packages selected for the assessment run.
arRulesPackageARNs :: Lens' AssessmentRun (NonEmpty Text)
arRulesPackageARNs = lens _arRulesPackageARNs (\s a -> s {_arRulesPackageARNs = a}) . _List1

-- | The user-defined attributes that are assigned to every generated finding.
arUserAttributesForFindings :: Lens' AssessmentRun [Attribute]
arUserAttributesForFindings = lens _arUserAttributesForFindings (\s a -> s {_arUserAttributesForFindings = a}) . _Coerce

-- | The time when 'StartAssessmentRun' was called.
arCreatedAt :: Lens' AssessmentRun UTCTime
arCreatedAt = lens _arCreatedAt (\s a -> s {_arCreatedAt = a}) . _Time

-- | The last time when the assessment run's state changed.
arStateChangedAt :: Lens' AssessmentRun UTCTime
arStateChangedAt = lens _arStateChangedAt (\s a -> s {_arStateChangedAt = a}) . _Time

-- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
arDataCollected :: Lens' AssessmentRun Bool
arDataCollected = lens _arDataCollected (\s a -> s {_arDataCollected = a})

-- | A list of the assessment run state changes.
arStateChanges :: Lens' AssessmentRun [AssessmentRunStateChange]
arStateChanges = lens _arStateChanges (\s a -> s {_arStateChanges = a}) . _Coerce

-- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
arNotifications :: Lens' AssessmentRun [AssessmentRunNotification]
arNotifications = lens _arNotifications (\s a -> s {_arNotifications = a}) . _Coerce

-- | Provides a total count of generated findings per severity.
arFindingCounts :: Lens' AssessmentRun (HashMap Severity (Int))
arFindingCounts = lens _arFindingCounts (\s a -> s {_arFindingCounts = a}) . _Map

instance FromJSON AssessmentRun where
  parseJSON =
    withObject
      "AssessmentRun"
      ( \x ->
          AssessmentRun'
            <$> (x .:? "startedAt")
            <*> (x .:? "completedAt")
            <*> (x .: "arn")
            <*> (x .: "name")
            <*> (x .: "assessmentTemplateArn")
            <*> (x .: "state")
            <*> (x .: "durationInSeconds")
            <*> (x .: "rulesPackageArns")
            <*> (x .:? "userAttributesForFindings" .!= mempty)
            <*> (x .: "createdAt")
            <*> (x .: "stateChangedAt")
            <*> (x .: "dataCollected")
            <*> (x .:? "stateChanges" .!= mempty)
            <*> (x .:? "notifications" .!= mempty)
            <*> (x .:? "findingCounts" .!= mempty)
      )

instance Hashable AssessmentRun

instance NFData AssessmentRun
