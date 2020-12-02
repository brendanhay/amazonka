{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlan where

import Network.AWS.AutoScalingPlans.Types.ApplicationSource
import Network.AWS.AutoScalingPlans.Types.ScalingInstruction
import Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a scaling plan.
--
--
--
-- /See:/ 'scalingPlan' smart constructor.
data ScalingPlan = ScalingPlan'
  { _spCreationTime :: !(Maybe POSIX),
    _spStatusStartTime :: !(Maybe POSIX),
    _spStatusMessage :: !(Maybe Text),
    _spScalingPlanName :: !Text,
    _spScalingPlanVersion :: !Integer,
    _spApplicationSource :: !ApplicationSource,
    _spScalingInstructions :: ![ScalingInstruction],
    _spStatusCode :: !ScalingPlanStatusCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spCreationTime' - The Unix time stamp when the scaling plan was created.
--
-- * 'spStatusStartTime' - The Unix time stamp when the scaling plan entered the current status.
--
-- * 'spStatusMessage' - A simple message about the current status of the scaling plan.
--
-- * 'spScalingPlanName' - The name of the scaling plan.
--
-- * 'spScalingPlanVersion' - The version number of the scaling plan.
--
-- * 'spApplicationSource' - The application source.
--
-- * 'spScalingInstructions' - The scaling instructions.
--
-- * 'spStatusCode' - The status of the scaling plan.     * @Active@ - The scaling plan is active.     * @ActiveWithProblems@ - The scaling plan is active, but the scaling configuration for one or more resources could not be applied.     * @CreationInProgress@ - The scaling plan is being created.     * @CreationFailed@ - The scaling plan could not be created.     * @DeletionInProgress@ - The scaling plan is being deleted.     * @DeletionFailed@ - The scaling plan could not be deleted.     * @UpdateInProgress@ - The scaling plan is being updated.     * @UpdateFailed@ - The scaling plan could not be updated.
scalingPlan ::
  -- | 'spScalingPlanName'
  Text ->
  -- | 'spScalingPlanVersion'
  Integer ->
  -- | 'spApplicationSource'
  ApplicationSource ->
  -- | 'spStatusCode'
  ScalingPlanStatusCode ->
  ScalingPlan
scalingPlan
  pScalingPlanName_
  pScalingPlanVersion_
  pApplicationSource_
  pStatusCode_ =
    ScalingPlan'
      { _spCreationTime = Nothing,
        _spStatusStartTime = Nothing,
        _spStatusMessage = Nothing,
        _spScalingPlanName = pScalingPlanName_,
        _spScalingPlanVersion = pScalingPlanVersion_,
        _spApplicationSource = pApplicationSource_,
        _spScalingInstructions = mempty,
        _spStatusCode = pStatusCode_
      }

-- | The Unix time stamp when the scaling plan was created.
spCreationTime :: Lens' ScalingPlan (Maybe UTCTime)
spCreationTime = lens _spCreationTime (\s a -> s {_spCreationTime = a}) . mapping _Time

-- | The Unix time stamp when the scaling plan entered the current status.
spStatusStartTime :: Lens' ScalingPlan (Maybe UTCTime)
spStatusStartTime = lens _spStatusStartTime (\s a -> s {_spStatusStartTime = a}) . mapping _Time

-- | A simple message about the current status of the scaling plan.
spStatusMessage :: Lens' ScalingPlan (Maybe Text)
spStatusMessage = lens _spStatusMessage (\s a -> s {_spStatusMessage = a})

-- | The name of the scaling plan.
spScalingPlanName :: Lens' ScalingPlan Text
spScalingPlanName = lens _spScalingPlanName (\s a -> s {_spScalingPlanName = a})

-- | The version number of the scaling plan.
spScalingPlanVersion :: Lens' ScalingPlan Integer
spScalingPlanVersion = lens _spScalingPlanVersion (\s a -> s {_spScalingPlanVersion = a})

-- | The application source.
spApplicationSource :: Lens' ScalingPlan ApplicationSource
spApplicationSource = lens _spApplicationSource (\s a -> s {_spApplicationSource = a})

-- | The scaling instructions.
spScalingInstructions :: Lens' ScalingPlan [ScalingInstruction]
spScalingInstructions = lens _spScalingInstructions (\s a -> s {_spScalingInstructions = a}) . _Coerce

-- | The status of the scaling plan.     * @Active@ - The scaling plan is active.     * @ActiveWithProblems@ - The scaling plan is active, but the scaling configuration for one or more resources could not be applied.     * @CreationInProgress@ - The scaling plan is being created.     * @CreationFailed@ - The scaling plan could not be created.     * @DeletionInProgress@ - The scaling plan is being deleted.     * @DeletionFailed@ - The scaling plan could not be deleted.     * @UpdateInProgress@ - The scaling plan is being updated.     * @UpdateFailed@ - The scaling plan could not be updated.
spStatusCode :: Lens' ScalingPlan ScalingPlanStatusCode
spStatusCode = lens _spStatusCode (\s a -> s {_spStatusCode = a})

instance FromJSON ScalingPlan where
  parseJSON =
    withObject
      "ScalingPlan"
      ( \x ->
          ScalingPlan'
            <$> (x .:? "CreationTime")
            <*> (x .:? "StatusStartTime")
            <*> (x .:? "StatusMessage")
            <*> (x .: "ScalingPlanName")
            <*> (x .: "ScalingPlanVersion")
            <*> (x .: "ApplicationSource")
            <*> (x .:? "ScalingInstructions" .!= mempty)
            <*> (x .: "StatusCode")
      )

instance Hashable ScalingPlan

instance NFData ScalingPlan
