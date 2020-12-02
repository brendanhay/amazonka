{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam

-- | Provides summary information for a work team.
--
--
--
-- /See:/ 'labelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { _ljfwsNumberOfHumanWorkersPerDataObject ::
      !(Maybe Nat),
    _ljfwsLabelCounters ::
      !( Maybe
           LabelCountersForWorkteam
       ),
    _ljfwsLabelingJobName ::
      !(Maybe Text),
    _ljfwsJobReferenceCode :: !Text,
    _ljfwsWorkRequesterAccountId ::
      !Text,
    _ljfwsCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobForWorkteamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljfwsNumberOfHumanWorkersPerDataObject' - The configured number of workers per data object.
--
-- * 'ljfwsLabelCounters' - Provides information about the progress of a labeling job.
--
-- * 'ljfwsLabelingJobName' - The name of the labeling job that the work team is assigned to.
--
-- * 'ljfwsJobReferenceCode' - A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
--
-- * 'ljfwsWorkRequesterAccountId' -
--
-- * 'ljfwsCreationTime' - The date and time that the labeling job was created.
labelingJobForWorkteamSummary ::
  -- | 'ljfwsJobReferenceCode'
  Text ->
  -- | 'ljfwsWorkRequesterAccountId'
  Text ->
  -- | 'ljfwsCreationTime'
  UTCTime ->
  LabelingJobForWorkteamSummary
labelingJobForWorkteamSummary
  pJobReferenceCode_
  pWorkRequesterAccountId_
  pCreationTime_ =
    LabelingJobForWorkteamSummary'
      { _ljfwsNumberOfHumanWorkersPerDataObject =
          Nothing,
        _ljfwsLabelCounters = Nothing,
        _ljfwsLabelingJobName = Nothing,
        _ljfwsJobReferenceCode = pJobReferenceCode_,
        _ljfwsWorkRequesterAccountId = pWorkRequesterAccountId_,
        _ljfwsCreationTime = _Time # pCreationTime_
      }

-- | The configured number of workers per data object.
ljfwsNumberOfHumanWorkersPerDataObject :: Lens' LabelingJobForWorkteamSummary (Maybe Natural)
ljfwsNumberOfHumanWorkersPerDataObject = lens _ljfwsNumberOfHumanWorkersPerDataObject (\s a -> s {_ljfwsNumberOfHumanWorkersPerDataObject = a}) . mapping _Nat

-- | Provides information about the progress of a labeling job.
ljfwsLabelCounters :: Lens' LabelingJobForWorkteamSummary (Maybe LabelCountersForWorkteam)
ljfwsLabelCounters = lens _ljfwsLabelCounters (\s a -> s {_ljfwsLabelCounters = a})

-- | The name of the labeling job that the work team is assigned to.
ljfwsLabelingJobName :: Lens' LabelingJobForWorkteamSummary (Maybe Text)
ljfwsLabelingJobName = lens _ljfwsLabelingJobName (\s a -> s {_ljfwsLabelingJobName = a})

-- | A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
ljfwsJobReferenceCode :: Lens' LabelingJobForWorkteamSummary Text
ljfwsJobReferenceCode = lens _ljfwsJobReferenceCode (\s a -> s {_ljfwsJobReferenceCode = a})

-- |
ljfwsWorkRequesterAccountId :: Lens' LabelingJobForWorkteamSummary Text
ljfwsWorkRequesterAccountId = lens _ljfwsWorkRequesterAccountId (\s a -> s {_ljfwsWorkRequesterAccountId = a})

-- | The date and time that the labeling job was created.
ljfwsCreationTime :: Lens' LabelingJobForWorkteamSummary UTCTime
ljfwsCreationTime = lens _ljfwsCreationTime (\s a -> s {_ljfwsCreationTime = a}) . _Time

instance FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    withObject
      "LabelingJobForWorkteamSummary"
      ( \x ->
          LabelingJobForWorkteamSummary'
            <$> (x .:? "NumberOfHumanWorkersPerDataObject")
            <*> (x .:? "LabelCounters")
            <*> (x .:? "LabelingJobName")
            <*> (x .: "JobReferenceCode")
            <*> (x .: "WorkRequesterAccountId")
            <*> (x .: "CreationTime")
      )

instance Hashable LabelingJobForWorkteamSummary

instance NFData LabelingJobForWorkteamSummary
