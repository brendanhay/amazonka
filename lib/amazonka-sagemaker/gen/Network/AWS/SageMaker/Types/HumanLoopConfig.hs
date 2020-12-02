{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice

-- | Describes the work to be performed by human workers.
--
--
--
-- /See:/ 'humanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { _hlcTaskKeywords ::
      !(Maybe (List1 Text)),
    _hlcPublicWorkforceTaskPrice ::
      !(Maybe PublicWorkforceTaskPrice),
    _hlcTaskTimeLimitInSeconds :: !(Maybe Nat),
    _hlcTaskAvailabilityLifetimeInSeconds :: !(Maybe Nat),
    _hlcWorkteamARN :: !Text,
    _hlcHumanTaskUiARN :: !Text,
    _hlcTaskTitle :: !Text,
    _hlcTaskDescription :: !Text,
    _hlcTaskCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlcTaskKeywords' - Keywords used to describe the task so that workers can discover the task.
--
-- * 'hlcPublicWorkforceTaskPrice' - Undocumented member.
--
-- * 'hlcTaskTimeLimitInSeconds' - The amount of time that a worker has to complete a task. The default value is 3,600 seconds (1 hour)
--
-- * 'hlcTaskAvailabilityLifetimeInSeconds' - The length of time that a task remains available for review by human workers.
--
-- * 'hlcWorkteamARN' - Amazon Resource Name (ARN) of a team of workers.
--
-- * 'hlcHumanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface.
--
-- * 'hlcTaskTitle' - A title for the human worker task.
--
-- * 'hlcTaskDescription' - A description for the human worker task.
--
-- * 'hlcTaskCount' - The number of distinct workers who will perform the same task on each object. For example, if @TaskCount@ is set to @3@ for an image classification labeling job, three workers will classify each input image. Increasing @TaskCount@ can improve label accuracy.
humanLoopConfig ::
  -- | 'hlcWorkteamARN'
  Text ->
  -- | 'hlcHumanTaskUiARN'
  Text ->
  -- | 'hlcTaskTitle'
  Text ->
  -- | 'hlcTaskDescription'
  Text ->
  -- | 'hlcTaskCount'
  Natural ->
  HumanLoopConfig
humanLoopConfig
  pWorkteamARN_
  pHumanTaskUiARN_
  pTaskTitle_
  pTaskDescription_
  pTaskCount_ =
    HumanLoopConfig'
      { _hlcTaskKeywords = Nothing,
        _hlcPublicWorkforceTaskPrice = Nothing,
        _hlcTaskTimeLimitInSeconds = Nothing,
        _hlcTaskAvailabilityLifetimeInSeconds = Nothing,
        _hlcWorkteamARN = pWorkteamARN_,
        _hlcHumanTaskUiARN = pHumanTaskUiARN_,
        _hlcTaskTitle = pTaskTitle_,
        _hlcTaskDescription = pTaskDescription_,
        _hlcTaskCount = _Nat # pTaskCount_
      }

-- | Keywords used to describe the task so that workers can discover the task.
hlcTaskKeywords :: Lens' HumanLoopConfig (Maybe (NonEmpty Text))
hlcTaskKeywords = lens _hlcTaskKeywords (\s a -> s {_hlcTaskKeywords = a}) . mapping _List1

-- | Undocumented member.
hlcPublicWorkforceTaskPrice :: Lens' HumanLoopConfig (Maybe PublicWorkforceTaskPrice)
hlcPublicWorkforceTaskPrice = lens _hlcPublicWorkforceTaskPrice (\s a -> s {_hlcPublicWorkforceTaskPrice = a})

-- | The amount of time that a worker has to complete a task. The default value is 3,600 seconds (1 hour)
hlcTaskTimeLimitInSeconds :: Lens' HumanLoopConfig (Maybe Natural)
hlcTaskTimeLimitInSeconds = lens _hlcTaskTimeLimitInSeconds (\s a -> s {_hlcTaskTimeLimitInSeconds = a}) . mapping _Nat

-- | The length of time that a task remains available for review by human workers.
hlcTaskAvailabilityLifetimeInSeconds :: Lens' HumanLoopConfig (Maybe Natural)
hlcTaskAvailabilityLifetimeInSeconds = lens _hlcTaskAvailabilityLifetimeInSeconds (\s a -> s {_hlcTaskAvailabilityLifetimeInSeconds = a}) . mapping _Nat

-- | Amazon Resource Name (ARN) of a team of workers.
hlcWorkteamARN :: Lens' HumanLoopConfig Text
hlcWorkteamARN = lens _hlcWorkteamARN (\s a -> s {_hlcWorkteamARN = a})

-- | The Amazon Resource Name (ARN) of the human task user interface.
hlcHumanTaskUiARN :: Lens' HumanLoopConfig Text
hlcHumanTaskUiARN = lens _hlcHumanTaskUiARN (\s a -> s {_hlcHumanTaskUiARN = a})

-- | A title for the human worker task.
hlcTaskTitle :: Lens' HumanLoopConfig Text
hlcTaskTitle = lens _hlcTaskTitle (\s a -> s {_hlcTaskTitle = a})

-- | A description for the human worker task.
hlcTaskDescription :: Lens' HumanLoopConfig Text
hlcTaskDescription = lens _hlcTaskDescription (\s a -> s {_hlcTaskDescription = a})

-- | The number of distinct workers who will perform the same task on each object. For example, if @TaskCount@ is set to @3@ for an image classification labeling job, three workers will classify each input image. Increasing @TaskCount@ can improve label accuracy.
hlcTaskCount :: Lens' HumanLoopConfig Natural
hlcTaskCount = lens _hlcTaskCount (\s a -> s {_hlcTaskCount = a}) . _Nat

instance FromJSON HumanLoopConfig where
  parseJSON =
    withObject
      "HumanLoopConfig"
      ( \x ->
          HumanLoopConfig'
            <$> (x .:? "TaskKeywords")
            <*> (x .:? "PublicWorkforceTaskPrice")
            <*> (x .:? "TaskTimeLimitInSeconds")
            <*> (x .:? "TaskAvailabilityLifetimeInSeconds")
            <*> (x .: "WorkteamArn")
            <*> (x .: "HumanTaskUiArn")
            <*> (x .: "TaskTitle")
            <*> (x .: "TaskDescription")
            <*> (x .: "TaskCount")
      )

instance Hashable HumanLoopConfig

instance NFData HumanLoopConfig

instance ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    object
      ( catMaybes
          [ ("TaskKeywords" .=) <$> _hlcTaskKeywords,
            ("PublicWorkforceTaskPrice" .=) <$> _hlcPublicWorkforceTaskPrice,
            ("TaskTimeLimitInSeconds" .=) <$> _hlcTaskTimeLimitInSeconds,
            ("TaskAvailabilityLifetimeInSeconds" .=)
              <$> _hlcTaskAvailabilityLifetimeInSeconds,
            Just ("WorkteamArn" .= _hlcWorkteamARN),
            Just ("HumanTaskUiArn" .= _hlcHumanTaskUiARN),
            Just ("TaskTitle" .= _hlcTaskTitle),
            Just ("TaskDescription" .= _hlcTaskDescription),
            Just ("TaskCount" .= _hlcTaskCount)
          ]
      )
