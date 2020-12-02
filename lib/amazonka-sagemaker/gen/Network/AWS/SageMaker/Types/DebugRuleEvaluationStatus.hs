{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
--
--
-- /See:/ 'debugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { _dresLastModifiedTime ::
      !(Maybe POSIX),
    _dresStatusDetails :: !(Maybe Text),
    _dresRuleEvaluationStatus ::
      !(Maybe RuleEvaluationStatus),
    _dresRuleEvaluationJobARN ::
      !(Maybe Text),
    _dresRuleConfigurationName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DebugRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dresLastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- * 'dresStatusDetails' - Details from the rule evaluation.
--
-- * 'dresRuleEvaluationStatus' - Status of the rule evaluation.
--
-- * 'dresRuleEvaluationJobARN' - The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- * 'dresRuleConfigurationName' - The name of the rule configuration
debugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
debugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { _dresLastModifiedTime = Nothing,
      _dresStatusDetails = Nothing,
      _dresRuleEvaluationStatus = Nothing,
      _dresRuleEvaluationJobARN = Nothing,
      _dresRuleConfigurationName = Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
dresLastModifiedTime :: Lens' DebugRuleEvaluationStatus (Maybe UTCTime)
dresLastModifiedTime = lens _dresLastModifiedTime (\s a -> s {_dresLastModifiedTime = a}) . mapping _Time

-- | Details from the rule evaluation.
dresStatusDetails :: Lens' DebugRuleEvaluationStatus (Maybe Text)
dresStatusDetails = lens _dresStatusDetails (\s a -> s {_dresStatusDetails = a})

-- | Status of the rule evaluation.
dresRuleEvaluationStatus :: Lens' DebugRuleEvaluationStatus (Maybe RuleEvaluationStatus)
dresRuleEvaluationStatus = lens _dresRuleEvaluationStatus (\s a -> s {_dresRuleEvaluationStatus = a})

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
dresRuleEvaluationJobARN :: Lens' DebugRuleEvaluationStatus (Maybe Text)
dresRuleEvaluationJobARN = lens _dresRuleEvaluationJobARN (\s a -> s {_dresRuleEvaluationJobARN = a})

-- | The name of the rule configuration
dresRuleConfigurationName :: Lens' DebugRuleEvaluationStatus (Maybe Text)
dresRuleConfigurationName = lens _dresRuleConfigurationName (\s a -> s {_dresRuleConfigurationName = a})

instance FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            <$> (x .:? "LastModifiedTime")
            <*> (x .:? "StatusDetails")
            <*> (x .:? "RuleEvaluationStatus")
            <*> (x .:? "RuleEvaluationJobArn")
            <*> (x .:? "RuleConfigurationName")
      )

instance Hashable DebugRuleEvaluationStatus

instance NFData DebugRuleEvaluationStatus
