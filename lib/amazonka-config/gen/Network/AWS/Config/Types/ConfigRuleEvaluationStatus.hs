{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleEvaluationStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.
--
--
-- This action does not return status information about custom AWS Config rules.
--
--
-- /See:/ 'configRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
  { _cresLastErrorCode ::
      !(Maybe Text),
    _cresLastFailedEvaluationTime ::
      !(Maybe POSIX),
    _cresFirstActivatedTime ::
      !(Maybe POSIX),
    _cresLastSuccessfulEvaluationTime ::
      !(Maybe POSIX),
    _cresLastDeactivatedTime ::
      !(Maybe POSIX),
    _cresConfigRuleName :: !(Maybe Text),
    _cresLastErrorMessage ::
      !(Maybe Text),
    _cresConfigRuleId :: !(Maybe Text),
    _cresLastFailedInvocationTime ::
      !(Maybe POSIX),
    _cresFirstEvaluationStarted ::
      !(Maybe Bool),
    _cresLastSuccessfulInvocationTime ::
      !(Maybe POSIX),
    _cresConfigRuleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cresLastErrorCode' - The error code that AWS Config returned when the rule last failed.
--
-- * 'cresLastFailedEvaluationTime' - The time that AWS Config last failed to evaluate your AWS resources against the rule.
--
-- * 'cresFirstActivatedTime' - The time that you first activated the AWS Config rule.
--
-- * 'cresLastSuccessfulEvaluationTime' - The time that AWS Config last successfully evaluated your AWS resources against the rule.
--
-- * 'cresLastDeactivatedTime' - The time that you last turned off the AWS Config rule.
--
-- * 'cresConfigRuleName' - The name of the AWS Config rule.
--
-- * 'cresLastErrorMessage' - The error message that AWS Config returned when the rule last failed.
--
-- * 'cresConfigRuleId' - The ID of the AWS Config rule.
--
-- * 'cresLastFailedInvocationTime' - The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
--
-- * 'cresFirstEvaluationStarted' - Indicates whether AWS Config has evaluated your resources against the rule at least once.     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
--
-- * 'cresLastSuccessfulInvocationTime' - The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
--
-- * 'cresConfigRuleARN' - The Amazon Resource Name (ARN) of the AWS Config rule.
configRuleEvaluationStatus ::
  ConfigRuleEvaluationStatus
configRuleEvaluationStatus =
  ConfigRuleEvaluationStatus'
    { _cresLastErrorCode = Nothing,
      _cresLastFailedEvaluationTime = Nothing,
      _cresFirstActivatedTime = Nothing,
      _cresLastSuccessfulEvaluationTime = Nothing,
      _cresLastDeactivatedTime = Nothing,
      _cresConfigRuleName = Nothing,
      _cresLastErrorMessage = Nothing,
      _cresConfigRuleId = Nothing,
      _cresLastFailedInvocationTime = Nothing,
      _cresFirstEvaluationStarted = Nothing,
      _cresLastSuccessfulInvocationTime = Nothing,
      _cresConfigRuleARN = Nothing
    }

-- | The error code that AWS Config returned when the rule last failed.
cresLastErrorCode :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresLastErrorCode = lens _cresLastErrorCode (\s a -> s {_cresLastErrorCode = a})

-- | The time that AWS Config last failed to evaluate your AWS resources against the rule.
cresLastFailedEvaluationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastFailedEvaluationTime = lens _cresLastFailedEvaluationTime (\s a -> s {_cresLastFailedEvaluationTime = a}) . mapping _Time

-- | The time that you first activated the AWS Config rule.
cresFirstActivatedTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresFirstActivatedTime = lens _cresFirstActivatedTime (\s a -> s {_cresFirstActivatedTime = a}) . mapping _Time

-- | The time that AWS Config last successfully evaluated your AWS resources against the rule.
cresLastSuccessfulEvaluationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastSuccessfulEvaluationTime = lens _cresLastSuccessfulEvaluationTime (\s a -> s {_cresLastSuccessfulEvaluationTime = a}) . mapping _Time

-- | The time that you last turned off the AWS Config rule.
cresLastDeactivatedTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastDeactivatedTime = lens _cresLastDeactivatedTime (\s a -> s {_cresLastDeactivatedTime = a}) . mapping _Time

-- | The name of the AWS Config rule.
cresConfigRuleName :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleName = lens _cresConfigRuleName (\s a -> s {_cresConfigRuleName = a})

-- | The error message that AWS Config returned when the rule last failed.
cresLastErrorMessage :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresLastErrorMessage = lens _cresLastErrorMessage (\s a -> s {_cresLastErrorMessage = a})

-- | The ID of the AWS Config rule.
cresConfigRuleId :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleId = lens _cresConfigRuleId (\s a -> s {_cresConfigRuleId = a})

-- | The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
cresLastFailedInvocationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastFailedInvocationTime = lens _cresLastFailedInvocationTime (\s a -> s {_cresLastFailedInvocationTime = a}) . mapping _Time

-- | Indicates whether AWS Config has evaluated your resources against the rule at least once.     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
cresFirstEvaluationStarted :: Lens' ConfigRuleEvaluationStatus (Maybe Bool)
cresFirstEvaluationStarted = lens _cresFirstEvaluationStarted (\s a -> s {_cresFirstEvaluationStarted = a})

-- | The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
cresLastSuccessfulInvocationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastSuccessfulInvocationTime = lens _cresLastSuccessfulInvocationTime (\s a -> s {_cresLastSuccessfulInvocationTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
cresConfigRuleARN :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleARN = lens _cresConfigRuleARN (\s a -> s {_cresConfigRuleARN = a})

instance FromJSON ConfigRuleEvaluationStatus where
  parseJSON =
    withObject
      "ConfigRuleEvaluationStatus"
      ( \x ->
          ConfigRuleEvaluationStatus'
            <$> (x .:? "LastErrorCode")
            <*> (x .:? "LastFailedEvaluationTime")
            <*> (x .:? "FirstActivatedTime")
            <*> (x .:? "LastSuccessfulEvaluationTime")
            <*> (x .:? "LastDeactivatedTime")
            <*> (x .:? "ConfigRuleName")
            <*> (x .:? "LastErrorMessage")
            <*> (x .:? "ConfigRuleId")
            <*> (x .:? "LastFailedInvocationTime")
            <*> (x .:? "FirstEvaluationStarted")
            <*> (x .:? "LastSuccessfulInvocationTime")
            <*> (x .:? "ConfigRuleArn")
      )

instance Hashable ConfigRuleEvaluationStatus

instance NFData ConfigRuleEvaluationStatus
