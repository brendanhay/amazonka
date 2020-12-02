{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRule where

import Network.AWS.Config.Types.ConfigRuleState
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).
--
--
-- For more information about developing and using AWS Config rules, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html Evaluating AWS Resource Configurations with AWS Config> in the /AWS Config Developer Guide/ .
--
--
-- /See:/ 'configRule' smart constructor.
data ConfigRule = ConfigRule'
  { _crInputParameters :: !(Maybe Text),
    _crConfigRuleName :: !(Maybe Text),
    _crCreatedBy :: !(Maybe Text),
    _crMaximumExecutionFrequency :: !(Maybe MaximumExecutionFrequency),
    _crConfigRuleId :: !(Maybe Text),
    _crScope :: !(Maybe Scope),
    _crConfigRuleState :: !(Maybe ConfigRuleState),
    _crDescription :: !(Maybe Text),
    _crConfigRuleARN :: !(Maybe Text),
    _crSource :: !Source
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crInputParameters' - A string, in JSON format, that is passed to the AWS Config rule Lambda function.
--
-- * 'crConfigRuleName' - The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
--
-- * 'crCreatedBy' - Service principal name of the service that created the rule.
--
-- * 'crMaximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:     * You are using an AWS managed rule that is triggered at a periodic frequency.     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
-- * 'crConfigRuleId' - The ID of the AWS Config rule.
--
-- * 'crScope' - Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
--
-- * 'crConfigRuleState' - Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule. AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule. AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule. AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
--
-- * 'crDescription' - The description that you provide for the AWS Config rule.
--
-- * 'crConfigRuleARN' - The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- * 'crSource' - Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
configRule ::
  -- | 'crSource'
  Source ->
  ConfigRule
configRule pSource_ =
  ConfigRule'
    { _crInputParameters = Nothing,
      _crConfigRuleName = Nothing,
      _crCreatedBy = Nothing,
      _crMaximumExecutionFrequency = Nothing,
      _crConfigRuleId = Nothing,
      _crScope = Nothing,
      _crConfigRuleState = Nothing,
      _crDescription = Nothing,
      _crConfigRuleARN = Nothing,
      _crSource = pSource_
    }

-- | A string, in JSON format, that is passed to the AWS Config rule Lambda function.
crInputParameters :: Lens' ConfigRule (Maybe Text)
crInputParameters = lens _crInputParameters (\s a -> s {_crInputParameters = a})

-- | The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
crConfigRuleName :: Lens' ConfigRule (Maybe Text)
crConfigRuleName = lens _crConfigRuleName (\s a -> s {_crConfigRuleName = a})

-- | Service principal name of the service that created the rule.
crCreatedBy :: Lens' ConfigRule (Maybe Text)
crCreatedBy = lens _crCreatedBy (\s a -> s {_crCreatedBy = a})

-- | The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:     * You are using an AWS managed rule that is triggered at a periodic frequency.     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
crMaximumExecutionFrequency :: Lens' ConfigRule (Maybe MaximumExecutionFrequency)
crMaximumExecutionFrequency = lens _crMaximumExecutionFrequency (\s a -> s {_crMaximumExecutionFrequency = a})

-- | The ID of the AWS Config rule.
crConfigRuleId :: Lens' ConfigRule (Maybe Text)
crConfigRuleId = lens _crConfigRuleId (\s a -> s {_crConfigRuleId = a})

-- | Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
crScope :: Lens' ConfigRule (Maybe Scope)
crScope = lens _crScope (\s a -> s {_crScope = a})

-- | Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule. AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule. AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule. AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
crConfigRuleState :: Lens' ConfigRule (Maybe ConfigRuleState)
crConfigRuleState = lens _crConfigRuleState (\s a -> s {_crConfigRuleState = a})

-- | The description that you provide for the AWS Config rule.
crDescription :: Lens' ConfigRule (Maybe Text)
crDescription = lens _crDescription (\s a -> s {_crDescription = a})

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
crConfigRuleARN :: Lens' ConfigRule (Maybe Text)
crConfigRuleARN = lens _crConfigRuleARN (\s a -> s {_crConfigRuleARN = a})

-- | Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
crSource :: Lens' ConfigRule Source
crSource = lens _crSource (\s a -> s {_crSource = a})

instance FromJSON ConfigRule where
  parseJSON =
    withObject
      "ConfigRule"
      ( \x ->
          ConfigRule'
            <$> (x .:? "InputParameters")
            <*> (x .:? "ConfigRuleName")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "MaximumExecutionFrequency")
            <*> (x .:? "ConfigRuleId")
            <*> (x .:? "Scope")
            <*> (x .:? "ConfigRuleState")
            <*> (x .:? "Description")
            <*> (x .:? "ConfigRuleArn")
            <*> (x .: "Source")
      )

instance Hashable ConfigRule

instance NFData ConfigRule

instance ToJSON ConfigRule where
  toJSON ConfigRule' {..} =
    object
      ( catMaybes
          [ ("InputParameters" .=) <$> _crInputParameters,
            ("ConfigRuleName" .=) <$> _crConfigRuleName,
            ("CreatedBy" .=) <$> _crCreatedBy,
            ("MaximumExecutionFrequency" .=) <$> _crMaximumExecutionFrequency,
            ("ConfigRuleId" .=) <$> _crConfigRuleId,
            ("Scope" .=) <$> _crScope,
            ("ConfigRuleState" .=) <$> _crConfigRuleState,
            ("Description" .=) <$> _crDescription,
            ("ConfigRuleArn" .=) <$> _crConfigRuleARN,
            Just ("Source" .= _crSource)
          ]
      )
