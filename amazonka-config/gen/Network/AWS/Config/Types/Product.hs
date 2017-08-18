{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Product where

import           Network.AWS.Config.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
--
--
-- /See:/ 'compliance' smart constructor.
data Compliance = Compliance'
    { _cComplianceContributorCount :: !(Maybe ComplianceContributorCount)
    , _cComplianceType             :: !(Maybe ComplianceType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Compliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cComplianceContributorCount' - The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
--
-- * 'cComplianceType' - Indicates whether an AWS resource or AWS Config rule is compliant. A resource is compliant if it complies with all of the AWS Config rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules. A rule is compliant if all of the resources that the rule evaluates comply with it, and it is noncompliant if any of these resources do not comply. AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or Config rule. For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
compliance
    :: Compliance
compliance =
    Compliance'
    { _cComplianceContributorCount = Nothing
    , _cComplianceType = Nothing
    }

-- | The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
cComplianceContributorCount :: Lens' Compliance (Maybe ComplianceContributorCount)
cComplianceContributorCount = lens _cComplianceContributorCount (\ s a -> s{_cComplianceContributorCount = a});

-- | Indicates whether an AWS resource or AWS Config rule is compliant. A resource is compliant if it complies with all of the AWS Config rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules. A rule is compliant if all of the resources that the rule evaluates comply with it, and it is noncompliant if any of these resources do not comply. AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or Config rule. For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
cComplianceType :: Lens' Compliance (Maybe ComplianceType)
cComplianceType = lens _cComplianceType (\ s a -> s{_cComplianceType = a});

instance FromJSON Compliance where
        parseJSON
          = withObject "Compliance"
              (\ x ->
                 Compliance' <$>
                   (x .:? "ComplianceContributorCount") <*>
                     (x .:? "ComplianceType"))

instance Hashable Compliance

instance NFData Compliance

-- | Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it, and it is noncompliant if any of these resources do not comply.
--
--
--
-- /See:/ 'complianceByConfigRule' smart constructor.
data ComplianceByConfigRule = ComplianceByConfigRule'
    { _cbcrCompliance     :: !(Maybe Compliance)
    , _cbcrConfigRuleName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComplianceByConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbcrCompliance' - Indicates whether the AWS Config rule is compliant.
--
-- * 'cbcrConfigRuleName' - The name of the AWS Config rule.
complianceByConfigRule
    :: ComplianceByConfigRule
complianceByConfigRule =
    ComplianceByConfigRule'
    { _cbcrCompliance = Nothing
    , _cbcrConfigRuleName = Nothing
    }

-- | Indicates whether the AWS Config rule is compliant.
cbcrCompliance :: Lens' ComplianceByConfigRule (Maybe Compliance)
cbcrCompliance = lens _cbcrCompliance (\ s a -> s{_cbcrCompliance = a});

-- | The name of the AWS Config rule.
cbcrConfigRuleName :: Lens' ComplianceByConfigRule (Maybe Text)
cbcrConfigRuleName = lens _cbcrConfigRuleName (\ s a -> s{_cbcrConfigRuleName = a});

instance FromJSON ComplianceByConfigRule where
        parseJSON
          = withObject "ComplianceByConfigRule"
              (\ x ->
                 ComplianceByConfigRule' <$>
                   (x .:? "Compliance") <*> (x .:? "ConfigRuleName"))

instance Hashable ComplianceByConfigRule

instance NFData ComplianceByConfigRule

-- | Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules.
--
--
--
-- /See:/ 'complianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
    { _cbrResourceId   :: !(Maybe Text)
    , _cbrResourceType :: !(Maybe Text)
    , _cbrCompliance   :: !(Maybe Compliance)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComplianceByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrResourceId' - The ID of the AWS resource that was evaluated.
--
-- * 'cbrResourceType' - The type of the AWS resource that was evaluated.
--
-- * 'cbrCompliance' - Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
complianceByResource
    :: ComplianceByResource
complianceByResource =
    ComplianceByResource'
    { _cbrResourceId = Nothing
    , _cbrResourceType = Nothing
    , _cbrCompliance = Nothing
    }

-- | The ID of the AWS resource that was evaluated.
cbrResourceId :: Lens' ComplianceByResource (Maybe Text)
cbrResourceId = lens _cbrResourceId (\ s a -> s{_cbrResourceId = a});

-- | The type of the AWS resource that was evaluated.
cbrResourceType :: Lens' ComplianceByResource (Maybe Text)
cbrResourceType = lens _cbrResourceType (\ s a -> s{_cbrResourceType = a});

-- | Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
cbrCompliance :: Lens' ComplianceByResource (Maybe Compliance)
cbrCompliance = lens _cbrCompliance (\ s a -> s{_cbrCompliance = a});

instance FromJSON ComplianceByResource where
        parseJSON
          = withObject "ComplianceByResource"
              (\ x ->
                 ComplianceByResource' <$>
                   (x .:? "ResourceId") <*> (x .:? "ResourceType") <*>
                     (x .:? "Compliance"))

instance Hashable ComplianceByResource

instance NFData ComplianceByResource

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.
--
--
--
-- /See:/ 'complianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
    { _cccCappedCount :: !(Maybe Int)
    , _cccCapExceeded :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComplianceContributorCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccCappedCount' - The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
--
-- * 'cccCapExceeded' - Indicates whether the maximum count is reached.
complianceContributorCount
    :: ComplianceContributorCount
complianceContributorCount =
    ComplianceContributorCount'
    { _cccCappedCount = Nothing
    , _cccCapExceeded = Nothing
    }

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
cccCappedCount :: Lens' ComplianceContributorCount (Maybe Int)
cccCappedCount = lens _cccCappedCount (\ s a -> s{_cccCappedCount = a});

-- | Indicates whether the maximum count is reached.
cccCapExceeded :: Lens' ComplianceContributorCount (Maybe Bool)
cccCapExceeded = lens _cccCapExceeded (\ s a -> s{_cccCapExceeded = a});

instance FromJSON ComplianceContributorCount where
        parseJSON
          = withObject "ComplianceContributorCount"
              (\ x ->
                 ComplianceContributorCount' <$>
                   (x .:? "CappedCount") <*> (x .:? "CapExceeded"))

instance Hashable ComplianceContributorCount

instance NFData ComplianceContributorCount

-- | The number of AWS Config rules or AWS resources that are compliant and noncompliant.
--
--
--
-- /See:/ 'complianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
    { _csComplianceSummaryTimestamp :: !(Maybe POSIX)
    , _csCompliantResourceCount     :: !(Maybe ComplianceContributorCount)
    , _csNonCompliantResourceCount  :: !(Maybe ComplianceContributorCount)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csComplianceSummaryTimestamp' - The time that AWS Config created the compliance summary.
--
-- * 'csCompliantResourceCount' - The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
--
-- * 'csNonCompliantResourceCount' - The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
complianceSummary
    :: ComplianceSummary
complianceSummary =
    ComplianceSummary'
    { _csComplianceSummaryTimestamp = Nothing
    , _csCompliantResourceCount = Nothing
    , _csNonCompliantResourceCount = Nothing
    }

-- | The time that AWS Config created the compliance summary.
csComplianceSummaryTimestamp :: Lens' ComplianceSummary (Maybe UTCTime)
csComplianceSummaryTimestamp = lens _csComplianceSummaryTimestamp (\ s a -> s{_csComplianceSummaryTimestamp = a}) . mapping _Time;

-- | The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
csCompliantResourceCount :: Lens' ComplianceSummary (Maybe ComplianceContributorCount)
csCompliantResourceCount = lens _csCompliantResourceCount (\ s a -> s{_csCompliantResourceCount = a});

-- | The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
csNonCompliantResourceCount :: Lens' ComplianceSummary (Maybe ComplianceContributorCount)
csNonCompliantResourceCount = lens _csNonCompliantResourceCount (\ s a -> s{_csNonCompliantResourceCount = a});

instance FromJSON ComplianceSummary where
        parseJSON
          = withObject "ComplianceSummary"
              (\ x ->
                 ComplianceSummary' <$>
                   (x .:? "ComplianceSummaryTimestamp") <*>
                     (x .:? "CompliantResourceCount")
                     <*> (x .:? "NonCompliantResourceCount"))

instance Hashable ComplianceSummary

instance NFData ComplianceSummary

-- | The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each compliance.
--
--
--
-- /See:/ 'complianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
    { _csbrtResourceType      :: !(Maybe Text)
    , _csbrtComplianceSummary :: !(Maybe ComplianceSummary)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComplianceSummaryByResourceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csbrtResourceType' - The type of AWS resource.
--
-- * 'csbrtComplianceSummary' - The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each compliance.
complianceSummaryByResourceType
    :: ComplianceSummaryByResourceType
complianceSummaryByResourceType =
    ComplianceSummaryByResourceType'
    { _csbrtResourceType = Nothing
    , _csbrtComplianceSummary = Nothing
    }

-- | The type of AWS resource.
csbrtResourceType :: Lens' ComplianceSummaryByResourceType (Maybe Text)
csbrtResourceType = lens _csbrtResourceType (\ s a -> s{_csbrtResourceType = a});

-- | The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each compliance.
csbrtComplianceSummary :: Lens' ComplianceSummaryByResourceType (Maybe ComplianceSummary)
csbrtComplianceSummary = lens _csbrtComplianceSummary (\ s a -> s{_csbrtComplianceSummary = a});

instance FromJSON ComplianceSummaryByResourceType
         where
        parseJSON
          = withObject "ComplianceSummaryByResourceType"
              (\ x ->
                 ComplianceSummaryByResourceType' <$>
                   (x .:? "ResourceType") <*>
                     (x .:? "ComplianceSummary"))

instance Hashable ComplianceSummaryByResourceType

instance NFData ComplianceSummaryByResourceType

-- | A list that contains the status of the delivery of either the snapshot or the configuration history to the specified Amazon S3 bucket.
--
--
--
-- /See:/ 'configExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
    { _cediLastErrorCode      :: !(Maybe Text)
    , _cediLastAttemptTime    :: !(Maybe POSIX)
    , _cediLastSuccessfulTime :: !(Maybe POSIX)
    , _cediLastStatus         :: !(Maybe DeliveryStatus)
    , _cediLastErrorMessage   :: !(Maybe Text)
    , _cediNextDeliveryTime   :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigExportDeliveryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cediLastErrorCode' - The error code from the last attempted delivery.
--
-- * 'cediLastAttemptTime' - The time of the last attempted delivery.
--
-- * 'cediLastSuccessfulTime' - The time of the last successful delivery.
--
-- * 'cediLastStatus' - Status of the last attempted delivery.
--
-- * 'cediLastErrorMessage' - The error message from the last attempted delivery.
--
-- * 'cediNextDeliveryTime' - The time that the next delivery occurs.
configExportDeliveryInfo
    :: ConfigExportDeliveryInfo
configExportDeliveryInfo =
    ConfigExportDeliveryInfo'
    { _cediLastErrorCode = Nothing
    , _cediLastAttemptTime = Nothing
    , _cediLastSuccessfulTime = Nothing
    , _cediLastStatus = Nothing
    , _cediLastErrorMessage = Nothing
    , _cediNextDeliveryTime = Nothing
    }

-- | The error code from the last attempted delivery.
cediLastErrorCode :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorCode = lens _cediLastErrorCode (\ s a -> s{_cediLastErrorCode = a});

-- | The time of the last attempted delivery.
cediLastAttemptTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastAttemptTime = lens _cediLastAttemptTime (\ s a -> s{_cediLastAttemptTime = a}) . mapping _Time;

-- | The time of the last successful delivery.
cediLastSuccessfulTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastSuccessfulTime = lens _cediLastSuccessfulTime (\ s a -> s{_cediLastSuccessfulTime = a}) . mapping _Time;

-- | Status of the last attempted delivery.
cediLastStatus :: Lens' ConfigExportDeliveryInfo (Maybe DeliveryStatus)
cediLastStatus = lens _cediLastStatus (\ s a -> s{_cediLastStatus = a});

-- | The error message from the last attempted delivery.
cediLastErrorMessage :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorMessage = lens _cediLastErrorMessage (\ s a -> s{_cediLastErrorMessage = a});

-- | The time that the next delivery occurs.
cediNextDeliveryTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediNextDeliveryTime = lens _cediNextDeliveryTime (\ s a -> s{_cediNextDeliveryTime = a}) . mapping _Time;

instance FromJSON ConfigExportDeliveryInfo where
        parseJSON
          = withObject "ConfigExportDeliveryInfo"
              (\ x ->
                 ConfigExportDeliveryInfo' <$>
                   (x .:? "lastErrorCode") <*> (x .:? "lastAttemptTime")
                     <*> (x .:? "lastSuccessfulTime")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage")
                     <*> (x .:? "nextDeliveryTime"))

instance Hashable ConfigExportDeliveryInfo

instance NFData ConfigExportDeliveryInfo

-- | An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).
--
--
-- For more information about developing and using AWS Config rules, see <http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html Evaluating AWS Resource Configurations with AWS Config> in the /AWS Config Developer Guide/ .
--
--
-- /See:/ 'configRule' smart constructor.
data ConfigRule = ConfigRule'
    { _crInputParameters           :: !(Maybe Text)
    , _crConfigRuleName            :: !(Maybe Text)
    , _crMaximumExecutionFrequency :: !(Maybe MaximumExecutionFrequency)
    , _crConfigRuleId              :: !(Maybe Text)
    , _crScope                     :: !(Maybe Scope)
    , _crConfigRuleState           :: !(Maybe ConfigRuleState)
    , _crDescription               :: !(Maybe Text)
    , _crConfigRuleARN             :: !(Maybe Text)
    , _crSource                    :: !Source
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crInputParameters' - A string in JSON format that is passed to the AWS Config rule Lambda function.
--
-- * 'crConfigRuleName' - The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
--
-- * 'crMaximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:     * You are using an AWS managed rule that is triggered at a periodic frequency.     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
-- * 'crConfigRuleId' - The ID of the AWS Config rule.
--
-- * 'crScope' - Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
--
-- * 'crConfigRuleState' - Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the Config rule. AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the Config rule. AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the Config rule. AWS Config sets the state of a rule to @DELETING@ temporarily after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
--
-- * 'crDescription' - The description that you provide for the AWS Config rule.
--
-- * 'crConfigRuleARN' - The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- * 'crSource' - Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
configRule
    :: Source -- ^ 'crSource'
    -> ConfigRule
configRule pSource_ =
    ConfigRule'
    { _crInputParameters = Nothing
    , _crConfigRuleName = Nothing
    , _crMaximumExecutionFrequency = Nothing
    , _crConfigRuleId = Nothing
    , _crScope = Nothing
    , _crConfigRuleState = Nothing
    , _crDescription = Nothing
    , _crConfigRuleARN = Nothing
    , _crSource = pSource_
    }

-- | A string in JSON format that is passed to the AWS Config rule Lambda function.
crInputParameters :: Lens' ConfigRule (Maybe Text)
crInputParameters = lens _crInputParameters (\ s a -> s{_crInputParameters = a});

-- | The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
crConfigRuleName :: Lens' ConfigRule (Maybe Text)
crConfigRuleName = lens _crConfigRuleName (\ s a -> s{_crConfigRuleName = a});

-- | The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:     * You are using an AWS managed rule that is triggered at a periodic frequency.     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
crMaximumExecutionFrequency :: Lens' ConfigRule (Maybe MaximumExecutionFrequency)
crMaximumExecutionFrequency = lens _crMaximumExecutionFrequency (\ s a -> s{_crMaximumExecutionFrequency = a});

-- | The ID of the AWS Config rule.
crConfigRuleId :: Lens' ConfigRule (Maybe Text)
crConfigRuleId = lens _crConfigRuleId (\ s a -> s{_crConfigRuleId = a});

-- | Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
crScope :: Lens' ConfigRule (Maybe Scope)
crScope = lens _crScope (\ s a -> s{_crScope = a});

-- | Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the Config rule. AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the Config rule. AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the Config rule. AWS Config sets the state of a rule to @DELETING@ temporarily after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
crConfigRuleState :: Lens' ConfigRule (Maybe ConfigRuleState)
crConfigRuleState = lens _crConfigRuleState (\ s a -> s{_crConfigRuleState = a});

-- | The description that you provide for the AWS Config rule.
crDescription :: Lens' ConfigRule (Maybe Text)
crDescription = lens _crDescription (\ s a -> s{_crDescription = a});

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
crConfigRuleARN :: Lens' ConfigRule (Maybe Text)
crConfigRuleARN = lens _crConfigRuleARN (\ s a -> s{_crConfigRuleARN = a});

-- | Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
crSource :: Lens' ConfigRule Source
crSource = lens _crSource (\ s a -> s{_crSource = a});

instance FromJSON ConfigRule where
        parseJSON
          = withObject "ConfigRule"
              (\ x ->
                 ConfigRule' <$>
                   (x .:? "InputParameters") <*>
                     (x .:? "ConfigRuleName")
                     <*> (x .:? "MaximumExecutionFrequency")
                     <*> (x .:? "ConfigRuleId")
                     <*> (x .:? "Scope")
                     <*> (x .:? "ConfigRuleState")
                     <*> (x .:? "Description")
                     <*> (x .:? "ConfigRuleArn")
                     <*> (x .: "Source"))

instance Hashable ConfigRule

instance NFData ConfigRule

instance ToJSON ConfigRule where
        toJSON ConfigRule'{..}
          = object
              (catMaybes
                 [("InputParameters" .=) <$> _crInputParameters,
                  ("ConfigRuleName" .=) <$> _crConfigRuleName,
                  ("MaximumExecutionFrequency" .=) <$>
                    _crMaximumExecutionFrequency,
                  ("ConfigRuleId" .=) <$> _crConfigRuleId,
                  ("Scope" .=) <$> _crScope,
                  ("ConfigRuleState" .=) <$> _crConfigRuleState,
                  ("Description" .=) <$> _crDescription,
                  ("ConfigRuleArn" .=) <$> _crConfigRuleARN,
                  Just ("Source" .= _crSource)])

-- | Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.
--
--
-- This action does not return status information about custom Config rules.
--
--
-- /See:/ 'configRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
    { _cresLastErrorCode                :: !(Maybe Text)
    , _cresLastFailedEvaluationTime     :: !(Maybe POSIX)
    , _cresFirstActivatedTime           :: !(Maybe POSIX)
    , _cresLastSuccessfulEvaluationTime :: !(Maybe POSIX)
    , _cresConfigRuleName               :: !(Maybe Text)
    , _cresLastErrorMessage             :: !(Maybe Text)
    , _cresConfigRuleId                 :: !(Maybe Text)
    , _cresLastFailedInvocationTime     :: !(Maybe POSIX)
    , _cresFirstEvaluationStarted       :: !(Maybe Bool)
    , _cresLastSuccessfulInvocationTime :: !(Maybe POSIX)
    , _cresConfigRuleARN                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
configRuleEvaluationStatus
    :: ConfigRuleEvaluationStatus
configRuleEvaluationStatus =
    ConfigRuleEvaluationStatus'
    { _cresLastErrorCode = Nothing
    , _cresLastFailedEvaluationTime = Nothing
    , _cresFirstActivatedTime = Nothing
    , _cresLastSuccessfulEvaluationTime = Nothing
    , _cresConfigRuleName = Nothing
    , _cresLastErrorMessage = Nothing
    , _cresConfigRuleId = Nothing
    , _cresLastFailedInvocationTime = Nothing
    , _cresFirstEvaluationStarted = Nothing
    , _cresLastSuccessfulInvocationTime = Nothing
    , _cresConfigRuleARN = Nothing
    }

-- | The error code that AWS Config returned when the rule last failed.
cresLastErrorCode :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresLastErrorCode = lens _cresLastErrorCode (\ s a -> s{_cresLastErrorCode = a});

-- | The time that AWS Config last failed to evaluate your AWS resources against the rule.
cresLastFailedEvaluationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastFailedEvaluationTime = lens _cresLastFailedEvaluationTime (\ s a -> s{_cresLastFailedEvaluationTime = a}) . mapping _Time;

-- | The time that you first activated the AWS Config rule.
cresFirstActivatedTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresFirstActivatedTime = lens _cresFirstActivatedTime (\ s a -> s{_cresFirstActivatedTime = a}) . mapping _Time;

-- | The time that AWS Config last successfully evaluated your AWS resources against the rule.
cresLastSuccessfulEvaluationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastSuccessfulEvaluationTime = lens _cresLastSuccessfulEvaluationTime (\ s a -> s{_cresLastSuccessfulEvaluationTime = a}) . mapping _Time;

-- | The name of the AWS Config rule.
cresConfigRuleName :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleName = lens _cresConfigRuleName (\ s a -> s{_cresConfigRuleName = a});

-- | The error message that AWS Config returned when the rule last failed.
cresLastErrorMessage :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresLastErrorMessage = lens _cresLastErrorMessage (\ s a -> s{_cresLastErrorMessage = a});

-- | The ID of the AWS Config rule.
cresConfigRuleId :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleId = lens _cresConfigRuleId (\ s a -> s{_cresConfigRuleId = a});

-- | The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
cresLastFailedInvocationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastFailedInvocationTime = lens _cresLastFailedInvocationTime (\ s a -> s{_cresLastFailedInvocationTime = a}) . mapping _Time;

-- | Indicates whether AWS Config has evaluated your resources against the rule at least once.     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
cresFirstEvaluationStarted :: Lens' ConfigRuleEvaluationStatus (Maybe Bool)
cresFirstEvaluationStarted = lens _cresFirstEvaluationStarted (\ s a -> s{_cresFirstEvaluationStarted = a});

-- | The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
cresLastSuccessfulInvocationTime :: Lens' ConfigRuleEvaluationStatus (Maybe UTCTime)
cresLastSuccessfulInvocationTime = lens _cresLastSuccessfulInvocationTime (\ s a -> s{_cresLastSuccessfulInvocationTime = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
cresConfigRuleARN :: Lens' ConfigRuleEvaluationStatus (Maybe Text)
cresConfigRuleARN = lens _cresConfigRuleARN (\ s a -> s{_cresConfigRuleARN = a});

instance FromJSON ConfigRuleEvaluationStatus where
        parseJSON
          = withObject "ConfigRuleEvaluationStatus"
              (\ x ->
                 ConfigRuleEvaluationStatus' <$>
                   (x .:? "LastErrorCode") <*>
                     (x .:? "LastFailedEvaluationTime")
                     <*> (x .:? "FirstActivatedTime")
                     <*> (x .:? "LastSuccessfulEvaluationTime")
                     <*> (x .:? "ConfigRuleName")
                     <*> (x .:? "LastErrorMessage")
                     <*> (x .:? "ConfigRuleId")
                     <*> (x .:? "LastFailedInvocationTime")
                     <*> (x .:? "FirstEvaluationStarted")
                     <*> (x .:? "LastSuccessfulInvocationTime")
                     <*> (x .:? "ConfigRuleArn"))

instance Hashable ConfigRuleEvaluationStatus

instance NFData ConfigRuleEvaluationStatus

-- | Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.
--
--
-- The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:
--
--     * The value for the @deliveryFrequency@ parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for Config rules.
--
--     * The value for the @MaximumExecutionFrequency@ parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see 'ConfigRule' .
--
--
--
-- If the @deliveryFrequency@ value is less frequent than the @MaximumExecutionFrequency@ value for a rule, AWS Config invokes the rule only as often as the @deliveryFrequency@ value.
--
--     * For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.
--
--     * You specify the @MaximumExecutionFrequency@ value for @Six_Hours@ .
--
--     * You then specify the delivery channel @deliveryFrequency@ value for @TwentyFour_Hours@ .
--
--     * Because the value for @deliveryFrequency@ is less frequent than @MaximumExecutionFrequency@ , AWS Config invokes evaluations for the rule every 24 hours.
--
--
--
-- You should set the @MaximumExecutionFrequency@ value to be at least as frequent as the @deliveryFrequency@ value. You can view the @deliveryFrequency@ value by using the @DescribeDeliveryChannnels@ action.
--
-- To update the @deliveryFrequency@ with which AWS Config delivers your configuration snapshots, use the @PutDeliveryChannel@ action.
--
--
-- /See:/ 'configSnapshotDeliveryProperties' smart constructor.
newtype ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties'
    { _csdpDeliveryFrequency :: Maybe MaximumExecutionFrequency
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigSnapshotDeliveryProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdpDeliveryFrequency' - The frequency with which AWS Config delivers configuration snapshots.
configSnapshotDeliveryProperties
    :: ConfigSnapshotDeliveryProperties
configSnapshotDeliveryProperties =
    ConfigSnapshotDeliveryProperties'
    { _csdpDeliveryFrequency = Nothing
    }

-- | The frequency with which AWS Config delivers configuration snapshots.
csdpDeliveryFrequency :: Lens' ConfigSnapshotDeliveryProperties (Maybe MaximumExecutionFrequency)
csdpDeliveryFrequency = lens _csdpDeliveryFrequency (\ s a -> s{_csdpDeliveryFrequency = a});

instance FromJSON ConfigSnapshotDeliveryProperties
         where
        parseJSON
          = withObject "ConfigSnapshotDeliveryProperties"
              (\ x ->
                 ConfigSnapshotDeliveryProperties' <$>
                   (x .:? "deliveryFrequency"))

instance Hashable ConfigSnapshotDeliveryProperties

instance NFData ConfigSnapshotDeliveryProperties

instance ToJSON ConfigSnapshotDeliveryProperties
         where
        toJSON ConfigSnapshotDeliveryProperties'{..}
          = object
              (catMaybes
                 [("deliveryFrequency" .=) <$>
                    _csdpDeliveryFrequency])

-- | A list that contains the status of the delivery of the configuration stream notification to the Amazon SNS topic.
--
--
--
-- /See:/ 'configStreamDeliveryInfo' smart constructor.
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
    { _csdiLastErrorCode        :: !(Maybe Text)
    , _csdiLastStatusChangeTime :: !(Maybe POSIX)
    , _csdiLastStatus           :: !(Maybe DeliveryStatus)
    , _csdiLastErrorMessage     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigStreamDeliveryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdiLastErrorCode' - The error code from the last attempted delivery.
--
-- * 'csdiLastStatusChangeTime' - The time from the last status change.
--
-- * 'csdiLastStatus' - Status of the last attempted delivery. __Note__ Providing an SNS topic on a <http://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel> for AWS Config is optional. If the SNS delivery is turned off, the last status will be __Not_Applicable__ .
--
-- * 'csdiLastErrorMessage' - The error message from the last attempted delivery.
configStreamDeliveryInfo
    :: ConfigStreamDeliveryInfo
configStreamDeliveryInfo =
    ConfigStreamDeliveryInfo'
    { _csdiLastErrorCode = Nothing
    , _csdiLastStatusChangeTime = Nothing
    , _csdiLastStatus = Nothing
    , _csdiLastErrorMessage = Nothing
    }

-- | The error code from the last attempted delivery.
csdiLastErrorCode :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorCode = lens _csdiLastErrorCode (\ s a -> s{_csdiLastErrorCode = a});

-- | The time from the last status change.
csdiLastStatusChangeTime :: Lens' ConfigStreamDeliveryInfo (Maybe UTCTime)
csdiLastStatusChangeTime = lens _csdiLastStatusChangeTime (\ s a -> s{_csdiLastStatusChangeTime = a}) . mapping _Time;

-- | Status of the last attempted delivery. __Note__ Providing an SNS topic on a <http://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel> for AWS Config is optional. If the SNS delivery is turned off, the last status will be __Not_Applicable__ .
csdiLastStatus :: Lens' ConfigStreamDeliveryInfo (Maybe DeliveryStatus)
csdiLastStatus = lens _csdiLastStatus (\ s a -> s{_csdiLastStatus = a});

-- | The error message from the last attempted delivery.
csdiLastErrorMessage :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorMessage = lens _csdiLastErrorMessage (\ s a -> s{_csdiLastErrorMessage = a});

instance FromJSON ConfigStreamDeliveryInfo where
        parseJSON
          = withObject "ConfigStreamDeliveryInfo"
              (\ x ->
                 ConfigStreamDeliveryInfo' <$>
                   (x .:? "lastErrorCode") <*>
                     (x .:? "lastStatusChangeTime")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage"))

instance Hashable ConfigStreamDeliveryInfo

instance NFData ConfigStreamDeliveryInfo

-- | A list that contains detailed configurations of a specified resource.
--
--
--
-- /See:/ 'configurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
    { _ciResourceId                   :: !(Maybe Text)
    , _ciResourceType                 :: !(Maybe ResourceType)
    , _ciConfigurationStateId         :: !(Maybe Text)
    , _ciArn                          :: !(Maybe Text)
    , _ciResourceName                 :: !(Maybe Text)
    , _ciResourceCreationTime         :: !(Maybe POSIX)
    , _ciConfigurationItemStatus      :: !(Maybe ConfigurationItemStatus)
    , _ciConfigurationItemCaptureTime :: !(Maybe POSIX)
    , _ciAccountId                    :: !(Maybe Text)
    , _ciSupplementaryConfiguration   :: !(Maybe (Map Text Text))
    , _ciAvailabilityZone             :: !(Maybe Text)
    , _ciRelationships                :: !(Maybe [Relationship])
    , _ciVersion                      :: !(Maybe Text)
    , _ciAwsRegion                    :: !(Maybe Text)
    , _ciRelatedEvents                :: !(Maybe [Text])
    , _ciConfiguration                :: !(Maybe Text)
    , _ciConfigurationItemMD5Hash     :: !(Maybe Text)
    , _ciTags                         :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciResourceId' - The ID of the resource (for example., @sg-xxxxxx@ ).
--
-- * 'ciResourceType' - The type of AWS resource.
--
-- * 'ciConfigurationStateId' - An identifier that indicates the ordering of the configuration items of a resource.
--
-- * 'ciArn' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'ciResourceName' - The custom name of the resource, if available.
--
-- * 'ciResourceCreationTime' - The time stamp when the resource was created.
--
-- * 'ciConfigurationItemStatus' - The configuration item status.
--
-- * 'ciConfigurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- * 'ciAccountId' - The 12 digit AWS account ID associated with the resource.
--
-- * 'ciSupplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
--
-- * 'ciAvailabilityZone' - The Availability Zone associated with the resource.
--
-- * 'ciRelationships' - A list of related AWS resources.
--
-- * 'ciVersion' - The version number of the resource configuration.
--
-- * 'ciAwsRegion' - The region where the resource resides.
--
-- * 'ciRelatedEvents' - A list of CloudTrail event IDs. A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What is AWS CloudTrail?> . An empty field indicates that the current configuration was not initiated by any event.
--
-- * 'ciConfiguration' - The description of the resource configuration.
--
-- * 'ciConfigurationItemMD5Hash' - Unique MD5 hash that represents the configuration item's state. You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
--
-- * 'ciTags' - A mapping of key value tags associated with the resource.
configurationItem
    :: ConfigurationItem
configurationItem =
    ConfigurationItem'
    { _ciResourceId = Nothing
    , _ciResourceType = Nothing
    , _ciConfigurationStateId = Nothing
    , _ciArn = Nothing
    , _ciResourceName = Nothing
    , _ciResourceCreationTime = Nothing
    , _ciConfigurationItemStatus = Nothing
    , _ciConfigurationItemCaptureTime = Nothing
    , _ciAccountId = Nothing
    , _ciSupplementaryConfiguration = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciRelationships = Nothing
    , _ciVersion = Nothing
    , _ciAwsRegion = Nothing
    , _ciRelatedEvents = Nothing
    , _ciConfiguration = Nothing
    , _ciConfigurationItemMD5Hash = Nothing
    , _ciTags = Nothing
    }

-- | The ID of the resource (for example., @sg-xxxxxx@ ).
ciResourceId :: Lens' ConfigurationItem (Maybe Text)
ciResourceId = lens _ciResourceId (\ s a -> s{_ciResourceId = a});

-- | The type of AWS resource.
ciResourceType :: Lens' ConfigurationItem (Maybe ResourceType)
ciResourceType = lens _ciResourceType (\ s a -> s{_ciResourceType = a});

-- | An identifier that indicates the ordering of the configuration items of a resource.
ciConfigurationStateId :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationStateId = lens _ciConfigurationStateId (\ s a -> s{_ciConfigurationStateId = a});

-- | The Amazon Resource Name (ARN) of the resource.
ciArn :: Lens' ConfigurationItem (Maybe Text)
ciArn = lens _ciArn (\ s a -> s{_ciArn = a});

-- | The custom name of the resource, if available.
ciResourceName :: Lens' ConfigurationItem (Maybe Text)
ciResourceName = lens _ciResourceName (\ s a -> s{_ciResourceName = a});

-- | The time stamp when the resource was created.
ciResourceCreationTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciResourceCreationTime = lens _ciResourceCreationTime (\ s a -> s{_ciResourceCreationTime = a}) . mapping _Time;

-- | The configuration item status.
ciConfigurationItemStatus :: Lens' ConfigurationItem (Maybe ConfigurationItemStatus)
ciConfigurationItemStatus = lens _ciConfigurationItemStatus (\ s a -> s{_ciConfigurationItemStatus = a});

-- | The time when the configuration recording was initiated.
ciConfigurationItemCaptureTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciConfigurationItemCaptureTime = lens _ciConfigurationItemCaptureTime (\ s a -> s{_ciConfigurationItemCaptureTime = a}) . mapping _Time;

-- | The 12 digit AWS account ID associated with the resource.
ciAccountId :: Lens' ConfigurationItem (Maybe Text)
ciAccountId = lens _ciAccountId (\ s a -> s{_ciAccountId = a});

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
ciSupplementaryConfiguration :: Lens' ConfigurationItem (HashMap Text Text)
ciSupplementaryConfiguration = lens _ciSupplementaryConfiguration (\ s a -> s{_ciSupplementaryConfiguration = a}) . _Default . _Map;

-- | The Availability Zone associated with the resource.
ciAvailabilityZone :: Lens' ConfigurationItem (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\ s a -> s{_ciAvailabilityZone = a});

-- | A list of related AWS resources.
ciRelationships :: Lens' ConfigurationItem [Relationship]
ciRelationships = lens _ciRelationships (\ s a -> s{_ciRelationships = a}) . _Default . _Coerce;

-- | The version number of the resource configuration.
ciVersion :: Lens' ConfigurationItem (Maybe Text)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a});

-- | The region where the resource resides.
ciAwsRegion :: Lens' ConfigurationItem (Maybe Text)
ciAwsRegion = lens _ciAwsRegion (\ s a -> s{_ciAwsRegion = a});

-- | A list of CloudTrail event IDs. A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What is AWS CloudTrail?> . An empty field indicates that the current configuration was not initiated by any event.
ciRelatedEvents :: Lens' ConfigurationItem [Text]
ciRelatedEvents = lens _ciRelatedEvents (\ s a -> s{_ciRelatedEvents = a}) . _Default . _Coerce;

-- | The description of the resource configuration.
ciConfiguration :: Lens' ConfigurationItem (Maybe Text)
ciConfiguration = lens _ciConfiguration (\ s a -> s{_ciConfiguration = a});

-- | Unique MD5 hash that represents the configuration item's state. You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
ciConfigurationItemMD5Hash :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationItemMD5Hash = lens _ciConfigurationItemMD5Hash (\ s a -> s{_ciConfigurationItemMD5Hash = a});

-- | A mapping of key value tags associated with the resource.
ciTags :: Lens' ConfigurationItem (HashMap Text Text)
ciTags = lens _ciTags (\ s a -> s{_ciTags = a}) . _Default . _Map;

instance FromJSON ConfigurationItem where
        parseJSON
          = withObject "ConfigurationItem"
              (\ x ->
                 ConfigurationItem' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "configurationStateId")
                     <*> (x .:? "arn")
                     <*> (x .:? "resourceName")
                     <*> (x .:? "resourceCreationTime")
                     <*> (x .:? "configurationItemStatus")
                     <*> (x .:? "configurationItemCaptureTime")
                     <*> (x .:? "accountId")
                     <*> (x .:? "supplementaryConfiguration" .!= mempty)
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "relationships" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "awsRegion")
                     <*> (x .:? "relatedEvents" .!= mempty)
                     <*> (x .:? "configuration")
                     <*> (x .:? "configurationItemMD5Hash")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable ConfigurationItem

instance NFData ConfigurationItem

-- | An object that represents the recording of configuration changes of an AWS resource.
--
--
--
-- /See:/ 'configurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
    { _crName           :: !(Maybe Text)
    , _crRecordingGroup :: !(Maybe RecordingGroup)
    , _crRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crName' - The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
--
-- * 'crRecordingGroup' - Specifies the types of AWS resource for which AWS Config records configuration changes.
--
-- * 'crRoleARN' - Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
configurationRecorder
    :: ConfigurationRecorder
configurationRecorder =
    ConfigurationRecorder'
    { _crName = Nothing
    , _crRecordingGroup = Nothing
    , _crRoleARN = Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
crName :: Lens' ConfigurationRecorder (Maybe Text)
crName = lens _crName (\ s a -> s{_crName = a});

-- | Specifies the types of AWS resource for which AWS Config records configuration changes.
crRecordingGroup :: Lens' ConfigurationRecorder (Maybe RecordingGroup)
crRecordingGroup = lens _crRecordingGroup (\ s a -> s{_crRecordingGroup = a});

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
crRoleARN :: Lens' ConfigurationRecorder (Maybe Text)
crRoleARN = lens _crRoleARN (\ s a -> s{_crRoleARN = a});

instance FromJSON ConfigurationRecorder where
        parseJSON
          = withObject "ConfigurationRecorder"
              (\ x ->
                 ConfigurationRecorder' <$>
                   (x .:? "name") <*> (x .:? "recordingGroup") <*>
                     (x .:? "roleARN"))

instance Hashable ConfigurationRecorder

instance NFData ConfigurationRecorder

instance ToJSON ConfigurationRecorder where
        toJSON ConfigurationRecorder'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _crName,
                  ("recordingGroup" .=) <$> _crRecordingGroup,
                  ("roleARN" .=) <$> _crRoleARN])

-- | The current status of the configuration recorder.
--
--
--
-- /See:/ 'configurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
    { _crsLastErrorCode        :: !(Maybe Text)
    , _crsLastStopTime         :: !(Maybe POSIX)
    , _crsLastStatusChangeTime :: !(Maybe POSIX)
    , _crsRecording            :: !(Maybe Bool)
    , _crsLastStatus           :: !(Maybe RecorderStatus)
    , _crsLastErrorMessage     :: !(Maybe Text)
    , _crsName                 :: !(Maybe Text)
    , _crsLastStartTime        :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationRecorderStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsLastErrorCode' - The error code indicating that the recording failed.
--
-- * 'crsLastStopTime' - The time the recorder was last stopped.
--
-- * 'crsLastStatusChangeTime' - The time when the status was last changed.
--
-- * 'crsRecording' - Specifies whether the recorder is currently recording or not.
--
-- * 'crsLastStatus' - The last (previous) status of the recorder.
--
-- * 'crsLastErrorMessage' - The message indicating that the recording failed due to an error.
--
-- * 'crsName' - The name of the configuration recorder.
--
-- * 'crsLastStartTime' - The time the recorder was last started.
configurationRecorderStatus
    :: ConfigurationRecorderStatus
configurationRecorderStatus =
    ConfigurationRecorderStatus'
    { _crsLastErrorCode = Nothing
    , _crsLastStopTime = Nothing
    , _crsLastStatusChangeTime = Nothing
    , _crsRecording = Nothing
    , _crsLastStatus = Nothing
    , _crsLastErrorMessage = Nothing
    , _crsName = Nothing
    , _crsLastStartTime = Nothing
    }

-- | The error code indicating that the recording failed.
crsLastErrorCode :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorCode = lens _crsLastErrorCode (\ s a -> s{_crsLastErrorCode = a});

-- | The time the recorder was last stopped.
crsLastStopTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStopTime = lens _crsLastStopTime (\ s a -> s{_crsLastStopTime = a}) . mapping _Time;

-- | The time when the status was last changed.
crsLastStatusChangeTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStatusChangeTime = lens _crsLastStatusChangeTime (\ s a -> s{_crsLastStatusChangeTime = a}) . mapping _Time;

-- | Specifies whether the recorder is currently recording or not.
crsRecording :: Lens' ConfigurationRecorderStatus (Maybe Bool)
crsRecording = lens _crsRecording (\ s a -> s{_crsRecording = a});

-- | The last (previous) status of the recorder.
crsLastStatus :: Lens' ConfigurationRecorderStatus (Maybe RecorderStatus)
crsLastStatus = lens _crsLastStatus (\ s a -> s{_crsLastStatus = a});

-- | The message indicating that the recording failed due to an error.
crsLastErrorMessage :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorMessage = lens _crsLastErrorMessage (\ s a -> s{_crsLastErrorMessage = a});

-- | The name of the configuration recorder.
crsName :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsName = lens _crsName (\ s a -> s{_crsName = a});

-- | The time the recorder was last started.
crsLastStartTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStartTime = lens _crsLastStartTime (\ s a -> s{_crsLastStartTime = a}) . mapping _Time;

instance FromJSON ConfigurationRecorderStatus where
        parseJSON
          = withObject "ConfigurationRecorderStatus"
              (\ x ->
                 ConfigurationRecorderStatus' <$>
                   (x .:? "lastErrorCode") <*> (x .:? "lastStopTime")
                     <*> (x .:? "lastStatusChangeTime")
                     <*> (x .:? "recording")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage")
                     <*> (x .:? "name")
                     <*> (x .:? "lastStartTime"))

instance Hashable ConfigurationRecorderStatus

instance NFData ConfigurationRecorderStatus

-- | The channel through which AWS Config delivers notifications and updated configuration states.
--
--
--
-- /See:/ 'deliveryChannel' smart constructor.
data DeliveryChannel = DeliveryChannel'
    { _dcS3KeyPrefix                      :: !(Maybe Text)
    , _dcSnsTopicARN                      :: !(Maybe Text)
    , _dcName                             :: !(Maybe Text)
    , _dcConfigSnapshotDeliveryProperties :: !(Maybe ConfigSnapshotDeliveryProperties)
    , _dcS3BucketName                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcS3KeyPrefix' - The prefix for the specified Amazon S3 bucket.
--
-- * 'dcSnsTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes. If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <http://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
--
-- * 'dcName' - The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
--
-- * 'dcConfigSnapshotDeliveryProperties' - The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
--
-- * 'dcS3BucketName' - The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files. If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <http://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
deliveryChannel
    :: DeliveryChannel
deliveryChannel =
    DeliveryChannel'
    { _dcS3KeyPrefix = Nothing
    , _dcSnsTopicARN = Nothing
    , _dcName = Nothing
    , _dcConfigSnapshotDeliveryProperties = Nothing
    , _dcS3BucketName = Nothing
    }

-- | The prefix for the specified Amazon S3 bucket.
dcS3KeyPrefix :: Lens' DeliveryChannel (Maybe Text)
dcS3KeyPrefix = lens _dcS3KeyPrefix (\ s a -> s{_dcS3KeyPrefix = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which AWS Config sends notifications about configuration changes. If you choose a topic from another account, the topic must have policies that grant access permissions to AWS Config. For more information, see <http://docs.aws.amazon.com/config/latest/developerguide/sns-topic-policy.html Permissions for the Amazon SNS Topic> in the AWS Config Developer Guide.
dcSnsTopicARN :: Lens' DeliveryChannel (Maybe Text)
dcSnsTopicARN = lens _dcSnsTopicARN (\ s a -> s{_dcSnsTopicARN = a});

-- | The name of the delivery channel. By default, AWS Config assigns the name "default" when creating the delivery channel. To change the delivery channel name, you must use the DeleteDeliveryChannel action to delete your current delivery channel, and then you must use the PutDeliveryChannel command to create a delivery channel that has the desired name.
dcName :: Lens' DeliveryChannel (Maybe Text)
dcName = lens _dcName (\ s a -> s{_dcName = a});

-- | The options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket.
dcConfigSnapshotDeliveryProperties :: Lens' DeliveryChannel (Maybe ConfigSnapshotDeliveryProperties)
dcConfigSnapshotDeliveryProperties = lens _dcConfigSnapshotDeliveryProperties (\ s a -> s{_dcConfigSnapshotDeliveryProperties = a});

-- | The name of the Amazon S3 bucket to which AWS Config delivers configuration snapshots and configuration history files. If you specify a bucket that belongs to another AWS account, that bucket must have policies that grant access permissions to AWS Config. For more information, see <http://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html Permissions for the Amazon S3 Bucket> in the AWS Config Developer Guide.
dcS3BucketName :: Lens' DeliveryChannel (Maybe Text)
dcS3BucketName = lens _dcS3BucketName (\ s a -> s{_dcS3BucketName = a});

instance FromJSON DeliveryChannel where
        parseJSON
          = withObject "DeliveryChannel"
              (\ x ->
                 DeliveryChannel' <$>
                   (x .:? "s3KeyPrefix") <*> (x .:? "snsTopicARN") <*>
                     (x .:? "name")
                     <*> (x .:? "configSnapshotDeliveryProperties")
                     <*> (x .:? "s3BucketName"))

instance Hashable DeliveryChannel

instance NFData DeliveryChannel

instance ToJSON DeliveryChannel where
        toJSON DeliveryChannel'{..}
          = object
              (catMaybes
                 [("s3KeyPrefix" .=) <$> _dcS3KeyPrefix,
                  ("snsTopicARN" .=) <$> _dcSnsTopicARN,
                  ("name" .=) <$> _dcName,
                  ("configSnapshotDeliveryProperties" .=) <$>
                    _dcConfigSnapshotDeliveryProperties,
                  ("s3BucketName" .=) <$> _dcS3BucketName])

-- | The status of a specified delivery channel.
--
--
-- Valid values: @Success@ | @Failure@
--
--
-- /See:/ 'deliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
    { _dcsConfigSnapshotDeliveryInfo :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsConfigStreamDeliveryInfo   :: !(Maybe ConfigStreamDeliveryInfo)
    , _dcsConfigHistoryDeliveryInfo  :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsName                       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryChannelStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsConfigSnapshotDeliveryInfo' - A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
--
-- * 'dcsConfigStreamDeliveryInfo' - A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
--
-- * 'dcsConfigHistoryDeliveryInfo' - A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
--
-- * 'dcsName' - The name of the delivery channel.
deliveryChannelStatus
    :: DeliveryChannelStatus
deliveryChannelStatus =
    DeliveryChannelStatus'
    { _dcsConfigSnapshotDeliveryInfo = Nothing
    , _dcsConfigStreamDeliveryInfo = Nothing
    , _dcsConfigHistoryDeliveryInfo = Nothing
    , _dcsName = Nothing
    }

-- | A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
dcsConfigSnapshotDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = lens _dcsConfigSnapshotDeliveryInfo (\ s a -> s{_dcsConfigSnapshotDeliveryInfo = a});

-- | A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
dcsConfigStreamDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = lens _dcsConfigStreamDeliveryInfo (\ s a -> s{_dcsConfigStreamDeliveryInfo = a});

-- | A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
dcsConfigHistoryDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo = lens _dcsConfigHistoryDeliveryInfo (\ s a -> s{_dcsConfigHistoryDeliveryInfo = a});

-- | The name of the delivery channel.
dcsName :: Lens' DeliveryChannelStatus (Maybe Text)
dcsName = lens _dcsName (\ s a -> s{_dcsName = a});

instance FromJSON DeliveryChannelStatus where
        parseJSON
          = withObject "DeliveryChannelStatus"
              (\ x ->
                 DeliveryChannelStatus' <$>
                   (x .:? "configSnapshotDeliveryInfo") <*>
                     (x .:? "configStreamDeliveryInfo")
                     <*> (x .:? "configHistoryDeliveryInfo")
                     <*> (x .:? "name"))

instance Hashable DeliveryChannelStatus

instance NFData DeliveryChannelStatus

-- | Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.
--
--
--
-- /See:/ 'evaluation' smart constructor.
data Evaluation = Evaluation'
    { _eAnnotation             :: !(Maybe Text)
    , _eComplianceResourceType :: !Text
    , _eComplianceResourceId   :: !Text
    , _eComplianceType         :: !ComplianceType
    , _eOrderingTimestamp      :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAnnotation' - Supplementary information about how the evaluation determined the compliance.
--
-- * 'eComplianceResourceType' - The type of AWS resource that was evaluated.
--
-- * 'eComplianceResourceId' - The ID of the AWS resource that was evaluated.
--
-- * 'eComplianceType' - Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against. For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type. Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
--
-- * 'eOrderingTimestamp' - The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
evaluation
    :: Text -- ^ 'eComplianceResourceType'
    -> Text -- ^ 'eComplianceResourceId'
    -> ComplianceType -- ^ 'eComplianceType'
    -> UTCTime -- ^ 'eOrderingTimestamp'
    -> Evaluation
evaluation pComplianceResourceType_ pComplianceResourceId_ pComplianceType_ pOrderingTimestamp_ =
    Evaluation'
    { _eAnnotation = Nothing
    , _eComplianceResourceType = pComplianceResourceType_
    , _eComplianceResourceId = pComplianceResourceId_
    , _eComplianceType = pComplianceType_
    , _eOrderingTimestamp = _Time # pOrderingTimestamp_
    }

-- | Supplementary information about how the evaluation determined the compliance.
eAnnotation :: Lens' Evaluation (Maybe Text)
eAnnotation = lens _eAnnotation (\ s a -> s{_eAnnotation = a});

-- | The type of AWS resource that was evaluated.
eComplianceResourceType :: Lens' Evaluation Text
eComplianceResourceType = lens _eComplianceResourceType (\ s a -> s{_eComplianceResourceType = a});

-- | The ID of the AWS resource that was evaluated.
eComplianceResourceId :: Lens' Evaluation Text
eComplianceResourceId = lens _eComplianceResourceId (\ s a -> s{_eComplianceResourceId = a});

-- | Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against. For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type. Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
eComplianceType :: Lens' Evaluation ComplianceType
eComplianceType = lens _eComplianceType (\ s a -> s{_eComplianceType = a});

-- | The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
eOrderingTimestamp :: Lens' Evaluation UTCTime
eOrderingTimestamp = lens _eOrderingTimestamp (\ s a -> s{_eOrderingTimestamp = a}) . _Time;

instance FromJSON Evaluation where
        parseJSON
          = withObject "Evaluation"
              (\ x ->
                 Evaluation' <$>
                   (x .:? "Annotation") <*>
                     (x .: "ComplianceResourceType")
                     <*> (x .: "ComplianceResourceId")
                     <*> (x .: "ComplianceType")
                     <*> (x .: "OrderingTimestamp"))

instance Hashable Evaluation

instance NFData Evaluation

instance ToJSON Evaluation where
        toJSON Evaluation'{..}
          = object
              (catMaybes
                 [("Annotation" .=) <$> _eAnnotation,
                  Just
                    ("ComplianceResourceType" .=
                       _eComplianceResourceType),
                  Just
                    ("ComplianceResourceId" .= _eComplianceResourceId),
                  Just ("ComplianceType" .= _eComplianceType),
                  Just ("OrderingTimestamp" .= _eOrderingTimestamp)])

-- | The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related timestamps, and supplementary information.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
    { _erEvaluationResultIdentifier :: !(Maybe EvaluationResultIdentifier)
    , _erAnnotation                 :: !(Maybe Text)
    , _erConfigRuleInvokedTime      :: !(Maybe POSIX)
    , _erResultRecordedTime         :: !(Maybe POSIX)
    , _erResultToken                :: !(Maybe Text)
    , _erComplianceType             :: !(Maybe ComplianceType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erEvaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- * 'erAnnotation' - Supplementary information about how the evaluation determined the compliance.
--
-- * 'erConfigRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
--
-- * 'erResultRecordedTime' - The time when AWS Config recorded the evaluation result.
--
-- * 'erResultToken' - An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
--
-- * 'erComplianceType' - Indicates whether the AWS resource complies with the AWS Config rule that evaluated it. For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
evaluationResult
    :: EvaluationResult
evaluationResult =
    EvaluationResult'
    { _erEvaluationResultIdentifier = Nothing
    , _erAnnotation = Nothing
    , _erConfigRuleInvokedTime = Nothing
    , _erResultRecordedTime = Nothing
    , _erResultToken = Nothing
    , _erComplianceType = Nothing
    }

-- | Uniquely identifies the evaluation result.
erEvaluationResultIdentifier :: Lens' EvaluationResult (Maybe EvaluationResultIdentifier)
erEvaluationResultIdentifier = lens _erEvaluationResultIdentifier (\ s a -> s{_erEvaluationResultIdentifier = a});

-- | Supplementary information about how the evaluation determined the compliance.
erAnnotation :: Lens' EvaluationResult (Maybe Text)
erAnnotation = lens _erAnnotation (\ s a -> s{_erAnnotation = a});

-- | The time when the AWS Config rule evaluated the AWS resource.
erConfigRuleInvokedTime :: Lens' EvaluationResult (Maybe UTCTime)
erConfigRuleInvokedTime = lens _erConfigRuleInvokedTime (\ s a -> s{_erConfigRuleInvokedTime = a}) . mapping _Time;

-- | The time when AWS Config recorded the evaluation result.
erResultRecordedTime :: Lens' EvaluationResult (Maybe UTCTime)
erResultRecordedTime = lens _erResultRecordedTime (\ s a -> s{_erResultRecordedTime = a}) . mapping _Time;

-- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
erResultToken :: Lens' EvaluationResult (Maybe Text)
erResultToken = lens _erResultToken (\ s a -> s{_erResultToken = a});

-- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it. For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
erComplianceType :: Lens' EvaluationResult (Maybe ComplianceType)
erComplianceType = lens _erComplianceType (\ s a -> s{_erComplianceType = a});

instance FromJSON EvaluationResult where
        parseJSON
          = withObject "EvaluationResult"
              (\ x ->
                 EvaluationResult' <$>
                   (x .:? "EvaluationResultIdentifier") <*>
                     (x .:? "Annotation")
                     <*> (x .:? "ConfigRuleInvokedTime")
                     <*> (x .:? "ResultRecordedTime")
                     <*> (x .:? "ResultToken")
                     <*> (x .:? "ComplianceType"))

instance Hashable EvaluationResult

instance NFData EvaluationResult

-- | Uniquely identifies an evaluation result.
--
--
--
-- /See:/ 'evaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
    { _eriEvaluationResultQualifier :: !(Maybe EvaluationResultQualifier)
    , _eriOrderingTimestamp         :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EvaluationResultIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eriEvaluationResultQualifier' - Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
--
-- * 'eriOrderingTimestamp' - The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
evaluationResultIdentifier
    :: EvaluationResultIdentifier
evaluationResultIdentifier =
    EvaluationResultIdentifier'
    { _eriEvaluationResultQualifier = Nothing
    , _eriOrderingTimestamp = Nothing
    }

-- | Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
eriEvaluationResultQualifier :: Lens' EvaluationResultIdentifier (Maybe EvaluationResultQualifier)
eriEvaluationResultQualifier = lens _eriEvaluationResultQualifier (\ s a -> s{_eriEvaluationResultQualifier = a});

-- | The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
eriOrderingTimestamp :: Lens' EvaluationResultIdentifier (Maybe UTCTime)
eriOrderingTimestamp = lens _eriOrderingTimestamp (\ s a -> s{_eriOrderingTimestamp = a}) . mapping _Time;

instance FromJSON EvaluationResultIdentifier where
        parseJSON
          = withObject "EvaluationResultIdentifier"
              (\ x ->
                 EvaluationResultIdentifier' <$>
                   (x .:? "EvaluationResultQualifier") <*>
                     (x .:? "OrderingTimestamp"))

instance Hashable EvaluationResultIdentifier

instance NFData EvaluationResultIdentifier

-- | Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.
--
--
--
-- /See:/ 'evaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
    { _erqResourceId     :: !(Maybe Text)
    , _erqResourceType   :: !(Maybe Text)
    , _erqConfigRuleName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EvaluationResultQualifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erqResourceId' - The ID of the evaluated AWS resource.
--
-- * 'erqResourceType' - The type of AWS resource that was evaluated.
--
-- * 'erqConfigRuleName' - The name of the AWS Config rule that was used in the evaluation.
evaluationResultQualifier
    :: EvaluationResultQualifier
evaluationResultQualifier =
    EvaluationResultQualifier'
    { _erqResourceId = Nothing
    , _erqResourceType = Nothing
    , _erqConfigRuleName = Nothing
    }

-- | The ID of the evaluated AWS resource.
erqResourceId :: Lens' EvaluationResultQualifier (Maybe Text)
erqResourceId = lens _erqResourceId (\ s a -> s{_erqResourceId = a});

-- | The type of AWS resource that was evaluated.
erqResourceType :: Lens' EvaluationResultQualifier (Maybe Text)
erqResourceType = lens _erqResourceType (\ s a -> s{_erqResourceType = a});

-- | The name of the AWS Config rule that was used in the evaluation.
erqConfigRuleName :: Lens' EvaluationResultQualifier (Maybe Text)
erqConfigRuleName = lens _erqConfigRuleName (\ s a -> s{_erqConfigRuleName = a});

instance FromJSON EvaluationResultQualifier where
        parseJSON
          = withObject "EvaluationResultQualifier"
              (\ x ->
                 EvaluationResultQualifier' <$>
                   (x .:? "ResourceId") <*> (x .:? "ResourceType") <*>
                     (x .:? "ConfigRuleName"))

instance Hashable EvaluationResultQualifier

instance NFData EvaluationResultQualifier

-- | Specifies the types of AWS resource for which AWS Config records configuration changes.
--
--
-- In the recording group, you specify whether all supported types or specific types of resources are recorded.
--
-- By default, AWS Config records configuration changes for all supported types of regional resources that AWS Config discovers in the region in which it is running. Regional resources are tied to a region and can be used only in that region. Examples of regional resources are EC2 instances and EBS volumes.
--
-- You can also have AWS Config record configuration changes for supported types of global resources (for example, IAM resources). Global resources are not tied to an individual region and can be used in all regions.
--
-- /Important:/ The configuration details for any global resource are the same in all regions. If you customize AWS Config in multiple regions to record global resources, it will create multiple configuration items each time a global resource changes: one configuration item for each region. These configuration items will contain identical data. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources, unless you want the configuration items to be available in multiple regions.
--
-- If you don't want AWS Config to record all resources, you can specify which types of resources it will record with the @resourceTypes@ parameter.
--
-- For a list of supported resource types, see <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported resource types> .
--
-- For more information, see <http://docs.aws.amazon.com/config/latest/developerguide/select-resources.html Selecting Which Resources AWS Config Records> .
--
--
-- /See:/ 'recordingGroup' smart constructor.
data RecordingGroup = RecordingGroup'
    { _rgAllSupported               :: !(Maybe Bool)
    , _rgIncludeGlobalResourceTypes :: !(Maybe Bool)
    , _rgResourceTypes              :: !(Maybe [ResourceType])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecordingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgAllSupported' - Specifies whether AWS Config records configuration changes for every supported type of regional resource. If you set this option to @true@ , when AWS Config adds support for a new type of regional resource, it automatically starts recording resources of that type. If you set this option to @true@ , you cannot enumerate a list of @resourceTypes@ .
--
-- * 'rgIncludeGlobalResourceTypes' - Specifies whether AWS Config includes all supported types of global resources (for example, IAM resources) with the resources that it records. Before you can set this option to @true@ , you must set the @allSupported@ option to @true@ . If you set this option to @true@ , when AWS Config adds support for a new type of global resource, it automatically starts recording resources of that type. The configuration details for any global resource are the same in all regions. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources.
--
-- * 'rgResourceTypes' - A comma-separated list that specifies the types of AWS resources for which AWS Config records configuration changes (for example, @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@ ). Before you can set this option to @true@ , you must set the @allSupported@ option to @false@ . If you set this option to @true@ , when AWS Config adds support for a new type of resource, it will not record resources of that type unless you manually add that type to your recording group. For a list of valid @resourceTypes@ values, see the __resourceType Value__ column in <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types> .
recordingGroup
    :: RecordingGroup
recordingGroup =
    RecordingGroup'
    { _rgAllSupported = Nothing
    , _rgIncludeGlobalResourceTypes = Nothing
    , _rgResourceTypes = Nothing
    }

-- | Specifies whether AWS Config records configuration changes for every supported type of regional resource. If you set this option to @true@ , when AWS Config adds support for a new type of regional resource, it automatically starts recording resources of that type. If you set this option to @true@ , you cannot enumerate a list of @resourceTypes@ .
rgAllSupported :: Lens' RecordingGroup (Maybe Bool)
rgAllSupported = lens _rgAllSupported (\ s a -> s{_rgAllSupported = a});

-- | Specifies whether AWS Config includes all supported types of global resources (for example, IAM resources) with the resources that it records. Before you can set this option to @true@ , you must set the @allSupported@ option to @true@ . If you set this option to @true@ , when AWS Config adds support for a new type of global resource, it automatically starts recording resources of that type. The configuration details for any global resource are the same in all regions. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources.
rgIncludeGlobalResourceTypes :: Lens' RecordingGroup (Maybe Bool)
rgIncludeGlobalResourceTypes = lens _rgIncludeGlobalResourceTypes (\ s a -> s{_rgIncludeGlobalResourceTypes = a});

-- | A comma-separated list that specifies the types of AWS resources for which AWS Config records configuration changes (for example, @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@ ). Before you can set this option to @true@ , you must set the @allSupported@ option to @false@ . If you set this option to @true@ , when AWS Config adds support for a new type of resource, it will not record resources of that type unless you manually add that type to your recording group. For a list of valid @resourceTypes@ values, see the __resourceType Value__ column in <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types> .
rgResourceTypes :: Lens' RecordingGroup [ResourceType]
rgResourceTypes = lens _rgResourceTypes (\ s a -> s{_rgResourceTypes = a}) . _Default . _Coerce;

instance FromJSON RecordingGroup where
        parseJSON
          = withObject "RecordingGroup"
              (\ x ->
                 RecordingGroup' <$>
                   (x .:? "allSupported") <*>
                     (x .:? "includeGlobalResourceTypes")
                     <*> (x .:? "resourceTypes" .!= mempty))

instance Hashable RecordingGroup

instance NFData RecordingGroup

instance ToJSON RecordingGroup where
        toJSON RecordingGroup'{..}
          = object
              (catMaybes
                 [("allSupported" .=) <$> _rgAllSupported,
                  ("includeGlobalResourceTypes" .=) <$>
                    _rgIncludeGlobalResourceTypes,
                  ("resourceTypes" .=) <$> _rgResourceTypes])

-- | The relationship of the related resource to the main resource.
--
--
--
-- /See:/ 'relationship' smart constructor.
data Relationship = Relationship'
    { _rResourceId       :: !(Maybe Text)
    , _rResourceType     :: !(Maybe ResourceType)
    , _rResourceName     :: !(Maybe Text)
    , _rRelationshipName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId' - The ID of the related resource (for example, @sg-xxxxxx@ ).
--
-- * 'rResourceType' - The resource type of the related resource.
--
-- * 'rResourceName' - The custom name of the related resource, if available.
--
-- * 'rRelationshipName' - The type of relationship with the related resource.
relationship
    :: Relationship
relationship =
    Relationship'
    { _rResourceId = Nothing
    , _rResourceType = Nothing
    , _rResourceName = Nothing
    , _rRelationshipName = Nothing
    }

-- | The ID of the related resource (for example, @sg-xxxxxx@ ).
rResourceId :: Lens' Relationship (Maybe Text)
rResourceId = lens _rResourceId (\ s a -> s{_rResourceId = a});

-- | The resource type of the related resource.
rResourceType :: Lens' Relationship (Maybe ResourceType)
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a});

-- | The custom name of the related resource, if available.
rResourceName :: Lens' Relationship (Maybe Text)
rResourceName = lens _rResourceName (\ s a -> s{_rResourceName = a});

-- | The type of relationship with the related resource.
rRelationshipName :: Lens' Relationship (Maybe Text)
rRelationshipName = lens _rRelationshipName (\ s a -> s{_rRelationshipName = a});

instance FromJSON Relationship where
        parseJSON
          = withObject "Relationship"
              (\ x ->
                 Relationship' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "resourceName")
                     <*> (x .:? "relationshipName"))

instance Hashable Relationship

instance NFData Relationship

-- | An object that contains the resource type and the number of resources.
--
--
--
-- /See:/ 'resourceCount' smart constructor.
data ResourceCount = ResourceCount'
    { _rcResourceType :: !(Maybe ResourceType)
    , _rcCount        :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcResourceType' - The resource type, for example @"AWS::EC2::Instance"@ .
--
-- * 'rcCount' - The number of resources.
resourceCount
    :: ResourceCount
resourceCount =
    ResourceCount'
    { _rcResourceType = Nothing
    , _rcCount = Nothing
    }

-- | The resource type, for example @"AWS::EC2::Instance"@ .
rcResourceType :: Lens' ResourceCount (Maybe ResourceType)
rcResourceType = lens _rcResourceType (\ s a -> s{_rcResourceType = a});

-- | The number of resources.
rcCount :: Lens' ResourceCount (Maybe Integer)
rcCount = lens _rcCount (\ s a -> s{_rcCount = a});

instance FromJSON ResourceCount where
        parseJSON
          = withObject "ResourceCount"
              (\ x ->
                 ResourceCount' <$>
                   (x .:? "resourceType") <*> (x .:? "count"))

instance Hashable ResourceCount

instance NFData ResourceCount

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
--
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
    { _riResourceId           :: !(Maybe Text)
    , _riResourceType         :: !(Maybe ResourceType)
    , _riResourceName         :: !(Maybe Text)
    , _riResourceDeletionTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceId' - The ID of the resource (for example., @sg-xxxxxx@ ).
--
-- * 'riResourceType' - The type of resource.
--
-- * 'riResourceName' - The custom name of the resource (if available).
--
-- * 'riResourceDeletionTime' - The time that the resource was deleted.
resourceIdentifier
    :: ResourceIdentifier
resourceIdentifier =
    ResourceIdentifier'
    { _riResourceId = Nothing
    , _riResourceType = Nothing
    , _riResourceName = Nothing
    , _riResourceDeletionTime = Nothing
    }

-- | The ID of the resource (for example., @sg-xxxxxx@ ).
riResourceId :: Lens' ResourceIdentifier (Maybe Text)
riResourceId = lens _riResourceId (\ s a -> s{_riResourceId = a});

-- | The type of resource.
riResourceType :: Lens' ResourceIdentifier (Maybe ResourceType)
riResourceType = lens _riResourceType (\ s a -> s{_riResourceType = a});

-- | The custom name of the resource (if available).
riResourceName :: Lens' ResourceIdentifier (Maybe Text)
riResourceName = lens _riResourceName (\ s a -> s{_riResourceName = a});

-- | The time that the resource was deleted.
riResourceDeletionTime :: Lens' ResourceIdentifier (Maybe UTCTime)
riResourceDeletionTime = lens _riResourceDeletionTime (\ s a -> s{_riResourceDeletionTime = a}) . mapping _Time;

instance FromJSON ResourceIdentifier where
        parseJSON
          = withObject "ResourceIdentifier"
              (\ x ->
                 ResourceIdentifier' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "resourceName")
                     <*> (x .:? "resourceDeletionTime"))

instance Hashable ResourceIdentifier

instance NFData ResourceIdentifier

-- | Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.
--
--
--
-- /See:/ 'scope' smart constructor.
data Scope = Scope'
    { _sComplianceResourceTypes :: !(Maybe [Text])
    , _sComplianceResourceId    :: !(Maybe Text)
    , _sTagValue                :: !(Maybe Text)
    , _sTagKey                  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Scope' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sComplianceResourceTypes' - The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
--
-- * 'sComplianceResourceId' - The IDs of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
--
-- * 'sTagValue' - The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
--
-- * 'sTagKey' - The tag key that is applied to only those AWS resources that you want you want to trigger an evaluation for the rule.
scope
    :: Scope
scope =
    Scope'
    { _sComplianceResourceTypes = Nothing
    , _sComplianceResourceId = Nothing
    , _sTagValue = Nothing
    , _sTagKey = Nothing
    }

-- | The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
sComplianceResourceTypes :: Lens' Scope [Text]
sComplianceResourceTypes = lens _sComplianceResourceTypes (\ s a -> s{_sComplianceResourceTypes = a}) . _Default . _Coerce;

-- | The IDs of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
sComplianceResourceId :: Lens' Scope (Maybe Text)
sComplianceResourceId = lens _sComplianceResourceId (\ s a -> s{_sComplianceResourceId = a});

-- | The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
sTagValue :: Lens' Scope (Maybe Text)
sTagValue = lens _sTagValue (\ s a -> s{_sTagValue = a});

-- | The tag key that is applied to only those AWS resources that you want you want to trigger an evaluation for the rule.
sTagKey :: Lens' Scope (Maybe Text)
sTagKey = lens _sTagKey (\ s a -> s{_sTagKey = a});

instance FromJSON Scope where
        parseJSON
          = withObject "Scope"
              (\ x ->
                 Scope' <$>
                   (x .:? "ComplianceResourceTypes" .!= mempty) <*>
                     (x .:? "ComplianceResourceId")
                     <*> (x .:? "TagValue")
                     <*> (x .:? "TagKey"))

instance Hashable Scope

instance NFData Scope

instance ToJSON Scope where
        toJSON Scope'{..}
          = object
              (catMaybes
                 [("ComplianceResourceTypes" .=) <$>
                    _sComplianceResourceTypes,
                  ("ComplianceResourceId" .=) <$>
                    _sComplianceResourceId,
                  ("TagValue" .=) <$> _sTagValue,
                  ("TagKey" .=) <$> _sTagKey])

-- | Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.
--
--
--
-- /See:/ 'source' smart constructor.
data Source = Source'
    { _sSourceDetails    :: !(Maybe [SourceDetail])
    , _sOwner            :: !Owner
    , _sSourceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSourceDetails' - Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
--
-- * 'sOwner' - Indicates whether AWS or the customer owns and manages the AWS Config rule.
--
-- * 'sSourceIdentifier' - For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> . For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
source
    :: Owner -- ^ 'sOwner'
    -> Text -- ^ 'sSourceIdentifier'
    -> Source
source pOwner_ pSourceIdentifier_ =
    Source'
    { _sSourceDetails = Nothing
    , _sOwner = pOwner_
    , _sSourceIdentifier = pSourceIdentifier_
    }

-- | Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
sSourceDetails :: Lens' Source [SourceDetail]
sSourceDetails = lens _sSourceDetails (\ s a -> s{_sSourceDetails = a}) . _Default . _Coerce;

-- | Indicates whether AWS or the customer owns and manages the AWS Config rule.
sOwner :: Lens' Source Owner
sOwner = lens _sOwner (\ s a -> s{_sOwner = a});

-- | For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> . For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
sSourceIdentifier :: Lens' Source Text
sSourceIdentifier = lens _sSourceIdentifier (\ s a -> s{_sSourceIdentifier = a});

instance FromJSON Source where
        parseJSON
          = withObject "Source"
              (\ x ->
                 Source' <$>
                   (x .:? "SourceDetails" .!= mempty) <*> (x .: "Owner")
                     <*> (x .: "SourceIdentifier"))

instance Hashable Source

instance NFData Source

instance ToJSON Source where
        toJSON Source'{..}
          = object
              (catMaybes
                 [("SourceDetails" .=) <$> _sSourceDetails,
                  Just ("Owner" .= _sOwner),
                  Just ("SourceIdentifier" .= _sSourceIdentifier)])

-- | Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for @SourceDetail@ only for custom rules.
--
--
--
-- /See:/ 'sourceDetail' smart constructor.
data SourceDetail = SourceDetail'
    { _sdMessageType               :: !(Maybe MessageType)
    , _sdMaximumExecutionFrequency :: !(Maybe MaximumExecutionFrequency)
    , _sdEventSource               :: !(Maybe EventSource)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdMessageType' - The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot. If you want your custom rule to be triggered by configuration changes, specify both @ConfigurationItemChangeNotification@ and @OversizedConfigurationItemChangeNotification@ .
--
-- * 'sdMaximumExecutionFrequency' - The frequency that you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
--
-- * 'sdEventSource' - The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
sourceDetail
    :: SourceDetail
sourceDetail =
    SourceDetail'
    { _sdMessageType = Nothing
    , _sdMaximumExecutionFrequency = Nothing
    , _sdEventSource = Nothing
    }

-- | The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot. If you want your custom rule to be triggered by configuration changes, specify both @ConfigurationItemChangeNotification@ and @OversizedConfigurationItemChangeNotification@ .
sdMessageType :: Lens' SourceDetail (Maybe MessageType)
sdMessageType = lens _sdMessageType (\ s a -> s{_sdMessageType = a});

-- | The frequency that you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
sdMaximumExecutionFrequency :: Lens' SourceDetail (Maybe MaximumExecutionFrequency)
sdMaximumExecutionFrequency = lens _sdMaximumExecutionFrequency (\ s a -> s{_sdMaximumExecutionFrequency = a});

-- | The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
sdEventSource :: Lens' SourceDetail (Maybe EventSource)
sdEventSource = lens _sdEventSource (\ s a -> s{_sdEventSource = a});

instance FromJSON SourceDetail where
        parseJSON
          = withObject "SourceDetail"
              (\ x ->
                 SourceDetail' <$>
                   (x .:? "MessageType") <*>
                     (x .:? "MaximumExecutionFrequency")
                     <*> (x .:? "EventSource"))

instance Hashable SourceDetail

instance NFData SourceDetail

instance ToJSON SourceDetail where
        toJSON SourceDetail'{..}
          = object
              (catMaybes
                 [("MessageType" .=) <$> _sdMessageType,
                  ("MaximumExecutionFrequency" .=) <$>
                    _sdMaximumExecutionFrequency,
                  ("EventSource" .=) <$> _sdEventSource])
