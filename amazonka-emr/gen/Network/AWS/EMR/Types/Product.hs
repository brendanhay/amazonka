{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Product where

import Network.AWS.EMR.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | With Amazon EMR release version 4.0 and later, the only accepted parameter is the application name. To pass arguments to applications, you use configuration classifications specified using configuration JSON objects. For more information, see <http://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
--
-- With earlier Amazon EMR releases, the application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument.
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
  { _aArgs           :: !(Maybe [Text])
  , _aAdditionalInfo :: !(Maybe (Map Text Text))
  , _aName           :: !(Maybe Text)
  , _aVersion        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArgs' - Arguments for Amazon EMR to pass to the application.
--
-- * 'aAdditionalInfo' - This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
--
-- * 'aName' - The name of the application.
--
-- * 'aVersion' - The version of the application.
application
    :: Application
application =
  Application'
    { _aArgs = Nothing
    , _aAdditionalInfo = Nothing
    , _aName = Nothing
    , _aVersion = Nothing
    }


-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application [Text]
aArgs = lens _aArgs (\ s a -> s{_aArgs = a}) . _Default . _Coerce

-- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (HashMap Text Text)
aAdditionalInfo = lens _aAdditionalInfo (\ s a -> s{_aAdditionalInfo = a}) . _Default . _Map

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\ s a -> s{_aVersion = a})

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "Args" .!= mempty) <*>
                     (x .:? "AdditionalInfo" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Version"))

instance Hashable Application where

instance NFData Application where

instance ToJSON Application where
        toJSON Application'{..}
          = object
              (catMaybes
                 [("Args" .=) <$> _aArgs,
                  ("AdditionalInfo" .=) <$> _aAdditionalInfo,
                  ("Name" .=) <$> _aName,
                  ("Version" .=) <$> _aVersion])

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
--
--
-- /See:/ 'autoScalingPolicy' smart constructor.
data AutoScalingPolicy = AutoScalingPolicy'
  { _aspConstraints :: !ScalingConstraints
  , _aspRules       :: ![ScalingRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspConstraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- * 'aspRules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
autoScalingPolicy
    :: ScalingConstraints -- ^ 'aspConstraints'
    -> AutoScalingPolicy
autoScalingPolicy pConstraints_ =
  AutoScalingPolicy' {_aspConstraints = pConstraints_, _aspRules = mempty}


-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspConstraints :: Lens' AutoScalingPolicy ScalingConstraints
aspConstraints = lens _aspConstraints (\ s a -> s{_aspConstraints = a})

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspRules :: Lens' AutoScalingPolicy [ScalingRule]
aspRules = lens _aspRules (\ s a -> s{_aspRules = a}) . _Coerce

instance Hashable AutoScalingPolicy where

instance NFData AutoScalingPolicy where

instance ToJSON AutoScalingPolicy where
        toJSON AutoScalingPolicy'{..}
          = object
              (catMaybes
                 [Just ("Constraints" .= _aspConstraints),
                  Just ("Rules" .= _aspRules)])

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
--
--
-- /See:/ 'autoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { _aspdStatus      :: !(Maybe AutoScalingPolicyStatus)
  , _aspdRules       :: !(Maybe [ScalingRule])
  , _aspdConstraints :: !(Maybe ScalingConstraints)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspdStatus' - The status of an automatic scaling policy.
--
-- * 'aspdRules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- * 'aspdConstraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
autoScalingPolicyDescription
    :: AutoScalingPolicyDescription
autoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    {_aspdStatus = Nothing, _aspdRules = Nothing, _aspdConstraints = Nothing}


-- | The status of an automatic scaling policy.
aspdStatus :: Lens' AutoScalingPolicyDescription (Maybe AutoScalingPolicyStatus)
aspdStatus = lens _aspdStatus (\ s a -> s{_aspdStatus = a})

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspdRules :: Lens' AutoScalingPolicyDescription [ScalingRule]
aspdRules = lens _aspdRules (\ s a -> s{_aspdRules = a}) . _Default . _Coerce

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspdConstraints :: Lens' AutoScalingPolicyDescription (Maybe ScalingConstraints)
aspdConstraints = lens _aspdConstraints (\ s a -> s{_aspdConstraints = a})

instance FromJSON AutoScalingPolicyDescription where
        parseJSON
          = withObject "AutoScalingPolicyDescription"
              (\ x ->
                 AutoScalingPolicyDescription' <$>
                   (x .:? "Status") <*> (x .:? "Rules" .!= mempty) <*>
                     (x .:? "Constraints"))

instance Hashable AutoScalingPolicyDescription where

instance NFData AutoScalingPolicyDescription where

-- | The reason for an 'AutoScalingPolicyStatus' change.
--
--
--
-- /See:/ 'autoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { _aspscrCode    :: !(Maybe AutoScalingPolicyStateChangeReasonCode)
  , _aspscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicyStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspscrCode' - The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
--
-- * 'aspscrMessage' - A friendly, more verbose message that accompanies an automatic scaling policy state change.
autoScalingPolicyStateChangeReason
    :: AutoScalingPolicyStateChangeReason
autoScalingPolicyStateChangeReason =
  AutoScalingPolicyStateChangeReason'
    {_aspscrCode = Nothing, _aspscrMessage = Nothing}


-- | The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
aspscrCode :: Lens' AutoScalingPolicyStateChangeReason (Maybe AutoScalingPolicyStateChangeReasonCode)
aspscrCode = lens _aspscrCode (\ s a -> s{_aspscrCode = a})

-- | A friendly, more verbose message that accompanies an automatic scaling policy state change.
aspscrMessage :: Lens' AutoScalingPolicyStateChangeReason (Maybe Text)
aspscrMessage = lens _aspscrMessage (\ s a -> s{_aspscrMessage = a})

instance FromJSON AutoScalingPolicyStateChangeReason
         where
        parseJSON
          = withObject "AutoScalingPolicyStateChangeReason"
              (\ x ->
                 AutoScalingPolicyStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable AutoScalingPolicyStateChangeReason
         where

instance NFData AutoScalingPolicyStateChangeReason
         where

-- | The status of an automatic scaling policy.
--
--
--
-- /See:/ 'autoScalingPolicyStatus' smart constructor.
data AutoScalingPolicyStatus = AutoScalingPolicyStatus'
  { _aspsState             :: !(Maybe AutoScalingPolicyState)
  , _aspsStateChangeReason :: !(Maybe AutoScalingPolicyStateChangeReason)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspsState' - Indicates the status of the automatic scaling policy.
--
-- * 'aspsStateChangeReason' - The reason for a change in status.
autoScalingPolicyStatus
    :: AutoScalingPolicyStatus
autoScalingPolicyStatus =
  AutoScalingPolicyStatus'
    {_aspsState = Nothing, _aspsStateChangeReason = Nothing}


-- | Indicates the status of the automatic scaling policy.
aspsState :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyState)
aspsState = lens _aspsState (\ s a -> s{_aspsState = a})

-- | The reason for a change in status.
aspsStateChangeReason :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyStateChangeReason)
aspsStateChangeReason = lens _aspsStateChangeReason (\ s a -> s{_aspsStateChangeReason = a})

instance FromJSON AutoScalingPolicyStatus where
        parseJSON
          = withObject "AutoScalingPolicyStatus"
              (\ x ->
                 AutoScalingPolicyStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason"))

instance Hashable AutoScalingPolicyStatus where

instance NFData AutoScalingPolicyStatus where

-- | Configuration of a bootstrap action.
--
--
--
-- /See:/ 'bootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { _bacName                  :: !Text
  , _bacScriptBootstrapAction :: !ScriptBootstrapActionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BootstrapActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bacName' - The name of the bootstrap action.
--
-- * 'bacScriptBootstrapAction' - The script run by the bootstrap action.
bootstrapActionConfig
    :: Text -- ^ 'bacName'
    -> ScriptBootstrapActionConfig -- ^ 'bacScriptBootstrapAction'
    -> BootstrapActionConfig
bootstrapActionConfig pName_ pScriptBootstrapAction_ =
  BootstrapActionConfig'
    {_bacName = pName_, _bacScriptBootstrapAction = pScriptBootstrapAction_}


-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig Text
bacName = lens _bacName (\ s a -> s{_bacName = a})

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction = lens _bacScriptBootstrapAction (\ s a -> s{_bacScriptBootstrapAction = a})

instance Hashable BootstrapActionConfig where

instance NFData BootstrapActionConfig where

instance ToJSON BootstrapActionConfig where
        toJSON BootstrapActionConfig'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _bacName),
                  Just
                    ("ScriptBootstrapAction" .=
                       _bacScriptBootstrapAction)])

-- | Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.
--
--
--
-- /See:/ 'cancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
  { _csiStatus :: !(Maybe CancelStepsRequestStatus)
  , _csiStepId :: !(Maybe Text)
  , _csiReason :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelStepsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiStatus' - The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
--
-- * 'csiStepId' - The encrypted StepId of a step.
--
-- * 'csiReason' - The reason for the failure if the CancelSteps request fails.
cancelStepsInfo
    :: CancelStepsInfo
cancelStepsInfo =
  CancelStepsInfo'
    {_csiStatus = Nothing, _csiStepId = Nothing, _csiReason = Nothing}


-- | The status of a CancelSteps Request. The value may be SUBMITTED or FAILED.
csiStatus :: Lens' CancelStepsInfo (Maybe CancelStepsRequestStatus)
csiStatus = lens _csiStatus (\ s a -> s{_csiStatus = a})

-- | The encrypted StepId of a step.
csiStepId :: Lens' CancelStepsInfo (Maybe Text)
csiStepId = lens _csiStepId (\ s a -> s{_csiStepId = a})

-- | The reason for the failure if the CancelSteps request fails.
csiReason :: Lens' CancelStepsInfo (Maybe Text)
csiReason = lens _csiReason (\ s a -> s{_csiReason = a})

instance FromJSON CancelStepsInfo where
        parseJSON
          = withObject "CancelStepsInfo"
              (\ x ->
                 CancelStepsInfo' <$>
                   (x .:? "Status") <*> (x .:? "StepId") <*>
                     (x .:? "Reason"))

instance Hashable CancelStepsInfo where

instance NFData CancelStepsInfo where

-- | The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.
--
--
--
-- /See:/ 'cloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { _cwadEvaluationPeriods  :: !(Maybe Int)
  , _cwadNamespace          :: !(Maybe Text)
  , _cwadDimensions         :: !(Maybe [MetricDimension])
  , _cwadUnit               :: !(Maybe Unit)
  , _cwadStatistic          :: !(Maybe Statistic)
  , _cwadComparisonOperator :: !ComparisonOperator
  , _cwadMetricName         :: !Text
  , _cwadPeriod             :: !Int
  , _cwadThreshold          :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchAlarmDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwadEvaluationPeriods' - The number of periods, expressed in seconds using @Period@ , during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
--
-- * 'cwadNamespace' - The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
--
-- * 'cwadDimensions' - A CloudWatch metric dimension.
--
-- * 'cwadUnit' - The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
--
-- * 'cwadStatistic' - The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
--
-- * 'cwadComparisonOperator' - Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
--
-- * 'cwadMetricName' - The name of the CloudWatch metric that is watched to determine an alarm condition.
--
-- * 'cwadPeriod' - The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
--
-- * 'cwadThreshold' - The value against which the specified statistic is compared.
cloudWatchAlarmDefinition
    :: ComparisonOperator -- ^ 'cwadComparisonOperator'
    -> Text -- ^ 'cwadMetricName'
    -> Int -- ^ 'cwadPeriod'
    -> Double -- ^ 'cwadThreshold'
    -> CloudWatchAlarmDefinition
cloudWatchAlarmDefinition pComparisonOperator_ pMetricName_ pPeriod_ pThreshold_ =
  CloudWatchAlarmDefinition'
    { _cwadEvaluationPeriods = Nothing
    , _cwadNamespace = Nothing
    , _cwadDimensions = Nothing
    , _cwadUnit = Nothing
    , _cwadStatistic = Nothing
    , _cwadComparisonOperator = pComparisonOperator_
    , _cwadMetricName = pMetricName_
    , _cwadPeriod = pPeriod_
    , _cwadThreshold = pThreshold_
    }


-- | The number of periods, expressed in seconds using @Period@ , during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
cwadEvaluationPeriods :: Lens' CloudWatchAlarmDefinition (Maybe Int)
cwadEvaluationPeriods = lens _cwadEvaluationPeriods (\ s a -> s{_cwadEvaluationPeriods = a})

-- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
cwadNamespace :: Lens' CloudWatchAlarmDefinition (Maybe Text)
cwadNamespace = lens _cwadNamespace (\ s a -> s{_cwadNamespace = a})

-- | A CloudWatch metric dimension.
cwadDimensions :: Lens' CloudWatchAlarmDefinition [MetricDimension]
cwadDimensions = lens _cwadDimensions (\ s a -> s{_cwadDimensions = a}) . _Default . _Coerce

-- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
cwadUnit :: Lens' CloudWatchAlarmDefinition (Maybe Unit)
cwadUnit = lens _cwadUnit (\ s a -> s{_cwadUnit = a})

-- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
cwadStatistic :: Lens' CloudWatchAlarmDefinition (Maybe Statistic)
cwadStatistic = lens _cwadStatistic (\ s a -> s{_cwadStatistic = a})

-- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
cwadComparisonOperator :: Lens' CloudWatchAlarmDefinition ComparisonOperator
cwadComparisonOperator = lens _cwadComparisonOperator (\ s a -> s{_cwadComparisonOperator = a})

-- | The name of the CloudWatch metric that is watched to determine an alarm condition.
cwadMetricName :: Lens' CloudWatchAlarmDefinition Text
cwadMetricName = lens _cwadMetricName (\ s a -> s{_cwadMetricName = a})

-- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
cwadPeriod :: Lens' CloudWatchAlarmDefinition Int
cwadPeriod = lens _cwadPeriod (\ s a -> s{_cwadPeriod = a})

-- | The value against which the specified statistic is compared.
cwadThreshold :: Lens' CloudWatchAlarmDefinition Double
cwadThreshold = lens _cwadThreshold (\ s a -> s{_cwadThreshold = a})

instance FromJSON CloudWatchAlarmDefinition where
        parseJSON
          = withObject "CloudWatchAlarmDefinition"
              (\ x ->
                 CloudWatchAlarmDefinition' <$>
                   (x .:? "EvaluationPeriods") <*> (x .:? "Namespace")
                     <*> (x .:? "Dimensions" .!= mempty)
                     <*> (x .:? "Unit")
                     <*> (x .:? "Statistic")
                     <*> (x .: "ComparisonOperator")
                     <*> (x .: "MetricName")
                     <*> (x .: "Period")
                     <*> (x .: "Threshold"))

instance Hashable CloudWatchAlarmDefinition where

instance NFData CloudWatchAlarmDefinition where

instance ToJSON CloudWatchAlarmDefinition where
        toJSON CloudWatchAlarmDefinition'{..}
          = object
              (catMaybes
                 [("EvaluationPeriods" .=) <$> _cwadEvaluationPeriods,
                  ("Namespace" .=) <$> _cwadNamespace,
                  ("Dimensions" .=) <$> _cwadDimensions,
                  ("Unit" .=) <$> _cwadUnit,
                  ("Statistic" .=) <$> _cwadStatistic,
                  Just
                    ("ComparisonOperator" .= _cwadComparisonOperator),
                  Just ("MetricName" .= _cwadMetricName),
                  Just ("Period" .= _cwadPeriod),
                  Just ("Threshold" .= _cwadThreshold)])

-- | The detailed description of the cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cluRequestedAMIVersion     :: !(Maybe Text)
  , _cluEBSRootVolumeSize       :: !(Maybe Int)
  , _cluEC2InstanceAttributes   :: !(Maybe EC2InstanceAttributes)
  , _cluNormalizedInstanceHours :: !(Maybe Int)
  , _cluConfigurations          :: !(Maybe [Configuration])
  , _cluCustomAMIId             :: !(Maybe Text)
  , _cluAutoScalingRole         :: !(Maybe Text)
  , _cluSecurityConfiguration   :: !(Maybe Text)
  , _cluScaleDownBehavior       :: !(Maybe ScaleDownBehavior)
  , _cluInstanceCollectionType  :: !(Maybe InstanceCollectionType)
  , _cluReleaseLabel            :: !(Maybe Text)
  , _cluRepoUpgradeOnBoot       :: !(Maybe RepoUpgradeOnBoot)
  , _cluLogURI                  :: !(Maybe Text)
  , _cluKerberosAttributes      :: !(Maybe KerberosAttributes)
  , _cluRunningAMIVersion       :: !(Maybe Text)
  , _cluMasterPublicDNSName     :: !(Maybe Text)
  , _cluTerminationProtected    :: !(Maybe Bool)
  , _cluVisibleToAllUsers       :: !(Maybe Bool)
  , _cluAutoTerminate           :: !(Maybe Bool)
  , _cluApplications            :: !(Maybe [Application])
  , _cluTags                    :: !(Maybe [Tag])
  , _cluServiceRole             :: !(Maybe Text)
  , _cluId                      :: !Text
  , _cluName                    :: !Text
  , _cluStatus                  :: !ClusterStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluRequestedAMIVersion' - The AMI version requested for this cluster.
--
-- * 'cluEBSRootVolumeSize' - The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- * 'cluEC2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- * 'cluNormalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'cluConfigurations' - Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
--
-- * 'cluCustomAMIId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- * 'cluAutoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- * 'cluSecurityConfiguration' - The name of the security configuration applied to the cluster.
--
-- * 'cluScaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- * 'cluInstanceCollectionType' - The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- * 'cluReleaseLabel' - The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version, for example, @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <http://docs.aws.amazon.com/emr/latest/ReleaseGuide/ http://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases versions 4.x and later. Earlier versions use @AmiVersion@ .
--
-- * 'cluRepoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
--
-- * 'cluLogURI' - The path to the Amazon S3 location where logs for this cluster are stored.
--
-- * 'cluKerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
--
-- * 'cluRunningAMIVersion' - The AMI version running on this cluster.
--
-- * 'cluMasterPublicDNSName' - The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
--
-- * 'cluTerminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
--
-- * 'cluVisibleToAllUsers' - Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and manage the cluster if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
--
-- * 'cluAutoTerminate' - Specifies whether the cluster should terminate after completing all steps.
--
-- * 'cluApplications' - The applications installed on this cluster.
--
-- * 'cluTags' - A list of tags associated with a cluster.
--
-- * 'cluServiceRole' - The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- * 'cluId' - The unique identifier for the cluster.
--
-- * 'cluName' - The name of the cluster.
--
-- * 'cluStatus' - The current status details about the cluster.
cluster
    :: Text -- ^ 'cluId'
    -> Text -- ^ 'cluName'
    -> ClusterStatus -- ^ 'cluStatus'
    -> Cluster
cluster pId_ pName_ pStatus_ =
  Cluster'
    { _cluRequestedAMIVersion = Nothing
    , _cluEBSRootVolumeSize = Nothing
    , _cluEC2InstanceAttributes = Nothing
    , _cluNormalizedInstanceHours = Nothing
    , _cluConfigurations = Nothing
    , _cluCustomAMIId = Nothing
    , _cluAutoScalingRole = Nothing
    , _cluSecurityConfiguration = Nothing
    , _cluScaleDownBehavior = Nothing
    , _cluInstanceCollectionType = Nothing
    , _cluReleaseLabel = Nothing
    , _cluRepoUpgradeOnBoot = Nothing
    , _cluLogURI = Nothing
    , _cluKerberosAttributes = Nothing
    , _cluRunningAMIVersion = Nothing
    , _cluMasterPublicDNSName = Nothing
    , _cluTerminationProtected = Nothing
    , _cluVisibleToAllUsers = Nothing
    , _cluAutoTerminate = Nothing
    , _cluApplications = Nothing
    , _cluTags = Nothing
    , _cluServiceRole = Nothing
    , _cluId = pId_
    , _cluName = pName_
    , _cluStatus = pStatus_
    }


-- | The AMI version requested for this cluster.
cluRequestedAMIVersion :: Lens' Cluster (Maybe Text)
cluRequestedAMIVersion = lens _cluRequestedAMIVersion (\ s a -> s{_cluRequestedAMIVersion = a})

-- | The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
cluEBSRootVolumeSize :: Lens' Cluster (Maybe Int)
cluEBSRootVolumeSize = lens _cluEBSRootVolumeSize (\ s a -> s{_cluEBSRootVolumeSize = a})

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
cluEC2InstanceAttributes :: Lens' Cluster (Maybe EC2InstanceAttributes)
cluEC2InstanceAttributes = lens _cluEC2InstanceAttributes (\ s a -> s{_cluEC2InstanceAttributes = a})

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
cluNormalizedInstanceHours :: Lens' Cluster (Maybe Int)
cluNormalizedInstanceHours = lens _cluNormalizedInstanceHours (\ s a -> s{_cluNormalizedInstanceHours = a})

-- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
cluConfigurations :: Lens' Cluster [Configuration]
cluConfigurations = lens _cluConfigurations (\ s a -> s{_cluConfigurations = a}) . _Default . _Coerce

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
cluCustomAMIId :: Lens' Cluster (Maybe Text)
cluCustomAMIId = lens _cluCustomAMIId (\ s a -> s{_cluCustomAMIId = a})

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
cluAutoScalingRole :: Lens' Cluster (Maybe Text)
cluAutoScalingRole = lens _cluAutoScalingRole (\ s a -> s{_cluAutoScalingRole = a})

-- | The name of the security configuration applied to the cluster.
cluSecurityConfiguration :: Lens' Cluster (Maybe Text)
cluSecurityConfiguration = lens _cluSecurityConfiguration (\ s a -> s{_cluSecurityConfiguration = a})

-- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
cluScaleDownBehavior :: Lens' Cluster (Maybe ScaleDownBehavior)
cluScaleDownBehavior = lens _cluScaleDownBehavior (\ s a -> s{_cluScaleDownBehavior = a})

-- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
cluInstanceCollectionType :: Lens' Cluster (Maybe InstanceCollectionType)
cluInstanceCollectionType = lens _cluInstanceCollectionType (\ s a -> s{_cluInstanceCollectionType = a})

-- | The Amazon EMR release label, which determines the version of open-source application packages installed on the cluster. Release labels are in the form @emr-x.x.x@ , where x.x.x is an Amazon EMR release version, for example, @emr-5.14.0@ . For more information about Amazon EMR release versions and included application versions and features, see <http://docs.aws.amazon.com/emr/latest/ReleaseGuide/ http://docs.aws.amazon.com/emr/latest/ReleaseGuide/> . The release label applies only to Amazon EMR releases versions 4.x and later. Earlier versions use @AmiVersion@ .
cluReleaseLabel :: Lens' Cluster (Maybe Text)
cluReleaseLabel = lens _cluReleaseLabel (\ s a -> s{_cluReleaseLabel = a})

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
cluRepoUpgradeOnBoot :: Lens' Cluster (Maybe RepoUpgradeOnBoot)
cluRepoUpgradeOnBoot = lens _cluRepoUpgradeOnBoot (\ s a -> s{_cluRepoUpgradeOnBoot = a})

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cluLogURI :: Lens' Cluster (Maybe Text)
cluLogURI = lens _cluLogURI (\ s a -> s{_cluLogURI = a})

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
cluKerberosAttributes :: Lens' Cluster (Maybe KerberosAttributes)
cluKerberosAttributes = lens _cluKerberosAttributes (\ s a -> s{_cluKerberosAttributes = a})

-- | The AMI version running on this cluster.
cluRunningAMIVersion :: Lens' Cluster (Maybe Text)
cluRunningAMIVersion = lens _cluRunningAMIVersion (\ s a -> s{_cluRunningAMIVersion = a})

-- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
cluMasterPublicDNSName :: Lens' Cluster (Maybe Text)
cluMasterPublicDNSName = lens _cluMasterPublicDNSName (\ s a -> s{_cluMasterPublicDNSName = a})

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
cluTerminationProtected :: Lens' Cluster (Maybe Bool)
cluTerminationProtected = lens _cluTerminationProtected (\ s a -> s{_cluTerminationProtected = a})

-- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and manage the cluster if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
cluVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cluVisibleToAllUsers = lens _cluVisibleToAllUsers (\ s a -> s{_cluVisibleToAllUsers = a})

-- | Specifies whether the cluster should terminate after completing all steps.
cluAutoTerminate :: Lens' Cluster (Maybe Bool)
cluAutoTerminate = lens _cluAutoTerminate (\ s a -> s{_cluAutoTerminate = a})

-- | The applications installed on this cluster.
cluApplications :: Lens' Cluster [Application]
cluApplications = lens _cluApplications (\ s a -> s{_cluApplications = a}) . _Default . _Coerce

-- | A list of tags associated with a cluster.
cluTags :: Lens' Cluster [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default . _Coerce

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
cluServiceRole :: Lens' Cluster (Maybe Text)
cluServiceRole = lens _cluServiceRole (\ s a -> s{_cluServiceRole = a})

-- | The unique identifier for the cluster.
cluId :: Lens' Cluster Text
cluId = lens _cluId (\ s a -> s{_cluId = a})

-- | The name of the cluster.
cluName :: Lens' Cluster Text
cluName = lens _cluName (\ s a -> s{_cluName = a})

-- | The current status details about the cluster.
cluStatus :: Lens' Cluster ClusterStatus
cluStatus = lens _cluStatus (\ s a -> s{_cluStatus = a})

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "RequestedAmiVersion") <*>
                     (x .:? "EbsRootVolumeSize")
                     <*> (x .:? "Ec2InstanceAttributes")
                     <*> (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Configurations" .!= mempty)
                     <*> (x .:? "CustomAmiId")
                     <*> (x .:? "AutoScalingRole")
                     <*> (x .:? "SecurityConfiguration")
                     <*> (x .:? "ScaleDownBehavior")
                     <*> (x .:? "InstanceCollectionType")
                     <*> (x .:? "ReleaseLabel")
                     <*> (x .:? "RepoUpgradeOnBoot")
                     <*> (x .:? "LogUri")
                     <*> (x .:? "KerberosAttributes")
                     <*> (x .:? "RunningAmiVersion")
                     <*> (x .:? "MasterPublicDnsName")
                     <*> (x .:? "TerminationProtected")
                     <*> (x .:? "VisibleToAllUsers")
                     <*> (x .:? "AutoTerminate")
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "ServiceRole")
                     <*> (x .: "Id")
                     <*> (x .: "Name")
                     <*> (x .: "Status"))

instance Hashable Cluster where

instance NFData Cluster where

-- | The reason that the cluster changed to its current state.
--
--
--
-- /See:/ 'clusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
  { _cscrCode    :: !(Maybe ClusterStateChangeReasonCode)
  , _cscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscrCode' - The programmatic code for the state change reason.
--
-- * 'cscrMessage' - The descriptive message for the state change reason.
clusterStateChangeReason
    :: ClusterStateChangeReason
clusterStateChangeReason =
  ClusterStateChangeReason' {_cscrCode = Nothing, _cscrMessage = Nothing}


-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe ClusterStateChangeReasonCode)
cscrCode = lens _cscrCode (\ s a -> s{_cscrCode = a})

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\ s a -> s{_cscrMessage = a})

instance FromJSON ClusterStateChangeReason where
        parseJSON
          = withObject "ClusterStateChangeReason"
              (\ x ->
                 ClusterStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ClusterStateChangeReason where

instance NFData ClusterStateChangeReason where

-- | The detailed status of the cluster.
--
--
--
-- /See:/ 'clusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
  { _csState             :: !(Maybe ClusterState)
  , _csStateChangeReason :: !(Maybe ClusterStateChangeReason)
  , _csTimeline          :: !(Maybe ClusterTimeline)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csState' - The current state of the cluster.
--
-- * 'csStateChangeReason' - The reason for the cluster status change.
--
-- * 'csTimeline' - A timeline that represents the status of a cluster over the lifetime of the cluster.
clusterStatus
    :: ClusterStatus
clusterStatus =
  ClusterStatus'
    {_csState = Nothing, _csStateChangeReason = Nothing, _csTimeline = Nothing}


-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe ClusterState)
csState = lens _csState (\ s a -> s{_csState = a})

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason = lens _csStateChangeReason (\ s a -> s{_csStateChangeReason = a})

-- | A timeline that represents the status of a cluster over the lifetime of the cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\ s a -> s{_csTimeline = a})

instance FromJSON ClusterStatus where
        parseJSON
          = withObject "ClusterStatus"
              (\ x ->
                 ClusterStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

instance Hashable ClusterStatus where

instance NFData ClusterStatus where

-- | The summary description of the cluster.
--
--
--
-- /See:/ 'clusterSummary' smart constructor.
data ClusterSummary = ClusterSummary'
  { _csStatus                  :: !(Maybe ClusterStatus)
  , _csNormalizedInstanceHours :: !(Maybe Int)
  , _csName                    :: !(Maybe Text)
  , _csId                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStatus' - The details about the current status of the cluster.
--
-- * 'csNormalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'csName' - The name of the cluster.
--
-- * 'csId' - The unique identifier for the cluster.
clusterSummary
    :: ClusterSummary
clusterSummary =
  ClusterSummary'
    { _csStatus = Nothing
    , _csNormalizedInstanceHours = Nothing
    , _csName = Nothing
    , _csId = Nothing
    }


-- | The details about the current status of the cluster.
csStatus :: Lens' ClusterSummary (Maybe ClusterStatus)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
csNormalizedInstanceHours :: Lens' ClusterSummary (Maybe Int)
csNormalizedInstanceHours = lens _csNormalizedInstanceHours (\ s a -> s{_csNormalizedInstanceHours = a})

-- | The name of the cluster.
csName :: Lens' ClusterSummary (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a})

-- | The unique identifier for the cluster.
csId :: Lens' ClusterSummary (Maybe Text)
csId = lens _csId (\ s a -> s{_csId = a})

instance FromJSON ClusterSummary where
        parseJSON
          = withObject "ClusterSummary"
              (\ x ->
                 ClusterSummary' <$>
                   (x .:? "Status") <*>
                     (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable ClusterSummary where

instance NFData ClusterSummary where

-- | Represents the timeline of the cluster's lifecycle.
--
--
--
-- /See:/ 'clusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
  { _ctReadyDateTime    :: !(Maybe POSIX)
  , _ctCreationDateTime :: !(Maybe POSIX)
  , _ctEndDateTime      :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClusterTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctReadyDateTime' - The date and time when the cluster was ready to execute steps.
--
-- * 'ctCreationDateTime' - The creation date and time of the cluster.
--
-- * 'ctEndDateTime' - The date and time when the cluster was terminated.
clusterTimeline
    :: ClusterTimeline
clusterTimeline =
  ClusterTimeline'
    { _ctReadyDateTime = Nothing
    , _ctCreationDateTime = Nothing
    , _ctEndDateTime = Nothing
    }


-- | The date and time when the cluster was ready to execute steps.
ctReadyDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctReadyDateTime = lens _ctReadyDateTime (\ s a -> s{_ctReadyDateTime = a}) . mapping _Time

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctCreationDateTime = lens _ctCreationDateTime (\ s a -> s{_ctCreationDateTime = a}) . mapping _Time

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctEndDateTime = lens _ctEndDateTime (\ s a -> s{_ctEndDateTime = a}) . mapping _Time

instance FromJSON ClusterTimeline where
        parseJSON
          = withObject "ClusterTimeline"
              (\ x ->
                 ClusterTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

instance Hashable ClusterTimeline where

instance NFData ClusterTimeline where

-- | An entity describing an executable that runs on a cluster.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
  { _cArgs       :: !(Maybe [Text])
  , _cScriptPath :: !(Maybe Text)
  , _cName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cArgs' - Arguments for Amazon EMR to pass to the command for execution.
--
-- * 'cScriptPath' - The Amazon S3 location of the command script.
--
-- * 'cName' - The name of the command.
command
    :: Command
command = Command' {_cArgs = Nothing, _cScriptPath = Nothing, _cName = Nothing}


-- | Arguments for Amazon EMR to pass to the command for execution.
cArgs :: Lens' Command [Text]
cArgs = lens _cArgs (\ s a -> s{_cArgs = a}) . _Default . _Coerce

-- | The Amazon S3 location of the command script.
cScriptPath :: Lens' Command (Maybe Text)
cScriptPath = lens _cScriptPath (\ s a -> s{_cScriptPath = a})

-- | The name of the command.
cName :: Lens' Command (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "ScriptPath")
                     <*> (x .:? "Name"))

instance Hashable Command where

instance NFData Command where

-- | An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <http://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
--
--
-- /See:/ 'configuration' smart constructor.
data Configuration = Configuration'
  { _cConfigurations :: !(Maybe [Configuration])
  , _cClassification :: !(Maybe Text)
  , _cProperties     :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConfigurations' - A list of additional configurations to apply within a configuration object.
--
-- * 'cClassification' - The classification within a configuration.
--
-- * 'cProperties' - A set of properties specified within a configuration classification.
configuration
    :: Configuration
configuration =
  Configuration'
    { _cConfigurations = Nothing
    , _cClassification = Nothing
    , _cProperties = Nothing
    }


-- | A list of additional configurations to apply within a configuration object.
cConfigurations :: Lens' Configuration [Configuration]
cConfigurations = lens _cConfigurations (\ s a -> s{_cConfigurations = a}) . _Default . _Coerce

-- | The classification within a configuration.
cClassification :: Lens' Configuration (Maybe Text)
cClassification = lens _cClassification (\ s a -> s{_cClassification = a})

-- | A set of properties specified within a configuration classification.
cProperties :: Lens' Configuration (HashMap Text Text)
cProperties = lens _cProperties (\ s a -> s{_cProperties = a}) . _Default . _Map

instance FromJSON Configuration where
        parseJSON
          = withObject "Configuration"
              (\ x ->
                 Configuration' <$>
                   (x .:? "Configurations" .!= mempty) <*>
                     (x .:? "Classification")
                     <*> (x .:? "Properties" .!= mempty))

instance Hashable Configuration where

instance NFData Configuration where

instance ToJSON Configuration where
        toJSON Configuration'{..}
          = object
              (catMaybes
                 [("Configurations" .=) <$> _cConfigurations,
                  ("Classification" .=) <$> _cClassification,
                  ("Properties" .=) <$> _cProperties])

-- | Configuration of requested EBS block device associated with the instance group.
--
--
--
-- /See:/ 'ebsBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { _ebdDevice              :: !(Maybe Text)
  , _ebdVolumeSpecification :: !(Maybe VolumeSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdDevice' - The device name that is exposed to the instance, such as /dev/sdh.
--
-- * 'ebdVolumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebsBlockDevice
    :: EBSBlockDevice
ebsBlockDevice =
  EBSBlockDevice' {_ebdDevice = Nothing, _ebdVolumeSpecification = Nothing}


-- | The device name that is exposed to the instance, such as /dev/sdh.
ebdDevice :: Lens' EBSBlockDevice (Maybe Text)
ebdDevice = lens _ebdDevice (\ s a -> s{_ebdDevice = a})

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdVolumeSpecification :: Lens' EBSBlockDevice (Maybe VolumeSpecification)
ebdVolumeSpecification = lens _ebdVolumeSpecification (\ s a -> s{_ebdVolumeSpecification = a})

instance FromJSON EBSBlockDevice where
        parseJSON
          = withObject "EBSBlockDevice"
              (\ x ->
                 EBSBlockDevice' <$>
                   (x .:? "Device") <*> (x .:? "VolumeSpecification"))

instance Hashable EBSBlockDevice where

instance NFData EBSBlockDevice where

-- | Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.
--
--
--
-- /See:/ 'ebsBlockDeviceConfig' smart constructor.
data EBSBlockDeviceConfig = EBSBlockDeviceConfig'
  { _ebdcVolumesPerInstance  :: !(Maybe Int)
  , _ebdcVolumeSpecification :: !VolumeSpecification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSBlockDeviceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdcVolumesPerInstance' - Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
--
-- * 'ebdcVolumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebsBlockDeviceConfig
    :: VolumeSpecification -- ^ 'ebdcVolumeSpecification'
    -> EBSBlockDeviceConfig
ebsBlockDeviceConfig pVolumeSpecification_ =
  EBSBlockDeviceConfig'
    { _ebdcVolumesPerInstance = Nothing
    , _ebdcVolumeSpecification = pVolumeSpecification_
    }


-- | Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
ebdcVolumesPerInstance :: Lens' EBSBlockDeviceConfig (Maybe Int)
ebdcVolumesPerInstance = lens _ebdcVolumesPerInstance (\ s a -> s{_ebdcVolumesPerInstance = a})

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdcVolumeSpecification :: Lens' EBSBlockDeviceConfig VolumeSpecification
ebdcVolumeSpecification = lens _ebdcVolumeSpecification (\ s a -> s{_ebdcVolumeSpecification = a})

instance Hashable EBSBlockDeviceConfig where

instance NFData EBSBlockDeviceConfig where

instance ToJSON EBSBlockDeviceConfig where
        toJSON EBSBlockDeviceConfig'{..}
          = object
              (catMaybes
                 [("VolumesPerInstance" .=) <$>
                    _ebdcVolumesPerInstance,
                  Just
                    ("VolumeSpecification" .= _ebdcVolumeSpecification)])

-- | The Amazon EBS configuration of a cluster instance.
--
--
--
-- /See:/ 'ebsConfiguration' smart constructor.
data EBSConfiguration = EBSConfiguration'
  { _ecEBSOptimized          :: !(Maybe Bool)
  , _ecEBSBlockDeviceConfigs :: !(Maybe [EBSBlockDeviceConfig])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecEBSOptimized' - Indicates whether an Amazon EBS volume is EBS-optimized.
--
-- * 'ecEBSBlockDeviceConfigs' - An array of Amazon EBS volume specifications attached to a cluster instance.
ebsConfiguration
    :: EBSConfiguration
ebsConfiguration =
  EBSConfiguration'
    {_ecEBSOptimized = Nothing, _ecEBSBlockDeviceConfigs = Nothing}


-- | Indicates whether an Amazon EBS volume is EBS-optimized.
ecEBSOptimized :: Lens' EBSConfiguration (Maybe Bool)
ecEBSOptimized = lens _ecEBSOptimized (\ s a -> s{_ecEBSOptimized = a})

-- | An array of Amazon EBS volume specifications attached to a cluster instance.
ecEBSBlockDeviceConfigs :: Lens' EBSConfiguration [EBSBlockDeviceConfig]
ecEBSBlockDeviceConfigs = lens _ecEBSBlockDeviceConfigs (\ s a -> s{_ecEBSBlockDeviceConfigs = a}) . _Default . _Coerce

instance Hashable EBSConfiguration where

instance NFData EBSConfiguration where

instance ToJSON EBSConfiguration where
        toJSON EBSConfiguration'{..}
          = object
              (catMaybes
                 [("EbsOptimized" .=) <$> _ecEBSOptimized,
                  ("EbsBlockDeviceConfigs" .=) <$>
                    _ecEBSBlockDeviceConfigs])

-- | EBS block device that's attached to an EC2 instance.
--
--
--
-- /See:/ 'ebsVolume' smart constructor.
data EBSVolume = EBSVolume'
  { _evDevice   :: !(Maybe Text)
  , _evVolumeId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evDevice' - The device name that is exposed to the instance, such as /dev/sdh.
--
-- * 'evVolumeId' - The volume identifier of the EBS volume.
ebsVolume
    :: EBSVolume
ebsVolume = EBSVolume' {_evDevice = Nothing, _evVolumeId = Nothing}


-- | The device name that is exposed to the instance, such as /dev/sdh.
evDevice :: Lens' EBSVolume (Maybe Text)
evDevice = lens _evDevice (\ s a -> s{_evDevice = a})

-- | The volume identifier of the EBS volume.
evVolumeId :: Lens' EBSVolume (Maybe Text)
evVolumeId = lens _evVolumeId (\ s a -> s{_evVolumeId = a})

instance FromJSON EBSVolume where
        parseJSON
          = withObject "EBSVolume"
              (\ x ->
                 EBSVolume' <$>
                   (x .:? "Device") <*> (x .:? "VolumeId"))

instance Hashable EBSVolume where

instance NFData EBSVolume where

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
--
--
-- /See:/ 'ec2InstanceAttributes' smart constructor.
data EC2InstanceAttributes = EC2InstanceAttributes'
  { _eiaEC2KeyName                     :: !(Maybe Text)
  , _eiaEmrManagedSlaveSecurityGroup   :: !(Maybe Text)
  , _eiaAdditionalSlaveSecurityGroups  :: !(Maybe [Text])
  , _eiaRequestedEC2SubnetIds          :: !(Maybe [Text])
  , _eiaAdditionalMasterSecurityGroups :: !(Maybe [Text])
  , _eiaIAMInstanceProfile             :: !(Maybe Text)
  , _eiaEmrManagedMasterSecurityGroup  :: !(Maybe Text)
  , _eiaEC2SubnetId                    :: !(Maybe Text)
  , _eiaRequestedEC2AvailabilityZones  :: !(Maybe [Text])
  , _eiaServiceAccessSecurityGroup     :: !(Maybe Text)
  , _eiaEC2AvailabilityZone            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2InstanceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiaEC2KeyName' - The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
--
-- * 'eiaEmrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- * 'eiaAdditionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- * 'eiaRequestedEC2SubnetIds' - Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- * 'eiaAdditionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- * 'eiaIAMInstanceProfile' - The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
--
-- * 'eiaEmrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- * 'eiaEC2SubnetId' - To launch the cluster in Amazon VPC, set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, the cluster is launched in the normal AWS cloud, outside of a VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge instance type for nodes of a cluster launched in a VPC.
--
-- * 'eiaRequestedEC2AvailabilityZones' - Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- * 'eiaServiceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- * 'eiaEC2AvailabilityZone' - The Availability Zone in which the cluster will run.
ec2InstanceAttributes
    :: EC2InstanceAttributes
ec2InstanceAttributes =
  EC2InstanceAttributes'
    { _eiaEC2KeyName = Nothing
    , _eiaEmrManagedSlaveSecurityGroup = Nothing
    , _eiaAdditionalSlaveSecurityGroups = Nothing
    , _eiaRequestedEC2SubnetIds = Nothing
    , _eiaAdditionalMasterSecurityGroups = Nothing
    , _eiaIAMInstanceProfile = Nothing
    , _eiaEmrManagedMasterSecurityGroup = Nothing
    , _eiaEC2SubnetId = Nothing
    , _eiaRequestedEC2AvailabilityZones = Nothing
    , _eiaServiceAccessSecurityGroup = Nothing
    , _eiaEC2AvailabilityZone = Nothing
    }


-- | The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
eiaEC2KeyName :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2KeyName = lens _eiaEC2KeyName (\ s a -> s{_eiaEC2KeyName = a})

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
eiaEmrManagedSlaveSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedSlaveSecurityGroup = lens _eiaEmrManagedSlaveSecurityGroup (\ s a -> s{_eiaEmrManagedSlaveSecurityGroup = a})

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
eiaAdditionalSlaveSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalSlaveSecurityGroups = lens _eiaAdditionalSlaveSecurityGroups (\ s a -> s{_eiaAdditionalSlaveSecurityGroups = a}) . _Default . _Coerce

-- | Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
eiaRequestedEC2SubnetIds :: Lens' EC2InstanceAttributes [Text]
eiaRequestedEC2SubnetIds = lens _eiaRequestedEC2SubnetIds (\ s a -> s{_eiaRequestedEC2SubnetIds = a}) . _Default . _Coerce

-- | A list of additional Amazon EC2 security group IDs for the master node.
eiaAdditionalMasterSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalMasterSecurityGroups = lens _eiaAdditionalMasterSecurityGroups (\ s a -> s{_eiaAdditionalMasterSecurityGroups = a}) . _Default . _Coerce

-- | The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
eiaIAMInstanceProfile :: Lens' EC2InstanceAttributes (Maybe Text)
eiaIAMInstanceProfile = lens _eiaIAMInstanceProfile (\ s a -> s{_eiaIAMInstanceProfile = a})

-- | The identifier of the Amazon EC2 security group for the master node.
eiaEmrManagedMasterSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedMasterSecurityGroup = lens _eiaEmrManagedMasterSecurityGroup (\ s a -> s{_eiaEmrManagedMasterSecurityGroup = a})

-- | To launch the cluster in Amazon VPC, set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, the cluster is launched in the normal AWS cloud, outside of a VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge instance type for nodes of a cluster launched in a VPC.
eiaEC2SubnetId :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2SubnetId = lens _eiaEC2SubnetId (\ s a -> s{_eiaEC2SubnetId = a})

-- | Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
eiaRequestedEC2AvailabilityZones :: Lens' EC2InstanceAttributes [Text]
eiaRequestedEC2AvailabilityZones = lens _eiaRequestedEC2AvailabilityZones (\ s a -> s{_eiaRequestedEC2AvailabilityZones = a}) . _Default . _Coerce

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
eiaServiceAccessSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaServiceAccessSecurityGroup = lens _eiaServiceAccessSecurityGroup (\ s a -> s{_eiaServiceAccessSecurityGroup = a})

-- | The Availability Zone in which the cluster will run.
eiaEC2AvailabilityZone :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2AvailabilityZone = lens _eiaEC2AvailabilityZone (\ s a -> s{_eiaEC2AvailabilityZone = a})

instance FromJSON EC2InstanceAttributes where
        parseJSON
          = withObject "EC2InstanceAttributes"
              (\ x ->
                 EC2InstanceAttributes' <$>
                   (x .:? "Ec2KeyName") <*>
                     (x .:? "EmrManagedSlaveSecurityGroup")
                     <*>
                     (x .:? "AdditionalSlaveSecurityGroups" .!= mempty)
                     <*> (x .:? "RequestedEc2SubnetIds" .!= mempty)
                     <*>
                     (x .:? "AdditionalMasterSecurityGroups" .!= mempty)
                     <*> (x .:? "IamInstanceProfile")
                     <*> (x .:? "EmrManagedMasterSecurityGroup")
                     <*> (x .:? "Ec2SubnetId")
                     <*>
                     (x .:? "RequestedEc2AvailabilityZones" .!= mempty)
                     <*> (x .:? "ServiceAccessSecurityGroup")
                     <*> (x .:? "Ec2AvailabilityZone"))

instance Hashable EC2InstanceAttributes where

instance NFData EC2InstanceAttributes where

-- | The details of the step failure. The service attempts to detect the root cause for many common failures.
--
--
--
-- /See:/ 'failureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { _fdLogFile :: !(Maybe Text)
  , _fdReason  :: !(Maybe Text)
  , _fdMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdLogFile' - The path to the log file where the step failure root cause was originally recorded.
--
-- * 'fdReason' - The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
--
-- * 'fdMessage' - The descriptive message including the error the EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
failureDetails
    :: FailureDetails
failureDetails =
  FailureDetails'
    {_fdLogFile = Nothing, _fdReason = Nothing, _fdMessage = Nothing}


-- | The path to the log file where the step failure root cause was originally recorded.
fdLogFile :: Lens' FailureDetails (Maybe Text)
fdLogFile = lens _fdLogFile (\ s a -> s{_fdLogFile = a})

-- | The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
fdReason :: Lens' FailureDetails (Maybe Text)
fdReason = lens _fdReason (\ s a -> s{_fdReason = a})

-- | The descriptive message including the error the EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
fdMessage :: Lens' FailureDetails (Maybe Text)
fdMessage = lens _fdMessage (\ s a -> s{_fdMessage = a})

instance FromJSON FailureDetails where
        parseJSON
          = withObject "FailureDetails"
              (\ x ->
                 FailureDetails' <$>
                   (x .:? "LogFile") <*> (x .:? "Reason") <*>
                     (x .:? "Message"))

instance Hashable FailureDetails where

instance NFData FailureDetails where

-- | A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
--
--
-- /See:/ 'hadoopJARStepConfig' smart constructor.
data HadoopJARStepConfig = HadoopJARStepConfig'
  { _hjscArgs       :: !(Maybe [Text])
  , _hjscMainClass  :: !(Maybe Text)
  , _hjscProperties :: !(Maybe [KeyValue])
  , _hjscJAR        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HadoopJARStepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hjscArgs' - A list of command line arguments passed to the JAR file's main function when executed.
--
-- * 'hjscMainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
--
-- * 'hjscProperties' - A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
--
-- * 'hjscJAR' - A path to a JAR file run during the step.
hadoopJARStepConfig
    :: Text -- ^ 'hjscJAR'
    -> HadoopJARStepConfig
hadoopJARStepConfig pJAR_ =
  HadoopJARStepConfig'
    { _hjscArgs = Nothing
    , _hjscMainClass = Nothing
    , _hjscProperties = Nothing
    , _hjscJAR = pJAR_
    }


-- | A list of command line arguments passed to the JAR file's main function when executed.
hjscArgs :: Lens' HadoopJARStepConfig [Text]
hjscArgs = lens _hjscArgs (\ s a -> s{_hjscArgs = a}) . _Default . _Coerce

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJARStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\ s a -> s{_hjscMainClass = a})

-- | A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJARStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\ s a -> s{_hjscProperties = a}) . _Default . _Coerce

-- | A path to a JAR file run during the step.
hjscJAR :: Lens' HadoopJARStepConfig Text
hjscJAR = lens _hjscJAR (\ s a -> s{_hjscJAR = a})

instance Hashable HadoopJARStepConfig where

instance NFData HadoopJARStepConfig where

instance ToJSON HadoopJARStepConfig where
        toJSON HadoopJARStepConfig'{..}
          = object
              (catMaybes
                 [("Args" .=) <$> _hjscArgs,
                  ("MainClass" .=) <$> _hjscMainClass,
                  ("Properties" .=) <$> _hjscProperties,
                  Just ("Jar" .= _hjscJAR)])

-- | A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
--
--
-- /See:/ 'hadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { _hscArgs       :: !(Maybe [Text])
  , _hscJAR        :: !(Maybe Text)
  , _hscMainClass  :: !(Maybe Text)
  , _hscProperties :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HadoopStepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hscArgs' - The list of command line arguments to pass to the JAR file's main function for execution.
--
-- * 'hscJAR' - The path to the JAR file that runs during the step.
--
-- * 'hscMainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
--
-- * 'hscProperties' - The list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hadoopStepConfig
    :: HadoopStepConfig
hadoopStepConfig =
  HadoopStepConfig'
    { _hscArgs = Nothing
    , _hscJAR = Nothing
    , _hscMainClass = Nothing
    , _hscProperties = Nothing
    }


-- | The list of command line arguments to pass to the JAR file's main function for execution.
hscArgs :: Lens' HadoopStepConfig [Text]
hscArgs = lens _hscArgs (\ s a -> s{_hscArgs = a}) . _Default . _Coerce

-- | The path to the JAR file that runs during the step.
hscJAR :: Lens' HadoopStepConfig (Maybe Text)
hscJAR = lens _hscJAR (\ s a -> s{_hscJAR = a})

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\ s a -> s{_hscMainClass = a})

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (HashMap Text Text)
hscProperties = lens _hscProperties (\ s a -> s{_hscProperties = a}) . _Default . _Map

instance FromJSON HadoopStepConfig where
        parseJSON
          = withObject "HadoopStepConfig"
              (\ x ->
                 HadoopStepConfig' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "Jar") <*>
                     (x .:? "MainClass")
                     <*> (x .:? "Properties" .!= mempty))

instance Hashable HadoopStepConfig where

instance NFData HadoopStepConfig where

-- | Represents an EC2 instance provisioned as part of cluster.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iStatus           :: !(Maybe InstanceStatus)
  , _iPublicDNSName    :: !(Maybe Text)
  , _iEBSVolumes       :: !(Maybe [EBSVolume])
  , _iEC2InstanceId    :: !(Maybe Text)
  , _iInstanceType     :: !(Maybe Text)
  , _iMarket           :: !(Maybe MarketType)
  , _iPrivateIPAddress :: !(Maybe Text)
  , _iInstanceFleetId  :: !(Maybe Text)
  , _iId               :: !(Maybe Text)
  , _iInstanceGroupId  :: !(Maybe Text)
  , _iPrivateDNSName   :: !(Maybe Text)
  , _iPublicIPAddress  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iStatus' - The current status of the instance.
--
-- * 'iPublicDNSName' - The public DNS name of the instance.
--
-- * 'iEBSVolumes' - The list of EBS volumes that are attached to this instance.
--
-- * 'iEC2InstanceId' - The unique identifier of the instance in Amazon EC2.
--
-- * 'iInstanceType' - The EC2 instance type, for example @m3.xlarge@ .
--
-- * 'iMarket' - The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
--
-- * 'iPrivateIPAddress' - The private IP address of the instance.
--
-- * 'iInstanceFleetId' - The unique identifier of the instance fleet to which an EC2 instance belongs.
--
-- * 'iId' - The unique identifier for the instance in Amazon EMR.
--
-- * 'iInstanceGroupId' - The identifier of the instance group to which this instance belongs.
--
-- * 'iPrivateDNSName' - The private DNS name of the instance.
--
-- * 'iPublicIPAddress' - The public IP address of the instance.
instance'
    :: Instance
instance' =
  Instance'
    { _iStatus = Nothing
    , _iPublicDNSName = Nothing
    , _iEBSVolumes = Nothing
    , _iEC2InstanceId = Nothing
    , _iInstanceType = Nothing
    , _iMarket = Nothing
    , _iPrivateIPAddress = Nothing
    , _iInstanceFleetId = Nothing
    , _iId = Nothing
    , _iInstanceGroupId = Nothing
    , _iPrivateDNSName = Nothing
    , _iPublicIPAddress = Nothing
    }


-- | The current status of the instance.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | The public DNS name of the instance.
iPublicDNSName :: Lens' Instance (Maybe Text)
iPublicDNSName = lens _iPublicDNSName (\ s a -> s{_iPublicDNSName = a})

-- | The list of EBS volumes that are attached to this instance.
iEBSVolumes :: Lens' Instance [EBSVolume]
iEBSVolumes = lens _iEBSVolumes (\ s a -> s{_iEBSVolumes = a}) . _Default . _Coerce

-- | The unique identifier of the instance in Amazon EC2.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\ s a -> s{_iEC2InstanceId = a})

-- | The EC2 instance type, for example @m3.xlarge@ .
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\ s a -> s{_iInstanceType = a})

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
iMarket :: Lens' Instance (Maybe MarketType)
iMarket = lens _iMarket (\ s a -> s{_iMarket = a})

-- | The private IP address of the instance.
iPrivateIPAddress :: Lens' Instance (Maybe Text)
iPrivateIPAddress = lens _iPrivateIPAddress (\ s a -> s{_iPrivateIPAddress = a})

-- | The unique identifier of the instance fleet to which an EC2 instance belongs.
iInstanceFleetId :: Lens' Instance (Maybe Text)
iInstanceFleetId = lens _iInstanceFleetId (\ s a -> s{_iInstanceFleetId = a})

-- | The unique identifier for the instance in Amazon EMR.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a})

-- | The identifier of the instance group to which this instance belongs.
iInstanceGroupId :: Lens' Instance (Maybe Text)
iInstanceGroupId = lens _iInstanceGroupId (\ s a -> s{_iInstanceGroupId = a})

-- | The private DNS name of the instance.
iPrivateDNSName :: Lens' Instance (Maybe Text)
iPrivateDNSName = lens _iPrivateDNSName (\ s a -> s{_iPrivateDNSName = a})

-- | The public IP address of the instance.
iPublicIPAddress :: Lens' Instance (Maybe Text)
iPublicIPAddress = lens _iPublicIPAddress (\ s a -> s{_iPublicIPAddress = a})

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "Status") <*> (x .:? "PublicDnsName") <*>
                     (x .:? "EbsVolumes" .!= mempty)
                     <*> (x .:? "Ec2InstanceId")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "Market")
                     <*> (x .:? "PrivateIpAddress")
                     <*> (x .:? "InstanceFleetId")
                     <*> (x .:? "Id")
                     <*> (x .:? "InstanceGroupId")
                     <*> (x .:? "PrivateDnsName")
                     <*> (x .:? "PublicIpAddress"))

instance Hashable Instance where

instance NFData Instance where

-- | Describes an instance fleet, which is a group of EC2 instances that host a particular node type (master, core, or task) in an Amazon EMR cluster. Instance fleets can consist of a mix of instance types and On-Demand and Spot instances, which are provisioned to meet a defined target capacity.
--
--
--
-- /See:/ 'instanceFleet' smart constructor.
data InstanceFleet = InstanceFleet'
  { _ifProvisionedSpotCapacity :: !(Maybe Nat)
  , _ifStatus :: !(Maybe InstanceFleetStatus)
  , _ifTargetOnDemandCapacity :: !(Maybe Nat)
  , _ifInstanceFleetType :: !(Maybe InstanceFleetType)
  , _ifInstanceTypeSpecifications :: !(Maybe [InstanceTypeSpecification])
  , _ifName :: !(Maybe Text)
  , _ifProvisionedOnDemandCapacity :: !(Maybe Nat)
  , _ifTargetSpotCapacity :: !(Maybe Nat)
  , _ifId :: !(Maybe Text)
  , _ifLaunchSpecifications :: !(Maybe InstanceFleetProvisioningSpecifications)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifProvisionedSpotCapacity' - The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
--
-- * 'ifStatus' - The current status of the instance fleet.
--
-- * 'ifTargetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- * 'ifInstanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK.
--
-- * 'ifInstanceTypeSpecifications' - The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet.
--
-- * 'ifName' - A friendly name for the instance fleet.
--
-- * 'ifProvisionedOnDemandCapacity' - The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
--
-- * 'ifTargetSpotCapacity' - The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- * 'ifId' - The unique identifier of the instance fleet.
--
-- * 'ifLaunchSpecifications' - Describes the launch specification for an instance fleet.
instanceFleet
    :: InstanceFleet
instanceFleet =
  InstanceFleet'
    { _ifProvisionedSpotCapacity = Nothing
    , _ifStatus = Nothing
    , _ifTargetOnDemandCapacity = Nothing
    , _ifInstanceFleetType = Nothing
    , _ifInstanceTypeSpecifications = Nothing
    , _ifName = Nothing
    , _ifProvisionedOnDemandCapacity = Nothing
    , _ifTargetSpotCapacity = Nothing
    , _ifId = Nothing
    , _ifLaunchSpecifications = Nothing
    }


-- | The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
ifProvisionedSpotCapacity :: Lens' InstanceFleet (Maybe Natural)
ifProvisionedSpotCapacity = lens _ifProvisionedSpotCapacity (\ s a -> s{_ifProvisionedSpotCapacity = a}) . mapping _Nat

-- | The current status of the instance fleet.
ifStatus :: Lens' InstanceFleet (Maybe InstanceFleetStatus)
ifStatus = lens _ifStatus (\ s a -> s{_ifStatus = a})

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
ifTargetOnDemandCapacity :: Lens' InstanceFleet (Maybe Natural)
ifTargetOnDemandCapacity = lens _ifTargetOnDemandCapacity (\ s a -> s{_ifTargetOnDemandCapacity = a}) . mapping _Nat

-- | The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK.
ifInstanceFleetType :: Lens' InstanceFleet (Maybe InstanceFleetType)
ifInstanceFleetType = lens _ifInstanceFleetType (\ s a -> s{_ifInstanceFleetType = a})

-- | The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet.
ifInstanceTypeSpecifications :: Lens' InstanceFleet [InstanceTypeSpecification]
ifInstanceTypeSpecifications = lens _ifInstanceTypeSpecifications (\ s a -> s{_ifInstanceTypeSpecifications = a}) . _Default . _Coerce

-- | A friendly name for the instance fleet.
ifName :: Lens' InstanceFleet (Maybe Text)
ifName = lens _ifName (\ s a -> s{_ifName = a})

-- | The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
ifProvisionedOnDemandCapacity :: Lens' InstanceFleet (Maybe Natural)
ifProvisionedOnDemandCapacity = lens _ifProvisionedOnDemandCapacity (\ s a -> s{_ifProvisionedOnDemandCapacity = a}) . mapping _Nat

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
ifTargetSpotCapacity :: Lens' InstanceFleet (Maybe Natural)
ifTargetSpotCapacity = lens _ifTargetSpotCapacity (\ s a -> s{_ifTargetSpotCapacity = a}) . mapping _Nat

-- | The unique identifier of the instance fleet.
ifId :: Lens' InstanceFleet (Maybe Text)
ifId = lens _ifId (\ s a -> s{_ifId = a})

-- | Describes the launch specification for an instance fleet.
ifLaunchSpecifications :: Lens' InstanceFleet (Maybe InstanceFleetProvisioningSpecifications)
ifLaunchSpecifications = lens _ifLaunchSpecifications (\ s a -> s{_ifLaunchSpecifications = a})

instance FromJSON InstanceFleet where
        parseJSON
          = withObject "InstanceFleet"
              (\ x ->
                 InstanceFleet' <$>
                   (x .:? "ProvisionedSpotCapacity") <*>
                     (x .:? "Status")
                     <*> (x .:? "TargetOnDemandCapacity")
                     <*> (x .:? "InstanceFleetType")
                     <*> (x .:? "InstanceTypeSpecifications" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "ProvisionedOnDemandCapacity")
                     <*> (x .:? "TargetSpotCapacity")
                     <*> (x .:? "Id")
                     <*> (x .:? "LaunchSpecifications"))

instance Hashable InstanceFleet where

instance NFData InstanceFleet where

-- | The configuration that defines an instance fleet.
--
--
--
-- /See:/ 'instanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { _ifcInstanceTypeConfigs :: !(Maybe [InstanceTypeConfig])
  , _ifcTargetOnDemandCapacity :: !(Maybe Nat)
  , _ifcName :: !(Maybe Text)
  , _ifcTargetSpotCapacity :: !(Maybe Nat)
  , _ifcLaunchSpecifications :: !(Maybe InstanceFleetProvisioningSpecifications)
  , _ifcInstanceFleetType :: !InstanceFleetType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleetConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifcInstanceTypeConfigs' - The instance type configurations that define the EC2 instances in the instance fleet.
--
-- * 'ifcTargetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- * 'ifcName' - The friendly name of the instance fleet.
--
-- * 'ifcTargetSpotCapacity' - The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- * 'ifcLaunchSpecifications' - The launch specification for the instance fleet.
--
-- * 'ifcInstanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
instanceFleetConfig
    :: InstanceFleetType -- ^ 'ifcInstanceFleetType'
    -> InstanceFleetConfig
instanceFleetConfig pInstanceFleetType_ =
  InstanceFleetConfig'
    { _ifcInstanceTypeConfigs = Nothing
    , _ifcTargetOnDemandCapacity = Nothing
    , _ifcName = Nothing
    , _ifcTargetSpotCapacity = Nothing
    , _ifcLaunchSpecifications = Nothing
    , _ifcInstanceFleetType = pInstanceFleetType_
    }


-- | The instance type configurations that define the EC2 instances in the instance fleet.
ifcInstanceTypeConfigs :: Lens' InstanceFleetConfig [InstanceTypeConfig]
ifcInstanceTypeConfigs = lens _ifcInstanceTypeConfigs (\ s a -> s{_ifcInstanceTypeConfigs = a}) . _Default . _Coerce

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
ifcTargetOnDemandCapacity :: Lens' InstanceFleetConfig (Maybe Natural)
ifcTargetOnDemandCapacity = lens _ifcTargetOnDemandCapacity (\ s a -> s{_ifcTargetOnDemandCapacity = a}) . mapping _Nat

-- | The friendly name of the instance fleet.
ifcName :: Lens' InstanceFleetConfig (Maybe Text)
ifcName = lens _ifcName (\ s a -> s{_ifcName = a})

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
ifcTargetSpotCapacity :: Lens' InstanceFleetConfig (Maybe Natural)
ifcTargetSpotCapacity = lens _ifcTargetSpotCapacity (\ s a -> s{_ifcTargetSpotCapacity = a}) . mapping _Nat

-- | The launch specification for the instance fleet.
ifcLaunchSpecifications :: Lens' InstanceFleetConfig (Maybe InstanceFleetProvisioningSpecifications)
ifcLaunchSpecifications = lens _ifcLaunchSpecifications (\ s a -> s{_ifcLaunchSpecifications = a})

-- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
ifcInstanceFleetType :: Lens' InstanceFleetConfig InstanceFleetType
ifcInstanceFleetType = lens _ifcInstanceFleetType (\ s a -> s{_ifcInstanceFleetType = a})

instance Hashable InstanceFleetConfig where

instance NFData InstanceFleetConfig where

instance ToJSON InstanceFleetConfig where
        toJSON InstanceFleetConfig'{..}
          = object
              (catMaybes
                 [("InstanceTypeConfigs" .=) <$>
                    _ifcInstanceTypeConfigs,
                  ("TargetOnDemandCapacity" .=) <$>
                    _ifcTargetOnDemandCapacity,
                  ("Name" .=) <$> _ifcName,
                  ("TargetSpotCapacity" .=) <$> _ifcTargetSpotCapacity,
                  ("LaunchSpecifications" .=) <$>
                    _ifcLaunchSpecifications,
                  Just ("InstanceFleetType" .= _ifcInstanceFleetType)])

-- | Configuration parameters for an instance fleet modification request.
--
--
--
-- /See:/ 'instanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { _ifmcTargetOnDemandCapacity :: !(Maybe Nat)
  , _ifmcTargetSpotCapacity     :: !(Maybe Nat)
  , _ifmcInstanceFleetId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleetModifyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifmcTargetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
--
-- * 'ifmcTargetSpotCapacity' - The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
--
-- * 'ifmcInstanceFleetId' - A unique identifier for the instance fleet.
instanceFleetModifyConfig
    :: Text -- ^ 'ifmcInstanceFleetId'
    -> InstanceFleetModifyConfig
instanceFleetModifyConfig pInstanceFleetId_ =
  InstanceFleetModifyConfig'
    { _ifmcTargetOnDemandCapacity = Nothing
    , _ifmcTargetSpotCapacity = Nothing
    , _ifmcInstanceFleetId = pInstanceFleetId_
    }


-- | The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
ifmcTargetOnDemandCapacity :: Lens' InstanceFleetModifyConfig (Maybe Natural)
ifmcTargetOnDemandCapacity = lens _ifmcTargetOnDemandCapacity (\ s a -> s{_ifmcTargetOnDemandCapacity = a}) . mapping _Nat

-- | The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
ifmcTargetSpotCapacity :: Lens' InstanceFleetModifyConfig (Maybe Natural)
ifmcTargetSpotCapacity = lens _ifmcTargetSpotCapacity (\ s a -> s{_ifmcTargetSpotCapacity = a}) . mapping _Nat

-- | A unique identifier for the instance fleet.
ifmcInstanceFleetId :: Lens' InstanceFleetModifyConfig Text
ifmcInstanceFleetId = lens _ifmcInstanceFleetId (\ s a -> s{_ifmcInstanceFleetId = a})

instance Hashable InstanceFleetModifyConfig where

instance NFData InstanceFleetModifyConfig where

instance ToJSON InstanceFleetModifyConfig where
        toJSON InstanceFleetModifyConfig'{..}
          = object
              (catMaybes
                 [("TargetOnDemandCapacity" .=) <$>
                    _ifmcTargetOnDemandCapacity,
                  ("TargetSpotCapacity" .=) <$>
                    _ifmcTargetSpotCapacity,
                  Just ("InstanceFleetId" .= _ifmcInstanceFleetId)])

-- | The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
--
--
--
-- /See:/ 'instanceFleetProvisioningSpecifications' smart constructor.
newtype InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'
  { _ifpsSpotSpecification :: SpotProvisioningSpecification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleetProvisioningSpecifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifpsSpotSpecification' - The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
instanceFleetProvisioningSpecifications
    :: SpotProvisioningSpecification -- ^ 'ifpsSpotSpecification'
    -> InstanceFleetProvisioningSpecifications
instanceFleetProvisioningSpecifications pSpotSpecification_ =
  InstanceFleetProvisioningSpecifications'
    {_ifpsSpotSpecification = pSpotSpecification_}


-- | The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
ifpsSpotSpecification :: Lens' InstanceFleetProvisioningSpecifications SpotProvisioningSpecification
ifpsSpotSpecification = lens _ifpsSpotSpecification (\ s a -> s{_ifpsSpotSpecification = a})

instance FromJSON
           InstanceFleetProvisioningSpecifications
         where
        parseJSON
          = withObject
              "InstanceFleetProvisioningSpecifications"
              (\ x ->
                 InstanceFleetProvisioningSpecifications' <$>
                   (x .: "SpotSpecification"))

instance Hashable
           InstanceFleetProvisioningSpecifications
         where

instance NFData
           InstanceFleetProvisioningSpecifications
         where

instance ToJSON
           InstanceFleetProvisioningSpecifications
         where
        toJSON InstanceFleetProvisioningSpecifications'{..}
          = object
              (catMaybes
                 [Just
                    ("SpotSpecification" .= _ifpsSpotSpecification)])

-- | Provides status change reason details for the instance fleet.
--
--
--
-- /See:/ 'instanceFleetStateChangeReason' smart constructor.
data InstanceFleetStateChangeReason = InstanceFleetStateChangeReason'
  { _ifscrCode    :: !(Maybe InstanceFleetStateChangeReasonCode)
  , _ifscrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleetStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifscrCode' - A code corresponding to the reason the state change occurred.
--
-- * 'ifscrMessage' - An explanatory message.
instanceFleetStateChangeReason
    :: InstanceFleetStateChangeReason
instanceFleetStateChangeReason =
  InstanceFleetStateChangeReason'
    {_ifscrCode = Nothing, _ifscrMessage = Nothing}


-- | A code corresponding to the reason the state change occurred.
ifscrCode :: Lens' InstanceFleetStateChangeReason (Maybe InstanceFleetStateChangeReasonCode)
ifscrCode = lens _ifscrCode (\ s a -> s{_ifscrCode = a})

-- | An explanatory message.
ifscrMessage :: Lens' InstanceFleetStateChangeReason (Maybe Text)
ifscrMessage = lens _ifscrMessage (\ s a -> s{_ifscrMessage = a})

instance FromJSON InstanceFleetStateChangeReason
         where
        parseJSON
          = withObject "InstanceFleetStateChangeReason"
              (\ x ->
                 InstanceFleetStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable InstanceFleetStateChangeReason
         where

instance NFData InstanceFleetStateChangeReason where

-- | The status of the instance fleet.
--
--
--
-- /See:/ 'instanceFleetStatus' smart constructor.
data InstanceFleetStatus = InstanceFleetStatus'
  { _ifsState             :: !(Maybe InstanceFleetState)
  , _ifsStateChangeReason :: !(Maybe InstanceFleetStateChangeReason)
  , _ifsTimeline          :: !(Maybe InstanceFleetTimeline)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceFleetStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifsState' - A code representing the instance fleet status.     * @PROVISIONING@
