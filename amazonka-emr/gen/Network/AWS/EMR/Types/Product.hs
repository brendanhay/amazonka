{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Product where

import           Network.AWS.EMR.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | An application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-mapr.html Launch a Job Flow on the MapR Distribution for Hadoop> . Currently supported values are:
--
--
--     * "mapr-m3" - launch the job flow using MapR M3 Edition.
--
--     * "mapr-m5" - launch the job flow using MapR M5 Edition.
--
--     * "mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow using MapR M3 or M5 Edition, respectively.
--
--
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
    { _aArgs           :: !(Maybe [Text])
    , _aAdditionalInfo :: !(Maybe (Map Text Text))
    , _aName           :: !(Maybe Text)
    , _aVersion        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
aArgs = lens _aArgs (\ s a -> s{_aArgs = a}) . _Default . _Coerce;

-- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (HashMap Text Text)
aAdditionalInfo = lens _aAdditionalInfo (\ s a -> s{_aAdditionalInfo = a}) . _Default . _Map;

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\ s a -> s{_aVersion = a});

instance FromJSON Application where
        parseJSON
          = withObject "Application"
              (\ x ->
                 Application' <$>
                   (x .:? "Args" .!= mempty) <*>
                     (x .:? "AdditionalInfo" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Version"))

instance Hashable Application

instance NFData Application

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    AutoScalingPolicy'
    { _aspConstraints = pConstraints_
    , _aspRules = mempty
    }

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspConstraints :: Lens' AutoScalingPolicy ScalingConstraints
aspConstraints = lens _aspConstraints (\ s a -> s{_aspConstraints = a});

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspRules :: Lens' AutoScalingPolicy [ScalingRule]
aspRules = lens _aspRules (\ s a -> s{_aspRules = a}) . _Coerce;

instance Hashable AutoScalingPolicy

instance NFData AutoScalingPolicy

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _aspdStatus = Nothing
    , _aspdRules = Nothing
    , _aspdConstraints = Nothing
    }

-- | The status of an automatic scaling policy.
aspdStatus :: Lens' AutoScalingPolicyDescription (Maybe AutoScalingPolicyStatus)
aspdStatus = lens _aspdStatus (\ s a -> s{_aspdStatus = a});

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspdRules :: Lens' AutoScalingPolicyDescription [ScalingRule]
aspdRules = lens _aspdRules (\ s a -> s{_aspdRules = a}) . _Default . _Coerce;

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspdConstraints :: Lens' AutoScalingPolicyDescription (Maybe ScalingConstraints)
aspdConstraints = lens _aspdConstraints (\ s a -> s{_aspdConstraints = a});

instance FromJSON AutoScalingPolicyDescription where
        parseJSON
          = withObject "AutoScalingPolicyDescription"
              (\ x ->
                 AutoScalingPolicyDescription' <$>
                   (x .:? "Status") <*> (x .:? "Rules" .!= mempty) <*>
                     (x .:? "Constraints"))

instance Hashable AutoScalingPolicyDescription

instance NFData AutoScalingPolicyDescription

-- | The reason for an 'AutoScalingPolicyStatus' change.
--
--
--
-- /See:/ 'autoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
    { _aspscrCode    :: !(Maybe AutoScalingPolicyStateChangeReasonCode)
    , _aspscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutoScalingPolicyStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspscrCode' - The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates something unclean happened.-->
--
-- * 'aspscrMessage' - A friendly, more verbose message that accompanies an automatic scaling policy state change.
autoScalingPolicyStateChangeReason
    :: AutoScalingPolicyStateChangeReason
autoScalingPolicyStateChangeReason =
    AutoScalingPolicyStateChangeReason'
    { _aspscrCode = Nothing
    , _aspscrMessage = Nothing
    }

-- | The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates something unclean happened.-->
aspscrCode :: Lens' AutoScalingPolicyStateChangeReason (Maybe AutoScalingPolicyStateChangeReasonCode)
aspscrCode = lens _aspscrCode (\ s a -> s{_aspscrCode = a});

-- | A friendly, more verbose message that accompanies an automatic scaling policy state change.
aspscrMessage :: Lens' AutoScalingPolicyStateChangeReason (Maybe Text)
aspscrMessage = lens _aspscrMessage (\ s a -> s{_aspscrMessage = a});

instance FromJSON AutoScalingPolicyStateChangeReason
         where
        parseJSON
          = withObject "AutoScalingPolicyStateChangeReason"
              (\ x ->
                 AutoScalingPolicyStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable AutoScalingPolicyStateChangeReason

instance NFData AutoScalingPolicyStateChangeReason

-- | The status of an automatic scaling policy.
--
--
--
-- /See:/ 'autoScalingPolicyStatus' smart constructor.
data AutoScalingPolicyStatus = AutoScalingPolicyStatus'
    { _aspsState             :: !(Maybe AutoScalingPolicyState)
    , _aspsStateChangeReason :: !(Maybe AutoScalingPolicyStateChangeReason)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutoScalingPolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspsState' -
--
-- * 'aspsStateChangeReason' - The reason for a change in status.
autoScalingPolicyStatus
    :: AutoScalingPolicyStatus
autoScalingPolicyStatus =
    AutoScalingPolicyStatus'
    { _aspsState = Nothing
    , _aspsStateChangeReason = Nothing
    }

-- |
aspsState :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyState)
aspsState = lens _aspsState (\ s a -> s{_aspsState = a});

-- | The reason for a change in status.
aspsStateChangeReason :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyStateChangeReason)
aspsStateChangeReason = lens _aspsStateChangeReason (\ s a -> s{_aspsStateChangeReason = a});

instance FromJSON AutoScalingPolicyStatus where
        parseJSON
          = withObject "AutoScalingPolicyStatus"
              (\ x ->
                 AutoScalingPolicyStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason"))

instance Hashable AutoScalingPolicyStatus

instance NFData AutoScalingPolicyStatus

-- | Configuration of a bootstrap action.
--
--
--
-- /See:/ 'bootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
    { _bacName                  :: !Text
    , _bacScriptBootstrapAction :: !ScriptBootstrapActionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _bacName = pName_
    , _bacScriptBootstrapAction = pScriptBootstrapAction_
    }

-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig Text
bacName = lens _bacName (\ s a -> s{_bacName = a});

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction = lens _bacScriptBootstrapAction (\ s a -> s{_bacScriptBootstrapAction = a});

instance Hashable BootstrapActionConfig

instance NFData BootstrapActionConfig

instance ToJSON BootstrapActionConfig where
        toJSON BootstrapActionConfig'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _bacName),
                  Just
                    ("ScriptBootstrapAction" .=
                       _bacScriptBootstrapAction)])

-- | /See:/ 'cancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
    { _csiStatus :: !(Maybe CancelStepsRequestStatus)
    , _csiStepId :: !(Maybe Text)
    , _csiReason :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelStepsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiStatus' - Undocumented member.
--
-- * 'csiStepId' - Undocumented member.
--
-- * 'csiReason' - Undocumented member.
cancelStepsInfo
    :: CancelStepsInfo
cancelStepsInfo =
    CancelStepsInfo'
    { _csiStatus = Nothing
    , _csiStepId = Nothing
    , _csiReason = Nothing
    }

-- | Undocumented member.
csiStatus :: Lens' CancelStepsInfo (Maybe CancelStepsRequestStatus)
csiStatus = lens _csiStatus (\ s a -> s{_csiStatus = a});

-- | Undocumented member.
csiStepId :: Lens' CancelStepsInfo (Maybe Text)
csiStepId = lens _csiStepId (\ s a -> s{_csiStepId = a});

-- | Undocumented member.
csiReason :: Lens' CancelStepsInfo (Maybe Text)
csiReason = lens _csiReason (\ s a -> s{_csiReason = a});

instance FromJSON CancelStepsInfo where
        parseJSON
          = withObject "CancelStepsInfo"
              (\ x ->
                 CancelStepsInfo' <$>
                   (x .:? "Status") <*> (x .:? "StepId") <*>
                     (x .:? "Reason"))

instance Hashable CancelStepsInfo

instance NFData CancelStepsInfo

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
cwadEvaluationPeriods = lens _cwadEvaluationPeriods (\ s a -> s{_cwadEvaluationPeriods = a});

-- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
cwadNamespace :: Lens' CloudWatchAlarmDefinition (Maybe Text)
cwadNamespace = lens _cwadNamespace (\ s a -> s{_cwadNamespace = a});

-- | A CloudWatch metric dimension.
cwadDimensions :: Lens' CloudWatchAlarmDefinition [MetricDimension]
cwadDimensions = lens _cwadDimensions (\ s a -> s{_cwadDimensions = a}) . _Default . _Coerce;

-- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
cwadUnit :: Lens' CloudWatchAlarmDefinition (Maybe Unit)
cwadUnit = lens _cwadUnit (\ s a -> s{_cwadUnit = a});

-- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
cwadStatistic :: Lens' CloudWatchAlarmDefinition (Maybe Statistic)
cwadStatistic = lens _cwadStatistic (\ s a -> s{_cwadStatistic = a});

-- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
cwadComparisonOperator :: Lens' CloudWatchAlarmDefinition ComparisonOperator
cwadComparisonOperator = lens _cwadComparisonOperator (\ s a -> s{_cwadComparisonOperator = a});

-- | The name of the CloudWatch metric that is watched to determine an alarm condition.
cwadMetricName :: Lens' CloudWatchAlarmDefinition Text
cwadMetricName = lens _cwadMetricName (\ s a -> s{_cwadMetricName = a});

-- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
cwadPeriod :: Lens' CloudWatchAlarmDefinition Int
cwadPeriod = lens _cwadPeriod (\ s a -> s{_cwadPeriod = a});

-- | The value against which the specified statistic is compared.
cwadThreshold :: Lens' CloudWatchAlarmDefinition Double
cwadThreshold = lens _cwadThreshold (\ s a -> s{_cwadThreshold = a});

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

instance Hashable CloudWatchAlarmDefinition

instance NFData CloudWatchAlarmDefinition

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
    , _cluEC2InstanceAttributes   :: !(Maybe EC2InstanceAttributes)
    , _cluNormalizedInstanceHours :: !(Maybe Int)
    , _cluConfigurations          :: !(Maybe [Configuration])
    , _cluAutoScalingRole         :: !(Maybe Text)
    , _cluSecurityConfiguration   :: !(Maybe Text)
    , _cluScaleDownBehavior       :: !(Maybe ScaleDownBehavior)
    , _cluReleaseLabel            :: !(Maybe Text)
    , _cluLogURI                  :: !(Maybe Text)
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluRequestedAMIVersion' - The AMI version requested for this cluster.
--
-- * 'cluEC2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- * 'cluNormalizedInstanceHours' - An approximation of the cost of the job flow, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'cluConfigurations' - The list of Configurations supplied to the EMR cluster.
--
-- * 'cluAutoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- * 'cluSecurityConfiguration' - The name of the security configuration applied to the cluster.
--
-- * 'cluScaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- * 'cluReleaseLabel' - The release label for the Amazon EMR release. For Amazon EMR 3.x and 2.x AMIs, use amiVersion instead instead of ReleaseLabel.
--
-- * 'cluLogURI' - The path to the Amazon S3 location where logs for this cluster are stored.
--
-- * 'cluRunningAMIVersion' - The AMI version running on this cluster.
--
-- * 'cluMasterPublicDNSName' - The public DNS name of the master EC2 instance.
--
-- * 'cluTerminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
--
-- * 'cluVisibleToAllUsers' - Indicates whether the job flow is visible to all IAM users of the AWS account associated with the job flow. If this value is set to @true@ , all IAM users of that AWS account can view and manage the job flow if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
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
    , _cluEC2InstanceAttributes = Nothing
    , _cluNormalizedInstanceHours = Nothing
    , _cluConfigurations = Nothing
    , _cluAutoScalingRole = Nothing
    , _cluSecurityConfiguration = Nothing
    , _cluScaleDownBehavior = Nothing
    , _cluReleaseLabel = Nothing
    , _cluLogURI = Nothing
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
cluRequestedAMIVersion = lens _cluRequestedAMIVersion (\ s a -> s{_cluRequestedAMIVersion = a});

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
cluEC2InstanceAttributes :: Lens' Cluster (Maybe EC2InstanceAttributes)
cluEC2InstanceAttributes = lens _cluEC2InstanceAttributes (\ s a -> s{_cluEC2InstanceAttributes = a});

-- | An approximation of the cost of the job flow, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
cluNormalizedInstanceHours :: Lens' Cluster (Maybe Int)
cluNormalizedInstanceHours = lens _cluNormalizedInstanceHours (\ s a -> s{_cluNormalizedInstanceHours = a});

-- | The list of Configurations supplied to the EMR cluster.
cluConfigurations :: Lens' Cluster [Configuration]
cluConfigurations = lens _cluConfigurations (\ s a -> s{_cluConfigurations = a}) . _Default . _Coerce;

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
cluAutoScalingRole :: Lens' Cluster (Maybe Text)
cluAutoScalingRole = lens _cluAutoScalingRole (\ s a -> s{_cluAutoScalingRole = a});

-- | The name of the security configuration applied to the cluster.
cluSecurityConfiguration :: Lens' Cluster (Maybe Text)
cluSecurityConfiguration = lens _cluSecurityConfiguration (\ s a -> s{_cluSecurityConfiguration = a});

-- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
cluScaleDownBehavior :: Lens' Cluster (Maybe ScaleDownBehavior)
cluScaleDownBehavior = lens _cluScaleDownBehavior (\ s a -> s{_cluScaleDownBehavior = a});

-- | The release label for the Amazon EMR release. For Amazon EMR 3.x and 2.x AMIs, use amiVersion instead instead of ReleaseLabel.
cluReleaseLabel :: Lens' Cluster (Maybe Text)
cluReleaseLabel = lens _cluReleaseLabel (\ s a -> s{_cluReleaseLabel = a});

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cluLogURI :: Lens' Cluster (Maybe Text)
cluLogURI = lens _cluLogURI (\ s a -> s{_cluLogURI = a});

-- | The AMI version running on this cluster.
cluRunningAMIVersion :: Lens' Cluster (Maybe Text)
cluRunningAMIVersion = lens _cluRunningAMIVersion (\ s a -> s{_cluRunningAMIVersion = a});

-- | The public DNS name of the master EC2 instance.
cluMasterPublicDNSName :: Lens' Cluster (Maybe Text)
cluMasterPublicDNSName = lens _cluMasterPublicDNSName (\ s a -> s{_cluMasterPublicDNSName = a});

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
cluTerminationProtected :: Lens' Cluster (Maybe Bool)
cluTerminationProtected = lens _cluTerminationProtected (\ s a -> s{_cluTerminationProtected = a});

-- | Indicates whether the job flow is visible to all IAM users of the AWS account associated with the job flow. If this value is set to @true@ , all IAM users of that AWS account can view and manage the job flow if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
cluVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cluVisibleToAllUsers = lens _cluVisibleToAllUsers (\ s a -> s{_cluVisibleToAllUsers = a});

-- | Specifies whether the cluster should terminate after completing all steps.
cluAutoTerminate :: Lens' Cluster (Maybe Bool)
cluAutoTerminate = lens _cluAutoTerminate (\ s a -> s{_cluAutoTerminate = a});

-- | The applications installed on this cluster.
cluApplications :: Lens' Cluster [Application]
cluApplications = lens _cluApplications (\ s a -> s{_cluApplications = a}) . _Default . _Coerce;

-- | A list of tags associated with a cluster.
cluTags :: Lens' Cluster [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default . _Coerce;

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
cluServiceRole :: Lens' Cluster (Maybe Text)
cluServiceRole = lens _cluServiceRole (\ s a -> s{_cluServiceRole = a});

-- | The unique identifier for the cluster.
cluId :: Lens' Cluster Text
cluId = lens _cluId (\ s a -> s{_cluId = a});

-- | The name of the cluster.
cluName :: Lens' Cluster Text
cluName = lens _cluName (\ s a -> s{_cluName = a});

-- | The current status details about the cluster.
cluStatus :: Lens' Cluster ClusterStatus
cluStatus = lens _cluStatus (\ s a -> s{_cluStatus = a});

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "RequestedAmiVersion") <*>
                     (x .:? "Ec2InstanceAttributes")
                     <*> (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Configurations" .!= mempty)
                     <*> (x .:? "AutoScalingRole")
                     <*> (x .:? "SecurityConfiguration")
                     <*> (x .:? "ScaleDownBehavior")
                     <*> (x .:? "ReleaseLabel")
                     <*> (x .:? "LogUri")
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

instance Hashable Cluster

instance NFData Cluster

-- | The reason that the cluster changed to its current state.
--
--
--
-- /See:/ 'clusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
    { _cscrCode    :: !(Maybe ClusterStateChangeReasonCode)
    , _cscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    ClusterStateChangeReason'
    { _cscrCode = Nothing
    , _cscrMessage = Nothing
    }

-- | The programmatic code for the state change reason.
cscrCode :: Lens' ClusterStateChangeReason (Maybe ClusterStateChangeReasonCode)
cscrCode = lens _cscrCode (\ s a -> s{_cscrCode = a});

-- | The descriptive message for the state change reason.
cscrMessage :: Lens' ClusterStateChangeReason (Maybe Text)
cscrMessage = lens _cscrMessage (\ s a -> s{_cscrMessage = a});

instance FromJSON ClusterStateChangeReason where
        parseJSON
          = withObject "ClusterStateChangeReason"
              (\ x ->
                 ClusterStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable ClusterStateChangeReason

instance NFData ClusterStateChangeReason

-- | The detailed status of the cluster.
--
--
--
-- /See:/ 'clusterStatus' smart constructor.
data ClusterStatus = ClusterStatus'
    { _csState             :: !(Maybe ClusterState)
    , _csStateChangeReason :: !(Maybe ClusterStateChangeReason)
    , _csTimeline          :: !(Maybe ClusterTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _csState = Nothing
    , _csStateChangeReason = Nothing
    , _csTimeline = Nothing
    }

-- | The current state of the cluster.
csState :: Lens' ClusterStatus (Maybe ClusterState)
csState = lens _csState (\ s a -> s{_csState = a});

-- | The reason for the cluster status change.
csStateChangeReason :: Lens' ClusterStatus (Maybe ClusterStateChangeReason)
csStateChangeReason = lens _csStateChangeReason (\ s a -> s{_csStateChangeReason = a});

-- | A timeline that represents the status of a cluster over the lifetime of the cluster.
csTimeline :: Lens' ClusterStatus (Maybe ClusterTimeline)
csTimeline = lens _csTimeline (\ s a -> s{_csTimeline = a});

instance FromJSON ClusterStatus where
        parseJSON
          = withObject "ClusterStatus"
              (\ x ->
                 ClusterStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

instance Hashable ClusterStatus

instance NFData ClusterStatus

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ClusterSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStatus' - The details about the current status of the cluster.
--
-- * 'csNormalizedInstanceHours' - An approximation of the cost of the job flow, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
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
csStatus = lens _csStatus (\ s a -> s{_csStatus = a});

-- | An approximation of the cost of the job flow, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
csNormalizedInstanceHours :: Lens' ClusterSummary (Maybe Int)
csNormalizedInstanceHours = lens _csNormalizedInstanceHours (\ s a -> s{_csNormalizedInstanceHours = a});

-- | The name of the cluster.
csName :: Lens' ClusterSummary (Maybe Text)
csName = lens _csName (\ s a -> s{_csName = a});

-- | The unique identifier for the cluster.
csId :: Lens' ClusterSummary (Maybe Text)
csId = lens _csId (\ s a -> s{_csId = a});

instance FromJSON ClusterSummary where
        parseJSON
          = withObject "ClusterSummary"
              (\ x ->
                 ClusterSummary' <$>
                   (x .:? "Status") <*>
                     (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable ClusterSummary

instance NFData ClusterSummary

-- | Represents the timeline of the cluster's lifecycle.
--
--
--
-- /See:/ 'clusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
    { _ctReadyDateTime    :: !(Maybe POSIX)
    , _ctCreationDateTime :: !(Maybe POSIX)
    , _ctEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
ctReadyDateTime = lens _ctReadyDateTime (\ s a -> s{_ctReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctCreationDateTime = lens _ctCreationDateTime (\ s a -> s{_ctCreationDateTime = a}) . mapping _Time;

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctEndDateTime = lens _ctEndDateTime (\ s a -> s{_ctEndDateTime = a}) . mapping _Time;

instance FromJSON ClusterTimeline where
        parseJSON
          = withObject "ClusterTimeline"
              (\ x ->
                 ClusterTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

instance Hashable ClusterTimeline

instance NFData ClusterTimeline

-- | An entity describing an executable that runs on a cluster.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
    { _cArgs       :: !(Maybe [Text])
    , _cScriptPath :: !(Maybe Text)
    , _cName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
command =
    Command'
    { _cArgs = Nothing
    , _cScriptPath = Nothing
    , _cName = Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
cArgs :: Lens' Command [Text]
cArgs = lens _cArgs (\ s a -> s{_cArgs = a}) . _Default . _Coerce;

-- | The Amazon S3 location of the command script.
cScriptPath :: Lens' Command (Maybe Text)
cScriptPath = lens _cScriptPath (\ s a -> s{_cScriptPath = a});

-- | The name of the command.
cName :: Lens' Command (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "ScriptPath")
                     <*> (x .:? "Name"))

instance Hashable Command

instance NFData Command

-- | Specifies a hardware and software configuration of the EMR cluster. This includes configurations for applications and software bundled with Amazon EMR. The Configuration object is a JSON object which is defined by a classification and a set of properties. Configurations can be nested, so a configuration may have its own Configuration objects listed.
--
--
--
-- /See:/ 'configuration' smart constructor.
data Configuration = Configuration'
    { _cConfigurations :: !(Maybe [Configuration])
    , _cClassification :: !(Maybe Text)
    , _cProperties     :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConfigurations' - A list of configurations you apply to this configuration object.
--
-- * 'cClassification' - The classification of a configuration. For more information see, <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/EmrConfigurations.html Amazon EMR Configurations> .
--
-- * 'cProperties' - A set of properties supplied to the Configuration object.
configuration
    :: Configuration
configuration =
    Configuration'
    { _cConfigurations = Nothing
    , _cClassification = Nothing
    , _cProperties = Nothing
    }

-- | A list of configurations you apply to this configuration object.
cConfigurations :: Lens' Configuration [Configuration]
cConfigurations = lens _cConfigurations (\ s a -> s{_cConfigurations = a}) . _Default . _Coerce;

-- | The classification of a configuration. For more information see, <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/EmrConfigurations.html Amazon EMR Configurations> .
cClassification :: Lens' Configuration (Maybe Text)
cClassification = lens _cClassification (\ s a -> s{_cClassification = a});

-- | A set of properties supplied to the Configuration object.
cProperties :: Lens' Configuration (HashMap Text Text)
cProperties = lens _cProperties (\ s a -> s{_cProperties = a}) . _Default . _Map;

instance FromJSON Configuration where
        parseJSON
          = withObject "Configuration"
              (\ x ->
                 Configuration' <$>
                   (x .:? "Configurations" .!= mempty) <*>
                     (x .:? "Classification")
                     <*> (x .:? "Properties" .!= mempty))

instance Hashable Configuration

instance NFData Configuration

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    EBSBlockDevice'
    { _ebdDevice = Nothing
    , _ebdVolumeSpecification = Nothing
    }

-- | The device name that is exposed to the instance, such as /dev/sdh.
ebdDevice :: Lens' EBSBlockDevice (Maybe Text)
ebdDevice = lens _ebdDevice (\ s a -> s{_ebdDevice = a});

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdVolumeSpecification :: Lens' EBSBlockDevice (Maybe VolumeSpecification)
ebdVolumeSpecification = lens _ebdVolumeSpecification (\ s a -> s{_ebdVolumeSpecification = a});

instance FromJSON EBSBlockDevice where
        parseJSON
          = withObject "EBSBlockDevice"
              (\ x ->
                 EBSBlockDevice' <$>
                   (x .:? "Device") <*> (x .:? "VolumeSpecification"))

instance Hashable EBSBlockDevice

instance NFData EBSBlockDevice

-- | Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.
--
--
--
-- /See:/ 'ebsBlockDeviceConfig' smart constructor.
data EBSBlockDeviceConfig = EBSBlockDeviceConfig'
    { _ebdcVolumesPerInstance  :: !(Maybe Int)
    , _ebdcVolumeSpecification :: !VolumeSpecification
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
ebdcVolumesPerInstance = lens _ebdcVolumesPerInstance (\ s a -> s{_ebdcVolumesPerInstance = a});

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdcVolumeSpecification :: Lens' EBSBlockDeviceConfig VolumeSpecification
ebdcVolumeSpecification = lens _ebdcVolumeSpecification (\ s a -> s{_ebdcVolumeSpecification = a});

instance Hashable EBSBlockDeviceConfig

instance NFData EBSBlockDeviceConfig

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _ecEBSOptimized = Nothing
    , _ecEBSBlockDeviceConfigs = Nothing
    }

-- | Indicates whether an Amazon EBS volume is EBS-optimized.
ecEBSOptimized :: Lens' EBSConfiguration (Maybe Bool)
ecEBSOptimized = lens _ecEBSOptimized (\ s a -> s{_ecEBSOptimized = a});

-- | An array of Amazon EBS volume specifications attached to a cluster instance.
ecEBSBlockDeviceConfigs :: Lens' EBSConfiguration [EBSBlockDeviceConfig]
ecEBSBlockDeviceConfigs = lens _ecEBSBlockDeviceConfigs (\ s a -> s{_ecEBSBlockDeviceConfigs = a}) . _Default . _Coerce;

instance Hashable EBSConfiguration

instance NFData EBSConfiguration

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EBSVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evDevice' - The device name that is exposed to the instance, such as /dev/sdh.
--
-- * 'evVolumeId' - The volume identifier of the EBS volume.
ebsVolume
    :: EBSVolume
ebsVolume =
    EBSVolume'
    { _evDevice = Nothing
    , _evVolumeId = Nothing
    }

-- | The device name that is exposed to the instance, such as /dev/sdh.
evDevice :: Lens' EBSVolume (Maybe Text)
evDevice = lens _evDevice (\ s a -> s{_evDevice = a});

-- | The volume identifier of the EBS volume.
evVolumeId :: Lens' EBSVolume (Maybe Text)
evVolumeId = lens _evVolumeId (\ s a -> s{_evVolumeId = a});

instance FromJSON EBSVolume where
        parseJSON
          = withObject "EBSVolume"
              (\ x ->
                 EBSVolume' <$>
                   (x .:? "Device") <*> (x .:? "VolumeId"))

instance Hashable EBSVolume

instance NFData EBSVolume

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
--
--
-- /See:/ 'ec2InstanceAttributes' smart constructor.
data EC2InstanceAttributes = EC2InstanceAttributes'
    { _eiaEC2KeyName                     :: !(Maybe Text)
    , _eiaEmrManagedSlaveSecurityGroup   :: !(Maybe Text)
    , _eiaAdditionalSlaveSecurityGroups  :: !(Maybe [Text])
    , _eiaAdditionalMasterSecurityGroups :: !(Maybe [Text])
    , _eiaIAMInstanceProfile             :: !(Maybe Text)
    , _eiaEmrManagedMasterSecurityGroup  :: !(Maybe Text)
    , _eiaEC2SubnetId                    :: !(Maybe Text)
    , _eiaServiceAccessSecurityGroup     :: !(Maybe Text)
    , _eiaEC2AvailabilityZone            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EC2InstanceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiaEC2KeyName' - The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
--
-- * 'eiaEmrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the slave nodes.
--
-- * 'eiaAdditionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the slave nodes.
--
-- * 'eiaAdditionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- * 'eiaIAMInstanceProfile' - The IAM role that was specified when the job flow was launched. The EC2 instances of the job flow assume this role.
--
-- * 'eiaEmrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- * 'eiaEC2SubnetId' - To launch the job flow in Amazon VPC, set this parameter to the identifier of the Amazon VPC subnet where you want the job flow to launch. If you do not specify this value, the job flow is launched in the normal AWS cloud, outside of a VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
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
    , _eiaAdditionalMasterSecurityGroups = Nothing
    , _eiaIAMInstanceProfile = Nothing
    , _eiaEmrManagedMasterSecurityGroup = Nothing
    , _eiaEC2SubnetId = Nothing
    , _eiaServiceAccessSecurityGroup = Nothing
    , _eiaEC2AvailabilityZone = Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
eiaEC2KeyName :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2KeyName = lens _eiaEC2KeyName (\ s a -> s{_eiaEC2KeyName = a});

-- | The identifier of the Amazon EC2 security group for the slave nodes.
eiaEmrManagedSlaveSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedSlaveSecurityGroup = lens _eiaEmrManagedSlaveSecurityGroup (\ s a -> s{_eiaEmrManagedSlaveSecurityGroup = a});

-- | A list of additional Amazon EC2 security group IDs for the slave nodes.
eiaAdditionalSlaveSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalSlaveSecurityGroups = lens _eiaAdditionalSlaveSecurityGroups (\ s a -> s{_eiaAdditionalSlaveSecurityGroups = a}) . _Default . _Coerce;

-- | A list of additional Amazon EC2 security group IDs for the master node.
eiaAdditionalMasterSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalMasterSecurityGroups = lens _eiaAdditionalMasterSecurityGroups (\ s a -> s{_eiaAdditionalMasterSecurityGroups = a}) . _Default . _Coerce;

-- | The IAM role that was specified when the job flow was launched. The EC2 instances of the job flow assume this role.
eiaIAMInstanceProfile :: Lens' EC2InstanceAttributes (Maybe Text)
eiaIAMInstanceProfile = lens _eiaIAMInstanceProfile (\ s a -> s{_eiaIAMInstanceProfile = a});

-- | The identifier of the Amazon EC2 security group for the master node.
eiaEmrManagedMasterSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedMasterSecurityGroup = lens _eiaEmrManagedMasterSecurityGroup (\ s a -> s{_eiaEmrManagedMasterSecurityGroup = a});

-- | To launch the job flow in Amazon VPC, set this parameter to the identifier of the Amazon VPC subnet where you want the job flow to launch. If you do not specify this value, the job flow is launched in the normal AWS cloud, outside of a VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
eiaEC2SubnetId :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2SubnetId = lens _eiaEC2SubnetId (\ s a -> s{_eiaEC2SubnetId = a});

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
eiaServiceAccessSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaServiceAccessSecurityGroup = lens _eiaServiceAccessSecurityGroup (\ s a -> s{_eiaServiceAccessSecurityGroup = a});

-- | The Availability Zone in which the cluster will run.
eiaEC2AvailabilityZone :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2AvailabilityZone = lens _eiaEC2AvailabilityZone (\ s a -> s{_eiaEC2AvailabilityZone = a});

instance FromJSON EC2InstanceAttributes where
        parseJSON
          = withObject "EC2InstanceAttributes"
              (\ x ->
                 EC2InstanceAttributes' <$>
                   (x .:? "Ec2KeyName") <*>
                     (x .:? "EmrManagedSlaveSecurityGroup")
                     <*>
                     (x .:? "AdditionalSlaveSecurityGroups" .!= mempty)
                     <*>
                     (x .:? "AdditionalMasterSecurityGroups" .!= mempty)
                     <*> (x .:? "IamInstanceProfile")
                     <*> (x .:? "EmrManagedMasterSecurityGroup")
                     <*> (x .:? "Ec2SubnetId")
                     <*> (x .:? "ServiceAccessSecurityGroup")
                     <*> (x .:? "Ec2AvailabilityZone"))

instance Hashable EC2InstanceAttributes

instance NFData EC2InstanceAttributes

-- | The details of the step failure. The service attempts to detect the root cause for many common failures.
--
--
--
-- /See:/ 'failureDetails' smart constructor.
data FailureDetails = FailureDetails'
    { _fdLogFile :: !(Maybe Text)
    , _fdReason  :: !(Maybe Text)
    , _fdMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _fdLogFile = Nothing
    , _fdReason = Nothing
    , _fdMessage = Nothing
    }

-- | The path to the log file where the step failure root cause was originally recorded.
fdLogFile :: Lens' FailureDetails (Maybe Text)
fdLogFile = lens _fdLogFile (\ s a -> s{_fdLogFile = a});

-- | The reason for the step failure. In the case where the service cannot successfully determine the root cause of the failure, it returns "Unknown Error" as a reason.
fdReason :: Lens' FailureDetails (Maybe Text)
fdReason = lens _fdReason (\ s a -> s{_fdReason = a});

-- | The descriptive message including the error the EMR service has identified as the cause of step failure. This is text from an error log that describes the root cause of the failure.
fdMessage :: Lens' FailureDetails (Maybe Text)
fdMessage = lens _fdMessage (\ s a -> s{_fdMessage = a});

instance FromJSON FailureDetails where
        parseJSON
          = withObject "FailureDetails"
              (\ x ->
                 FailureDetails' <$>
                   (x .:? "LogFile") <*> (x .:? "Reason") <*>
                     (x .:? "Message"))

instance Hashable FailureDetails

instance NFData FailureDetails

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
hjscArgs = lens _hjscArgs (\ s a -> s{_hjscArgs = a}) . _Default . _Coerce;

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJARStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\ s a -> s{_hjscMainClass = a});

-- | A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJARStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\ s a -> s{_hjscProperties = a}) . _Default . _Coerce;

-- | A path to a JAR file run during the step.
hjscJAR :: Lens' HadoopJARStepConfig Text
hjscJAR = lens _hjscJAR (\ s a -> s{_hjscJAR = a});

instance Hashable HadoopJARStepConfig

instance NFData HadoopJARStepConfig

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
hscArgs = lens _hscArgs (\ s a -> s{_hscArgs = a}) . _Default . _Coerce;

-- | The path to the JAR file that runs during the step.
hscJAR :: Lens' HadoopStepConfig (Maybe Text)
hscJAR = lens _hscJAR (\ s a -> s{_hscJAR = a});

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\ s a -> s{_hscMainClass = a});

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (HashMap Text Text)
hscProperties = lens _hscProperties (\ s a -> s{_hscProperties = a}) . _Default . _Map;

instance FromJSON HadoopStepConfig where
        parseJSON
          = withObject "HadoopStepConfig"
              (\ x ->
                 HadoopStepConfig' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "Jar") <*>
                     (x .:? "MainClass")
                     <*> (x .:? "Properties" .!= mempty))

instance Hashable HadoopStepConfig

instance NFData HadoopStepConfig

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
    , _iPrivateIPAddress :: !(Maybe Text)
    , _iId               :: !(Maybe Text)
    , _iInstanceGroupId  :: !(Maybe Text)
    , _iPrivateDNSName   :: !(Maybe Text)
    , _iPublicIPAddress  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- * 'iPrivateIPAddress' - The private IP address of the instance.
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
    , _iPrivateIPAddress = Nothing
    , _iId = Nothing
    , _iInstanceGroupId = Nothing
    , _iPrivateDNSName = Nothing
    , _iPublicIPAddress = Nothing
    }

-- | The current status of the instance.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a});

-- | The public DNS name of the instance.
iPublicDNSName :: Lens' Instance (Maybe Text)
iPublicDNSName = lens _iPublicDNSName (\ s a -> s{_iPublicDNSName = a});

-- | The list of EBS volumes that are attached to this instance.
iEBSVolumes :: Lens' Instance [EBSVolume]
iEBSVolumes = lens _iEBSVolumes (\ s a -> s{_iEBSVolumes = a}) . _Default . _Coerce;

-- | The unique identifier of the instance in Amazon EC2.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\ s a -> s{_iEC2InstanceId = a});

-- | The private IP address of the instance.
iPrivateIPAddress :: Lens' Instance (Maybe Text)
iPrivateIPAddress = lens _iPrivateIPAddress (\ s a -> s{_iPrivateIPAddress = a});

-- | The unique identifier for the instance in Amazon EMR.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a});

-- | The identifier of the instance group to which this instance belongs.
iInstanceGroupId :: Lens' Instance (Maybe Text)
iInstanceGroupId = lens _iInstanceGroupId (\ s a -> s{_iInstanceGroupId = a});

-- | The private DNS name of the instance.
iPrivateDNSName :: Lens' Instance (Maybe Text)
iPrivateDNSName = lens _iPrivateDNSName (\ s a -> s{_iPrivateDNSName = a});

-- | The public IP address of the instance.
iPublicIPAddress :: Lens' Instance (Maybe Text)
iPublicIPAddress = lens _iPublicIPAddress (\ s a -> s{_iPublicIPAddress = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "Status") <*> (x .:? "PublicDnsName") <*>
                     (x .:? "EbsVolumes" .!= mempty)
                     <*> (x .:? "Ec2InstanceId")
                     <*> (x .:? "PrivateIpAddress")
                     <*> (x .:? "Id")
                     <*> (x .:? "InstanceGroupId")
                     <*> (x .:? "PrivateDnsName")
                     <*> (x .:? "PublicIpAddress"))

instance Hashable Instance

instance NFData Instance

-- | This entity represents an instance group, which is a group of instances that have common purpose. For example, CORE instance group is used for HDFS.
--
--
--
-- /See:/ 'instanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
    { _igStatus                 :: !(Maybe InstanceGroupStatus)
    , _igBidPrice               :: !(Maybe Text)
    , _igRequestedInstanceCount :: !(Maybe Int)
    , _igRunningInstanceCount   :: !(Maybe Int)
    , _igConfigurations         :: !(Maybe [Configuration])
    , _igInstanceGroupType      :: !(Maybe InstanceGroupType)
    , _igEBSBlockDevices        :: !(Maybe [EBSBlockDevice])
    , _igInstanceType           :: !(Maybe Text)
    , _igEBSOptimized           :: !(Maybe Bool)
    , _igMarket                 :: !(Maybe MarketType)
    , _igName                   :: !(Maybe Text)
    , _igAutoScalingPolicy      :: !(Maybe AutoScalingPolicyDescription)
    , _igShrinkPolicy           :: !(Maybe ShrinkPolicy)
    , _igId                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igStatus' - The current status of the instance group.
--
-- * 'igBidPrice' - The bid price for each EC2 instance in the instance group when launching nodes as Spot Instances, expressed in USD.
--
-- * 'igRequestedInstanceCount' - The target number of instances for the instance group.
--
-- * 'igRunningInstanceCount' - The number of instances currently running in this instance group.
--
-- * 'igConfigurations' - The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- * 'igInstanceGroupType' - The type of the instance group. Valid values are MASTER, CORE or TASK.
--
-- * 'igEBSBlockDevices' - The EBS block devices that are mapped to this instance group.
--
-- * 'igInstanceType' - The EC2 instance type for all instances in the instance group.
--
-- * 'igEBSOptimized' - If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
--
-- * 'igMarket' - The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
--
-- * 'igName' - The name of the instance group.
--
-- * 'igAutoScalingPolicy' - An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- * 'igShrinkPolicy' - Policy for customizing shrink operations.
--
-- * 'igId' - The identifier of the instance group.
instanceGroup
    :: InstanceGroup
instanceGroup =
    InstanceGroup'
    { _igStatus = Nothing
    , _igBidPrice = Nothing
    , _igRequestedInstanceCount = Nothing
    , _igRunningInstanceCount = Nothing
    , _igConfigurations = Nothing
    , _igInstanceGroupType = Nothing
    , _igEBSBlockDevices = Nothing
    , _igInstanceType = Nothing
    , _igEBSOptimized = Nothing
    , _igMarket = Nothing
    , _igName = Nothing
    , _igAutoScalingPolicy = Nothing
    , _igShrinkPolicy = Nothing
    , _igId = Nothing
    }

-- | The current status of the instance group.
igStatus :: Lens' InstanceGroup (Maybe InstanceGroupStatus)
igStatus = lens _igStatus (\ s a -> s{_igStatus = a});

-- | The bid price for each EC2 instance in the instance group when launching nodes as Spot Instances, expressed in USD.
igBidPrice :: Lens' InstanceGroup (Maybe Text)
igBidPrice = lens _igBidPrice (\ s a -> s{_igBidPrice = a});

-- | The target number of instances for the instance group.
igRequestedInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRequestedInstanceCount = lens _igRequestedInstanceCount (\ s a -> s{_igRequestedInstanceCount = a});

-- | The number of instances currently running in this instance group.
igRunningInstanceCount :: Lens' InstanceGroup (Maybe Int)
igRunningInstanceCount = lens _igRunningInstanceCount (\ s a -> s{_igRunningInstanceCount = a});

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
igConfigurations :: Lens' InstanceGroup [Configuration]
igConfigurations = lens _igConfigurations (\ s a -> s{_igConfigurations = a}) . _Default . _Coerce;

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
igInstanceGroupType :: Lens' InstanceGroup (Maybe InstanceGroupType)
igInstanceGroupType = lens _igInstanceGroupType (\ s a -> s{_igInstanceGroupType = a});

-- | The EBS block devices that are mapped to this instance group.
igEBSBlockDevices :: Lens' InstanceGroup [EBSBlockDevice]
igEBSBlockDevices = lens _igEBSBlockDevices (\ s a -> s{_igEBSBlockDevices = a}) . _Default . _Coerce;

-- | The EC2 instance type for all instances in the instance group.
igInstanceType :: Lens' InstanceGroup (Maybe Text)
igInstanceType = lens _igInstanceType (\ s a -> s{_igInstanceType = a});

-- | If the instance group is EBS-optimized. An Amazon EBS-optimized instance uses an optimized configuration stack and provides additional, dedicated capacity for Amazon EBS I/O.
igEBSOptimized :: Lens' InstanceGroup (Maybe Bool)
igEBSOptimized = lens _igEBSOptimized (\ s a -> s{_igEBSOptimized = a});

-- | The marketplace to provision instances for this group. Valid values are ON_DEMAND or SPOT.
igMarket :: Lens' InstanceGroup (Maybe MarketType)
igMarket = lens _igMarket (\ s a -> s{_igMarket = a});

-- | The name of the instance group.
igName :: Lens' InstanceGroup (Maybe Text)
igName = lens _igName (\ s a -> s{_igName = a});

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
igAutoScalingPolicy :: Lens' InstanceGroup (Maybe AutoScalingPolicyDescription)
igAutoScalingPolicy = lens _igAutoScalingPolicy (\ s a -> s{_igAutoScalingPolicy = a});

-- | Policy for customizing shrink operations.
igShrinkPolicy :: Lens' InstanceGroup (Maybe ShrinkPolicy)
igShrinkPolicy = lens _igShrinkPolicy (\ s a -> s{_igShrinkPolicy = a});

-- | The identifier of the instance group.
igId :: Lens' InstanceGroup (Maybe Text)
igId = lens _igId (\ s a -> s{_igId = a});

instance FromJSON InstanceGroup where
        parseJSON
          = withObject "InstanceGroup"
              (\ x ->
                 InstanceGroup' <$>
                   (x .:? "Status") <*> (x .:? "BidPrice") <*>
                     (x .:? "RequestedInstanceCount")
                     <*> (x .:? "RunningInstanceCount")
                     <*> (x .:? "Configurations" .!= mempty)
                     <*> (x .:? "InstanceGroupType")
                     <*> (x .:? "EbsBlockDevices" .!= mempty)
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "EbsOptimized")
                     <*> (x .:? "Market")
                     <*> (x .:? "Name")
                     <*> (x .:? "AutoScalingPolicy")
                     <*> (x .:? "ShrinkPolicy")
                     <*> (x .:? "Id"))

instance Hashable InstanceGroup

instance NFData InstanceGroup

-- | Configuration defining a new instance group.
--
--
--
-- /See:/ 'instanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
    { _igcEBSConfiguration  :: !(Maybe EBSConfiguration)
    , _igcBidPrice          :: !(Maybe Text)
    , _igcConfigurations    :: !(Maybe [Configuration])
    , _igcMarket            :: !(Maybe MarketType)
    , _igcName              :: !(Maybe Text)
    , _igcAutoScalingPolicy :: !(Maybe AutoScalingPolicy)
    , _igcInstanceRole      :: !InstanceRoleType
    , _igcInstanceType      :: !Text
    , _igcInstanceCount     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igcEBSConfiguration' - EBS configurations that will be attached to each EC2 instance in the instance group.
--
-- * 'igcBidPrice' - Bid price for each EC2 instance in the instance group when launching nodes as Spot Instances, expressed in USD.
--
-- * 'igcConfigurations' - The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- * 'igcMarket' - Market type of the EC2 instances used to create a cluster node.
--
-- * 'igcName' - Friendly name given to the instance group.
--
-- * 'igcAutoScalingPolicy' - An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- * 'igcInstanceRole' - The role of the instance group in the cluster.
--
-- * 'igcInstanceType' - The EC2 instance type for all instances in the instance group.
--
-- * 'igcInstanceCount' - Target number of instances for the instance group.
instanceGroupConfig
    :: InstanceRoleType -- ^ 'igcInstanceRole'
    -> Text -- ^ 'igcInstanceType'
    -> Int -- ^ 'igcInstanceCount'
    -> InstanceGroupConfig
instanceGroupConfig pInstanceRole_ pInstanceType_ pInstanceCount_ =
    InstanceGroupConfig'
    { _igcEBSConfiguration = Nothing
    , _igcBidPrice = Nothing
    , _igcConfigurations = Nothing
    , _igcMarket = Nothing
    , _igcName = Nothing
    , _igcAutoScalingPolicy = Nothing
    , _igcInstanceRole = pInstanceRole_
    , _igcInstanceType = pInstanceType_
    , _igcInstanceCount = pInstanceCount_
    }

-- | EBS configurations that will be attached to each EC2 instance in the instance group.
igcEBSConfiguration :: Lens' InstanceGroupConfig (Maybe EBSConfiguration)
igcEBSConfiguration = lens _igcEBSConfiguration (\ s a -> s{_igcEBSConfiguration = a});

-- | Bid price for each EC2 instance in the instance group when launching nodes as Spot Instances, expressed in USD.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\ s a -> s{_igcBidPrice = a});

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
igcConfigurations :: Lens' InstanceGroupConfig [Configuration]
igcConfigurations = lens _igcConfigurations (\ s a -> s{_igcConfigurations = a}) . _Default . _Coerce;

-- | Market type of the EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket = lens _igcMarket (\ s a -> s{_igcMarket = a});

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\ s a -> s{_igcName = a});

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
igcAutoScalingPolicy :: Lens' InstanceGroupConfig (Maybe AutoScalingPolicy)
igcAutoScalingPolicy = lens _igcAutoScalingPolicy (\ s a -> s{_igcAutoScalingPolicy = a});

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig InstanceRoleType
igcInstanceRole = lens _igcInstanceRole (\ s a -> s{_igcInstanceRole = a});

-- | The EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig Text
igcInstanceType = lens _igcInstanceType (\ s a -> s{_igcInstanceType = a});

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig Int
igcInstanceCount = lens _igcInstanceCount (\ s a -> s{_igcInstanceCount = a});

instance Hashable InstanceGroupConfig

instance NFData InstanceGroupConfig

instance ToJSON InstanceGroupConfig where
        toJSON InstanceGroupConfig'{..}
          = object
              (catMaybes
                 [("EbsConfiguration" .=) <$> _igcEBSConfiguration,
                  ("BidPrice" .=) <$> _igcBidPrice,
                  ("Configurations" .=) <$> _igcConfigurations,
                  ("Market" .=) <$> _igcMarket,
                  ("Name" .=) <$> _igcName,
                  ("AutoScalingPolicy" .=) <$> _igcAutoScalingPolicy,
                  Just ("InstanceRole" .= _igcInstanceRole),
                  Just ("InstanceType" .= _igcInstanceType),
                  Just ("InstanceCount" .= _igcInstanceCount)])

-- | Modify an instance group size.
--
--
--
-- /See:/ 'instanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
    { _igmcInstanceCount             :: !(Maybe Int)
    , _igmcEC2InstanceIdsToTerminate :: !(Maybe [Text])
    , _igmcShrinkPolicy              :: !(Maybe ShrinkPolicy)
    , _igmcInstanceGroupId           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupModifyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igmcInstanceCount' - Target size for the instance group.
--
-- * 'igmcEC2InstanceIdsToTerminate' - The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
--
-- * 'igmcShrinkPolicy' - Policy for customizing shrink operations.
--
-- * 'igmcInstanceGroupId' - Unique ID of the instance group to expand or shrink.
instanceGroupModifyConfig
    :: Text -- ^ 'igmcInstanceGroupId'
    -> InstanceGroupModifyConfig
instanceGroupModifyConfig pInstanceGroupId_ =
    InstanceGroupModifyConfig'
    { _igmcInstanceCount = Nothing
    , _igmcEC2InstanceIdsToTerminate = Nothing
    , _igmcShrinkPolicy = Nothing
    , _igmcInstanceGroupId = pInstanceGroupId_
    }

-- | Target size for the instance group.
igmcInstanceCount :: Lens' InstanceGroupModifyConfig (Maybe Int)
igmcInstanceCount = lens _igmcInstanceCount (\ s a -> s{_igmcInstanceCount = a});

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
igmcEC2InstanceIdsToTerminate :: Lens' InstanceGroupModifyConfig [Text]
igmcEC2InstanceIdsToTerminate = lens _igmcEC2InstanceIdsToTerminate (\ s a -> s{_igmcEC2InstanceIdsToTerminate = a}) . _Default . _Coerce;

-- | Policy for customizing shrink operations.
igmcShrinkPolicy :: Lens' InstanceGroupModifyConfig (Maybe ShrinkPolicy)
igmcShrinkPolicy = lens _igmcShrinkPolicy (\ s a -> s{_igmcShrinkPolicy = a});

-- | Unique ID of the instance group to expand or shrink.
igmcInstanceGroupId :: Lens' InstanceGroupModifyConfig Text
igmcInstanceGroupId = lens _igmcInstanceGroupId (\ s a -> s{_igmcInstanceGroupId = a});

instance Hashable InstanceGroupModifyConfig

instance NFData InstanceGroupModifyConfig

instance ToJSON InstanceGroupModifyConfig where
        toJSON InstanceGroupModifyConfig'{..}
          = object
              (catMaybes
                 [("InstanceCount" .=) <$> _igmcInstanceCount,
                  ("EC2InstanceIdsToTerminate" .=) <$>
                    _igmcEC2InstanceIdsToTerminate,
                  ("ShrinkPolicy" .=) <$> _igmcShrinkPolicy,
                  Just ("InstanceGroupId" .= _igmcInstanceGroupId)])

-- | The status change reason details for the instance group.
--
--
--
-- /See:/ 'instanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
    { _igscrCode    :: !(Maybe InstanceGroupStateChangeReasonCode)
    , _igscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igscrCode' - The programmable code for the state change reason.
--
-- * 'igscrMessage' - The status change reason description.
instanceGroupStateChangeReason
    :: InstanceGroupStateChangeReason
instanceGroupStateChangeReason =
    InstanceGroupStateChangeReason'
    { _igscrCode = Nothing
    , _igscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
igscrCode :: Lens' InstanceGroupStateChangeReason (Maybe InstanceGroupStateChangeReasonCode)
igscrCode = lens _igscrCode (\ s a -> s{_igscrCode = a});

-- | The status change reason description.
igscrMessage :: Lens' InstanceGroupStateChangeReason (Maybe Text)
igscrMessage = lens _igscrMessage (\ s a -> s{_igscrMessage = a});

instance FromJSON InstanceGroupStateChangeReason
         where
        parseJSON
          = withObject "InstanceGroupStateChangeReason"
              (\ x ->
                 InstanceGroupStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable InstanceGroupStateChangeReason

instance NFData InstanceGroupStateChangeReason

-- | The details of the instance group status.
--
--
--
-- /See:/ 'instanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
    { _igsState             :: !(Maybe InstanceGroupState)
    , _igsStateChangeReason :: !(Maybe InstanceGroupStateChangeReason)
    , _igsTimeline          :: !(Maybe InstanceGroupTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igsState' - The current state of the instance group.
--
-- * 'igsStateChangeReason' - The status change reason details for the instance group.
--
-- * 'igsTimeline' - The timeline of the instance group status over time.
instanceGroupStatus
    :: InstanceGroupStatus
instanceGroupStatus =
    InstanceGroupStatus'
    { _igsState = Nothing
    , _igsStateChangeReason = Nothing
    , _igsTimeline = Nothing
    }

-- | The current state of the instance group.
igsState :: Lens' InstanceGroupStatus (Maybe InstanceGroupState)
igsState = lens _igsState (\ s a -> s{_igsState = a});

-- | The status change reason details for the instance group.
igsStateChangeReason :: Lens' InstanceGroupStatus (Maybe InstanceGroupStateChangeReason)
igsStateChangeReason = lens _igsStateChangeReason (\ s a -> s{_igsStateChangeReason = a});

-- | The timeline of the instance group status over time.
igsTimeline :: Lens' InstanceGroupStatus (Maybe InstanceGroupTimeline)
igsTimeline = lens _igsTimeline (\ s a -> s{_igsTimeline = a});

instance FromJSON InstanceGroupStatus where
        parseJSON
          = withObject "InstanceGroupStatus"
              (\ x ->
                 InstanceGroupStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

instance Hashable InstanceGroupStatus

instance NFData InstanceGroupStatus

-- | The timeline of the instance group lifecycle.
--
--
--
-- /See:/ 'instanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
    { _igtReadyDateTime    :: !(Maybe POSIX)
    , _igtCreationDateTime :: !(Maybe POSIX)
    , _igtEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igtReadyDateTime' - The date and time when the instance group became ready to perform tasks.
--
-- * 'igtCreationDateTime' - The creation date and time of the instance group.
--
-- * 'igtEndDateTime' - The date and time when the instance group terminated.
instanceGroupTimeline
    :: InstanceGroupTimeline
instanceGroupTimeline =
    InstanceGroupTimeline'
    { _igtReadyDateTime = Nothing
    , _igtCreationDateTime = Nothing
    , _igtEndDateTime = Nothing
    }

-- | The date and time when the instance group became ready to perform tasks.
igtReadyDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtReadyDateTime = lens _igtReadyDateTime (\ s a -> s{_igtReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the instance group.
igtCreationDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtCreationDateTime = lens _igtCreationDateTime (\ s a -> s{_igtCreationDateTime = a}) . mapping _Time;

-- | The date and time when the instance group terminated.
igtEndDateTime :: Lens' InstanceGroupTimeline (Maybe UTCTime)
igtEndDateTime = lens _igtEndDateTime (\ s a -> s{_igtEndDateTime = a}) . mapping _Time;

instance FromJSON InstanceGroupTimeline where
        parseJSON
          = withObject "InstanceGroupTimeline"
              (\ x ->
                 InstanceGroupTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

instance Hashable InstanceGroupTimeline

instance NFData InstanceGroupTimeline

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
--
--
-- /See:/ 'instanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
    { _irpInstancesToProtect         :: !(Maybe [Text])
    , _irpInstancesToTerminate       :: !(Maybe [Text])
    , _irpInstanceTerminationTimeout :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceResizePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irpInstancesToProtect' - Specific list of instances to be protected when shrinking an instance group.
--
-- * 'irpInstancesToTerminate' - Specific list of instances to be terminated when shrinking an instance group.
--
-- * 'irpInstanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to be terminated.
instanceResizePolicy
    :: InstanceResizePolicy
instanceResizePolicy =
    InstanceResizePolicy'
    { _irpInstancesToProtect = Nothing
    , _irpInstancesToTerminate = Nothing
    , _irpInstanceTerminationTimeout = Nothing
    }

-- | Specific list of instances to be protected when shrinking an instance group.
irpInstancesToProtect :: Lens' InstanceResizePolicy [Text]
irpInstancesToProtect = lens _irpInstancesToProtect (\ s a -> s{_irpInstancesToProtect = a}) . _Default . _Coerce;

-- | Specific list of instances to be terminated when shrinking an instance group.
irpInstancesToTerminate :: Lens' InstanceResizePolicy [Text]
irpInstancesToTerminate = lens _irpInstancesToTerminate (\ s a -> s{_irpInstancesToTerminate = a}) . _Default . _Coerce;

-- | Decommissioning timeout override for the specific list of instances to be terminated.
irpInstanceTerminationTimeout :: Lens' InstanceResizePolicy (Maybe Int)
irpInstanceTerminationTimeout = lens _irpInstanceTerminationTimeout (\ s a -> s{_irpInstanceTerminationTimeout = a});

instance FromJSON InstanceResizePolicy where
        parseJSON
          = withObject "InstanceResizePolicy"
              (\ x ->
                 InstanceResizePolicy' <$>
                   (x .:? "InstancesToProtect" .!= mempty) <*>
                     (x .:? "InstancesToTerminate" .!= mempty)
                     <*> (x .:? "InstanceTerminationTimeout"))

instance Hashable InstanceResizePolicy

instance NFData InstanceResizePolicy

instance ToJSON InstanceResizePolicy where
        toJSON InstanceResizePolicy'{..}
          = object
              (catMaybes
                 [("InstancesToProtect" .=) <$>
                    _irpInstancesToProtect,
                  ("InstancesToTerminate" .=) <$>
                    _irpInstancesToTerminate,
                  ("InstanceTerminationTimeout" .=) <$>
                    _irpInstanceTerminationTimeout])

-- | The details of the status change reason for the instance.
--
--
--
-- /See:/ 'instanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
    { _iscrCode    :: !(Maybe InstanceStateChangeReasonCode)
    , _iscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The programmable code for the state change reason.
--
-- * 'iscrMessage' - The status change reason description.
instanceStateChangeReason
    :: InstanceStateChangeReason
instanceStateChangeReason =
    InstanceStateChangeReason'
    { _iscrCode = Nothing
    , _iscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe InstanceStateChangeReasonCode)
iscrCode = lens _iscrCode (\ s a -> s{_iscrCode = a});

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\ s a -> s{_iscrMessage = a});

instance FromJSON InstanceStateChangeReason where
        parseJSON
          = withObject "InstanceStateChangeReason"
              (\ x ->
                 InstanceStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable InstanceStateChangeReason

instance NFData InstanceStateChangeReason

-- | The instance status details.
--
--
--
-- /See:/ 'instanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
    { _isState             :: !(Maybe InstanceState)
    , _isStateChangeReason :: !(Maybe InstanceStateChangeReason)
    , _isTimeline          :: !(Maybe InstanceTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isState' - The current state of the instance.
--
-- * 'isStateChangeReason' - The details of the status change reason for the instance.
--
-- * 'isTimeline' - The timeline of the instance status over time.
instanceStatus
    :: InstanceStatus
instanceStatus =
    InstanceStatus'
    { _isState = Nothing
    , _isStateChangeReason = Nothing
    , _isTimeline = Nothing
    }

-- | The current state of the instance.
isState :: Lens' InstanceStatus (Maybe InstanceState)
isState = lens _isState (\ s a -> s{_isState = a});

-- | The details of the status change reason for the instance.
isStateChangeReason :: Lens' InstanceStatus (Maybe InstanceStateChangeReason)
isStateChangeReason = lens _isStateChangeReason (\ s a -> s{_isStateChangeReason = a});

-- | The timeline of the instance status over time.
isTimeline :: Lens' InstanceStatus (Maybe InstanceTimeline)
isTimeline = lens _isTimeline (\ s a -> s{_isTimeline = a});

instance FromJSON InstanceStatus where
        parseJSON
          = withObject "InstanceStatus"
              (\ x ->
                 InstanceStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "Timeline"))

instance Hashable InstanceStatus

instance NFData InstanceStatus

-- | The timeline of the instance lifecycle.
--
--
--
-- /See:/ 'instanceTimeline' smart constructor.
data InstanceTimeline = InstanceTimeline'
    { _itReadyDateTime    :: !(Maybe POSIX)
    , _itCreationDateTime :: !(Maybe POSIX)
    , _itEndDateTime      :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itReadyDateTime' - The date and time when the instance was ready to perform tasks.
--
-- * 'itCreationDateTime' - The creation date and time of the instance.
--
-- * 'itEndDateTime' - The date and time when the instance was terminated.
instanceTimeline
    :: InstanceTimeline
instanceTimeline =
    InstanceTimeline'
    { _itReadyDateTime = Nothing
    , _itCreationDateTime = Nothing
    , _itEndDateTime = Nothing
    }

-- | The date and time when the instance was ready to perform tasks.
itReadyDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itReadyDateTime = lens _itReadyDateTime (\ s a -> s{_itReadyDateTime = a}) . mapping _Time;

-- | The creation date and time of the instance.
itCreationDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itCreationDateTime = lens _itCreationDateTime (\ s a -> s{_itCreationDateTime = a}) . mapping _Time;

-- | The date and time when the instance was terminated.
itEndDateTime :: Lens' InstanceTimeline (Maybe UTCTime)
itEndDateTime = lens _itEndDateTime (\ s a -> s{_itEndDateTime = a}) . mapping _Time;

instance FromJSON InstanceTimeline where
        parseJSON
          = withObject "InstanceTimeline"
              (\ x ->
                 InstanceTimeline' <$>
                   (x .:? "ReadyDateTime") <*>
                     (x .:? "CreationDateTime")
                     <*> (x .:? "EndDateTime"))

instance Hashable InstanceTimeline

instance NFData InstanceTimeline

-- | A description of the Amazon EC2 instance running the job flow. A valid JobFlowInstancesConfig must contain at least InstanceGroups, which is the recommended configuration. However, a valid alternative is to have MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must be present).
--
--
--
-- /See:/ 'jobFlowInstancesConfig' smart constructor.
data JobFlowInstancesConfig = JobFlowInstancesConfig'
    { _jficEC2KeyName                     :: !(Maybe Text)
    , _jficSlaveInstanceType              :: !(Maybe Text)
    , _jficInstanceCount                  :: !(Maybe Int)
    , _jficEmrManagedSlaveSecurityGroup   :: !(Maybe Text)
    , _jficAdditionalSlaveSecurityGroups  :: !(Maybe [Text])
    , _jficHadoopVersion                  :: !(Maybe Text)
    , _jficAdditionalMasterSecurityGroups :: !(Maybe [Text])
    , _jficEmrManagedMasterSecurityGroup  :: !(Maybe Text)
    , _jficEC2SubnetId                    :: !(Maybe Text)
    , _jficMasterInstanceType             :: !(Maybe Text)
    , _jficInstanceGroups                 :: !(Maybe [InstanceGroupConfig])
    , _jficKeepJobFlowAliveWhenNoSteps    :: !(Maybe Bool)
    , _jficServiceAccessSecurityGroup     :: !(Maybe Text)
    , _jficTerminationProtected           :: !(Maybe Bool)
    , _jficPlacement                      :: !(Maybe PlacementType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'JobFlowInstancesConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jficEC2KeyName' - The name of the EC2 key pair that can be used to ssh to the master node as the user called "hadoop."
--
-- * 'jficSlaveInstanceType' - The EC2 instance type of the slave nodes.
--
-- * 'jficInstanceCount' - The number of EC2 instances used to execute the job flow.
--
-- * 'jficEmrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the slave nodes.
--
-- * 'jficAdditionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the slave nodes.
--
-- * 'jficHadoopVersion' - The Hadoop version for the job flow. Valid inputs are "0.18" (deprecated), "0.20" (deprecated), "0.20.205" (deprecated), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the AmiVersion parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
--
-- * 'jficAdditionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- * 'jficEmrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- * 'jficEC2SubnetId' - To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the job flow to launch. If you do not specify this value, the job flow is launched in the normal Amazon Web Services cloud, outside of an Amazon VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance type for nodes of a job flow launched in a Amazon VPC.
--
-- * 'jficMasterInstanceType' - The EC2 instance type of the master node.
--
-- * 'jficInstanceGroups' - Configuration for the job flow's instance groups.
--
-- * 'jficKeepJobFlowAliveWhenNoSteps' - Specifies whether the job flow should be kept alive after completing all steps.
--
-- * 'jficServiceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- * 'jficTerminationProtected' - Specifies whether to lock the job flow to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job flow error.
--
-- * 'jficPlacement' - The Availability Zone the job flow will run in.
jobFlowInstancesConfig
    :: JobFlowInstancesConfig
jobFlowInstancesConfig =
    JobFlowInstancesConfig'
    { _jficEC2KeyName = Nothing
    , _jficSlaveInstanceType = Nothing
    , _jficInstanceCount = Nothing
    , _jficEmrManagedSlaveSecurityGroup = Nothing
    , _jficAdditionalSlaveSecurityGroups = Nothing
    , _jficHadoopVersion = Nothing
    , _jficAdditionalMasterSecurityGroups = Nothing
    , _jficEmrManagedMasterSecurityGroup = Nothing
    , _jficEC2SubnetId = Nothing
    , _jficMasterInstanceType = Nothing
    , _jficInstanceGroups = Nothing
    , _jficKeepJobFlowAliveWhenNoSteps = Nothing
    , _jficServiceAccessSecurityGroup = Nothing
    , _jficTerminationProtected = Nothing
    , _jficPlacement = Nothing
    }

-- | The name of the EC2 key pair that can be used to ssh to the master node as the user called "hadoop."
jficEC2KeyName :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEC2KeyName = lens _jficEC2KeyName (\ s a -> s{_jficEC2KeyName = a});

-- | The EC2 instance type of the slave nodes.
jficSlaveInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficSlaveInstanceType = lens _jficSlaveInstanceType (\ s a -> s{_jficSlaveInstanceType = a});

-- | The number of EC2 instances used to execute the job flow.
jficInstanceCount :: Lens' JobFlowInstancesConfig (Maybe Int)
jficInstanceCount = lens _jficInstanceCount (\ s a -> s{_jficInstanceCount = a});

-- | The identifier of the Amazon EC2 security group for the slave nodes.
jficEmrManagedSlaveSecurityGroup :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEmrManagedSlaveSecurityGroup = lens _jficEmrManagedSlaveSecurityGroup (\ s a -> s{_jficEmrManagedSlaveSecurityGroup = a});

-- | A list of additional Amazon EC2 security group IDs for the slave nodes.
jficAdditionalSlaveSecurityGroups :: Lens' JobFlowInstancesConfig [Text]
jficAdditionalSlaveSecurityGroups = lens _jficAdditionalSlaveSecurityGroups (\ s a -> s{_jficAdditionalSlaveSecurityGroups = a}) . _Default . _Coerce;

-- | The Hadoop version for the job flow. Valid inputs are "0.18" (deprecated), "0.20" (deprecated), "0.20.205" (deprecated), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the AmiVersion parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
jficHadoopVersion :: Lens' JobFlowInstancesConfig (Maybe Text)
jficHadoopVersion = lens _jficHadoopVersion (\ s a -> s{_jficHadoopVersion = a});

-- | A list of additional Amazon EC2 security group IDs for the master node.
jficAdditionalMasterSecurityGroups :: Lens' JobFlowInstancesConfig [Text]
jficAdditionalMasterSecurityGroups = lens _jficAdditionalMasterSecurityGroups (\ s a -> s{_jficAdditionalMasterSecurityGroups = a}) . _Default . _Coerce;

-- | The identifier of the Amazon EC2 security group for the master node.
jficEmrManagedMasterSecurityGroup :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEmrManagedMasterSecurityGroup = lens _jficEmrManagedMasterSecurityGroup (\ s a -> s{_jficEmrManagedMasterSecurityGroup = a});

-- | To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the job flow to launch. If you do not specify this value, the job flow is launched in the normal Amazon Web Services cloud, outside of an Amazon VPC. Amazon VPC currently does not support cluster compute quadruple extra large (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance type for nodes of a job flow launched in a Amazon VPC.
jficEC2SubnetId :: Lens' JobFlowInstancesConfig (Maybe Text)
jficEC2SubnetId = lens _jficEC2SubnetId (\ s a -> s{_jficEC2SubnetId = a});

-- | The EC2 instance type of the master node.
jficMasterInstanceType :: Lens' JobFlowInstancesConfig (Maybe Text)
jficMasterInstanceType = lens _jficMasterInstanceType (\ s a -> s{_jficMasterInstanceType = a});

-- | Configuration for the job flow's instance groups.
jficInstanceGroups :: Lens' JobFlowInstancesConfig [InstanceGroupConfig]
jficInstanceGroups = lens _jficInstanceGroups (\ s a -> s{_jficInstanceGroups = a}) . _Default . _Coerce;

-- | Specifies whether the job flow should be kept alive after completing all steps.
jficKeepJobFlowAliveWhenNoSteps :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficKeepJobFlowAliveWhenNoSteps = lens _jficKeepJobFlowAliveWhenNoSteps (\ s a -> s{_jficKeepJobFlowAliveWhenNoSteps = a});

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
jficServiceAccessSecurityGroup :: Lens' JobFlowInstancesConfig (Maybe Text)
jficServiceAccessSecurityGroup = lens _jficServiceAccessSecurityGroup (\ s a -> s{_jficServiceAccessSecurityGroup = a});

-- | Specifies whether to lock the job flow to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job flow error.
jficTerminationProtected :: Lens' JobFlowInstancesConfig (Maybe Bool)
jficTerminationProtected = lens _jficTerminationProtected (\ s a -> s{_jficTerminationProtected = a});

-- | The Availability Zone the job flow will run in.
jficPlacement :: Lens' JobFlowInstancesConfig (Maybe PlacementType)
jficPlacement = lens _jficPlacement (\ s a -> s{_jficPlacement = a});

instance Hashable JobFlowInstancesConfig

instance NFData JobFlowInstancesConfig

instance ToJSON JobFlowInstancesConfig where
        toJSON JobFlowInstancesConfig'{..}
          = object
              (catMaybes
                 [("Ec2KeyName" .=) <$> _jficEC2KeyName,
                  ("SlaveInstanceType" .=) <$> _jficSlaveInstanceType,
                  ("InstanceCount" .=) <$> _jficInstanceCount,
                  ("EmrManagedSlaveSecurityGroup" .=) <$>
                    _jficEmrManagedSlaveSecurityGroup,
                  ("AdditionalSlaveSecurityGroups" .=) <$>
                    _jficAdditionalSlaveSecurityGroups,
                  ("HadoopVersion" .=) <$> _jficHadoopVersion,
                  ("AdditionalMasterSecurityGroups" .=) <$>
                    _jficAdditionalMasterSecurityGroups,
                  ("EmrManagedMasterSecurityGroup" .=) <$>
                    _jficEmrManagedMasterSecurityGroup,
                  ("Ec2SubnetId" .=) <$> _jficEC2SubnetId,
                  ("MasterInstanceType" .=) <$>
                    _jficMasterInstanceType,
                  ("InstanceGroups" .=) <$> _jficInstanceGroups,
                  ("KeepJobFlowAliveWhenNoSteps" .=) <$>
                    _jficKeepJobFlowAliveWhenNoSteps,
                  ("ServiceAccessSecurityGroup" .=) <$>
                    _jficServiceAccessSecurityGroup,
                  ("TerminationProtected" .=) <$>
                    _jficTerminationProtected,
                  ("Placement" .=) <$> _jficPlacement])

-- | A key value pair.
--
--
--
-- /See:/ 'keyValue' smart constructor.
data KeyValue = KeyValue'
    { _kvValue :: !(Maybe Text)
    , _kvKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvValue' - The value part of the identified key.
--
-- * 'kvKey' - The unique identifier of a key value pair.
keyValue
    :: KeyValue
keyValue =
    KeyValue'
    { _kvValue = Nothing
    , _kvKey = Nothing
    }

-- | The value part of the identified key.
kvValue :: Lens' KeyValue (Maybe Text)
kvValue = lens _kvValue (\ s a -> s{_kvValue = a});

-- | The unique identifier of a key value pair.
kvKey :: Lens' KeyValue (Maybe Text)
kvKey = lens _kvKey (\ s a -> s{_kvKey = a});

instance Hashable KeyValue

instance NFData KeyValue

instance ToJSON KeyValue where
        toJSON KeyValue'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _kvValue, ("Key" .=) <$> _kvKey])

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a @Name@ in CloudWatch), Value pair. By default, Amazon EMR uses one dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable representing the cluster ID, which is @> {emr:cluster_id}@ . This enables the rule to bootstrap when the cluster ID becomes available, and also enables a single automatic scaling policy to be reused for multiple clusters and instance groups.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
    { _mdValue :: !(Maybe Text)
    , _mdKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdValue' - The dimension value.
--
-- * 'mdKey' - The dimension name.
metricDimension
    :: MetricDimension
metricDimension =
    MetricDimension'
    { _mdValue = Nothing
    , _mdKey = Nothing
    }

-- | The dimension value.
mdValue :: Lens' MetricDimension (Maybe Text)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a});

-- | The dimension name.
mdKey :: Lens' MetricDimension (Maybe Text)
mdKey = lens _mdKey (\ s a -> s{_mdKey = a});

instance FromJSON MetricDimension where
        parseJSON
          = withObject "MetricDimension"
              (\ x ->
                 MetricDimension' <$>
                   (x .:? "Value") <*> (x .:? "Key"))

instance Hashable MetricDimension

instance NFData MetricDimension

instance ToJSON MetricDimension where
        toJSON MetricDimension'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _mdValue, ("Key" .=) <$> _mdKey])

-- | The Amazon EC2 location for the job flow.
--
--
--
-- /See:/ 'placementType' smart constructor.
newtype PlacementType = PlacementType'
    { _ptAvailabilityZone :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptAvailabilityZone' - The Amazon EC2 Availability Zone for the job flow.
placementType
    :: Text -- ^ 'ptAvailabilityZone'
    -> PlacementType
placementType pAvailabilityZone_ =
    PlacementType'
    { _ptAvailabilityZone = pAvailabilityZone_
    }

-- | The Amazon EC2 Availability Zone for the job flow.
ptAvailabilityZone :: Lens' PlacementType Text
ptAvailabilityZone = lens _ptAvailabilityZone (\ s a -> s{_ptAvailabilityZone = a});

instance Hashable PlacementType

instance NFData PlacementType

instance ToJSON PlacementType where
        toJSON PlacementType'{..}
          = object
              (catMaybes
                 [Just ("AvailabilityZone" .= _ptAvailabilityZone)])

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
--
--
-- /See:/ 'scalingAction' smart constructor.
data ScalingAction = ScalingAction'
    { _saMarket                           :: !(Maybe MarketType)
    , _saSimpleScalingPolicyConfiguration :: !SimpleScalingPolicyConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMarket' - Not available for instance groups. Instance groups use the market type specified for the group.
--
-- * 'saSimpleScalingPolicyConfiguration' - The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
scalingAction
    :: SimpleScalingPolicyConfiguration -- ^ 'saSimpleScalingPolicyConfiguration'
    -> ScalingAction
scalingAction pSimpleScalingPolicyConfiguration_ =
    ScalingAction'
    { _saMarket = Nothing
    , _saSimpleScalingPolicyConfiguration = pSimpleScalingPolicyConfiguration_
    }

-- | Not available for instance groups. Instance groups use the market type specified for the group.
saMarket :: Lens' ScalingAction (Maybe MarketType)
saMarket = lens _saMarket (\ s a -> s{_saMarket = a});

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
saSimpleScalingPolicyConfiguration :: Lens' ScalingAction SimpleScalingPolicyConfiguration
saSimpleScalingPolicyConfiguration = lens _saSimpleScalingPolicyConfiguration (\ s a -> s{_saSimpleScalingPolicyConfiguration = a});

instance FromJSON ScalingAction where
        parseJSON
          = withObject "ScalingAction"
              (\ x ->
                 ScalingAction' <$>
                   (x .:? "Market") <*>
                     (x .: "SimpleScalingPolicyConfiguration"))

instance Hashable ScalingAction

instance NFData ScalingAction

instance ToJSON ScalingAction where
        toJSON ScalingAction'{..}
          = object
              (catMaybes
                 [("Market" .=) <$> _saMarket,
                  Just
                    ("SimpleScalingPolicyConfiguration" .=
                       _saSimpleScalingPolicyConfiguration)])

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.
--
--
--
-- /See:/ 'scalingConstraints' smart constructor.
data ScalingConstraints = ScalingConstraints'
    { _scMinCapacity :: !Int
    , _scMaxCapacity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMinCapacity' - The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
--
-- * 'scMaxCapacity' - The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
scalingConstraints
    :: Int -- ^ 'scMinCapacity'
    -> Int -- ^ 'scMaxCapacity'
    -> ScalingConstraints
scalingConstraints pMinCapacity_ pMaxCapacity_ =
    ScalingConstraints'
    { _scMinCapacity = pMinCapacity_
    , _scMaxCapacity = pMaxCapacity_
    }

-- | The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
scMinCapacity :: Lens' ScalingConstraints Int
scMinCapacity = lens _scMinCapacity (\ s a -> s{_scMinCapacity = a});

-- | The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
scMaxCapacity :: Lens' ScalingConstraints Int
scMaxCapacity = lens _scMaxCapacity (\ s a -> s{_scMaxCapacity = a});

instance FromJSON ScalingConstraints where
        parseJSON
          = withObject "ScalingConstraints"
              (\ x ->
                 ScalingConstraints' <$>
                   (x .: "MinCapacity") <*> (x .: "MaxCapacity"))

instance Hashable ScalingConstraints

instance NFData ScalingConstraints

instance ToJSON ScalingConstraints where
        toJSON ScalingConstraints'{..}
          = object
              (catMaybes
                 [Just ("MinCapacity" .= _scMinCapacity),
                  Just ("MaxCapacity" .= _scMaxCapacity)])

-- | A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.
--
--
--
-- /See:/ 'scalingRule' smart constructor.
data ScalingRule = ScalingRule'
    { _srDescription :: !(Maybe Text)
    , _srName        :: !Text
    , _srAction      :: !ScalingAction
    , _srTrigger     :: !ScalingTrigger
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srDescription' - A friendly, more verbose description of the automatic scaling rule.
--
-- * 'srName' - The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
--
-- * 'srAction' - The conditions that trigger an automatic scaling activity.
--
-- * 'srTrigger' - The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
scalingRule
    :: Text -- ^ 'srName'
    -> ScalingAction -- ^ 'srAction'
    -> ScalingTrigger -- ^ 'srTrigger'
    -> ScalingRule
scalingRule pName_ pAction_ pTrigger_ =
    ScalingRule'
    { _srDescription = Nothing
    , _srName = pName_
    , _srAction = pAction_
    , _srTrigger = pTrigger_
    }

-- | A friendly, more verbose description of the automatic scaling rule.
srDescription :: Lens' ScalingRule (Maybe Text)
srDescription = lens _srDescription (\ s a -> s{_srDescription = a});

-- | The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
srName :: Lens' ScalingRule Text
srName = lens _srName (\ s a -> s{_srName = a});

-- | The conditions that trigger an automatic scaling activity.
srAction :: Lens' ScalingRule ScalingAction
srAction = lens _srAction (\ s a -> s{_srAction = a});

-- | The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
srTrigger :: Lens' ScalingRule ScalingTrigger
srTrigger = lens _srTrigger (\ s a -> s{_srTrigger = a});

instance FromJSON ScalingRule where
        parseJSON
          = withObject "ScalingRule"
              (\ x ->
                 ScalingRule' <$>
                   (x .:? "Description") <*> (x .: "Name") <*>
                     (x .: "Action")
                     <*> (x .: "Trigger"))

instance Hashable ScalingRule

instance NFData ScalingRule

instance ToJSON ScalingRule where
        toJSON ScalingRule'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _srDescription,
                  Just ("Name" .= _srName),
                  Just ("Action" .= _srAction),
                  Just ("Trigger" .= _srTrigger)])

-- | The conditions that trigger an automatic scaling activity.
--
--
--
-- /See:/ 'scalingTrigger' smart constructor.
newtype ScalingTrigger = ScalingTrigger'
    { _stCloudWatchAlarmDefinition :: CloudWatchAlarmDefinition
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCloudWatchAlarmDefinition' - The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
scalingTrigger
    :: CloudWatchAlarmDefinition -- ^ 'stCloudWatchAlarmDefinition'
    -> ScalingTrigger
scalingTrigger pCloudWatchAlarmDefinition_ =
    ScalingTrigger'
    { _stCloudWatchAlarmDefinition = pCloudWatchAlarmDefinition_
    }

-- | The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
stCloudWatchAlarmDefinition :: Lens' ScalingTrigger CloudWatchAlarmDefinition
stCloudWatchAlarmDefinition = lens _stCloudWatchAlarmDefinition (\ s a -> s{_stCloudWatchAlarmDefinition = a});

instance FromJSON ScalingTrigger where
        parseJSON
          = withObject "ScalingTrigger"
              (\ x ->
                 ScalingTrigger' <$>
                   (x .: "CloudWatchAlarmDefinition"))

instance Hashable ScalingTrigger

instance NFData ScalingTrigger

instance ToJSON ScalingTrigger where
        toJSON ScalingTrigger'{..}
          = object
              (catMaybes
                 [Just
                    ("CloudWatchAlarmDefinition" .=
                       _stCloudWatchAlarmDefinition)])

-- | Configuration of the script to run during a bootstrap action.
--
--
--
-- /See:/ 'scriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
    { _sbacArgs :: !(Maybe [Text])
    , _sbacPath :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScriptBootstrapActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbacArgs' - A list of command line arguments to pass to the bootstrap action script.
--
-- * 'sbacPath' - Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
scriptBootstrapActionConfig
    :: Text -- ^ 'sbacPath'
    -> ScriptBootstrapActionConfig
scriptBootstrapActionConfig pPath_ =
    ScriptBootstrapActionConfig'
    { _sbacArgs = Nothing
    , _sbacPath = pPath_
    }

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig [Text]
sbacArgs = lens _sbacArgs (\ s a -> s{_sbacArgs = a}) . _Default . _Coerce;

-- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig Text
sbacPath = lens _sbacPath (\ s a -> s{_sbacPath = a});

instance Hashable ScriptBootstrapActionConfig

instance NFData ScriptBootstrapActionConfig

instance ToJSON ScriptBootstrapActionConfig where
        toJSON ScriptBootstrapActionConfig'{..}
          = object
              (catMaybes
                 [("Args" .=) <$> _sbacArgs,
                  Just ("Path" .= _sbacPath)])

-- | The creation date and time, and name, of a security configuration.
--
--
--
-- /See:/ 'securityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
    { _scsName             :: !(Maybe Text)
    , _scsCreationDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SecurityConfigurationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsName' - The name of the security configuration.
--
-- * 'scsCreationDateTime' - The date and time the security configuration was created.
securityConfigurationSummary
    :: SecurityConfigurationSummary
securityConfigurationSummary =
    SecurityConfigurationSummary'
    { _scsName = Nothing
    , _scsCreationDateTime = Nothing
    }

-- | The name of the security configuration.
scsName :: Lens' SecurityConfigurationSummary (Maybe Text)
scsName = lens _scsName (\ s a -> s{_scsName = a});

-- | The date and time the security configuration was created.
scsCreationDateTime :: Lens' SecurityConfigurationSummary (Maybe UTCTime)
scsCreationDateTime = lens _scsCreationDateTime (\ s a -> s{_scsCreationDateTime = a}) . mapping _Time;

instance FromJSON SecurityConfigurationSummary where
        parseJSON
          = withObject "SecurityConfigurationSummary"
              (\ x ->
                 SecurityConfigurationSummary' <$>
                   (x .:? "Name") <*> (x .:? "CreationDateTime"))

instance Hashable SecurityConfigurationSummary

instance NFData SecurityConfigurationSummary

-- | Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.
--
--
--
-- /See:/ 'shrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
    { _spDecommissionTimeout  :: !(Maybe Int)
    , _spInstanceResizePolicy :: !(Maybe InstanceResizePolicy)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShrinkPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spDecommissionTimeout' - The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
--
-- * 'spInstanceResizePolicy' - Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
shrinkPolicy
    :: ShrinkPolicy
shrinkPolicy =
    ShrinkPolicy'
    { _spDecommissionTimeout = Nothing
    , _spInstanceResizePolicy = Nothing
    }

-- | The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
spDecommissionTimeout :: Lens' ShrinkPolicy (Maybe Int)
spDecommissionTimeout = lens _spDecommissionTimeout (\ s a -> s{_spDecommissionTimeout = a});

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
spInstanceResizePolicy :: Lens' ShrinkPolicy (Maybe InstanceResizePolicy)
spInstanceResizePolicy = lens _spInstanceResizePolicy (\ s a -> s{_spInstanceResizePolicy = a});

instance FromJSON ShrinkPolicy where
        parseJSON
          = withObject "ShrinkPolicy"
              (\ x ->
                 ShrinkPolicy' <$>
                   (x .:? "DecommissionTimeout") <*>
                     (x .:? "InstanceResizePolicy"))

instance Hashable ShrinkPolicy

instance NFData ShrinkPolicy

instance ToJSON ShrinkPolicy where
        toJSON ShrinkPolicy'{..}
          = object
              (catMaybes
                 [("DecommissionTimeout" .=) <$>
                    _spDecommissionTimeout,
                  ("InstanceResizePolicy" .=) <$>
                    _spInstanceResizePolicy])

-- | An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.
--
--
--
-- /See:/ 'simpleScalingPolicyConfiguration' smart constructor.
data SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration'
    { _sspcAdjustmentType    :: !(Maybe AdjustmentType)
    , _sspcCoolDown          :: !(Maybe Int)
    , _sspcScalingAdjustment :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SimpleScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcAdjustmentType' - The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as a decimal, for example, 0.20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
--
-- * 'sspcCoolDown' - The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
--
-- * 'sspcScalingAdjustment' - The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as a decimal. For example, -0.20 indicates a decrease in 20% increments of cluster capacity.
simpleScalingPolicyConfiguration
    :: Int -- ^ 'sspcScalingAdjustment'
    -> SimpleScalingPolicyConfiguration
simpleScalingPolicyConfiguration pScalingAdjustment_ =
    SimpleScalingPolicyConfiguration'
    { _sspcAdjustmentType = Nothing
    , _sspcCoolDown = Nothing
    , _sspcScalingAdjustment = pScalingAdjustment_
    }

-- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as a decimal, for example, 0.20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
sspcAdjustmentType :: Lens' SimpleScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\ s a -> s{_sspcAdjustmentType = a});

-- | The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
sspcCoolDown :: Lens' SimpleScalingPolicyConfiguration (Maybe Int)
sspcCoolDown = lens _sspcCoolDown (\ s a -> s{_sspcCoolDown = a});

-- | The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as a decimal. For example, -0.20 indicates a decrease in 20% increments of cluster capacity.
sspcScalingAdjustment :: Lens' SimpleScalingPolicyConfiguration Int
sspcScalingAdjustment = lens _sspcScalingAdjustment (\ s a -> s{_sspcScalingAdjustment = a});

instance FromJSON SimpleScalingPolicyConfiguration
         where
        parseJSON
          = withObject "SimpleScalingPolicyConfiguration"
              (\ x ->
                 SimpleScalingPolicyConfiguration' <$>
                   (x .:? "AdjustmentType") <*> (x .:? "CoolDown") <*>
                     (x .: "ScalingAdjustment"))

instance Hashable SimpleScalingPolicyConfiguration

instance NFData SimpleScalingPolicyConfiguration

instance ToJSON SimpleScalingPolicyConfiguration
         where
        toJSON SimpleScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("AdjustmentType" .=) <$> _sspcAdjustmentType,
                  ("CoolDown" .=) <$> _sspcCoolDown,
                  Just
                    ("ScalingAdjustment" .= _sspcScalingAdjustment)])

-- | This represents a step in a cluster.
--
--
--
-- /See:/ 'step' smart constructor.
data Step = Step'
    { _sStatus          :: !(Maybe StepStatus)
    , _sActionOnFailure :: !(Maybe ActionOnFailure)
    , _sConfig          :: !(Maybe HadoopStepConfig)
    , _sName            :: !(Maybe Text)
    , _sId              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Step' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The current execution status details of the cluster step.
--
-- * 'sActionOnFailure' - This specifies what action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
--
-- * 'sConfig' - The Hadoop job configuration of the cluster step.
--
-- * 'sName' - The name of the cluster step.
--
-- * 'sId' - The identifier of the cluster step.
step
    :: Step
step =
    Step'
    { _sStatus = Nothing
    , _sActionOnFailure = Nothing
    , _sConfig = Nothing
    , _sName = Nothing
    , _sId = Nothing
    }

-- | The current execution status details of the cluster step.
sStatus :: Lens' Step (Maybe StepStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | This specifies what action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
sActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
sActionOnFailure = lens _sActionOnFailure (\ s a -> s{_sActionOnFailure = a});

-- | The Hadoop job configuration of the cluster step.
sConfig :: Lens' Step (Maybe HadoopStepConfig)
sConfig = lens _sConfig (\ s a -> s{_sConfig = a});

-- | The name of the cluster step.
sName :: Lens' Step (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | The identifier of the cluster step.
sId :: Lens' Step (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a});

instance FromJSON Step where
        parseJSON
          = withObject "Step"
              (\ x ->
                 Step' <$>
                   (x .:? "Status") <*> (x .:? "ActionOnFailure") <*>
                     (x .:? "Config")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable Step

instance NFData Step

-- | Specification of a job flow step.
--
--
--
-- /See:/ 'stepConfig' smart constructor.
data StepConfig = StepConfig'
    { _scActionOnFailure :: !(Maybe ActionOnFailure)
    , _scName            :: !Text
    , _scHadoopJARStep   :: !HadoopJARStepConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scActionOnFailure' - The action to take if the job flow step fails.
--
-- * 'scName' - The name of the job flow step.
--
-- * 'scHadoopJARStep' - The JAR file used for the job flow step.
stepConfig
    :: Text -- ^ 'scName'
    -> HadoopJARStepConfig -- ^ 'scHadoopJARStep'
    -> StepConfig
stepConfig pName_ pHadoopJARStep_ =
    StepConfig'
    { _scActionOnFailure = Nothing
    , _scName = pName_
    , _scHadoopJARStep = pHadoopJARStep_
    }

-- | The action to take if the job flow step fails.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure = lens _scActionOnFailure (\ s a -> s{_scActionOnFailure = a});

-- | The name of the job flow step.
scName :: Lens' StepConfig Text
scName = lens _scName (\ s a -> s{_scName = a});

-- | The JAR file used for the job flow step.
scHadoopJARStep :: Lens' StepConfig HadoopJARStepConfig
scHadoopJARStep = lens _scHadoopJARStep (\ s a -> s{_scHadoopJARStep = a});

instance Hashable StepConfig

instance NFData StepConfig

instance ToJSON StepConfig where
        toJSON StepConfig'{..}
          = object
              (catMaybes
                 [("ActionOnFailure" .=) <$> _scActionOnFailure,
                  Just ("Name" .= _scName),
                  Just ("HadoopJarStep" .= _scHadoopJARStep)])

-- | The details of the step state change reason.
--
--
--
-- /See:/ 'stepStateChangeReason' smart constructor.
data StepStateChangeReason = StepStateChangeReason'
    { _sscrCode    :: !(Maybe StepStateChangeReasonCode)
    , _sscrMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscrCode' - The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
--
-- * 'sscrMessage' - The descriptive message for the state change reason.
stepStateChangeReason
    :: StepStateChangeReason
stepStateChangeReason =
    StepStateChangeReason'
    { _sscrCode = Nothing
    , _sscrMessage = Nothing
    }

-- | The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
sscrCode :: Lens' StepStateChangeReason (Maybe StepStateChangeReasonCode)
sscrCode = lens _sscrCode (\ s a -> s{_sscrCode = a});

-- | The descriptive message for the state change reason.
sscrMessage :: Lens' StepStateChangeReason (Maybe Text)
sscrMessage = lens _sscrMessage (\ s a -> s{_sscrMessage = a});

instance FromJSON StepStateChangeReason where
        parseJSON
          = withObject "StepStateChangeReason"
              (\ x ->
                 StepStateChangeReason' <$>
                   (x .:? "Code") <*> (x .:? "Message"))

instance Hashable StepStateChangeReason

instance NFData StepStateChangeReason

-- | The execution status details of the cluster step.
--
--
--
-- /See:/ 'stepStatus' smart constructor.
data StepStatus = StepStatus'
    { _ssState             :: !(Maybe StepState)
    , _ssFailureDetails    :: !(Maybe FailureDetails)
    , _ssStateChangeReason :: !(Maybe StepStateChangeReason)
    , _ssTimeline          :: !(Maybe StepTimeline)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssState' - The execution state of the cluster step.
--
-- * 'ssFailureDetails' - The details for the step failure including reason, message, and log file path where the root cause was identified.
--
-- * 'ssStateChangeReason' - The reason for the step execution status change.
--
-- * 'ssTimeline' - The timeline of the cluster step status over time.
stepStatus
    :: StepStatus
stepStatus =
    StepStatus'
    { _ssState = Nothing
    , _ssFailureDetails = Nothing
    , _ssStateChangeReason = Nothing
    , _ssTimeline = Nothing
    }

-- | The execution state of the cluster step.
ssState :: Lens' StepStatus (Maybe StepState)
ssState = lens _ssState (\ s a -> s{_ssState = a});

-- | The details for the step failure including reason, message, and log file path where the root cause was identified.
ssFailureDetails :: Lens' StepStatus (Maybe FailureDetails)
ssFailureDetails = lens _ssFailureDetails (\ s a -> s{_ssFailureDetails = a});

-- | The reason for the step execution status change.
ssStateChangeReason :: Lens' StepStatus (Maybe StepStateChangeReason)
ssStateChangeReason = lens _ssStateChangeReason (\ s a -> s{_ssStateChangeReason = a});

-- | The timeline of the cluster step status over time.
ssTimeline :: Lens' StepStatus (Maybe StepTimeline)
ssTimeline = lens _ssTimeline (\ s a -> s{_ssTimeline = a});

instance FromJSON StepStatus where
        parseJSON
          = withObject "StepStatus"
              (\ x ->
                 StepStatus' <$>
                   (x .:? "State") <*> (x .:? "FailureDetails") <*>
                     (x .:? "StateChangeReason")
                     <*> (x .:? "Timeline"))

instance Hashable StepStatus

instance NFData StepStatus

-- | The summary of the cluster step.
--
--
--
-- /See:/ 'stepSummary' smart constructor.
data StepSummary = StepSummary'
    { _ssStatus          :: !(Maybe StepStatus)
    , _ssActionOnFailure :: !(Maybe ActionOnFailure)
    , _ssConfig          :: !(Maybe HadoopStepConfig)
    , _ssName            :: !(Maybe Text)
    , _ssId              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStatus' - The current execution status details of the cluster step.
--
-- * 'ssActionOnFailure' - This specifies what action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
--
-- * 'ssConfig' - The Hadoop job configuration of the cluster step.
--
-- * 'ssName' - The name of the cluster step.
--
-- * 'ssId' - The identifier of the cluster step.
stepSummary
    :: StepSummary
stepSummary =
    StepSummary'
    { _ssStatus = Nothing
    , _ssActionOnFailure = Nothing
    , _ssConfig = Nothing
    , _ssName = Nothing
    , _ssId = Nothing
    }

-- | The current execution status details of the cluster step.
ssStatus :: Lens' StepSummary (Maybe StepStatus)
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a});

-- | This specifies what action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE.
ssActionOnFailure :: Lens' StepSummary (Maybe ActionOnFailure)
ssActionOnFailure = lens _ssActionOnFailure (\ s a -> s{_ssActionOnFailure = a});

-- | The Hadoop job configuration of the cluster step.
ssConfig :: Lens' StepSummary (Maybe HadoopStepConfig)
ssConfig = lens _ssConfig (\ s a -> s{_ssConfig = a});

-- | The name of the cluster step.
ssName :: Lens' StepSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The identifier of the cluster step.
ssId :: Lens' StepSummary (Maybe Text)
ssId = lens _ssId (\ s a -> s{_ssId = a});

instance FromJSON StepSummary where
        parseJSON
          = withObject "StepSummary"
              (\ x ->
                 StepSummary' <$>
                   (x .:? "Status") <*> (x .:? "ActionOnFailure") <*>
                     (x .:? "Config")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable StepSummary

instance NFData StepSummary

-- | The timeline of the cluster step lifecycle.
--
--
--
-- /See:/ 'stepTimeline' smart constructor.
data StepTimeline = StepTimeline'
    { _stCreationDateTime :: !(Maybe POSIX)
    , _stEndDateTime      :: !(Maybe POSIX)
    , _stStartDateTime    :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCreationDateTime' - The date and time when the cluster step was created.
--
-- * 'stEndDateTime' - The date and time when the cluster step execution completed or failed.
--
-- * 'stStartDateTime' - The date and time when the cluster step execution started.
stepTimeline
    :: StepTimeline
stepTimeline =
    StepTimeline'
    { _stCreationDateTime = Nothing
    , _stEndDateTime = Nothing
    , _stStartDateTime = Nothing
    }

-- | The date and time when the cluster step was created.
stCreationDateTime :: Lens' StepTimeline (Maybe UTCTime)
stCreationDateTime = lens _stCreationDateTime (\ s a -> s{_stCreationDateTime = a}) . mapping _Time;

-- | The date and time when the cluster step execution completed or failed.
stEndDateTime :: Lens' StepTimeline (Maybe UTCTime)
stEndDateTime = lens _stEndDateTime (\ s a -> s{_stEndDateTime = a}) . mapping _Time;

-- | The date and time when the cluster step execution started.
stStartDateTime :: Lens' StepTimeline (Maybe UTCTime)
stStartDateTime = lens _stStartDateTime (\ s a -> s{_stStartDateTime = a}) . mapping _Time;

instance FromJSON StepTimeline where
        parseJSON
          = withObject "StepTimeline"
              (\ x ->
                 StepTimeline' <$>
                   (x .:? "CreationDateTime") <*> (x .:? "EndDateTime")
                     <*> (x .:? "StartDateTime"))

instance Hashable StepTimeline

instance NFData StepTimeline

-- | The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.
--
--
--
-- /See:/ 'supportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
    { _spcArgs :: !(Maybe [Text])
    , _spcName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SupportedProductConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spcArgs' - The list of user-supplied arguments.
--
-- * 'spcName' - The name of the product configuration.
supportedProductConfig
    :: SupportedProductConfig
supportedProductConfig =
    SupportedProductConfig'
    { _spcArgs = Nothing
    , _spcName = Nothing
    }

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig [Text]
spcArgs = lens _spcArgs (\ s a -> s{_spcArgs = a}) . _Default . _Coerce;

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\ s a -> s{_spcName = a});

instance Hashable SupportedProductConfig

instance NFData SupportedProductConfig

instance ToJSON SupportedProductConfig where
        toJSON SupportedProductConfig'{..}
          = object
              (catMaybes
                 [("Args" .=) <$> _spcArgs, ("Name" .=) <$> _spcName])

-- | A key/value pair containing user-defined metadata that you can associate with an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources> .
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A user-defined value, which is optional in a tag. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources> .
--
-- * 'tagKey' - A user-defined key, which is the minimum required information for a valid tag. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources> .
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | A user-defined value, which is optional in a tag. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources> .
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A user-defined key, which is the minimum required information for a valid tag. For more information, see <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources> .
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
--
--
--
-- /See:/ 'volumeSpecification' smart constructor.
data VolumeSpecification = VolumeSpecification'
    { _vsIOPS       :: !(Maybe Int)
    , _vsVolumeType :: !Text
    , _vsSizeInGB   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VolumeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsIOPS' - The number of I/O operations per second (IOPS) that the volume supports.
--
-- * 'vsVolumeType' - The volume type. Volume types supported are gp2, io1, standard.
--
-- * 'vsSizeInGB' - The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
volumeSpecification
    :: Text -- ^ 'vsVolumeType'
    -> Int -- ^ 'vsSizeInGB'
    -> VolumeSpecification
volumeSpecification pVolumeType_ pSizeInGB_ =
    VolumeSpecification'
    { _vsIOPS = Nothing
    , _vsVolumeType = pVolumeType_
    , _vsSizeInGB = pSizeInGB_
    }

-- | The number of I/O operations per second (IOPS) that the volume supports.
vsIOPS :: Lens' VolumeSpecification (Maybe Int)
vsIOPS = lens _vsIOPS (\ s a -> s{_vsIOPS = a});

-- | The volume type. Volume types supported are gp2, io1, standard.
vsVolumeType :: Lens' VolumeSpecification Text
vsVolumeType = lens _vsVolumeType (\ s a -> s{_vsVolumeType = a});

-- | The volume size, in gibibytes (GiB). This can be a number from 1 - 1024. If the volume type is EBS-optimized, the minimum value is 10.
vsSizeInGB :: Lens' VolumeSpecification Int
vsSizeInGB = lens _vsSizeInGB (\ s a -> s{_vsSizeInGB = a});

instance FromJSON VolumeSpecification where
        parseJSON
          = withObject "VolumeSpecification"
              (\ x ->
                 VolumeSpecification' <$>
                   (x .:? "Iops") <*> (x .: "VolumeType") <*>
                     (x .: "SizeInGB"))

instance Hashable VolumeSpecification

instance NFData VolumeSpecification

instance ToJSON VolumeSpecification where
        toJSON VolumeSpecification'{..}
          = object
              (catMaybes
                 [("Iops" .=) <$> _vsIOPS,
                  Just ("VolumeType" .= _vsVolumeType),
                  Just ("SizeInGB" .= _vsSizeInGB)])
