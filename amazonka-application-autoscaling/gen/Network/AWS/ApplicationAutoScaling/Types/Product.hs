{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Product where

import           Network.AWS.ApplicationAutoScaling.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Represents a CloudWatch alarm associated with a scaling policy.
--
--
--
-- /See:/ 'alarm' smart constructor.
data Alarm = Alarm'
    { _aAlarmName :: !Text
    , _aAlarmARN  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlarmName' - The name of the alarm.
--
-- * 'aAlarmARN' - The Amazon Resource Name (ARN) of the alarm.
alarm
    :: Text -- ^ 'aAlarmName'
    -> Text -- ^ 'aAlarmARN'
    -> Alarm
alarm pAlarmName_ pAlarmARN_ =
    Alarm'
    { _aAlarmName = pAlarmName_
    , _aAlarmARN = pAlarmARN_
    }

-- | The name of the alarm.
aAlarmName :: Lens' Alarm Text
aAlarmName = lens _aAlarmName (\ s a -> s{_aAlarmName = a});

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm Text
aAlarmARN = lens _aAlarmARN (\ s a -> s{_aAlarmARN = a});

instance FromJSON Alarm where
        parseJSON
          = withObject "Alarm"
              (\ x ->
                 Alarm' <$> (x .: "AlarmName") <*> (x .: "AlarmARN"))

instance Hashable Alarm

instance NFData Alarm

-- | Represents a scalable target.
--
--
--
-- /See:/ 'scalableTarget' smart constructor.
data ScalableTarget = ScalableTarget'
    { _stServiceNamespace  :: !ServiceNamespace
    , _stResourceId        :: !Text
    , _stScalableDimension :: !ScalableDimension
    , _stMinCapacity       :: !Int
    , _stMaxCapacity       :: !Int
    , _stRoleARN           :: !Text
    , _stCreationTime      :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'stResourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
-- * 'stScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
-- * 'stMinCapacity' - The minimum value to scale to in response to a scale in event.
--
-- * 'stMaxCapacity' - The maximum value to scale to in response to a scale out event.
--
-- * 'stRoleARN' - The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
--
-- * 'stCreationTime' - The Unix timestamp for when the scalable target was created.
scalableTarget
    :: ServiceNamespace -- ^ 'stServiceNamespace'
    -> Text -- ^ 'stResourceId'
    -> ScalableDimension -- ^ 'stScalableDimension'
    -> Int -- ^ 'stMinCapacity'
    -> Int -- ^ 'stMaxCapacity'
    -> Text -- ^ 'stRoleARN'
    -> UTCTime -- ^ 'stCreationTime'
    -> ScalableTarget
scalableTarget pServiceNamespace_ pResourceId_ pScalableDimension_ pMinCapacity_ pMaxCapacity_ pRoleARN_ pCreationTime_ =
    ScalableTarget'
    { _stServiceNamespace = pServiceNamespace_
    , _stResourceId = pResourceId_
    , _stScalableDimension = pScalableDimension_
    , _stMinCapacity = pMinCapacity_
    , _stMaxCapacity = pMaxCapacity_
    , _stRoleARN = pRoleARN_
    , _stCreationTime = _Time # pCreationTime_
    }

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
stServiceNamespace :: Lens' ScalableTarget ServiceNamespace
stServiceNamespace = lens _stServiceNamespace (\ s a -> s{_stServiceNamespace = a});

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
stResourceId :: Lens' ScalableTarget Text
stResourceId = lens _stResourceId (\ s a -> s{_stResourceId = a});

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
stScalableDimension :: Lens' ScalableTarget ScalableDimension
stScalableDimension = lens _stScalableDimension (\ s a -> s{_stScalableDimension = a});

-- | The minimum value to scale to in response to a scale in event.
stMinCapacity :: Lens' ScalableTarget Int
stMinCapacity = lens _stMinCapacity (\ s a -> s{_stMinCapacity = a});

-- | The maximum value to scale to in response to a scale out event.
stMaxCapacity :: Lens' ScalableTarget Int
stMaxCapacity = lens _stMaxCapacity (\ s a -> s{_stMaxCapacity = a});

-- | The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
stRoleARN :: Lens' ScalableTarget Text
stRoleARN = lens _stRoleARN (\ s a -> s{_stRoleARN = a});

-- | The Unix timestamp for when the scalable target was created.
stCreationTime :: Lens' ScalableTarget UTCTime
stCreationTime = lens _stCreationTime (\ s a -> s{_stCreationTime = a}) . _Time;

instance FromJSON ScalableTarget where
        parseJSON
          = withObject "ScalableTarget"
              (\ x ->
                 ScalableTarget' <$>
                   (x .: "ServiceNamespace") <*> (x .: "ResourceId") <*>
                     (x .: "ScalableDimension")
                     <*> (x .: "MinCapacity")
                     <*> (x .: "MaxCapacity")
                     <*> (x .: "RoleARN")
                     <*> (x .: "CreationTime"))

instance Hashable ScalableTarget

instance NFData ScalableTarget

-- | Represents a scaling activity.
--
--
--
-- /See:/ 'scalingActivity' smart constructor.
data ScalingActivity = ScalingActivity'
    { _saStatusMessage     :: !(Maybe Text)
    , _saEndTime           :: !(Maybe POSIX)
    , _saDetails           :: !(Maybe Text)
    , _saActivityId        :: !Text
    , _saServiceNamespace  :: !ServiceNamespace
    , _saResourceId        :: !Text
    , _saScalableDimension :: !ScalableDimension
    , _saDescription       :: !Text
    , _saCause             :: !Text
    , _saStartTime         :: !POSIX
    , _saStatusCode        :: !ScalingActivityStatusCode
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saStatusMessage' - A simple message about the current status of the scaling activity.
--
-- * 'saEndTime' - The Unix timestamp for when the scaling activity ended.
--
-- * 'saDetails' - The details about the scaling activity.
--
-- * 'saActivityId' - The unique identifier of the scaling activity.
--
-- * 'saServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'saResourceId' - The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
-- * 'saScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
-- * 'saDescription' - A simple description of what action the scaling activity intends to accomplish.
--
-- * 'saCause' - A simple description of what caused the scaling activity to happen.
--
-- * 'saStartTime' - The Unix timestamp for when the scaling activity began.
--
-- * 'saStatusCode' - Indicates the status of the scaling activity.
scalingActivity
    :: Text -- ^ 'saActivityId'
    -> ServiceNamespace -- ^ 'saServiceNamespace'
    -> Text -- ^ 'saResourceId'
    -> ScalableDimension -- ^ 'saScalableDimension'
    -> Text -- ^ 'saDescription'
    -> Text -- ^ 'saCause'
    -> UTCTime -- ^ 'saStartTime'
    -> ScalingActivityStatusCode -- ^ 'saStatusCode'
    -> ScalingActivity
scalingActivity pActivityId_ pServiceNamespace_ pResourceId_ pScalableDimension_ pDescription_ pCause_ pStartTime_ pStatusCode_ =
    ScalingActivity'
    { _saStatusMessage = Nothing
    , _saEndTime = Nothing
    , _saDetails = Nothing
    , _saActivityId = pActivityId_
    , _saServiceNamespace = pServiceNamespace_
    , _saResourceId = pResourceId_
    , _saScalableDimension = pScalableDimension_
    , _saDescription = pDescription_
    , _saCause = pCause_
    , _saStartTime = _Time # pStartTime_
    , _saStatusCode = pStatusCode_
    }

-- | A simple message about the current status of the scaling activity.
saStatusMessage :: Lens' ScalingActivity (Maybe Text)
saStatusMessage = lens _saStatusMessage (\ s a -> s{_saStatusMessage = a});

-- | The Unix timestamp for when the scaling activity ended.
saEndTime :: Lens' ScalingActivity (Maybe UTCTime)
saEndTime = lens _saEndTime (\ s a -> s{_saEndTime = a}) . mapping _Time;

-- | The details about the scaling activity.
saDetails :: Lens' ScalingActivity (Maybe Text)
saDetails = lens _saDetails (\ s a -> s{_saDetails = a});

-- | The unique identifier of the scaling activity.
saActivityId :: Lens' ScalingActivity Text
saActivityId = lens _saActivityId (\ s a -> s{_saActivityId = a});

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
saServiceNamespace :: Lens' ScalingActivity ServiceNamespace
saServiceNamespace = lens _saServiceNamespace (\ s a -> s{_saServiceNamespace = a});

-- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
saResourceId :: Lens' ScalingActivity Text
saResourceId = lens _saResourceId (\ s a -> s{_saResourceId = a});

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
saScalableDimension :: Lens' ScalingActivity ScalableDimension
saScalableDimension = lens _saScalableDimension (\ s a -> s{_saScalableDimension = a});

-- | A simple description of what action the scaling activity intends to accomplish.
saDescription :: Lens' ScalingActivity Text
saDescription = lens _saDescription (\ s a -> s{_saDescription = a});

-- | A simple description of what caused the scaling activity to happen.
saCause :: Lens' ScalingActivity Text
saCause = lens _saCause (\ s a -> s{_saCause = a});

-- | The Unix timestamp for when the scaling activity began.
saStartTime :: Lens' ScalingActivity UTCTime
saStartTime = lens _saStartTime (\ s a -> s{_saStartTime = a}) . _Time;

-- | Indicates the status of the scaling activity.
saStatusCode :: Lens' ScalingActivity ScalingActivityStatusCode
saStatusCode = lens _saStatusCode (\ s a -> s{_saStatusCode = a});

instance FromJSON ScalingActivity where
        parseJSON
          = withObject "ScalingActivity"
              (\ x ->
                 ScalingActivity' <$>
                   (x .:? "StatusMessage") <*> (x .:? "EndTime") <*>
                     (x .:? "Details")
                     <*> (x .: "ActivityId")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "Description")
                     <*> (x .: "Cause")
                     <*> (x .: "StartTime")
                     <*> (x .: "StatusCode"))

instance Hashable ScalingActivity

instance NFData ScalingActivity

-- | Represents a scaling policy.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
    { _spStepScalingPolicyConfiguration :: !(Maybe StepScalingPolicyConfiguration)
    , _spAlarms                         :: !(Maybe [Alarm])
    , _spPolicyARN                      :: !Text
    , _spPolicyName                     :: !Text
    , _spServiceNamespace               :: !ServiceNamespace
    , _spResourceId                     :: !Text
    , _spScalableDimension              :: !ScalableDimension
    , _spPolicyType                     :: !PolicyType
    , _spCreationTime                   :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStepScalingPolicyConfiguration' - The configuration for the step scaling policy.
--
-- * 'spAlarms' - The CloudWatch alarms associated with the scaling policy.
--
-- * 'spPolicyARN' - The Amazon Resource Name (ARN) of the scaling policy.
--
-- * 'spPolicyName' - The name of the scaling policy.
--
-- * 'spServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'spResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
-- * 'spScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
-- * 'spPolicyType' - The scaling policy type.
--
-- * 'spCreationTime' - The Unix timestamp for when the scaling policy was created.
scalingPolicy
    :: Text -- ^ 'spPolicyARN'
    -> Text -- ^ 'spPolicyName'
    -> ServiceNamespace -- ^ 'spServiceNamespace'
    -> Text -- ^ 'spResourceId'
    -> ScalableDimension -- ^ 'spScalableDimension'
    -> PolicyType -- ^ 'spPolicyType'
    -> UTCTime -- ^ 'spCreationTime'
    -> ScalingPolicy
scalingPolicy pPolicyARN_ pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ pPolicyType_ pCreationTime_ =
    ScalingPolicy'
    { _spStepScalingPolicyConfiguration = Nothing
    , _spAlarms = Nothing
    , _spPolicyARN = pPolicyARN_
    , _spPolicyName = pPolicyName_
    , _spServiceNamespace = pServiceNamespace_
    , _spResourceId = pResourceId_
    , _spScalableDimension = pScalableDimension_
    , _spPolicyType = pPolicyType_
    , _spCreationTime = _Time # pCreationTime_
    }

-- | The configuration for the step scaling policy.
spStepScalingPolicyConfiguration :: Lens' ScalingPolicy (Maybe StepScalingPolicyConfiguration)
spStepScalingPolicyConfiguration = lens _spStepScalingPolicyConfiguration (\ s a -> s{_spStepScalingPolicyConfiguration = a});

-- | The CloudWatch alarms associated with the scaling policy.
spAlarms :: Lens' ScalingPolicy [Alarm]
spAlarms = lens _spAlarms (\ s a -> s{_spAlarms = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the scaling policy.
spPolicyARN :: Lens' ScalingPolicy Text
spPolicyARN = lens _spPolicyARN (\ s a -> s{_spPolicyARN = a});

-- | The name of the scaling policy.
spPolicyName :: Lens' ScalingPolicy Text
spPolicyName = lens _spPolicyName (\ s a -> s{_spPolicyName = a});

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
spServiceNamespace :: Lens' ScalingPolicy ServiceNamespace
spServiceNamespace = lens _spServiceNamespace (\ s a -> s{_spServiceNamespace = a});

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
spResourceId :: Lens' ScalingPolicy Text
spResourceId = lens _spResourceId (\ s a -> s{_spResourceId = a});

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
spScalableDimension :: Lens' ScalingPolicy ScalableDimension
spScalableDimension = lens _spScalableDimension (\ s a -> s{_spScalableDimension = a});

-- | The scaling policy type.
spPolicyType :: Lens' ScalingPolicy PolicyType
spPolicyType = lens _spPolicyType (\ s a -> s{_spPolicyType = a});

-- | The Unix timestamp for when the scaling policy was created.
spCreationTime :: Lens' ScalingPolicy UTCTime
spCreationTime = lens _spCreationTime (\ s a -> s{_spCreationTime = a}) . _Time;

instance FromJSON ScalingPolicy where
        parseJSON
          = withObject "ScalingPolicy"
              (\ x ->
                 ScalingPolicy' <$>
                   (x .:? "StepScalingPolicyConfiguration") <*>
                     (x .:? "Alarms" .!= mempty)
                     <*> (x .: "PolicyARN")
                     <*> (x .: "PolicyName")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "PolicyType")
                     <*> (x .: "CreationTime"))

instance Hashable ScalingPolicy

instance NFData ScalingPolicy

-- | Represents a step adjustment for a 'StepScalingPolicyConfiguration' . Describes an adjustment based on the difference between the value of the aggregated CloudWatch metric and the breach threshold that you've defined for the alarm.
--
--
-- For the following examples, suppose that you have an alarm with a breach threshold of 50:
--
--     * To trigger the adjustment when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.
--
--     * To trigger the adjustment when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.
--
--
--
-- There are a few rules for the step adjustments for your step policy:
--
--     * The ranges of your step adjustments can't overlap or have a gap.
--
--     * At most one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.
--
--     * At most one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.
--
--     * The upper and lower bound can't be null in the same step adjustment.
--
--
--
--
-- /See:/ 'stepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
    { _saMetricIntervalLowerBound :: !(Maybe Double)
    , _saMetricIntervalUpperBound :: !(Maybe Double)
    , _saScalingAdjustment        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepAdjustment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMetricIntervalLowerBound' - The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
--
-- * 'saMetricIntervalUpperBound' - The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
--
-- * 'saScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current scalable dimension while a negative number removes from the current scalable dimension.
stepAdjustment
    :: Int -- ^ 'saScalingAdjustment'
    -> StepAdjustment
stepAdjustment pScalingAdjustment_ =
    StepAdjustment'
    { _saMetricIntervalLowerBound = Nothing
    , _saMetricIntervalUpperBound = Nothing
    , _saScalingAdjustment = pScalingAdjustment_
    }

-- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
saMetricIntervalLowerBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalLowerBound = lens _saMetricIntervalLowerBound (\ s a -> s{_saMetricIntervalLowerBound = a});

-- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
saMetricIntervalUpperBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalUpperBound = lens _saMetricIntervalUpperBound (\ s a -> s{_saMetricIntervalUpperBound = a});

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current scalable dimension while a negative number removes from the current scalable dimension.
saScalingAdjustment :: Lens' StepAdjustment Int
saScalingAdjustment = lens _saScalingAdjustment (\ s a -> s{_saScalingAdjustment = a});

instance FromJSON StepAdjustment where
        parseJSON
          = withObject "StepAdjustment"
              (\ x ->
                 StepAdjustment' <$>
                   (x .:? "MetricIntervalLowerBound") <*>
                     (x .:? "MetricIntervalUpperBound")
                     <*> (x .: "ScalingAdjustment"))

instance Hashable StepAdjustment

instance NFData StepAdjustment

instance ToJSON StepAdjustment where
        toJSON StepAdjustment'{..}
          = object
              (catMaybes
                 [("MetricIntervalLowerBound" .=) <$>
                    _saMetricIntervalLowerBound,
                  ("MetricIntervalUpperBound" .=) <$>
                    _saMetricIntervalUpperBound,
                  Just ("ScalingAdjustment" .= _saScalingAdjustment)])

-- | Represents a step scaling policy configuration.
--
--
--
-- /See:/ 'stepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'
    { _sspcStepAdjustments        :: !(Maybe [StepAdjustment])
    , _sspcAdjustmentType         :: !(Maybe AdjustmentType)
    , _sspcCooldown               :: !(Maybe Int)
    , _sspcMetricAggregationType  :: !(Maybe MetricAggregationType)
    , _sspcMinAdjustmentMagnitude :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StepScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach.
--
-- * 'sspcAdjustmentType' - The adjustment type, which specifies how the @ScalingAdjustment@ parameter in a 'StepAdjustment' is interpreted.
--
-- * 'sspcCooldown' - The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale out policies, while @Cooldown@ is in effect, the capacity that has been added by the previous scale out event that initiated the @Cooldown@ is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a @Cooldown@ period of 5 minutes starts. During the @Cooldown@ period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale out event are considered part of that capacity and only 1 additional task is added to the desired count. For scale in policies, the @Cooldown@ period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the @Cooldown@ period after a scale-in, Application Auto Scaling scales out your scalable target immediately.
--
-- * 'sspcMetricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ .
--
-- * 'sspcMinAdjustmentMagnitude' - The minimum number to adjust your scalable dimension as a result of a scaling activity. If the adjustment type is @PercentChangeInCapacity@ , the scaling policy changes the scalable dimension of the scalable target by this amount.
stepScalingPolicyConfiguration
    :: StepScalingPolicyConfiguration
stepScalingPolicyConfiguration =
    StepScalingPolicyConfiguration'
    { _sspcStepAdjustments = Nothing
    , _sspcAdjustmentType = Nothing
    , _sspcCooldown = Nothing
    , _sspcMetricAggregationType = Nothing
    , _sspcMinAdjustmentMagnitude = Nothing
    }

-- | A set of adjustments that enable you to scale based on the size of the alarm breach.
sspcStepAdjustments :: Lens' StepScalingPolicyConfiguration [StepAdjustment]
sspcStepAdjustments = lens _sspcStepAdjustments (\ s a -> s{_sspcStepAdjustments = a}) . _Default . _Coerce;

-- | The adjustment type, which specifies how the @ScalingAdjustment@ parameter in a 'StepAdjustment' is interpreted.
sspcAdjustmentType :: Lens' StepScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\ s a -> s{_sspcAdjustmentType = a});

-- | The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale out policies, while @Cooldown@ is in effect, the capacity that has been added by the previous scale out event that initiated the @Cooldown@ is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a @Cooldown@ period of 5 minutes starts. During the @Cooldown@ period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale out event are considered part of that capacity and only 1 additional task is added to the desired count. For scale in policies, the @Cooldown@ period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the @Cooldown@ period after a scale-in, Application Auto Scaling scales out your scalable target immediately.
sspcCooldown :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcCooldown = lens _sspcCooldown (\ s a -> s{_sspcCooldown = a});

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ .
sspcMetricAggregationType :: Lens' StepScalingPolicyConfiguration (Maybe MetricAggregationType)
sspcMetricAggregationType = lens _sspcMetricAggregationType (\ s a -> s{_sspcMetricAggregationType = a});

-- | The minimum number to adjust your scalable dimension as a result of a scaling activity. If the adjustment type is @PercentChangeInCapacity@ , the scaling policy changes the scalable dimension of the scalable target by this amount.
sspcMinAdjustmentMagnitude :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcMinAdjustmentMagnitude = lens _sspcMinAdjustmentMagnitude (\ s a -> s{_sspcMinAdjustmentMagnitude = a});

instance FromJSON StepScalingPolicyConfiguration
         where
        parseJSON
          = withObject "StepScalingPolicyConfiguration"
              (\ x ->
                 StepScalingPolicyConfiguration' <$>
                   (x .:? "StepAdjustments" .!= mempty) <*>
                     (x .:? "AdjustmentType")
                     <*> (x .:? "Cooldown")
                     <*> (x .:? "MetricAggregationType")
                     <*> (x .:? "MinAdjustmentMagnitude"))

instance Hashable StepScalingPolicyConfiguration

instance NFData StepScalingPolicyConfiguration

instance ToJSON StepScalingPolicyConfiguration where
        toJSON StepScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("StepAdjustments" .=) <$> _sspcStepAdjustments,
                  ("AdjustmentType" .=) <$> _sspcAdjustmentType,
                  ("Cooldown" .=) <$> _sspcCooldown,
                  ("MetricAggregationType" .=) <$>
                    _sspcMetricAggregationType,
                  ("MinAdjustmentMagnitude" .=) <$>
                    _sspcMinAdjustmentMagnitude])
