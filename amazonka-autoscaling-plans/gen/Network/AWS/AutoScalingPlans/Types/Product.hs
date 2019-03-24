{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.Product where

import Network.AWS.AutoScalingPlans.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an application source.
--
--
--
-- /See:/ 'applicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { _asTagFilters             :: !(Maybe [TagFilter])
  , _asCloudFormationStackARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTagFilters' - A set of tags (up to 50).
--
-- * 'asCloudFormationStackARN' - The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
applicationSource
    :: ApplicationSource
applicationSource =
  ApplicationSource'
    {_asTagFilters = Nothing, _asCloudFormationStackARN = Nothing}


-- | A set of tags (up to 50).
asTagFilters :: Lens' ApplicationSource [TagFilter]
asTagFilters = lens _asTagFilters (\ s a -> s{_asTagFilters = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
asCloudFormationStackARN :: Lens' ApplicationSource (Maybe Text)
asCloudFormationStackARN = lens _asCloudFormationStackARN (\ s a -> s{_asCloudFormationStackARN = a})

instance FromJSON ApplicationSource where
        parseJSON
          = withObject "ApplicationSource"
              (\ x ->
                 ApplicationSource' <$>
                   (x .:? "TagFilters" .!= mempty) <*>
                     (x .:? "CloudFormationStackARN"))

instance Hashable ApplicationSource where

instance NFData ApplicationSource where

instance ToJSON ApplicationSource where
        toJSON ApplicationSource'{..}
          = object
              (catMaybes
                 [("TagFilters" .=) <$> _asTagFilters,
                  ("CloudFormationStackARN" .=) <$>
                    _asCloudFormationStackARN])

-- | Represents a CloudWatch metric of your choosing that can be used for predictive scaling.
--
--
-- For predictive scaling to work with a customized load metric specification, AWS Auto Scaling needs access to the @Sum@ and @Average@ statistics that CloudWatch computes from metric data. Statistics are calculations used to aggregate data over specified time periods.
--
-- When you choose a load metric, make sure that the required @Sum@ and @Average@ statistics for your metric are available in CloudWatch and that they provide relevant data for predictive scaling. The @Sum@ statistic must represent the total load on the resource, and the @Average@ statistic must represent the average load per capacity unit of the resource. For example, there is a metric that counts the number of requests processed by your Auto Scaling group. If the @Sum@ statistic represents the total request count processed by the group, then the @Average@ statistic for the specified metric must represent the average request count processed by each instance of the group.
--
-- For information about terminology, available metrics, or how to publish new metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> in the /Amazon CloudWatch User Guide/ .
--
--
-- /See:/ 'customizedLoadMetricSpecification' smart constructor.
data CustomizedLoadMetricSpecification = CustomizedLoadMetricSpecification'
  { _clmsDimensions :: !(Maybe [MetricDimension])
  , _clmsUnit       :: !(Maybe Text)
  , _clmsMetricName :: !Text
  , _clmsNamespace  :: !Text
  , _clmsStatistic  :: !MetricStatistic
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomizedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
--
-- * 'clmsUnit' - The unit of the metric.
--
-- * 'clmsMetricName' - The name of the metric.
--
-- * 'clmsNamespace' - The namespace of the metric.
--
-- * 'clmsStatistic' - The statistic of the metric. Currently, the value must always be @Sum@ .
customizedLoadMetricSpecification
    :: Text -- ^ 'clmsMetricName'
    -> Text -- ^ 'clmsNamespace'
    -> MetricStatistic -- ^ 'clmsStatistic'
    -> CustomizedLoadMetricSpecification
customizedLoadMetricSpecification pMetricName_ pNamespace_ pStatistic_ =
  CustomizedLoadMetricSpecification'
    { _clmsDimensions = Nothing
    , _clmsUnit = Nothing
    , _clmsMetricName = pMetricName_
    , _clmsNamespace = pNamespace_
    , _clmsStatistic = pStatistic_
    }


-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
clmsDimensions :: Lens' CustomizedLoadMetricSpecification [MetricDimension]
clmsDimensions = lens _clmsDimensions (\ s a -> s{_clmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
clmsUnit :: Lens' CustomizedLoadMetricSpecification (Maybe Text)
clmsUnit = lens _clmsUnit (\ s a -> s{_clmsUnit = a})

-- | The name of the metric.
clmsMetricName :: Lens' CustomizedLoadMetricSpecification Text
clmsMetricName = lens _clmsMetricName (\ s a -> s{_clmsMetricName = a})

-- | The namespace of the metric.
clmsNamespace :: Lens' CustomizedLoadMetricSpecification Text
clmsNamespace = lens _clmsNamespace (\ s a -> s{_clmsNamespace = a})

-- | The statistic of the metric. Currently, the value must always be @Sum@ .
clmsStatistic :: Lens' CustomizedLoadMetricSpecification MetricStatistic
clmsStatistic = lens _clmsStatistic (\ s a -> s{_clmsStatistic = a})

instance FromJSON CustomizedLoadMetricSpecification
         where
        parseJSON
          = withObject "CustomizedLoadMetricSpecification"
              (\ x ->
                 CustomizedLoadMetricSpecification' <$>
                   (x .:? "Dimensions" .!= mempty) <*> (x .:? "Unit")
                     <*> (x .: "MetricName")
                     <*> (x .: "Namespace")
                     <*> (x .: "Statistic"))

instance Hashable CustomizedLoadMetricSpecification
         where

instance NFData CustomizedLoadMetricSpecification
         where

instance ToJSON CustomizedLoadMetricSpecification
         where
        toJSON CustomizedLoadMetricSpecification'{..}
          = object
              (catMaybes
                 [("Dimensions" .=) <$> _clmsDimensions,
                  ("Unit" .=) <$> _clmsUnit,
                  Just ("MetricName" .= _clmsMetricName),
                  Just ("Namespace" .= _clmsNamespace),
                  Just ("Statistic" .= _clmsStatistic)])

-- | Represents a CloudWatch metric of your choosing that can be used for dynamic scaling as part of a target tracking scaling policy.
--
--
-- To create your customized scaling metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases.
--
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
--
-- /See:/ 'customizedScalingMetricSpecification' smart constructor.
data CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification'
  { _csmsDimensions :: !(Maybe [MetricDimension])
  , _csmsUnit       :: !(Maybe Text)
  , _csmsMetricName :: !Text
  , _csmsNamespace  :: !Text
  , _csmsStatistic  :: !MetricStatistic
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomizedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmsDimensions' - The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
--
-- * 'csmsUnit' - The unit of the metric.
--
-- * 'csmsMetricName' - The name of the metric.
--
-- * 'csmsNamespace' - The namespace of the metric.
--
-- * 'csmsStatistic' - The statistic of the metric.
customizedScalingMetricSpecification
    :: Text -- ^ 'csmsMetricName'
    -> Text -- ^ 'csmsNamespace'
    -> MetricStatistic -- ^ 'csmsStatistic'
    -> CustomizedScalingMetricSpecification
customizedScalingMetricSpecification pMetricName_ pNamespace_ pStatistic_ =
  CustomizedScalingMetricSpecification'
    { _csmsDimensions = Nothing
    , _csmsUnit = Nothing
    , _csmsMetricName = pMetricName_
    , _csmsNamespace = pNamespace_
    , _csmsStatistic = pStatistic_
    }


-- | The dimensions of the metric. Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
csmsDimensions :: Lens' CustomizedScalingMetricSpecification [MetricDimension]
csmsDimensions = lens _csmsDimensions (\ s a -> s{_csmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
csmsUnit :: Lens' CustomizedScalingMetricSpecification (Maybe Text)
csmsUnit = lens _csmsUnit (\ s a -> s{_csmsUnit = a})

-- | The name of the metric.
csmsMetricName :: Lens' CustomizedScalingMetricSpecification Text
csmsMetricName = lens _csmsMetricName (\ s a -> s{_csmsMetricName = a})

-- | The namespace of the metric.
csmsNamespace :: Lens' CustomizedScalingMetricSpecification Text
csmsNamespace = lens _csmsNamespace (\ s a -> s{_csmsNamespace = a})

-- | The statistic of the metric.
csmsStatistic :: Lens' CustomizedScalingMetricSpecification MetricStatistic
csmsStatistic = lens _csmsStatistic (\ s a -> s{_csmsStatistic = a})

instance FromJSON
           CustomizedScalingMetricSpecification
         where
        parseJSON
          = withObject "CustomizedScalingMetricSpecification"
              (\ x ->
                 CustomizedScalingMetricSpecification' <$>
                   (x .:? "Dimensions" .!= mempty) <*> (x .:? "Unit")
                     <*> (x .: "MetricName")
                     <*> (x .: "Namespace")
                     <*> (x .: "Statistic"))

instance Hashable
           CustomizedScalingMetricSpecification
         where

instance NFData CustomizedScalingMetricSpecification
         where

instance ToJSON CustomizedScalingMetricSpecification
         where
        toJSON CustomizedScalingMetricSpecification'{..}
          = object
              (catMaybes
                 [("Dimensions" .=) <$> _csmsDimensions,
                  ("Unit" .=) <$> _csmsUnit,
                  Just ("MetricName" .= _csmsMetricName),
                  Just ("Namespace" .= _csmsNamespace),
                  Just ("Statistic" .= _csmsStatistic)])

-- | Represents a single value in the forecast data used for predictive scaling.
--
--
--
-- /See:/ 'datapoint' smart constructor.
data Datapoint = Datapoint'
  { _dValue     :: !(Maybe Double)
  , _dTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dValue' - The value of the data point.
--
-- * 'dTimestamp' - The time stamp for the data point in UTC format.
datapoint
    :: Datapoint
datapoint = Datapoint' {_dValue = Nothing, _dTimestamp = Nothing}


-- | The value of the data point.
dValue :: Lens' Datapoint (Maybe Double)
dValue = lens _dValue (\ s a -> s{_dValue = a})

-- | The time stamp for the data point in UTC format.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\ s a -> s{_dTimestamp = a}) . mapping _Time

instance FromJSON Datapoint where
        parseJSON
          = withObject "Datapoint"
              (\ x ->
                 Datapoint' <$>
                   (x .:? "Value") <*> (x .:? "Timestamp"))

instance Hashable Datapoint where

instance NFData Datapoint where

-- | Represents a dimension for a customized metric.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdName  :: !Text
  , _mdValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the dimension.
--
-- * 'mdValue' - The value of the dimension.
metricDimension
    :: Text -- ^ 'mdName'
    -> Text -- ^ 'mdValue'
    -> MetricDimension
metricDimension pName_ pValue_ =
  MetricDimension' {_mdName = pName_, _mdValue = pValue_}


-- | The name of the dimension.
mdName :: Lens' MetricDimension Text
mdName = lens _mdName (\ s a -> s{_mdName = a})

-- | The value of the dimension.
mdValue :: Lens' MetricDimension Text
mdValue = lens _mdValue (\ s a -> s{_mdValue = a})

instance FromJSON MetricDimension where
        parseJSON
          = withObject "MetricDimension"
              (\ x ->
                 MetricDimension' <$>
                   (x .: "Name") <*> (x .: "Value"))

instance Hashable MetricDimension where

instance NFData MetricDimension where

instance ToJSON MetricDimension where
        toJSON MetricDimension'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _mdName),
                  Just ("Value" .= _mdValue)])

-- | Represents a predefined metric that can be used for predictive scaling.
--
--
--
-- /See:/ 'predefinedLoadMetricSpecification' smart constructor.
data PredefinedLoadMetricSpecification = PredefinedLoadMetricSpecification'
  { _plmsResourceLabel            :: !(Maybe Text)
  , _plmsPredefinedLoadMetricType :: !LoadMetricType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PredefinedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'plmsPredefinedLoadMetricType' - The metric type.
predefinedLoadMetricSpecification
    :: LoadMetricType -- ^ 'plmsPredefinedLoadMetricType'
    -> PredefinedLoadMetricSpecification
predefinedLoadMetricSpecification pPredefinedLoadMetricType_ =
  PredefinedLoadMetricSpecification'
    { _plmsResourceLabel = Nothing
    , _plmsPredefinedLoadMetricType = pPredefinedLoadMetricType_
    }


-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
plmsResourceLabel :: Lens' PredefinedLoadMetricSpecification (Maybe Text)
plmsResourceLabel = lens _plmsResourceLabel (\ s a -> s{_plmsResourceLabel = a})

-- | The metric type.
plmsPredefinedLoadMetricType :: Lens' PredefinedLoadMetricSpecification LoadMetricType
plmsPredefinedLoadMetricType = lens _plmsPredefinedLoadMetricType (\ s a -> s{_plmsPredefinedLoadMetricType = a})

instance FromJSON PredefinedLoadMetricSpecification
         where
        parseJSON
          = withObject "PredefinedLoadMetricSpecification"
              (\ x ->
                 PredefinedLoadMetricSpecification' <$>
                   (x .:? "ResourceLabel") <*>
                     (x .: "PredefinedLoadMetricType"))

instance Hashable PredefinedLoadMetricSpecification
         where

instance NFData PredefinedLoadMetricSpecification
         where

instance ToJSON PredefinedLoadMetricSpecification
         where
        toJSON PredefinedLoadMetricSpecification'{..}
          = object
              (catMaybes
                 [("ResourceLabel" .=) <$> _plmsResourceLabel,
                  Just
                    ("PredefinedLoadMetricType" .=
                       _plmsPredefinedLoadMetricType)])

-- | Represents a predefined metric that can be used for dynamic scaling as part of a target tracking scaling policy.
--
--
--
-- /See:/ 'predefinedScalingMetricSpecification' smart constructor.
data PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification'
  { _psmsResourceLabel               :: !(Maybe Text)
  , _psmsPredefinedScalingMetricType :: !ScalingMetricType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PredefinedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'psmsPredefinedScalingMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
predefinedScalingMetricSpecification
    :: ScalingMetricType -- ^ 'psmsPredefinedScalingMetricType'
    -> PredefinedScalingMetricSpecification
predefinedScalingMetricSpecification pPredefinedScalingMetricType_ =
  PredefinedScalingMetricSpecification'
    { _psmsResourceLabel = Nothing
    , _psmsPredefinedScalingMetricType = pPredefinedScalingMetricType_
    }


-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN.     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
psmsResourceLabel :: Lens' PredefinedScalingMetricSpecification (Maybe Text)
psmsResourceLabel = lens _psmsResourceLabel (\ s a -> s{_psmsResourceLabel = a})

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Spot Fleet requests, and ECS services.
psmsPredefinedScalingMetricType :: Lens' PredefinedScalingMetricSpecification ScalingMetricType
psmsPredefinedScalingMetricType = lens _psmsPredefinedScalingMetricType (\ s a -> s{_psmsPredefinedScalingMetricType = a})

instance FromJSON
           PredefinedScalingMetricSpecification
         where
        parseJSON
          = withObject "PredefinedScalingMetricSpecification"
              (\ x ->
                 PredefinedScalingMetricSpecification' <$>
                   (x .:? "ResourceLabel") <*>
                     (x .: "PredefinedScalingMetricType"))

instance Hashable
           PredefinedScalingMetricSpecification
         where

instance NFData PredefinedScalingMetricSpecification
         where

instance ToJSON PredefinedScalingMetricSpecification
         where
        toJSON PredefinedScalingMetricSpecification'{..}
          = object
              (catMaybes
                 [("ResourceLabel" .=) <$> _psmsResourceLabel,
                  Just
                    ("PredefinedScalingMetricType" .=
                       _psmsPredefinedScalingMetricType)])

-- | Describes a scaling instruction for a scalable resource.
--
--
-- The scaling instruction is used in combination with a scaling plan, which is a set of instructions for configuring dynamic scaling and predictive scaling for the scalable resources in your application. Each scaling instruction applies to one resource.
--
-- AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.
--
-- AWS Auto Scaling also configures predictive scaling for your Amazon EC2 Auto Scaling groups using a subset of parameters, including the load metric, the scaling metric, the target value for the scaling metric, the predictive scaling mode (forecast and scale or forecast only), and the desired behavior when the forecast capacity exceeds the maximum capacity of the resource. With predictive scaling, AWS Auto Scaling generates forecasts with traffic predictions for the two days ahead and schedules scaling actions that proactively add and remove resource capacity to match the forecast.
--
-- We recommend waiting a minimum of 24 hours after creating an Auto Scaling group to configure predictive scaling. At minimum, there must be 24 hours of historical data to generate a forecast.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/plans/userguide/auto-scaling-getting-started.html Getting Started with AWS Auto Scaling> .
--
--
-- /See:/ 'scalingInstruction' smart constructor.
data ScalingInstruction = ScalingInstruction'
  { _siScheduledActionBufferTime :: !(Maybe Nat)
  , _siPredictiveScalingMaxCapacityBuffer :: !(Maybe Int)
  , _siScalingPolicyUpdateBehavior :: !(Maybe ScalingPolicyUpdateBehavior)
  , _siCustomizedLoadMetricSpecification :: !(Maybe CustomizedLoadMetricSpecification)
  , _siPredictiveScalingMode :: !(Maybe PredictiveScalingMode)
  , _siDisableDynamicScaling :: !(Maybe Bool)
  , _siPredictiveScalingMaxCapacityBehavior :: !(Maybe PredictiveScalingMaxCapacityBehavior)
  , _siPredefinedLoadMetricSpecification :: !(Maybe PredefinedLoadMetricSpecification)
  , _siServiceNamespace :: !ServiceNamespace
  , _siResourceId :: !Text
  , _siScalableDimension :: !ScalableDimension
  , _siMinCapacity :: !Int
  , _siMaxCapacity :: !Int
  , _siTargetTrackingConfigurations :: ![TargetTrackingConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingInstruction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siScheduledActionBufferTime' - The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.  The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.  Only valid when configuring predictive scaling.
--
-- * 'siPredictiveScalingMaxCapacityBuffer' - The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55. Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise. The range is 1-100.
--
-- * 'siScalingPolicyUpdateBehavior' - Controls whether a resource's externally created scaling policies are kept or replaced.  The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.  Only valid when configuring dynamic scaling.  Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
--
-- * 'siCustomizedLoadMetricSpecification' - The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- * 'siPredictiveScalingMode' - The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
--
-- * 'siDisableDynamicScaling' - Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.  The default is enabled (@false@ ).
--
-- * 'siPredictiveScalingMaxCapacityBehavior' - Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ . The following are possible values:     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.      * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.  Only valid when configuring predictive scaling.
--
-- * 'siPredefinedLoadMetricSpecification' - The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- * 'siServiceNamespace' - The namespace of the AWS service.
--
-- * 'siResourceId' - The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
-- * 'siScalableDimension' - The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
-- * 'siMinCapacity' - The minimum capacity of the resource.
--
-- * 'siMaxCapacity' - The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
--
-- * 'siTargetTrackingConfigurations' - The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.  With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.  Condition: The scaling metric must be unique across target tracking configurations.
scalingInstruction
    :: ServiceNamespace -- ^ 'siServiceNamespace'
    -> Text -- ^ 'siResourceId'
    -> ScalableDimension -- ^ 'siScalableDimension'
    -> Int -- ^ 'siMinCapacity'
    -> Int -- ^ 'siMaxCapacity'
    -> ScalingInstruction
scalingInstruction pServiceNamespace_ pResourceId_ pScalableDimension_ pMinCapacity_ pMaxCapacity_ =
  ScalingInstruction'
    { _siScheduledActionBufferTime = Nothing
    , _siPredictiveScalingMaxCapacityBuffer = Nothing
    , _siScalingPolicyUpdateBehavior = Nothing
    , _siCustomizedLoadMetricSpecification = Nothing
    , _siPredictiveScalingMode = Nothing
    , _siDisableDynamicScaling = Nothing
    , _siPredictiveScalingMaxCapacityBehavior = Nothing
    , _siPredefinedLoadMetricSpecification = Nothing
    , _siServiceNamespace = pServiceNamespace_
    , _siResourceId = pResourceId_
    , _siScalableDimension = pScalableDimension_
    , _siMinCapacity = pMinCapacity_
    , _siMaxCapacity = pMaxCapacity_
    , _siTargetTrackingConfigurations = mempty
    }


-- | The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.  The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.  Only valid when configuring predictive scaling.
siScheduledActionBufferTime :: Lens' ScalingInstruction (Maybe Natural)
siScheduledActionBufferTime = lens _siScheduledActionBufferTime (\ s a -> s{_siScheduledActionBufferTime = a}) . mapping _Nat

-- | The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55. Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise. The range is 1-100.
siPredictiveScalingMaxCapacityBuffer :: Lens' ScalingInstruction (Maybe Int)
siPredictiveScalingMaxCapacityBuffer = lens _siPredictiveScalingMaxCapacityBuffer (\ s a -> s{_siPredictiveScalingMaxCapacityBuffer = a})

-- | Controls whether a resource's externally created scaling policies are kept or replaced.  The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.  Only valid when configuring dynamic scaling.  Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
siScalingPolicyUpdateBehavior :: Lens' ScalingInstruction (Maybe ScalingPolicyUpdateBehavior)
siScalingPolicyUpdateBehavior = lens _siScalingPolicyUpdateBehavior (\ s a -> s{_siScalingPolicyUpdateBehavior = a})

-- | The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
siCustomizedLoadMetricSpecification :: Lens' ScalingInstruction (Maybe CustomizedLoadMetricSpecification)
siCustomizedLoadMetricSpecification = lens _siCustomizedLoadMetricSpecification (\ s a -> s{_siCustomizedLoadMetricSpecification = a})

-- | The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
siPredictiveScalingMode :: Lens' ScalingInstruction (Maybe PredictiveScalingMode)
siPredictiveScalingMode = lens _siPredictiveScalingMode (\ s a -> s{_siPredictiveScalingMode = a})

-- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.  The default is enabled (@false@ ).
siDisableDynamicScaling :: Lens' ScalingInstruction (Maybe Bool)
siDisableDynamicScaling = lens _siDisableDynamicScaling (\ s a -> s{_siDisableDynamicScaling = a})

-- | Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ . The following are possible values:     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.      * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.  Only valid when configuring predictive scaling.
siPredictiveScalingMaxCapacityBehavior :: Lens' ScalingInstruction (Maybe PredictiveScalingMaxCapacityBehavior)
siPredictiveScalingMaxCapacityBehavior = lens _siPredictiveScalingMaxCapacityBehavior (\ s a -> s{_siPredictiveScalingMaxCapacityBehavior = a})

-- | The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
siPredefinedLoadMetricSpecification :: Lens' ScalingInstruction (Maybe PredefinedLoadMetricSpecification)
siPredefinedLoadMetricSpecification = lens _siPredefinedLoadMetricSpecification (\ s a -> s{_siPredefinedLoadMetricSpecification = a})

-- | The namespace of the AWS service.
siServiceNamespace :: Lens' ScalingInstruction ServiceNamespace
siServiceNamespace = lens _siServiceNamespace (\ s a -> s{_siServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
siResourceId :: Lens' ScalingInstruction Text
siResourceId = lens _siResourceId (\ s a -> s{_siResourceId = a})

-- | The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
siScalableDimension :: Lens' ScalingInstruction ScalableDimension
siScalableDimension = lens _siScalableDimension (\ s a -> s{_siScalableDimension = a})

-- | The minimum capacity of the resource.
siMinCapacity :: Lens' ScalingInstruction Int
siMinCapacity = lens _siMinCapacity (\ s a -> s{_siMinCapacity = a})

-- | The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
siMaxCapacity :: Lens' ScalingInstruction Int
siMaxCapacity = lens _siMaxCapacity (\ s a -> s{_siMaxCapacity = a})

-- | The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.  With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.  Condition: The scaling metric must be unique across target tracking configurations.
siTargetTrackingConfigurations :: Lens' ScalingInstruction [TargetTrackingConfiguration]
siTargetTrackingConfigurations = lens _siTargetTrackingConfigurations (\ s a -> s{_siTargetTrackingConfigurations = a}) . _Coerce

instance FromJSON ScalingInstruction where
        parseJSON
          = withObject "ScalingInstruction"
              (\ x ->
                 ScalingInstruction' <$>
                   (x .:? "ScheduledActionBufferTime") <*>
                     (x .:? "PredictiveScalingMaxCapacityBuffer")
                     <*> (x .:? "ScalingPolicyUpdateBehavior")
                     <*> (x .:? "CustomizedLoadMetricSpecification")
                     <*> (x .:? "PredictiveScalingMode")
                     <*> (x .:? "DisableDynamicScaling")
                     <*> (x .:? "PredictiveScalingMaxCapacityBehavior")
                     <*> (x .:? "PredefinedLoadMetricSpecification")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "MinCapacity")
                     <*> (x .: "MaxCapacity")
                     <*>
                     (x .:? "TargetTrackingConfigurations" .!= mempty))

instance Hashable ScalingInstruction where

instance NFData ScalingInstruction where

instance ToJSON ScalingInstruction where
        toJSON ScalingInstruction'{..}
          = object
              (catMaybes
                 [("ScheduledActionBufferTime" .=) <$>
                    _siScheduledActionBufferTime,
                  ("PredictiveScalingMaxCapacityBuffer" .=) <$>
                    _siPredictiveScalingMaxCapacityBuffer,
                  ("ScalingPolicyUpdateBehavior" .=) <$>
                    _siScalingPolicyUpdateBehavior,
                  ("CustomizedLoadMetricSpecification" .=) <$>
                    _siCustomizedLoadMetricSpecification,
                  ("PredictiveScalingMode" .=) <$>
                    _siPredictiveScalingMode,
                  ("DisableDynamicScaling" .=) <$>
                    _siDisableDynamicScaling,
                  ("PredictiveScalingMaxCapacityBehavior" .=) <$>
                    _siPredictiveScalingMaxCapacityBehavior,
                  ("PredefinedLoadMetricSpecification" .=) <$>
                    _siPredefinedLoadMetricSpecification,
                  Just ("ServiceNamespace" .= _siServiceNamespace),
                  Just ("ResourceId" .= _siResourceId),
                  Just ("ScalableDimension" .= _siScalableDimension),
                  Just ("MinCapacity" .= _siMinCapacity),
                  Just ("MaxCapacity" .= _siMaxCapacity),
                  Just
                    ("TargetTrackingConfigurations" .=
                       _siTargetTrackingConfigurations)])

-- | Represents a scaling plan.
--
--
--
-- /See:/ 'scalingPlan' smart constructor.
data ScalingPlan = ScalingPlan'
  { _spCreationTime        :: !(Maybe POSIX)
  , _spStatusStartTime     :: !(Maybe POSIX)
  , _spStatusMessage       :: !(Maybe Text)
  , _spScalingPlanName     :: !Text
  , _spScalingPlanVersion  :: !Integer
  , _spApplicationSource   :: !ApplicationSource
  , _spScalingInstructions :: ![ScalingInstruction]
  , _spStatusCode          :: !ScalingPlanStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
scalingPlan
    :: Text -- ^ 'spScalingPlanName'
    -> Integer -- ^ 'spScalingPlanVersion'
    -> ApplicationSource -- ^ 'spApplicationSource'
    -> ScalingPlanStatusCode -- ^ 'spStatusCode'
    -> ScalingPlan
scalingPlan pScalingPlanName_ pScalingPlanVersion_ pApplicationSource_ pStatusCode_ =
  ScalingPlan'
    { _spCreationTime = Nothing
    , _spStatusStartTime = Nothing
    , _spStatusMessage = Nothing
    , _spScalingPlanName = pScalingPlanName_
    , _spScalingPlanVersion = pScalingPlanVersion_
    , _spApplicationSource = pApplicationSource_
    , _spScalingInstructions = mempty
    , _spStatusCode = pStatusCode_
    }


-- | The Unix time stamp when the scaling plan was created.
spCreationTime :: Lens' ScalingPlan (Maybe UTCTime)
spCreationTime = lens _spCreationTime (\ s a -> s{_spCreationTime = a}) . mapping _Time

-- | The Unix time stamp when the scaling plan entered the current status.
spStatusStartTime :: Lens' ScalingPlan (Maybe UTCTime)
spStatusStartTime = lens _spStatusStartTime (\ s a -> s{_spStatusStartTime = a}) . mapping _Time

-- | A simple message about the current status of the scaling plan.
spStatusMessage :: Lens' ScalingPlan (Maybe Text)
spStatusMessage = lens _spStatusMessage (\ s a -> s{_spStatusMessage = a})

-- | The name of the scaling plan.
spScalingPlanName :: Lens' ScalingPlan Text
spScalingPlanName = lens _spScalingPlanName (\ s a -> s{_spScalingPlanName = a})

-- | The version number of the scaling plan.
spScalingPlanVersion :: Lens' ScalingPlan Integer
spScalingPlanVersion = lens _spScalingPlanVersion (\ s a -> s{_spScalingPlanVersion = a})

-- | The application source.
spApplicationSource :: Lens' ScalingPlan ApplicationSource
spApplicationSource = lens _spApplicationSource (\ s a -> s{_spApplicationSource = a})

-- | The scaling instructions.
spScalingInstructions :: Lens' ScalingPlan [ScalingInstruction]
spScalingInstructions = lens _spScalingInstructions (\ s a -> s{_spScalingInstructions = a}) . _Coerce

-- | The status of the scaling plan.     * @Active@ - The scaling plan is active.     * @ActiveWithProblems@ - The scaling plan is active, but the scaling configuration for one or more resources could not be applied.     * @CreationInProgress@ - The scaling plan is being created.     * @CreationFailed@ - The scaling plan could not be created.     * @DeletionInProgress@ - The scaling plan is being deleted.     * @DeletionFailed@ - The scaling plan could not be deleted.     * @UpdateInProgress@ - The scaling plan is being updated.     * @UpdateFailed@ - The scaling plan could not be updated.
spStatusCode :: Lens' ScalingPlan ScalingPlanStatusCode
spStatusCode = lens _spStatusCode (\ s a -> s{_spStatusCode = a})

instance FromJSON ScalingPlan where
        parseJSON
          = withObject "ScalingPlan"
              (\ x ->
                 ScalingPlan' <$>
                   (x .:? "CreationTime") <*> (x .:? "StatusStartTime")
                     <*> (x .:? "StatusMessage")
                     <*> (x .: "ScalingPlanName")
                     <*> (x .: "ScalingPlanVersion")
                     <*> (x .: "ApplicationSource")
                     <*> (x .:? "ScalingInstructions" .!= mempty)
                     <*> (x .: "StatusCode"))

instance Hashable ScalingPlan where

instance NFData ScalingPlan where

-- | Represents a scalable resource.
--
--
--
-- /See:/ 'scalingPlanResource' smart constructor.
data ScalingPlanResource = ScalingPlanResource'
  { _sprScalingStatusMessage :: !(Maybe Text)
  , _sprScalingPolicies      :: !(Maybe [ScalingPolicy])
  , _sprScalingPlanName      :: !Text
  , _sprScalingPlanVersion   :: !Integer
  , _sprServiceNamespace     :: !ServiceNamespace
  , _sprResourceId           :: !Text
  , _sprScalableDimension    :: !ScalableDimension
  , _sprScalingStatusCode    :: !ScalingStatusCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingPlanResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sprScalingStatusMessage' - A simple message about the current scaling status of the resource.
--
-- * 'sprScalingPolicies' - The scaling policies.
--
-- * 'sprScalingPlanName' - The name of the scaling plan.
--
-- * 'sprScalingPlanVersion' - The version number of the scaling plan.
--
-- * 'sprServiceNamespace' - The namespace of the AWS service.
--
-- * 'sprResourceId' - The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
-- * 'sprScalableDimension' - The scalable dimension for the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
-- * 'sprScalingStatusCode' - The scaling status of the resource.     * @Active@ - The scaling configuration is active.     * @Inactive@ - The scaling configuration is not active because the scaling plan is being created or the scaling configuration could not be applied. Check the status message for more information.     * @PartiallyActive@ - The scaling configuration is partially active because the scaling plan is being created or deleted or the scaling configuration could not be fully applied. Check the status message for more information.
scalingPlanResource
    :: Text -- ^ 'sprScalingPlanName'
    -> Integer -- ^ 'sprScalingPlanVersion'
    -> ServiceNamespace -- ^ 'sprServiceNamespace'
    -> Text -- ^ 'sprResourceId'
    -> ScalableDimension -- ^ 'sprScalableDimension'
    -> ScalingStatusCode -- ^ 'sprScalingStatusCode'
    -> ScalingPlanResource
scalingPlanResource pScalingPlanName_ pScalingPlanVersion_ pServiceNamespace_ pResourceId_ pScalableDimension_ pScalingStatusCode_ =
  ScalingPlanResource'
    { _sprScalingStatusMessage = Nothing
    , _sprScalingPolicies = Nothing
    , _sprScalingPlanName = pScalingPlanName_
    , _sprScalingPlanVersion = pScalingPlanVersion_
    , _sprServiceNamespace = pServiceNamespace_
    , _sprResourceId = pResourceId_
    , _sprScalableDimension = pScalableDimension_
    , _sprScalingStatusCode = pScalingStatusCode_
    }


-- | A simple message about the current scaling status of the resource.
sprScalingStatusMessage :: Lens' ScalingPlanResource (Maybe Text)
sprScalingStatusMessage = lens _sprScalingStatusMessage (\ s a -> s{_sprScalingStatusMessage = a})

-- | The scaling policies.
sprScalingPolicies :: Lens' ScalingPlanResource [ScalingPolicy]
sprScalingPolicies = lens _sprScalingPolicies (\ s a -> s{_sprScalingPolicies = a}) . _Default . _Coerce

-- | The name of the scaling plan.
sprScalingPlanName :: Lens' ScalingPlanResource Text
sprScalingPlanName = lens _sprScalingPlanName (\ s a -> s{_sprScalingPlanName = a})

-- | The version number of the scaling plan.
sprScalingPlanVersion :: Lens' ScalingPlanResource Integer
sprScalingPlanVersion = lens _sprScalingPlanVersion (\ s a -> s{_sprScalingPlanVersion = a})

-- | The namespace of the AWS service.
sprServiceNamespace :: Lens' ScalingPlanResource ServiceNamespace
sprServiceNamespace = lens _sprServiceNamespace (\ s a -> s{_sprServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
sprResourceId :: Lens' ScalingPlanResource Text
sprResourceId = lens _sprResourceId (\ s a -> s{_sprResourceId = a})

-- | The scalable dimension for the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
sprScalableDimension :: Lens' ScalingPlanResource ScalableDimension
sprScalableDimension = lens _sprScalableDimension (\ s a -> s{_sprScalableDimension = a})

-- | The scaling status of the resource.     * @Active@ - The scaling configuration is active.     * @Inactive@ - The scaling configuration is not active because the scaling plan is being created or the scaling configuration could not be applied. Check the status message for more information.     * @PartiallyActive@ - The scaling configuration is partially active because the scaling plan is being created or deleted or the scaling configuration could not be fully applied. Check the status message for more information.
sprScalingStatusCode :: Lens' ScalingPlanResource ScalingStatusCode
sprScalingStatusCode = lens _sprScalingStatusCode (\ s a -> s{_sprScalingStatusCode = a})

instance FromJSON ScalingPlanResource where
        parseJSON
          = withObject "ScalingPlanResource"
              (\ x ->
                 ScalingPlanResource' <$>
                   (x .:? "ScalingStatusMessage") <*>
                     (x .:? "ScalingPolicies" .!= mempty)
                     <*> (x .: "ScalingPlanName")
                     <*> (x .: "ScalingPlanVersion")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "ScalingStatusCode"))

instance Hashable ScalingPlanResource where

instance NFData ScalingPlanResource where

-- | Represents a scaling policy.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _spTargetTrackingConfiguration :: !(Maybe TargetTrackingConfiguration)
  , _spPolicyName                  :: !Text
  , _spPolicyType                  :: !PolicyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spTargetTrackingConfiguration' - The target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- * 'spPolicyName' - The name of the scaling policy.
--
-- * 'spPolicyType' - The type of scaling policy.
scalingPolicy
    :: Text -- ^ 'spPolicyName'
    -> PolicyType -- ^ 'spPolicyType'
    -> ScalingPolicy
scalingPolicy pPolicyName_ pPolicyType_ =
  ScalingPolicy'
    { _spTargetTrackingConfiguration = Nothing
    , _spPolicyName = pPolicyName_
    , _spPolicyType = pPolicyType_
    }


-- | The target tracking scaling policy. Includes support for predefined or customized metrics.
spTargetTrackingConfiguration :: Lens' ScalingPolicy (Maybe TargetTrackingConfiguration)
spTargetTrackingConfiguration = lens _spTargetTrackingConfiguration (\ s a -> s{_spTargetTrackingConfiguration = a})

-- | The name of the scaling policy.
spPolicyName :: Lens' ScalingPolicy Text
spPolicyName = lens _spPolicyName (\ s a -> s{_spPolicyName = a})

-- | The type of scaling policy.
spPolicyType :: Lens' ScalingPolicy PolicyType
spPolicyType = lens _spPolicyType (\ s a -> s{_spPolicyType = a})

instance FromJSON ScalingPolicy where
        parseJSON
          = withObject "ScalingPolicy"
              (\ x ->
                 ScalingPolicy' <$>
                   (x .:? "TargetTrackingConfiguration") <*>
                     (x .: "PolicyName")
                     <*> (x .: "PolicyType"))

instance Hashable ScalingPolicy where

instance NFData ScalingPolicy where

-- | Represents a tag.
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
  { _tfValues :: !(Maybe [Text])
  , _tfKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfValues' - The tag values (0 to 20).
--
-- * 'tfKey' - The tag key.
tagFilter
    :: TagFilter
tagFilter = TagFilter' {_tfValues = Nothing, _tfKey = Nothing}


-- | The tag values (0 to 20).
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\ s a -> s{_tfValues = a}) . _Default . _Coerce

-- | The tag key.
tfKey :: Lens' TagFilter (Maybe Text)
tfKey = lens _tfKey (\ s a -> s{_tfKey = a})

instance FromJSON TagFilter where
        parseJSON
          = withObject "TagFilter"
              (\ x ->
                 TagFilter' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable TagFilter where

instance NFData TagFilter where

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tfValues, ("Key" .=) <$> _tfKey])

-- | Describes a target tracking configuration to use with AWS Auto Scaling. Used with 'ScalingInstruction' and 'ScalingPolicy' .
--
--
--
-- /See:/ 'targetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { _ttcEstimatedInstanceWarmup :: !(Maybe Int)
  , _ttcPredefinedScalingMetricSpecification :: !(Maybe PredefinedScalingMetricSpecification)
  , _ttcScaleInCooldown :: !(Maybe Int)
  , _ttcDisableScaleIn :: !(Maybe Bool)
  , _ttcCustomizedScalingMetricSpecification :: !(Maybe CustomizedScalingMetricSpecification)
  , _ttcScaleOutCooldown :: !(Maybe Int)
  , _ttcTargetValue :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttcEstimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
--
-- * 'ttcPredefinedScalingMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttcScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
--
-- * 'ttcDisableScaleIn' - Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.  The default value is @false@ .
--
-- * 'ttcCustomizedScalingMetricSpecification' - A customized metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttcScaleOutCooldown' - The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
--
-- * 'ttcTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
targetTrackingConfiguration
    :: Double -- ^ 'ttcTargetValue'
    -> TargetTrackingConfiguration
targetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { _ttcEstimatedInstanceWarmup = Nothing
    , _ttcPredefinedScalingMetricSpecification = Nothing
    , _ttcScaleInCooldown = Nothing
    , _ttcDisableScaleIn = Nothing
    , _ttcCustomizedScalingMetricSpecification = Nothing
    , _ttcScaleOutCooldown = Nothing
    , _ttcTargetValue = pTargetValue_
    }


-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
ttcEstimatedInstanceWarmup :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcEstimatedInstanceWarmup = lens _ttcEstimatedInstanceWarmup (\ s a -> s{_ttcEstimatedInstanceWarmup = a})

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
ttcPredefinedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe PredefinedScalingMetricSpecification)
ttcPredefinedScalingMetricSpecification = lens _ttcPredefinedScalingMetricSpecification (\ s a -> s{_ttcPredefinedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
ttcScaleInCooldown :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcScaleInCooldown = lens _ttcScaleInCooldown (\ s a -> s{_ttcScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.  The default value is @false@ .
ttcDisableScaleIn :: Lens' TargetTrackingConfiguration (Maybe Bool)
ttcDisableScaleIn = lens _ttcDisableScaleIn (\ s a -> s{_ttcDisableScaleIn = a})

-- | A customized metric. You can specify either a predefined metric or a customized metric.
ttcCustomizedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe CustomizedScalingMetricSpecification)
ttcCustomizedScalingMetricSpecification = lens _ttcCustomizedScalingMetricSpecification (\ s a -> s{_ttcCustomizedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
ttcScaleOutCooldown :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcScaleOutCooldown = lens _ttcScaleOutCooldown (\ s a -> s{_ttcScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
ttcTargetValue :: Lens' TargetTrackingConfiguration Double
ttcTargetValue = lens _ttcTargetValue (\ s a -> s{_ttcTargetValue = a})

instance FromJSON TargetTrackingConfiguration where
        parseJSON
          = withObject "TargetTrackingConfiguration"
              (\ x ->
                 TargetTrackingConfiguration' <$>
                   (x .:? "EstimatedInstanceWarmup") <*>
                     (x .:? "PredefinedScalingMetricSpecification")
                     <*> (x .:? "ScaleInCooldown")
                     <*> (x .:? "DisableScaleIn")
                     <*> (x .:? "CustomizedScalingMetricSpecification")
                     <*> (x .:? "ScaleOutCooldown")
                     <*> (x .: "TargetValue"))

instance Hashable TargetTrackingConfiguration where

instance NFData TargetTrackingConfiguration where

instance ToJSON TargetTrackingConfiguration where
        toJSON TargetTrackingConfiguration'{..}
          = object
              (catMaybes
                 [("EstimatedInstanceWarmup" .=) <$>
                    _ttcEstimatedInstanceWarmup,
                  ("PredefinedScalingMetricSpecification" .=) <$>
                    _ttcPredefinedScalingMetricSpecification,
                  ("ScaleInCooldown" .=) <$> _ttcScaleInCooldown,
                  ("DisableScaleIn" .=) <$> _ttcDisableScaleIn,
                  ("CustomizedScalingMetricSpecification" .=) <$>
                    _ttcCustomizedScalingMetricSpecification,
                  ("ScaleOutCooldown" .=) <$> _ttcScaleOutCooldown,
                  Just ("TargetValue" .= _ttcTargetValue)])
