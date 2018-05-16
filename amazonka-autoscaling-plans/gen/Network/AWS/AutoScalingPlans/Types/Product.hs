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
-- * 'asCloudFormationStackARN' - The Amazon Resource Name (ARN) of a CloudFormation stack.
applicationSource
    :: ApplicationSource
applicationSource =
  ApplicationSource'
    {_asTagFilters = Nothing, _asCloudFormationStackARN = Nothing}


-- | A set of tags (up to 50).
asTagFilters :: Lens' ApplicationSource [TagFilter]
asTagFilters = lens _asTagFilters (\ s a -> s{_asTagFilters = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of a CloudFormation stack.
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

-- | Represents a customized metric for a target tracking policy.
--
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
-- * 'csmsDimensions' - The dimensions of the metric.
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


-- | The dimensions of the metric.
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

-- | Represents a predefined metric for a target tracking policy.
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
-- * 'psmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'psmsPredefinedScalingMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Sport Fleet requests, and ECS services.
predefinedScalingMetricSpecification
    :: ScalingMetricType -- ^ 'psmsPredefinedScalingMetricType'
    -> PredefinedScalingMetricSpecification
predefinedScalingMetricSpecification pPredefinedScalingMetricType_ =
  PredefinedScalingMetricSpecification'
    { _psmsResourceLabel = Nothing
    , _psmsPredefinedScalingMetricType = pPredefinedScalingMetricType_
    }


-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group for an Application Load Balancer attached to the Auto Scaling group, Spot Fleet request, or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
psmsResourceLabel :: Lens' PredefinedScalingMetricSpecification (Maybe Text)
psmsResourceLabel = lens _psmsResourceLabel (\ s a -> s{_psmsResourceLabel = a})

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Auto Scaling groups, Sport Fleet requests, and ECS services.
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

-- | Specifies the scaling configuration for a scalable resource.
--
--
--
-- /See:/ 'scalingInstruction' smart constructor.
data ScalingInstruction = ScalingInstruction'
  { _siServiceNamespace             :: !ServiceNamespace
  , _siResourceId                   :: !Text
  , _siScalableDimension            :: !ScalableDimension
  , _siMinCapacity                  :: !Int
  , _siMaxCapacity                  :: !Int
  , _siTargetTrackingConfigurations :: ![TargetTrackingConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingInstruction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siServiceNamespace' - The namespace of the AWS service.
--
-- * 'siResourceId' - The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
-- * 'siScalableDimension' - The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.
--
-- * 'siMinCapacity' - The minimum value to scale to in response to a scale in event.
--
-- * 'siMaxCapacity' - The maximum value to scale to in response to a scale out event.
--
-- * 'siTargetTrackingConfigurations' - The target tracking scaling policies (up to 10).
scalingInstruction
    :: ServiceNamespace -- ^ 'siServiceNamespace'
    -> Text -- ^ 'siResourceId'
    -> ScalableDimension -- ^ 'siScalableDimension'
    -> Int -- ^ 'siMinCapacity'
    -> Int -- ^ 'siMaxCapacity'
    -> ScalingInstruction
scalingInstruction pServiceNamespace_ pResourceId_ pScalableDimension_ pMinCapacity_ pMaxCapacity_ =
  ScalingInstruction'
    { _siServiceNamespace = pServiceNamespace_
    , _siResourceId = pResourceId_
    , _siScalableDimension = pScalableDimension_
    , _siMinCapacity = pMinCapacity_
    , _siMaxCapacity = pMaxCapacity_
    , _siTargetTrackingConfigurations = mempty
    }


-- | The namespace of the AWS service.
siServiceNamespace :: Lens' ScalingInstruction ServiceNamespace
siServiceNamespace = lens _siServiceNamespace (\ s a -> s{_siServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
siResourceId :: Lens' ScalingInstruction Text
siResourceId = lens _siResourceId (\ s a -> s{_siResourceId = a})

-- | The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.
siScalableDimension :: Lens' ScalingInstruction ScalableDimension
siScalableDimension = lens _siScalableDimension (\ s a -> s{_siScalableDimension = a})

-- | The minimum value to scale to in response to a scale in event.
siMinCapacity :: Lens' ScalingInstruction Int
siMinCapacity = lens _siMinCapacity (\ s a -> s{_siMinCapacity = a})

-- | The maximum value to scale to in response to a scale out event.
siMaxCapacity :: Lens' ScalingInstruction Int
siMaxCapacity = lens _siMaxCapacity (\ s a -> s{_siMaxCapacity = a})

-- | The target tracking scaling policies (up to 10).
siTargetTrackingConfigurations :: Lens' ScalingInstruction [TargetTrackingConfiguration]
siTargetTrackingConfigurations = lens _siTargetTrackingConfigurations (\ s a -> s{_siTargetTrackingConfigurations = a}) . _Coerce

instance FromJSON ScalingInstruction where
        parseJSON
          = withObject "ScalingInstruction"
              (\ x ->
                 ScalingInstruction' <$>
                   (x .: "ServiceNamespace") <*> (x .: "ResourceId") <*>
                     (x .: "ScalableDimension")
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
                 [Just ("ServiceNamespace" .= _siServiceNamespace),
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
-- * 'spCreationTime' - The Unix timestamp when the scaling plan was created.
--
-- * 'spStatusStartTime' - The Unix timestamp when the scaling plan entered the current status.
--
-- * 'spStatusMessage' - A simple message about the current status of the scaling plan.
--
-- * 'spScalingPlanName' - The name of the scaling plan.
--
-- * 'spScalingPlanVersion' - The version of the scaling plan.
--
-- * 'spApplicationSource' - The application source.
--
-- * 'spScalingInstructions' - The scaling instructions.
--
-- * 'spStatusCode' - The status of the scaling plan.     * @Active@ - The scaling plan is active.     * @ActiveWithProblems@ - The scaling plan is active, but the scaling configuration for one or more resources could not be applied.     * @CreationInProgress@ - The scaling plan is being created.     * @CreationFailed@ - The scaling plan could not be created.     * @DeletionInProgress@ - The scaling plan is being deleted.     * @DeletionFailed@ - The scaling plan could not be deleted.
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


-- | The Unix timestamp when the scaling plan was created.
spCreationTime :: Lens' ScalingPlan (Maybe UTCTime)
spCreationTime = lens _spCreationTime (\ s a -> s{_spCreationTime = a}) . mapping _Time

-- | The Unix timestamp when the scaling plan entered the current status.
spStatusStartTime :: Lens' ScalingPlan (Maybe UTCTime)
spStatusStartTime = lens _spStatusStartTime (\ s a -> s{_spStatusStartTime = a}) . mapping _Time

-- | A simple message about the current status of the scaling plan.
spStatusMessage :: Lens' ScalingPlan (Maybe Text)
spStatusMessage = lens _spStatusMessage (\ s a -> s{_spStatusMessage = a})

-- | The name of the scaling plan.
spScalingPlanName :: Lens' ScalingPlan Text
spScalingPlanName = lens _spScalingPlanName (\ s a -> s{_spScalingPlanName = a})

-- | The version of the scaling plan.
spScalingPlanVersion :: Lens' ScalingPlan Integer
spScalingPlanVersion = lens _spScalingPlanVersion (\ s a -> s{_spScalingPlanVersion = a})

-- | The application source.
spApplicationSource :: Lens' ScalingPlan ApplicationSource
spApplicationSource = lens _spApplicationSource (\ s a -> s{_spApplicationSource = a})

-- | The scaling instructions.
spScalingInstructions :: Lens' ScalingPlan [ScalingInstruction]
spScalingInstructions = lens _spScalingInstructions (\ s a -> s{_spScalingInstructions = a}) . _Coerce

-- | The status of the scaling plan.     * @Active@ - The scaling plan is active.     * @ActiveWithProblems@ - The scaling plan is active, but the scaling configuration for one or more resources could not be applied.     * @CreationInProgress@ - The scaling plan is being created.     * @CreationFailed@ - The scaling plan could not be created.     * @DeletionInProgress@ - The scaling plan is being deleted.     * @DeletionFailed@ - The scaling plan could not be deleted.
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
-- * 'sprScalingPlanVersion' - The version of the scaling plan.
--
-- * 'sprServiceNamespace' - The namespace of the AWS service.
--
-- * 'sprResourceId' - The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
-- * 'sprScalableDimension' - The scalable dimension for the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.
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

-- | The version of the scaling plan.
sprScalingPlanVersion :: Lens' ScalingPlanResource Integer
sprScalingPlanVersion = lens _sprScalingPlanVersion (\ s a -> s{_sprScalingPlanVersion = a})

-- | The namespace of the AWS service.
sprServiceNamespace :: Lens' ScalingPlanResource ServiceNamespace
sprServiceNamespace = lens _sprServiceNamespace (\ s a -> s{_sprServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
sprResourceId :: Lens' ScalingPlanResource Text
sprResourceId = lens _sprResourceId (\ s a -> s{_sprResourceId = a})

-- | The scalable dimension for the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.
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
-- * 'spTargetTrackingConfiguration' - The target tracking scaling policy.
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


-- | The target tracking scaling policy.
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

-- | Represents a target tracking scaling policy.
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
-- * 'ttcPredefinedScalingMetricSpecification' - A predefined metric.
--
-- * 'ttcScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
--
-- * 'ttcDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is @true@ , scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is @false@ .
--
-- * 'ttcCustomizedScalingMetricSpecification' - A customized metric.
--
-- * 'ttcScaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
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

-- | A predefined metric.
ttcPredefinedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe PredefinedScalingMetricSpecification)
ttcPredefinedScalingMetricSpecification = lens _ttcPredefinedScalingMetricSpecification (\ s a -> s{_ttcPredefinedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
ttcScaleInCooldown :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcScaleInCooldown = lens _ttcScaleInCooldown (\ s a -> s{_ttcScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is @true@ , scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is @false@ .
ttcDisableScaleIn :: Lens' TargetTrackingConfiguration (Maybe Bool)
ttcDisableScaleIn = lens _ttcDisableScaleIn (\ s a -> s{_ttcDisableScaleIn = a})

-- | A customized metric.
ttcCustomizedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe CustomizedScalingMetricSpecification)
ttcCustomizedScalingMetricSpecification = lens _ttcCustomizedScalingMetricSpecification (\ s a -> s{_ttcCustomizedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
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
