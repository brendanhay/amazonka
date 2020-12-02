{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScheduledAction where

import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a scheduled action.
--
--
--
-- /See:/ 'scheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { _saScalableDimension ::
      !(Maybe ScalableDimension),
    _saStartTime :: !(Maybe POSIX),
    _saEndTime :: !(Maybe POSIX),
    _saScalableTargetAction :: !(Maybe ScalableTargetAction),
    _saScheduledActionName :: !Text,
    _saScheduledActionARN :: !Text,
    _saServiceNamespace :: !ServiceNamespace,
    _saSchedule :: !Text,
    _saResourceId :: !Text,
    _saCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
-- * 'saStartTime' - The date and time that the action is scheduled to begin.
--
-- * 'saEndTime' - The date and time that the action is scheduled to end.
--
-- * 'saScalableTargetAction' - The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- * 'saScheduledActionName' - The name of the scheduled action.
--
-- * 'saScheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- * 'saServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'saSchedule' - The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
--
-- * 'saResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
-- * 'saCreationTime' - The date and time that the scheduled action was created.
scheduledAction ::
  -- | 'saScheduledActionName'
  Text ->
  -- | 'saScheduledActionARN'
  Text ->
  -- | 'saServiceNamespace'
  ServiceNamespace ->
  -- | 'saSchedule'
  Text ->
  -- | 'saResourceId'
  Text ->
  -- | 'saCreationTime'
  UTCTime ->
  ScheduledAction
scheduledAction
  pScheduledActionName_
  pScheduledActionARN_
  pServiceNamespace_
  pSchedule_
  pResourceId_
  pCreationTime_ =
    ScheduledAction'
      { _saScalableDimension = Nothing,
        _saStartTime = Nothing,
        _saEndTime = Nothing,
        _saScalableTargetAction = Nothing,
        _saScheduledActionName = pScheduledActionName_,
        _saScheduledActionARN = pScheduledActionARN_,
        _saServiceNamespace = pServiceNamespace_,
        _saSchedule = pSchedule_,
        _saResourceId = pResourceId_,
        _saCreationTime = _Time # pCreationTime_
      }

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
saScalableDimension :: Lens' ScheduledAction (Maybe ScalableDimension)
saScalableDimension = lens _saScalableDimension (\s a -> s {_saScalableDimension = a})

-- | The date and time that the action is scheduled to begin.
saStartTime :: Lens' ScheduledAction (Maybe UTCTime)
saStartTime = lens _saStartTime (\s a -> s {_saStartTime = a}) . mapping _Time

-- | The date and time that the action is scheduled to end.
saEndTime :: Lens' ScheduledAction (Maybe UTCTime)
saEndTime = lens _saEndTime (\s a -> s {_saEndTime = a}) . mapping _Time

-- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
saScalableTargetAction :: Lens' ScheduledAction (Maybe ScalableTargetAction)
saScalableTargetAction = lens _saScalableTargetAction (\s a -> s {_saScalableTargetAction = a})

-- | The name of the scheduled action.
saScheduledActionName :: Lens' ScheduledAction Text
saScheduledActionName = lens _saScheduledActionName (\s a -> s {_saScheduledActionName = a})

-- | The Amazon Resource Name (ARN) of the scheduled action.
saScheduledActionARN :: Lens' ScheduledAction Text
saScheduledActionARN = lens _saScheduledActionARN (\s a -> s {_saScheduledActionARN = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
saServiceNamespace :: Lens' ScheduledAction ServiceNamespace
saServiceNamespace = lens _saServiceNamespace (\s a -> s {_saServiceNamespace = a})

-- | The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
saSchedule :: Lens' ScheduledAction Text
saSchedule = lens _saSchedule (\s a -> s {_saSchedule = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
saResourceId :: Lens' ScheduledAction Text
saResourceId = lens _saResourceId (\s a -> s {_saResourceId = a})

-- | The date and time that the scheduled action was created.
saCreationTime :: Lens' ScheduledAction UTCTime
saCreationTime = lens _saCreationTime (\s a -> s {_saCreationTime = a}) . _Time

instance FromJSON ScheduledAction where
  parseJSON =
    withObject
      "ScheduledAction"
      ( \x ->
          ScheduledAction'
            <$> (x .:? "ScalableDimension")
            <*> (x .:? "StartTime")
            <*> (x .:? "EndTime")
            <*> (x .:? "ScalableTargetAction")
            <*> (x .: "ScheduledActionName")
            <*> (x .: "ScheduledActionARN")
            <*> (x .: "ServiceNamespace")
            <*> (x .: "Schedule")
            <*> (x .: "ResourceId")
            <*> (x .: "CreationTime")
      )

instance Hashable ScheduledAction

instance NFData ScheduledAction
