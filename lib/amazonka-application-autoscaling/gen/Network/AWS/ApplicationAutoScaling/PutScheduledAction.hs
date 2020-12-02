{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.PutScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled action for an Application Auto Scaling scalable target.
--
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scheduled action applies to the scalable target identified by those three attributes. You cannot create a scheduled action until you have registered the resource as a scalable target.
--
-- When start and end times are specified with a recurring schedule using a cron expression or rates, they form the boundaries of when the recurring action starts and stops.
--
-- To update a scheduled action, specify the parameters that you want to change. If you don't specify start and end times, the old values are deleted.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
module Network.AWS.ApplicationAutoScaling.PutScheduledAction
  ( -- * Creating a Request
    putScheduledAction,
    PutScheduledAction,

    -- * Request Lenses
    psaStartTime,
    psaSchedule,
    psaEndTime,
    psaScalableTargetAction,
    psaServiceNamespace,
    psaScheduledActionName,
    psaResourceId,
    psaScalableDimension,

    -- * Destructuring the Response
    putScheduledActionResponse,
    PutScheduledActionResponse,

    -- * Response Lenses
    psarsResponseStatus,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScheduledAction' smart constructor.
data PutScheduledAction = PutScheduledAction'
  { _psaStartTime ::
      !(Maybe POSIX),
    _psaSchedule :: !(Maybe Text),
    _psaEndTime :: !(Maybe POSIX),
    _psaScalableTargetAction ::
      !(Maybe ScalableTargetAction),
    _psaServiceNamespace :: !ServiceNamespace,
    _psaScheduledActionName :: !Text,
    _psaResourceId :: !Text,
    _psaScalableDimension :: !ScalableDimension
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psaStartTime' - The date and time for this scheduled action to start.
--
-- * 'psaSchedule' - The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
--
-- * 'psaEndTime' - The date and time for the recurring schedule to end.
--
-- * 'psaScalableTargetAction' - The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- * 'psaServiceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- * 'psaScheduledActionName' - The name of the scheduled action. This name must be unique among all other scheduled actions on the specified scalable target.
--
-- * 'psaResourceId' - The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
-- * 'psaScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
putScheduledAction ::
  -- | 'psaServiceNamespace'
  ServiceNamespace ->
  -- | 'psaScheduledActionName'
  Text ->
  -- | 'psaResourceId'
  Text ->
  -- | 'psaScalableDimension'
  ScalableDimension ->
  PutScheduledAction
putScheduledAction
  pServiceNamespace_
  pScheduledActionName_
  pResourceId_
  pScalableDimension_ =
    PutScheduledAction'
      { _psaStartTime = Nothing,
        _psaSchedule = Nothing,
        _psaEndTime = Nothing,
        _psaScalableTargetAction = Nothing,
        _psaServiceNamespace = pServiceNamespace_,
        _psaScheduledActionName = pScheduledActionName_,
        _psaResourceId = pResourceId_,
        _psaScalableDimension = pScalableDimension_
      }

-- | The date and time for this scheduled action to start.
psaStartTime :: Lens' PutScheduledAction (Maybe UTCTime)
psaStartTime = lens _psaStartTime (\s a -> s {_psaStartTime = a}) . mapping _Time

-- | The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
psaSchedule :: Lens' PutScheduledAction (Maybe Text)
psaSchedule = lens _psaSchedule (\s a -> s {_psaSchedule = a})

-- | The date and time for the recurring schedule to end.
psaEndTime :: Lens' PutScheduledAction (Maybe UTCTime)
psaEndTime = lens _psaEndTime (\s a -> s {_psaEndTime = a}) . mapping _Time

-- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
psaScalableTargetAction :: Lens' PutScheduledAction (Maybe ScalableTargetAction)
psaScalableTargetAction = lens _psaScalableTargetAction (\s a -> s {_psaScalableTargetAction = a})

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
psaServiceNamespace :: Lens' PutScheduledAction ServiceNamespace
psaServiceNamespace = lens _psaServiceNamespace (\s a -> s {_psaServiceNamespace = a})

-- | The name of the scheduled action. This name must be unique among all other scheduled actions on the specified scalable target.
psaScheduledActionName :: Lens' PutScheduledAction Text
psaScheduledActionName = lens _psaScheduledActionName (\s a -> s {_psaScheduledActionName = a})

-- | The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
psaResourceId :: Lens' PutScheduledAction Text
psaResourceId = lens _psaResourceId (\s a -> s {_psaResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
psaScalableDimension :: Lens' PutScheduledAction ScalableDimension
psaScalableDimension = lens _psaScalableDimension (\s a -> s {_psaScalableDimension = a})

instance AWSRequest PutScheduledAction where
  type Rs PutScheduledAction = PutScheduledActionResponse
  request = postJSON applicationAutoScaling
  response =
    receiveEmpty
      (\s h x -> PutScheduledActionResponse' <$> (pure (fromEnum s)))

instance Hashable PutScheduledAction

instance NFData PutScheduledAction

instance ToHeaders PutScheduledAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AnyScaleFrontendService.PutScheduledAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutScheduledAction where
  toJSON PutScheduledAction' {..} =
    object
      ( catMaybes
          [ ("StartTime" .=) <$> _psaStartTime,
            ("Schedule" .=) <$> _psaSchedule,
            ("EndTime" .=) <$> _psaEndTime,
            ("ScalableTargetAction" .=) <$> _psaScalableTargetAction,
            Just ("ServiceNamespace" .= _psaServiceNamespace),
            Just ("ScheduledActionName" .= _psaScheduledActionName),
            Just ("ResourceId" .= _psaResourceId),
            Just ("ScalableDimension" .= _psaScalableDimension)
          ]
      )

instance ToPath PutScheduledAction where
  toPath = const "/"

instance ToQuery PutScheduledAction where
  toQuery = const mempty

-- | /See:/ 'putScheduledActionResponse' smart constructor.
newtype PutScheduledActionResponse = PutScheduledActionResponse'
  { _psarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScheduledActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psarsResponseStatus' - -- | The response status code.
putScheduledActionResponse ::
  -- | 'psarsResponseStatus'
  Int ->
  PutScheduledActionResponse
putScheduledActionResponse pResponseStatus_ =
  PutScheduledActionResponse'
    { _psarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
psarsResponseStatus :: Lens' PutScheduledActionResponse Int
psarsResponseStatus = lens _psarsResponseStatus (\s a -> s {_psarsResponseStatus = a})

instance NFData PutScheduledActionResponse
