{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.PutScheduledAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled action for an Application Auto Scaling scalable target.
--
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scheduled action applies to the scalable target identified by those three attributes. You cannot create a scheduled action until you register the scalable target using 'RegisterScalableTarget' .
--
-- To update an action, specify its name and the parameters that you want to change. If you don't specify start and end times, the old values are deleted. Any other parameters that you don't specify are not changed by this update request.
--
-- You can view the scheduled actions using 'DescribeScheduledActions' . If you are no longer using a scheduled action, you can delete it using 'DeleteScheduledAction' .
--
module Network.AWS.ApplicationAutoScaling.PutScheduledAction
    (
    -- * Creating a Request
      putScheduledAction
    , PutScheduledAction
    -- * Request Lenses
    , psaScalableDimension
    , psaStartTime
    , psaSchedule
    , psaEndTime
    , psaScalableTargetAction
    , psaServiceNamespace
    , psaScheduledActionName
    , psaResourceId

    -- * Destructuring the Response
    , putScheduledActionResponse
    , PutScheduledActionResponse
    -- * Response Lenses
    , psarsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScheduledAction' smart constructor.
data PutScheduledAction = PutScheduledAction'
  { _psaScalableDimension    :: !(Maybe ScalableDimension)
  , _psaStartTime            :: !(Maybe POSIX)
  , _psaSchedule             :: !(Maybe Text)
  , _psaEndTime              :: !(Maybe POSIX)
  , _psaScalableTargetAction :: !(Maybe ScalableTargetAction)
  , _psaServiceNamespace     :: !ServiceNamespace
  , _psaScheduledActionName  :: !Text
  , _psaResourceId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psaScalableDimension' - The scalable dimension. This parameter is required if you are creating a scheduled action. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
-- * 'psaStartTime' - The date and time for the scheduled action to start.
--
-- * 'psaSchedule' - The schedule for this action. The following formats are supported:     * At expressions - @at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@      * Rate expressions - @rate(/value/ /unit/ )@      * Cron expressions - @cron(/fields/ )@  At expressions are useful for one-time schedules. Specify the time, in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://en.wikipedia.org/wiki/Cron Cron> .
--
-- * 'psaEndTime' - The date and time for the scheduled action to end.
--
-- * 'psaScalableTargetAction' - The new minimum and maximum capacity. You can set both values or just one. During the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- * 'psaServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'psaScheduledActionName' - The name of the scheduled action.
--
-- * 'psaResourceId' - The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
putScheduledAction
    :: ServiceNamespace -- ^ 'psaServiceNamespace'
    -> Text -- ^ 'psaScheduledActionName'
    -> Text -- ^ 'psaResourceId'
    -> PutScheduledAction
putScheduledAction pServiceNamespace_ pScheduledActionName_ pResourceId_ =
  PutScheduledAction'
    { _psaScalableDimension = Nothing
    , _psaStartTime = Nothing
    , _psaSchedule = Nothing
    , _psaEndTime = Nothing
    , _psaScalableTargetAction = Nothing
    , _psaServiceNamespace = pServiceNamespace_
    , _psaScheduledActionName = pScheduledActionName_
    , _psaResourceId = pResourceId_
    }


-- | The scalable dimension. This parameter is required if you are creating a scheduled action. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
psaScalableDimension :: Lens' PutScheduledAction (Maybe ScalableDimension)
psaScalableDimension = lens _psaScalableDimension (\ s a -> s{_psaScalableDimension = a})

-- | The date and time for the scheduled action to start.
psaStartTime :: Lens' PutScheduledAction (Maybe UTCTime)
psaStartTime = lens _psaStartTime (\ s a -> s{_psaStartTime = a}) . mapping _Time

-- | The schedule for this action. The following formats are supported:     * At expressions - @at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@      * Rate expressions - @rate(/value/ /unit/ )@      * Cron expressions - @cron(/fields/ )@  At expressions are useful for one-time schedules. Specify the time, in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://en.wikipedia.org/wiki/Cron Cron> .
psaSchedule :: Lens' PutScheduledAction (Maybe Text)
psaSchedule = lens _psaSchedule (\ s a -> s{_psaSchedule = a})

-- | The date and time for the scheduled action to end.
psaEndTime :: Lens' PutScheduledAction (Maybe UTCTime)
psaEndTime = lens _psaEndTime (\ s a -> s{_psaEndTime = a}) . mapping _Time

-- | The new minimum and maximum capacity. You can set both values or just one. During the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
psaScalableTargetAction :: Lens' PutScheduledAction (Maybe ScalableTargetAction)
psaScalableTargetAction = lens _psaScalableTargetAction (\ s a -> s{_psaScalableTargetAction = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
psaServiceNamespace :: Lens' PutScheduledAction ServiceNamespace
psaServiceNamespace = lens _psaServiceNamespace (\ s a -> s{_psaServiceNamespace = a})

-- | The name of the scheduled action.
psaScheduledActionName :: Lens' PutScheduledAction Text
psaScheduledActionName = lens _psaScheduledActionName (\ s a -> s{_psaScheduledActionName = a})

-- | The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
psaResourceId :: Lens' PutScheduledAction Text
psaResourceId = lens _psaResourceId (\ s a -> s{_psaResourceId = a})

instance AWSRequest PutScheduledAction where
        type Rs PutScheduledAction =
             PutScheduledActionResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 PutScheduledActionResponse' <$> (pure (fromEnum s)))

instance Hashable PutScheduledAction where

instance NFData PutScheduledAction where

instance ToHeaders PutScheduledAction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.PutScheduledAction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutScheduledAction where
        toJSON PutScheduledAction'{..}
          = object
              (catMaybes
                 [("ScalableDimension" .=) <$> _psaScalableDimension,
                  ("StartTime" .=) <$> _psaStartTime,
                  ("Schedule" .=) <$> _psaSchedule,
                  ("EndTime" .=) <$> _psaEndTime,
                  ("ScalableTargetAction" .=) <$>
                    _psaScalableTargetAction,
                  Just ("ServiceNamespace" .= _psaServiceNamespace),
                  Just
                    ("ScheduledActionName" .= _psaScheduledActionName),
                  Just ("ResourceId" .= _psaResourceId)])

instance ToPath PutScheduledAction where
        toPath = const "/"

instance ToQuery PutScheduledAction where
        toQuery = const mempty

-- | /See:/ 'putScheduledActionResponse' smart constructor.
newtype PutScheduledActionResponse = PutScheduledActionResponse'
  { _psarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScheduledActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psarsResponseStatus' - -- | The response status code.
putScheduledActionResponse
    :: Int -- ^ 'psarsResponseStatus'
    -> PutScheduledActionResponse
putScheduledActionResponse pResponseStatus_ =
  PutScheduledActionResponse' {_psarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
psarsResponseStatus :: Lens' PutScheduledActionResponse Int
psarsResponseStatus = lens _psarsResponseStatus (\ s a -> s{_psarsResponseStatus = a})

instance NFData PutScheduledActionResponse where
