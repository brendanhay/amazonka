{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlanResource where

import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
import Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a scalable resource.
--
--
--
-- /See:/ 'scalingPlanResource' smart constructor.
data ScalingPlanResource = ScalingPlanResource'
  { _sprScalingStatusMessage ::
      !(Maybe Text),
    _sprScalingPolicies :: !(Maybe [ScalingPolicy]),
    _sprScalingPlanName :: !Text,
    _sprScalingPlanVersion :: !Integer,
    _sprServiceNamespace :: !ServiceNamespace,
    _sprResourceId :: !Text,
    _sprScalableDimension :: !ScalableDimension,
    _sprScalingStatusCode :: !ScalingStatusCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
scalingPlanResource ::
  -- | 'sprScalingPlanName'
  Text ->
  -- | 'sprScalingPlanVersion'
  Integer ->
  -- | 'sprServiceNamespace'
  ServiceNamespace ->
  -- | 'sprResourceId'
  Text ->
  -- | 'sprScalableDimension'
  ScalableDimension ->
  -- | 'sprScalingStatusCode'
  ScalingStatusCode ->
  ScalingPlanResource
scalingPlanResource
  pScalingPlanName_
  pScalingPlanVersion_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pScalingStatusCode_ =
    ScalingPlanResource'
      { _sprScalingStatusMessage = Nothing,
        _sprScalingPolicies = Nothing,
        _sprScalingPlanName = pScalingPlanName_,
        _sprScalingPlanVersion = pScalingPlanVersion_,
        _sprServiceNamespace = pServiceNamespace_,
        _sprResourceId = pResourceId_,
        _sprScalableDimension = pScalableDimension_,
        _sprScalingStatusCode = pScalingStatusCode_
      }

-- | A simple message about the current scaling status of the resource.
sprScalingStatusMessage :: Lens' ScalingPlanResource (Maybe Text)
sprScalingStatusMessage = lens _sprScalingStatusMessage (\s a -> s {_sprScalingStatusMessage = a})

-- | The scaling policies.
sprScalingPolicies :: Lens' ScalingPlanResource [ScalingPolicy]
sprScalingPolicies = lens _sprScalingPolicies (\s a -> s {_sprScalingPolicies = a}) . _Default . _Coerce

-- | The name of the scaling plan.
sprScalingPlanName :: Lens' ScalingPlanResource Text
sprScalingPlanName = lens _sprScalingPlanName (\s a -> s {_sprScalingPlanName = a})

-- | The version number of the scaling plan.
sprScalingPlanVersion :: Lens' ScalingPlanResource Integer
sprScalingPlanVersion = lens _sprScalingPlanVersion (\s a -> s {_sprScalingPlanVersion = a})

-- | The namespace of the AWS service.
sprServiceNamespace :: Lens' ScalingPlanResource ServiceNamespace
sprServiceNamespace = lens _sprServiceNamespace (\s a -> s {_sprServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
sprResourceId :: Lens' ScalingPlanResource Text
sprResourceId = lens _sprResourceId (\s a -> s {_sprResourceId = a})

-- | The scalable dimension for the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
sprScalableDimension :: Lens' ScalingPlanResource ScalableDimension
sprScalableDimension = lens _sprScalableDimension (\s a -> s {_sprScalableDimension = a})

-- | The scaling status of the resource.     * @Active@ - The scaling configuration is active.     * @Inactive@ - The scaling configuration is not active because the scaling plan is being created or the scaling configuration could not be applied. Check the status message for more information.     * @PartiallyActive@ - The scaling configuration is partially active because the scaling plan is being created or deleted or the scaling configuration could not be fully applied. Check the status message for more information.
sprScalingStatusCode :: Lens' ScalingPlanResource ScalingStatusCode
sprScalingStatusCode = lens _sprScalingStatusCode (\s a -> s {_sprScalingStatusCode = a})

instance FromJSON ScalingPlanResource where
  parseJSON =
    withObject
      "ScalingPlanResource"
      ( \x ->
          ScalingPlanResource'
            <$> (x .:? "ScalingStatusMessage")
            <*> (x .:? "ScalingPolicies" .!= mempty)
            <*> (x .: "ScalingPlanName")
            <*> (x .: "ScalingPlanVersion")
            <*> (x .: "ServiceNamespace")
            <*> (x .: "ResourceId")
            <*> (x .: "ScalableDimension")
            <*> (x .: "ScalingStatusCode")
      )

instance Hashable ScalingPlanResource

instance NFData ScalingPlanResource
