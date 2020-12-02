{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LoadBalancer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The load balancer configuration to use with a service or task set.
--
--
-- For specific notes and restrictions regarding the use of load balancers with services and task sets, see the CreateService and CreateTaskSet actions.
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbLoadBalancerName ::
      !(Maybe Text),
    _lbContainerName :: !(Maybe Text),
    _lbTargetGroupARN :: !(Maybe Text),
    _lbContainerPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbLoadBalancerName' - The name of the load balancer to associate with the Amazon ECS service or task set. A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
--
-- * 'lbContainerName' - The name of the container (as it appears in a container definition) to associate with the load balancer.
--
-- * 'lbTargetGroupARN' - The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set. A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted. For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ . For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ . /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
-- * 'lbContainerPort' - The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
loadBalancer ::
  LoadBalancer
loadBalancer =
  LoadBalancer'
    { _lbLoadBalancerName = Nothing,
      _lbContainerName = Nothing,
      _lbTargetGroupARN = Nothing,
      _lbContainerPort = Nothing
    }

-- | The name of the load balancer to associate with the Amazon ECS service or task set. A load balancer name is only specified when using a Classic Load Balancer. If you are using an Application Load Balancer or a Network Load Balancer the load balancer name parameter should be omitted.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\s a -> s {_lbLoadBalancerName = a})

-- | The name of the container (as it appears in a container definition) to associate with the load balancer.
lbContainerName :: Lens' LoadBalancer (Maybe Text)
lbContainerName = lens _lbContainerName (\s a -> s {_lbContainerName = a})

-- | The full Amazon Resource Name (ARN) of the Elastic Load Balancing target group or groups associated with a service or task set. A target group ARN is only specified when using an Application Load Balancer or Network Load Balancer. If you are using a Classic Load Balancer the target group ARN should be omitted. For services using the @ECS@ deployment controller, you can specify one or multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Registering Multiple Target Groups with a Service> in the /Amazon Elastic Container Service Developer Guide/ . For services using the @CODE_DEPLOY@ deployment controller, you are required to define two target groups for the load balancer. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-bluegreen.html Blue/Green Deployment with CodeDeploy> in the /Amazon Elastic Container Service Developer Guide/ . /Important:/ If your service's task definition uses the @awsvpc@ network mode (which is required for the Fargate launch type), you must choose @ip@ as the target type, not @instance@ , when creating your target groups because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
lbTargetGroupARN :: Lens' LoadBalancer (Maybe Text)
lbTargetGroupARN = lens _lbTargetGroupARN (\s a -> s {_lbTargetGroupARN = a})

-- | The port on the container to associate with the load balancer. This port must correspond to a @containerPort@ in the task definition the tasks in the service are using. For tasks that use the EC2 launch type, the container instance they are launched on must allow ingress traffic on the @hostPort@ of the port mapping.
lbContainerPort :: Lens' LoadBalancer (Maybe Int)
lbContainerPort = lens _lbContainerPort (\s a -> s {_lbContainerPort = a})

instance FromJSON LoadBalancer where
  parseJSON =
    withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            <$> (x .:? "loadBalancerName")
            <*> (x .:? "containerName")
            <*> (x .:? "targetGroupArn")
            <*> (x .:? "containerPort")
      )

instance Hashable LoadBalancer

instance NFData LoadBalancer

instance ToJSON LoadBalancer where
  toJSON LoadBalancer' {..} =
    object
      ( catMaybes
          [ ("loadBalancerName" .=) <$> _lbLoadBalancerName,
            ("containerName" .=) <$> _lbContainerName,
            ("targetGroupArn" .=) <$> _lbTargetGroupARN,
            ("containerPort" .=) <$> _lbContainerPort
          ]
      )
