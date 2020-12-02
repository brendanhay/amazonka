{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription where

import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.Trigger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the AWS resources in use by this environment. This data is live.
--
--
--
-- /See:/ 'environmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { _erdQueues ::
      !(Maybe [Queue]),
    _erdTriggers ::
      !(Maybe [Trigger]),
    _erdLaunchTemplates ::
      !(Maybe [LaunchTemplate]),
    _erdLoadBalancers ::
      !(Maybe [LoadBalancer]),
    _erdEnvironmentName ::
      !(Maybe Text),
    _erdInstances ::
      !(Maybe [Instance]),
    _erdLaunchConfigurations ::
      !( Maybe
           [LaunchConfiguration]
       ),
    _erdAutoScalingGroups ::
      !(Maybe [AutoScalingGroup])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentResourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdQueues' - The queues used by this environment.
--
-- * 'erdTriggers' - The @AutoScaling@ triggers in use by this environment.
--
-- * 'erdLaunchTemplates' - The Amazon EC2 launch templates in use by this environment.
--
-- * 'erdLoadBalancers' - The LoadBalancers in use by this environment.
--
-- * 'erdEnvironmentName' - The name of the environment.
--
-- * 'erdInstances' - The Amazon EC2 instances used by this environment.
--
-- * 'erdLaunchConfigurations' - The Auto Scaling launch configurations in use by this environment.
--
-- * 'erdAutoScalingGroups' - The @AutoScalingGroups@ used by this environment.
environmentResourceDescription ::
  EnvironmentResourceDescription
environmentResourceDescription =
  EnvironmentResourceDescription'
    { _erdQueues = Nothing,
      _erdTriggers = Nothing,
      _erdLaunchTemplates = Nothing,
      _erdLoadBalancers = Nothing,
      _erdEnvironmentName = Nothing,
      _erdInstances = Nothing,
      _erdLaunchConfigurations = Nothing,
      _erdAutoScalingGroups = Nothing
    }

-- | The queues used by this environment.
erdQueues :: Lens' EnvironmentResourceDescription [Queue]
erdQueues = lens _erdQueues (\s a -> s {_erdQueues = a}) . _Default . _Coerce

-- | The @AutoScaling@ triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\s a -> s {_erdTriggers = a}) . _Default . _Coerce

-- | The Amazon EC2 launch templates in use by this environment.
erdLaunchTemplates :: Lens' EnvironmentResourceDescription [LaunchTemplate]
erdLaunchTemplates = lens _erdLaunchTemplates (\s a -> s {_erdLaunchTemplates = a}) . _Default . _Coerce

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\s a -> s {_erdLoadBalancers = a}) . _Default . _Coerce

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdEnvironmentName = lens _erdEnvironmentName (\s a -> s {_erdEnvironmentName = a})

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\s a -> s {_erdInstances = a}) . _Default . _Coerce

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations = lens _erdLaunchConfigurations (\s a -> s {_erdLaunchConfigurations = a}) . _Default . _Coerce

-- | The @AutoScalingGroups@ used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups = lens _erdAutoScalingGroups (\s a -> s {_erdAutoScalingGroups = a}) . _Default . _Coerce

instance FromXML EnvironmentResourceDescription where
  parseXML x =
    EnvironmentResourceDescription'
      <$> (x .@? "Queues" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Triggers" .!@ mempty >>= may (parseXMLList "member"))
      <*> ( x .@? "LaunchTemplates" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "LoadBalancers" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "EnvironmentName")
      <*> (x .@? "Instances" .!@ mempty >>= may (parseXMLList "member"))
      <*> ( x .@? "LaunchConfigurations" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "AutoScalingGroups" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable EnvironmentResourceDescription

instance NFData EnvironmentResourceDescription
