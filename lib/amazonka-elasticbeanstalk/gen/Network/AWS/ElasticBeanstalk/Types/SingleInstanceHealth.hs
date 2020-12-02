{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth where

import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
import Network.AWS.ElasticBeanstalk.Types.Deployment
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.
--
--
--
-- /See:/ 'singleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { _sihInstanceId ::
      !(Maybe Text),
    _sihCauses :: !(Maybe [Text]),
    _sihSystem :: !(Maybe SystemStatus),
    _sihApplicationMetrics ::
      !(Maybe ApplicationMetrics),
    _sihColor :: !(Maybe Text),
    _sihInstanceType :: !(Maybe Text),
    _sihAvailabilityZone :: !(Maybe Text),
    _sihHealthStatus :: !(Maybe Text),
    _sihDeployment :: !(Maybe Deployment),
    _sihLaunchedAt :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SingleInstanceHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihInstanceId' - The ID of the Amazon EC2 instance.
--
-- * 'sihCauses' - Represents the causes, which provide more information about the current health status.
--
-- * 'sihSystem' - Operating system metrics from the instance.
--
-- * 'sihApplicationMetrics' - Request metrics from your application.
--
-- * 'sihColor' - Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'sihInstanceType' - The instance's type.
--
-- * 'sihAvailabilityZone' - The availability zone in which the instance runs.
--
-- * 'sihHealthStatus' - Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'sihDeployment' - Information about the most recent deployment to an instance.
--
-- * 'sihLaunchedAt' - The time at which the EC2 instance was launched.
singleInstanceHealth ::
  SingleInstanceHealth
singleInstanceHealth =
  SingleInstanceHealth'
    { _sihInstanceId = Nothing,
      _sihCauses = Nothing,
      _sihSystem = Nothing,
      _sihApplicationMetrics = Nothing,
      _sihColor = Nothing,
      _sihInstanceType = Nothing,
      _sihAvailabilityZone = Nothing,
      _sihHealthStatus = Nothing,
      _sihDeployment = Nothing,
      _sihLaunchedAt = Nothing
    }

-- | The ID of the Amazon EC2 instance.
sihInstanceId :: Lens' SingleInstanceHealth (Maybe Text)
sihInstanceId = lens _sihInstanceId (\s a -> s {_sihInstanceId = a})

-- | Represents the causes, which provide more information about the current health status.
sihCauses :: Lens' SingleInstanceHealth [Text]
sihCauses = lens _sihCauses (\s a -> s {_sihCauses = a}) . _Default . _Coerce

-- | Operating system metrics from the instance.
sihSystem :: Lens' SingleInstanceHealth (Maybe SystemStatus)
sihSystem = lens _sihSystem (\s a -> s {_sihSystem = a})

-- | Request metrics from your application.
sihApplicationMetrics :: Lens' SingleInstanceHealth (Maybe ApplicationMetrics)
sihApplicationMetrics = lens _sihApplicationMetrics (\s a -> s {_sihApplicationMetrics = a})

-- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
sihColor :: Lens' SingleInstanceHealth (Maybe Text)
sihColor = lens _sihColor (\s a -> s {_sihColor = a})

-- | The instance's type.
sihInstanceType :: Lens' SingleInstanceHealth (Maybe Text)
sihInstanceType = lens _sihInstanceType (\s a -> s {_sihInstanceType = a})

-- | The availability zone in which the instance runs.
sihAvailabilityZone :: Lens' SingleInstanceHealth (Maybe Text)
sihAvailabilityZone = lens _sihAvailabilityZone (\s a -> s {_sihAvailabilityZone = a})

-- | Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
sihHealthStatus :: Lens' SingleInstanceHealth (Maybe Text)
sihHealthStatus = lens _sihHealthStatus (\s a -> s {_sihHealthStatus = a})

-- | Information about the most recent deployment to an instance.
sihDeployment :: Lens' SingleInstanceHealth (Maybe Deployment)
sihDeployment = lens _sihDeployment (\s a -> s {_sihDeployment = a})

-- | The time at which the EC2 instance was launched.
sihLaunchedAt :: Lens' SingleInstanceHealth (Maybe UTCTime)
sihLaunchedAt = lens _sihLaunchedAt (\s a -> s {_sihLaunchedAt = a}) . mapping _Time

instance FromXML SingleInstanceHealth where
  parseXML x =
    SingleInstanceHealth'
      <$> (x .@? "InstanceId")
      <*> (x .@? "Causes" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "System")
      <*> (x .@? "ApplicationMetrics")
      <*> (x .@? "Color")
      <*> (x .@? "InstanceType")
      <*> (x .@? "AvailabilityZone")
      <*> (x .@? "HealthStatus")
      <*> (x .@? "Deployment")
      <*> (x .@? "LaunchedAt")

instance Hashable SingleInstanceHealth

instance NFData SingleInstanceHealth
