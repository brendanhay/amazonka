{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SystemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SystemStatus where

import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
--
--
-- /See:/ 'systemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { _ssCPUUtilization ::
      !(Maybe CPUUtilization),
    _ssLoadAverage :: !(Maybe [Double])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SystemStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCPUUtilization' - CPU utilization metrics for the instance.
--
-- * 'ssLoadAverage' - Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
systemStatus ::
  SystemStatus
systemStatus =
  SystemStatus'
    { _ssCPUUtilization = Nothing,
      _ssLoadAverage = Nothing
    }

-- | CPU utilization metrics for the instance.
ssCPUUtilization :: Lens' SystemStatus (Maybe CPUUtilization)
ssCPUUtilization = lens _ssCPUUtilization (\s a -> s {_ssCPUUtilization = a})

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
ssLoadAverage :: Lens' SystemStatus [Double]
ssLoadAverage = lens _ssLoadAverage (\s a -> s {_ssLoadAverage = a}) . _Default . _Coerce

instance FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      <$> (x .@? "CPUUtilization")
      <*> (x .@? "LoadAverage" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable SystemStatus

instance NFData SystemStatus
