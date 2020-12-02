{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics where

import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
--
--
-- /See:/ 'applicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { _amRequestCount ::
      !(Maybe Int),
    _amLatency :: !(Maybe Latency),
    _amStatusCodes :: !(Maybe StatusCodes),
    _amDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amRequestCount' - Average number of requests handled by the web server per second over the last 10 seconds.
--
-- * 'amLatency' - Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
--
-- * 'amStatusCodes' - Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
--
-- * 'amDuration' - The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
applicationMetrics ::
  ApplicationMetrics
applicationMetrics =
  ApplicationMetrics'
    { _amRequestCount = Nothing,
      _amLatency = Nothing,
      _amStatusCodes = Nothing,
      _amDuration = Nothing
    }

-- | Average number of requests handled by the web server per second over the last 10 seconds.
amRequestCount :: Lens' ApplicationMetrics (Maybe Int)
amRequestCount = lens _amRequestCount (\s a -> s {_amRequestCount = a})

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
amLatency :: Lens' ApplicationMetrics (Maybe Latency)
amLatency = lens _amLatency (\s a -> s {_amLatency = a})

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
amStatusCodes :: Lens' ApplicationMetrics (Maybe StatusCodes)
amStatusCodes = lens _amStatusCodes (\s a -> s {_amStatusCodes = a})

-- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
amDuration :: Lens' ApplicationMetrics (Maybe Int)
amDuration = lens _amDuration (\s a -> s {_amDuration = a})

instance FromXML ApplicationMetrics where
  parseXML x =
    ApplicationMetrics'
      <$> (x .@? "RequestCount")
      <*> (x .@? "Latency")
      <*> (x .@? "StatusCodes")
      <*> (x .@? "Duration")

instance Hashable ApplicationMetrics

instance NFData ApplicationMetrics
