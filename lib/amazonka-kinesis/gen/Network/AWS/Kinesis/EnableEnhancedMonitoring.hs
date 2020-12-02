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
-- Module      : Network.AWS.Kinesis.EnableEnhancedMonitoring
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables enhanced Kinesis data stream monitoring for shard-level metrics.
--
--
module Network.AWS.Kinesis.EnableEnhancedMonitoring
    (
    -- * Creating a Request
      enableEnhancedMonitoring
    , EnableEnhancedMonitoring
    -- * Request Lenses
    , eemStreamName
    , eemShardLevelMetrics

    -- * Destructuring the Response
    , enhancedMonitoringOutput
    , EnhancedMonitoringOutput
    -- * Response Lenses
    , emoDesiredShardLevelMetrics
    , emoCurrentShardLevelMetrics
    , emoStreamName
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for 'EnableEnhancedMonitoring' .
--
--
--
-- /See:/ 'enableEnhancedMonitoring' smart constructor.
data EnableEnhancedMonitoring = EnableEnhancedMonitoring'
  { _eemStreamName        :: !Text
  , _eemShardLevelMetrics :: ![MetricsName]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableEnhancedMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eemStreamName' - The name of the stream for which to enable enhanced monitoring.
--
-- * 'eemShardLevelMetrics' - List of shard-level metrics to enable. The following are the valid shard-level metrics. The value "@ALL@ " enables every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
enableEnhancedMonitoring
    :: Text -- ^ 'eemStreamName'
    -> EnableEnhancedMonitoring
enableEnhancedMonitoring pStreamName_ =
  EnableEnhancedMonitoring'
    {_eemStreamName = pStreamName_, _eemShardLevelMetrics = mempty}


-- | The name of the stream for which to enable enhanced monitoring.
eemStreamName :: Lens' EnableEnhancedMonitoring Text
eemStreamName = lens _eemStreamName (\ s a -> s{_eemStreamName = a})

-- | List of shard-level metrics to enable. The following are the valid shard-level metrics. The value "@ALL@ " enables every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
eemShardLevelMetrics :: Lens' EnableEnhancedMonitoring [MetricsName]
eemShardLevelMetrics = lens _eemShardLevelMetrics (\ s a -> s{_eemShardLevelMetrics = a}) . _Coerce

instance AWSRequest EnableEnhancedMonitoring where
        type Rs EnableEnhancedMonitoring =
             EnhancedMonitoringOutput
        request = postJSON kinesis
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable EnableEnhancedMonitoring where

instance NFData EnableEnhancedMonitoring where

instance ToHeaders EnableEnhancedMonitoring where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.EnableEnhancedMonitoring" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableEnhancedMonitoring where
        toJSON EnableEnhancedMonitoring'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _eemStreamName),
                  Just ("ShardLevelMetrics" .= _eemShardLevelMetrics)])

instance ToPath EnableEnhancedMonitoring where
        toPath = const "/"

instance ToQuery EnableEnhancedMonitoring where
        toQuery = const mempty
