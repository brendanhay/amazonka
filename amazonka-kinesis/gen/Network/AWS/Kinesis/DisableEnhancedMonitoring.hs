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
-- Module      : Network.AWS.Kinesis.DisableEnhancedMonitoring
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables enhanced monitoring.
--
--
module Network.AWS.Kinesis.DisableEnhancedMonitoring
    (
    -- * Creating a Request
      disableEnhancedMonitoring
    , DisableEnhancedMonitoring
    -- * Request Lenses
    , demStreamName
    , demShardLevelMetrics

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

-- | Represents the input for 'DisableEnhancedMonitoring' .
--
--
--
-- /See:/ 'disableEnhancedMonitoring' smart constructor.
data DisableEnhancedMonitoring = DisableEnhancedMonitoring'
  { _demStreamName        :: !Text
  , _demShardLevelMetrics :: ![MetricsName]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableEnhancedMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demStreamName' - The name of the Kinesis data stream for which to disable enhanced monitoring.
--
-- * 'demShardLevelMetrics' - List of shard-level metrics to disable. The following are the valid shard-level metrics. The value "@ALL@ " disables every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
disableEnhancedMonitoring
    :: Text -- ^ 'demStreamName'
    -> DisableEnhancedMonitoring
disableEnhancedMonitoring pStreamName_ =
  DisableEnhancedMonitoring'
    {_demStreamName = pStreamName_, _demShardLevelMetrics = mempty}


-- | The name of the Kinesis data stream for which to disable enhanced monitoring.
demStreamName :: Lens' DisableEnhancedMonitoring Text
demStreamName = lens _demStreamName (\ s a -> s{_demStreamName = a})

-- | List of shard-level metrics to disable. The following are the valid shard-level metrics. The value "@ALL@ " disables every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
demShardLevelMetrics :: Lens' DisableEnhancedMonitoring [MetricsName]
demShardLevelMetrics = lens _demShardLevelMetrics (\ s a -> s{_demShardLevelMetrics = a}) . _Coerce

instance AWSRequest DisableEnhancedMonitoring where
        type Rs DisableEnhancedMonitoring =
             EnhancedMonitoringOutput
        request = postJSON kinesis
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DisableEnhancedMonitoring where

instance NFData DisableEnhancedMonitoring where

instance ToHeaders DisableEnhancedMonitoring where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DisableEnhancedMonitoring" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableEnhancedMonitoring where
        toJSON DisableEnhancedMonitoring'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _demStreamName),
                  Just ("ShardLevelMetrics" .= _demShardLevelMetrics)])

instance ToPath DisableEnhancedMonitoring where
        toPath = const "/"

instance ToQuery DisableEnhancedMonitoring where
        toQuery = const mempty
