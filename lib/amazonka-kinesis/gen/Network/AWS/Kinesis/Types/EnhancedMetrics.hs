{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMetrics where

import Network.AWS.Kinesis.Types.MetricsName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents enhanced metrics types.
--
--
--
-- /See:/ 'enhancedMetrics' smart constructor.
newtype EnhancedMetrics = EnhancedMetrics'
  { _emShardLevelMetrics ::
      Maybe [MetricsName]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnhancedMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emShardLevelMetrics' - List of shard-level metrics. The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
enhancedMetrics ::
  EnhancedMetrics
enhancedMetrics = EnhancedMetrics' {_emShardLevelMetrics = Nothing}

-- | List of shard-level metrics. The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
emShardLevelMetrics :: Lens' EnhancedMetrics [MetricsName]
emShardLevelMetrics = lens _emShardLevelMetrics (\s a -> s {_emShardLevelMetrics = a}) . _Default . _Coerce

instance FromJSON EnhancedMetrics where
  parseJSON =
    withObject
      "EnhancedMetrics"
      ( \x ->
          EnhancedMetrics' <$> (x .:? "ShardLevelMetrics" .!= mempty)
      )

instance Hashable EnhancedMetrics

instance NFData EnhancedMetrics
