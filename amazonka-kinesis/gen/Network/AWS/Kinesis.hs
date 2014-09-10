-- Module      : Network.AWS.Kinesis
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Kinesis is a managed service that scales elastically for real-time
-- processing of streaming big data. The service takes in large streams of
-- data records that can then be consumed in real time by multiple
-- data-processing applications that can be run on Amazon EC2 instances.
module Network.AWS.Kinesis
    ( module Network.AWS.Kinesis.CreateStream
    , module Network.AWS.Kinesis.DeleteStream
    , module Network.AWS.Kinesis.DescribeStream
    , module Network.AWS.Kinesis.GetRecords
    , module Network.AWS.Kinesis.GetShardIterator
    , module Network.AWS.Kinesis.ListStreams
    , module Network.AWS.Kinesis.MergeShards
    , module Network.AWS.Kinesis.PutRecord
    , module Network.AWS.Kinesis.SplitShard
    , module Network.AWS.Kinesis.Types
    ) where

import Network.AWS.Kinesis.CreateStream
import Network.AWS.Kinesis.DeleteStream
import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.GetRecords
import Network.AWS.Kinesis.GetShardIterator
import Network.AWS.Kinesis.ListStreams
import Network.AWS.Kinesis.MergeShards
import Network.AWS.Kinesis.PutRecord
import Network.AWS.Kinesis.SplitShard
import Network.AWS.Kinesis.Types
