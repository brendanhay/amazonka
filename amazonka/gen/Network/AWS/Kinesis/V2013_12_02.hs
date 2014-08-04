-- Module      : Network.AWS.Kinesis.V2013_12_02
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
module Network.AWS.Kinesis.V2013_12_02 (module Export) where

import Network.AWS.Kinesis.V2013_12_02.CreateStream as Export
import Network.AWS.Kinesis.V2013_12_02.DeleteStream as Export
import Network.AWS.Kinesis.V2013_12_02.DescribeStream as Export
import Network.AWS.Kinesis.V2013_12_02.GetRecords as Export
import Network.AWS.Kinesis.V2013_12_02.GetShardIterator as Export
import Network.AWS.Kinesis.V2013_12_02.ListStreams as Export
import Network.AWS.Kinesis.V2013_12_02.MergeShards as Export
import Network.AWS.Kinesis.V2013_12_02.PutRecord as Export
import Network.AWS.Kinesis.V2013_12_02.SplitShard as Export
import Network.AWS.Kinesis.V2013_12_02.Types as Export
