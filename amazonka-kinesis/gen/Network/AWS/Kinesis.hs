-- Module      : Network.AWS.Kinesis
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Kinesis Service API Reference
--
-- Amazon Kinesis is a managed service that scales elastically for real
-- time processing of streaming big data.
module Network.AWS.Kinesis
    ( module Export
    ) where

import           Network.AWS.Kinesis.AddTagsToStream      as Export
import           Network.AWS.Kinesis.CreateStream         as Export
import           Network.AWS.Kinesis.DeleteStream         as Export
import           Network.AWS.Kinesis.DescribeStream       as Export
import           Network.AWS.Kinesis.GetRecords           as Export
import           Network.AWS.Kinesis.GetShardIterator     as Export
import           Network.AWS.Kinesis.ListStreams          as Export
import           Network.AWS.Kinesis.ListTagsForStream    as Export
import           Network.AWS.Kinesis.MergeShards          as Export
import           Network.AWS.Kinesis.PutRecord            as Export
import           Network.AWS.Kinesis.PutRecords           as Export
import           Network.AWS.Kinesis.RemoveTagsFromStream as Export
import           Network.AWS.Kinesis.SplitShard           as Export
import           Network.AWS.Kinesis.Types                as Export
import           Network.AWS.Kinesis.Waiters              as Export
