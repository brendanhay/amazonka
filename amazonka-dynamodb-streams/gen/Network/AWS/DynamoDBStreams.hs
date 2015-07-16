{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon DynamoDB Streams
--
-- This is the Amazon DynamoDB Streams API Reference. This guide describes
-- the low-level API actions for accessing streams and processing stream
-- records. For information about application development with DynamoDB
-- Streams, see the
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide//Streams.html Amazon DynamoDB Developer Guide>.
--
-- Note that this document is intended for use with the following DynamoDB
-- documentation:
--
-- -   <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ Amazon DynamoDB Developer Guide>
--
-- -   <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/ Amazon DynamoDB API Reference>
--
-- The following are short descriptions of each low-level DynamoDB Streams
-- API action, organized by function.
--
-- -   /DescribeStream/ - Returns detailed information about a particular
--     stream.
--
-- -   /GetRecords/ - Retrieves the stream records from within a shard.
--
-- -   /GetShardIterator/ - Returns information on how to retrieve the
--     streams record from a shard with a given shard ID.
--
-- -   /ListStreams/ - Returns a list of all the streams associated with
--     the current AWS account and endpoint.
--
module Network.AWS.DynamoDBStreams
    ( module Export
    ) where

import           Network.AWS.DynamoDBStreams.DescribeStream   as Export
import           Network.AWS.DynamoDBStreams.GetRecords       as Export
import           Network.AWS.DynamoDBStreams.GetShardIterator as Export
import           Network.AWS.DynamoDBStreams.ListStreams      as Export
import           Network.AWS.DynamoDBStreams.Types            as Export
import           Network.AWS.DynamoDBStreams.Waiters          as Export
