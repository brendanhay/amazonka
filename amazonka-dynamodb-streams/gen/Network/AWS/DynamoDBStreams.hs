{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon DynamoDB Streams
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
    (
    -- * Service Configuration
      dynamoDBStreams

    -- * Errors
    -- $errors

    -- ** ExpiredIteratorException
    , _ExpiredIteratorException

    -- ** InternalServerError
    , _InternalServerError

    -- ** TrimmedDataAccessException
    , _TrimmedDataAccessException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetShardIterator
    , module Network.AWS.DynamoDBStreams.GetShardIterator

    -- ** GetRecords
    , module Network.AWS.DynamoDBStreams.GetRecords

    -- ** ListStreams
    , module Network.AWS.DynamoDBStreams.ListStreams

    -- ** DescribeStream
    , module Network.AWS.DynamoDBStreams.DescribeStream

    -- * Types

    -- ** KeyType
    , KeyType (..)

    -- ** OperationType
    , OperationType (..)

    -- ** ShardIteratorType
    , ShardIteratorType (..)

    -- ** StreamStatus
    , StreamStatus (..)

    -- ** StreamViewType
    , StreamViewType (..)

    -- ** AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avNS
    , avM
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- ** KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- ** Record
    , Record
    , record
    , rEventVersion
    , rDynamodb
    , rAwsRegion
    , rEventName
    , rEventSource
    , rEventId

    -- ** SequenceNumberRange
    , SequenceNumberRange
    , sequenceNumberRange
    , snrStartingSequenceNumber
    , snrEndingSequenceNumber

    -- ** Shard
    , Shard
    , shard
    , sParentShardId
    , sSequenceNumberRange
    , sShardId

    -- ** Stream
    , Stream
    , stream
    , sStreamLabel
    , sStreamARN
    , sTableName

    -- ** StreamDescription
    , StreamDescription
    , streamDescription
    , sdLastEvaluatedShardId
    , sdStreamLabel
    , sdStreamStatus
    , sdKeySchema
    , sdStreamViewType
    , sdStreamARN
    , sdShards
    , sdTableName
    , sdCreationRequestDateTime

    -- ** StreamRecord
    , StreamRecord
    , streamRecord
    , srSizeBytes
    , srSequenceNumber
    , srStreamViewType
    , srKeys
    , srOldImage
    , srNewImage
    ) where

import           Network.AWS.DynamoDBStreams.DescribeStream
import           Network.AWS.DynamoDBStreams.GetRecords
import           Network.AWS.DynamoDBStreams.GetShardIterator
import           Network.AWS.DynamoDBStreams.ListStreams
import           Network.AWS.DynamoDBStreams.Types
import           Network.AWS.DynamoDBStreams.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DynamoDBStreams'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
