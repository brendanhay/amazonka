{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DynamoDBStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-08-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon DynamoDB
--
-- Amazon DynamoDB Streams provides API actions for accessing streams and
-- processing stream records. To learn more about application development
-- with Streams, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html Capturing Table Activity with DynamoDB Streams>
-- in the Amazon DynamoDB Developer Guide.
module Amazonka.DynamoDBStreams
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ExpiredIteratorException
    _ExpiredIteratorException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TrimmedDataAccessException
    _TrimmedDataAccessException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** GetRecords
    GetRecords (GetRecords'),
    newGetRecords,
    GetRecordsResponse (GetRecordsResponse'),
    newGetRecordsResponse,

    -- ** GetShardIterator
    GetShardIterator (GetShardIterator'),
    newGetShardIterator,
    GetShardIteratorResponse (GetShardIteratorResponse'),
    newGetShardIteratorResponse,

    -- ** ListStreams
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- * Types

    -- ** Common
    module Amazonka.DynamoDBStreams.Types.AttributeValue,

    -- ** KeyType
    KeyType (..),

    -- ** OperationType
    OperationType (..),

    -- ** ShardIteratorType
    ShardIteratorType (..),

    -- ** StreamStatus
    StreamStatus (..),

    -- ** StreamViewType
    StreamViewType (..),

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** KeySchemaElement
    KeySchemaElement (KeySchemaElement'),
    newKeySchemaElement,

    -- ** Record
    Record (Record'),
    newRecord,

    -- ** SequenceNumberRange
    SequenceNumberRange (SequenceNumberRange'),
    newSequenceNumberRange,

    -- ** Shard
    Shard (Shard'),
    newShard,

    -- ** Stream
    Stream (Stream'),
    newStream,

    -- ** StreamDescription
    StreamDescription (StreamDescription'),
    newStreamDescription,

    -- ** StreamRecord
    StreamRecord (StreamRecord'),
    newStreamRecord,
  )
where

import Amazonka.DynamoDBStreams.DescribeStream
import Amazonka.DynamoDBStreams.GetRecords
import Amazonka.DynamoDBStreams.GetShardIterator
import Amazonka.DynamoDBStreams.Lens
import Amazonka.DynamoDBStreams.ListStreams
import Amazonka.DynamoDBStreams.Types
import Amazonka.DynamoDBStreams.Types.AttributeValue
import Amazonka.DynamoDBStreams.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DynamoDBStreams'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
