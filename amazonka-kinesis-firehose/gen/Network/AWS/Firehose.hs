{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Kinesis Firehose API Reference
--
-- Amazon Kinesis Firehose is a fully-managed service that delivers
-- real-time streaming data to destinations such as Amazon S3 and Amazon
-- Redshift.
--
-- /See:/ <http://docs.aws.amazon.com/firehose/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.Firehose
    (
    -- * Service Configuration
      firehose

    -- * Errors
    -- $errors

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutRecord
    , module Network.AWS.Firehose.PutRecord

    -- ** UpdateDestination
    , module Network.AWS.Firehose.UpdateDestination

    -- ** PutRecordBatch
    , module Network.AWS.Firehose.PutRecordBatch

    -- ** CreateDeliveryStream
    , module Network.AWS.Firehose.CreateDeliveryStream

    -- ** DescribeDeliveryStream
    , module Network.AWS.Firehose.DescribeDeliveryStream

    -- ** ListDeliveryStreams
    , module Network.AWS.Firehose.ListDeliveryStreams

    -- ** DeleteDeliveryStream
    , module Network.AWS.Firehose.DeleteDeliveryStream

    -- * Types

    -- ** CompressionFormat
    , CompressionFormat (..)

    -- ** DeliveryStreamStatus
    , DeliveryStreamStatus (..)

    -- ** NoEncryptionConfig
    , NoEncryptionConfig (..)

    -- ** BufferingHints
    , BufferingHints
    , bufferingHints
    , bhSizeInMBs
    , bhIntervalInSeconds

    -- ** CopyCommand
    , CopyCommand
    , copyCommand
    , ccCopyOptions
    , ccDataTableColumns
    , ccDataTableName

    -- ** DeliveryStreamDescription
    , DeliveryStreamDescription
    , deliveryStreamDescription
    , dsdCreateTimestamp
    , dsdLastUpdateTimestamp
    , dsdDeliveryStreamName
    , dsdDeliveryStreamARN
    , dsdDeliveryStreamStatus
    , dsdVersionId
    , dsdDestinations
    , dsdHasMoreDestinations

    -- ** DestinationDescription
    , DestinationDescription
    , destinationDescription
    , ddS3DestinationDescription
    , ddRedshiftDestinationDescription
    , ddDestinationId

    -- ** EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecNoEncryptionConfig
    , ecKMSEncryptionConfig

    -- ** KMSEncryptionConfig
    , KMSEncryptionConfig
    , kmsEncryptionConfig
    , kecAWSKMSKeyARN

    -- ** PutRecordBatchResponseEntry
    , PutRecordBatchResponseEntry
    , putRecordBatchResponseEntry
    , prbreRecordId
    , prbreErrorCode
    , prbreErrorMessage

    -- ** Record
    , Record
    , record
    , rData

    -- ** RedshiftDestinationConfiguration
    , RedshiftDestinationConfiguration
    , redshiftDestinationConfiguration
    , rdcRoleARN
    , rdcClusterJDBCURL
    , rdcCopyCommand
    , rdcUsername
    , rdcPassword
    , rdcS3Configuration

    -- ** RedshiftDestinationDescription
    , RedshiftDestinationDescription
    , redshiftDestinationDescription
    , rddRoleARN
    , rddClusterJDBCURL
    , rddCopyCommand
    , rddUsername
    , rddS3DestinationDescription

    -- ** RedshiftDestinationUpdate
    , RedshiftDestinationUpdate
    , redshiftDestinationUpdate
    , rduUsername
    , rduS3Update
    , rduPassword
    , rduCopyCommand
    , rduClusterJDBCURL
    , rduRoleARN

    -- ** S3DestinationConfiguration
    , S3DestinationConfiguration
    , s3DestinationConfiguration
    , sdcPrefix
    , sdcEncryptionConfiguration
    , sdcCompressionFormat
    , sdcBufferingHints
    , sdcRoleARN
    , sdcBucketARN

    -- ** S3DestinationDescription
    , S3DestinationDescription
    , s3DestinationDescription
    , sddPrefix
    , sddRoleARN
    , sddBucketARN
    , sddBufferingHints
    , sddCompressionFormat
    , sddEncryptionConfiguration

    -- ** S3DestinationUpdate
    , S3DestinationUpdate
    , s3DestinationUpdate
    , sduPrefix
    , sduEncryptionConfiguration
    , sduCompressionFormat
    , sduBufferingHints
    , sduBucketARN
    , sduRoleARN
    ) where

import           Network.AWS.Firehose.CreateDeliveryStream
import           Network.AWS.Firehose.DeleteDeliveryStream
import           Network.AWS.Firehose.DescribeDeliveryStream
import           Network.AWS.Firehose.ListDeliveryStreams
import           Network.AWS.Firehose.PutRecord
import           Network.AWS.Firehose.PutRecordBatch
import           Network.AWS.Firehose.Types
import           Network.AWS.Firehose.UpdateDestination
import           Network.AWS.Firehose.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Firehose'.
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
