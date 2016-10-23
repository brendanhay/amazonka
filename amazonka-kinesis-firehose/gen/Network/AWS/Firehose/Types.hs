{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types
    (
    -- * Service Configuration
      firehose

    -- * Errors
    , _InvalidArgumentException
    , _ConcurrentModificationException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * CompressionFormat
    , CompressionFormat (..)

    -- * DeliveryStreamStatus
    , DeliveryStreamStatus (..)

    -- * ElasticsearchIndexRotationPeriod
    , ElasticsearchIndexRotationPeriod (..)

    -- * ElasticsearchS3BackupMode
    , ElasticsearchS3BackupMode (..)

    -- * NoEncryptionConfig
    , NoEncryptionConfig (..)

    -- * BufferingHints
    , BufferingHints
    , bufferingHints
    , bhSizeInMBs
    , bhIntervalInSeconds

    -- * CloudWatchLoggingOptions
    , CloudWatchLoggingOptions
    , cloudWatchLoggingOptions
    , cwloEnabled
    , cwloLogGroupName
    , cwloLogStreamName

    -- * CopyCommand
    , CopyCommand
    , copyCommand
    , ccCopyOptions
    , ccDataTableColumns
    , ccDataTableName

    -- * DeliveryStreamDescription
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

    -- * DestinationDescription
    , DestinationDescription
    , destinationDescription
    , ddS3DestinationDescription
    , ddElasticsearchDestinationDescription
    , ddRedshiftDestinationDescription
    , ddDestinationId

    -- * ElasticsearchBufferingHints
    , ElasticsearchBufferingHints
    , elasticsearchBufferingHints
    , ebhSizeInMBs
    , ebhIntervalInSeconds

    -- * ElasticsearchDestinationConfiguration
    , ElasticsearchDestinationConfiguration
    , elasticsearchDestinationConfiguration
    , edcIndexRotationPeriod
    , edcS3BackupMode
    , edcCloudWatchLoggingOptions
    , edcBufferingHints
    , edcRetryOptions
    , edcRoleARN
    , edcDomainARN
    , edcIndexName
    , edcTypeName
    , edcS3Configuration

    -- * ElasticsearchDestinationDescription
    , ElasticsearchDestinationDescription
    , elasticsearchDestinationDescription
    , eddIndexRotationPeriod
    , eddTypeName
    , eddS3BackupMode
    , eddDomainARN
    , eddCloudWatchLoggingOptions
    , eddS3DestinationDescription
    , eddBufferingHints
    , eddRetryOptions
    , eddRoleARN
    , eddIndexName

    -- * ElasticsearchDestinationUpdate
    , ElasticsearchDestinationUpdate
    , elasticsearchDestinationUpdate
    , eduIndexRotationPeriod
    , eduTypeName
    , eduDomainARN
    , eduCloudWatchLoggingOptions
    , eduS3Update
    , eduBufferingHints
    , eduRetryOptions
    , eduRoleARN
    , eduIndexName

    -- * ElasticsearchRetryOptions
    , ElasticsearchRetryOptions
    , elasticsearchRetryOptions
    , eroDurationInSeconds

    -- * EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecNoEncryptionConfig
    , ecKMSEncryptionConfig

    -- * KMSEncryptionConfig
    , KMSEncryptionConfig
    , kmsEncryptionConfig
    , kecAWSKMSKeyARN

    -- * PutRecordBatchResponseEntry
    , PutRecordBatchResponseEntry
    , putRecordBatchResponseEntry
    , prbreRecordId
    , prbreErrorCode
    , prbreErrorMessage

    -- * Record
    , Record
    , record
    , rData

    -- * RedshiftDestinationConfiguration
    , RedshiftDestinationConfiguration
    , redshiftDestinationConfiguration
    , rdcCloudWatchLoggingOptions
    , rdcRetryOptions
    , rdcRoleARN
    , rdcClusterJDBCURL
    , rdcCopyCommand
    , rdcUsername
    , rdcPassword
    , rdcS3Configuration

    -- * RedshiftDestinationDescription
    , RedshiftDestinationDescription
    , redshiftDestinationDescription
    , rddCloudWatchLoggingOptions
    , rddRetryOptions
    , rddRoleARN
    , rddClusterJDBCURL
    , rddCopyCommand
    , rddUsername
    , rddS3DestinationDescription

    -- * RedshiftDestinationUpdate
    , RedshiftDestinationUpdate
    , redshiftDestinationUpdate
    , rduCloudWatchLoggingOptions
    , rduUsername
    , rduS3Update
    , rduPassword
    , rduCopyCommand
    , rduRetryOptions
    , rduClusterJDBCURL
    , rduRoleARN

    -- * RedshiftRetryOptions
    , RedshiftRetryOptions
    , redshiftRetryOptions
    , rroDurationInSeconds

    -- * S3DestinationConfiguration
    , S3DestinationConfiguration
    , s3DestinationConfiguration
    , sdcPrefix
    , sdcCloudWatchLoggingOptions
    , sdcEncryptionConfiguration
    , sdcCompressionFormat
    , sdcBufferingHints
    , sdcRoleARN
    , sdcBucketARN

    -- * S3DestinationDescription
    , S3DestinationDescription
    , s3DestinationDescription
    , sddPrefix
    , sddCloudWatchLoggingOptions
    , sddRoleARN
    , sddBucketARN
    , sddBufferingHints
    , sddCompressionFormat
    , sddEncryptionConfiguration

    -- * S3DestinationUpdate
    , S3DestinationUpdate
    , s3DestinationUpdate
    , sduPrefix
    , sduCloudWatchLoggingOptions
    , sduEncryptionConfiguration
    , sduCompressionFormat
    , sduBufferingHints
    , sduBucketARN
    , sduRoleARN
    ) where

import           Network.AWS.Firehose.Types.Product
import           Network.AWS.Firehose.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-08-04' of the Amazon Kinesis Firehose SDK configuration.
firehose :: Service
firehose =
    Service
    { _svcAbbrev = "Firehose"
    , _svcSigner = v4
    , _svcPrefix = "firehose"
    , _svcVersion = "2015-08-04"
    , _svcEndpoint = defaultEndpoint firehose
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Firehose"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The specified input parameter has an value that is not valid.
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException = _ServiceError . hasCode "InvalidArgumentException"

-- | Another modification has already happened. Fetch __VersionId__ again and use it to update the destination.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasCode "ConcurrentModificationException"

-- | The service is unavailable, back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <http://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Firehose Limits>.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasCode "ServiceUnavailableException"

-- | The specified resource could not be found.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | You have already reached the limit for a requested resource.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"

-- | The resource is already in use and not available for this operation.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _ServiceError . hasCode "ResourceInUseException"
