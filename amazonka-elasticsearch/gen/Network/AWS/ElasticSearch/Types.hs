{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types
    (
    -- * Service Configuration
      elasticSearch

    -- * Errors
    , _ValidationException
    , _ResourceAlreadyExistsException
    , _BaseException
    , _DisabledOperationException
    , _InternalException
    , _InvalidTypeException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- * OptionState
    , OptionState (..)

    -- * VolumeType
    , VolumeType (..)

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AdvancedOptionsStatus
    , AdvancedOptionsStatus
    , advancedOptionsStatus
    , aosOptions
    , aosStatus

    -- * DomainInfo
    , DomainInfo
    , domainInfo
    , diDomainName

    -- * EBSOptions
    , EBSOptions
    , ebsOptions
    , eoVolumeSize
    , eoIOPS
    , eoVolumeType
    , eoEBSEnabled

    -- * EBSOptionsStatus
    , EBSOptionsStatus
    , ebsOptionsStatus
    , eosOptions
    , eosStatus

    -- * ElasticsearchClusterConfig
    , ElasticsearchClusterConfig
    , elasticsearchClusterConfig
    , eccDedicatedMasterCount
    , eccDedicatedMasterType
    , eccDedicatedMasterEnabled
    , eccInstanceCount
    , eccZoneAwarenessEnabled
    , eccInstanceType

    -- * ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus
    , elasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- * ElasticsearchDomainConfig
    , ElasticsearchDomainConfig
    , elasticsearchDomainConfig
    , edcEBSOptions
    , edcAccessPolicies
    , edcElasticsearchClusterConfig
    , edcSnapshotOptions
    , edcAdvancedOptions
    , edcElasticsearchVersion

    -- * ElasticsearchDomainStatus
    , ElasticsearchDomainStatus
    , elasticsearchDomainStatus
    , edsEBSOptions
    , edsAccessPolicies
    , edsCreated
    , edsSnapshotOptions
    , edsDeleted
    , edsProcessing
    , edsEndpoint
    , edsAdvancedOptions
    , edsElasticsearchVersion
    , edsDomainId
    , edsDomainName
    , edsARN
    , edsElasticsearchClusterConfig

    -- * ElasticsearchVersionStatus
    , ElasticsearchVersionStatus
    , elasticsearchVersionStatus
    , evsOptions
    , evsStatus

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- * SnapshotOptions
    , SnapshotOptions
    , snapshotOptions
    , soAutomatedSnapshotStartHour

    -- * SnapshotOptionsStatus
    , SnapshotOptionsStatus
    , snapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.ElasticSearch.Types.Product
import           Network.AWS.ElasticSearch.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
elasticSearch :: Service
elasticSearch =
    Service
    { _svcAbbrev = "ElasticSearch"
    , _svcSigner = v4
    , _svcPrefix = "es"
    , _svcVersion = "2015-01-01"
    , _svcEndpoint = defaultEndpoint elasticSearch
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "ElasticSearch"
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

-- | An exception for missing / invalid input fields. Gives http status code of 400.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException =
    _ServiceError . hasStatus 400 . hasCode "ValidationException"

-- | An exception for creating a resource that already exists. Gives http status code of 400.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
    _ServiceError . hasStatus 409 . hasCode "ResourceAlreadyExistsException"

-- | An error occurred while processing the request.
--
--
_BaseException :: AsError a => Getting (First ServiceError) a ServiceError
_BaseException = _ServiceError . hasCode "BaseException"

-- | An error occured because the client wanted to access a not supported operation. Gives http status code of 409.
--
--
_DisabledOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledOperationException =
    _ServiceError . hasStatus 409 . hasCode "DisabledOperationException"

-- | The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException =
    _ServiceError . hasStatus 500 . hasCode "InternalException"

-- | An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.
--
--
_InvalidTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException =
    _ServiceError . hasStatus 409 . hasCode "InvalidTypeException"

-- | An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 409 . hasCode "ResourceNotFoundException"

-- | An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 409 . hasCode "LimitExceededException"
