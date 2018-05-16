{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- * LogType
    , LogType (..)

    -- * OptionState
    , OptionState (..)

    -- * ReservedElasticsearchInstancePaymentOption
    , ReservedElasticsearchInstancePaymentOption (..)

    -- * VolumeType
    , VolumeType (..)

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AdditionalLimit
    , AdditionalLimit
    , additionalLimit
    , alLimitName
    , alLimitValues

    -- * AdvancedOptionsStatus
    , AdvancedOptionsStatus
    , advancedOptionsStatus
    , aosOptions
    , aosStatus

    -- * CognitoOptions
    , CognitoOptions
    , cognitoOptions
    , coIdentityPoolId
    , coEnabled
    , coUserPoolId
    , coRoleARN

    -- * CognitoOptionsStatus
    , CognitoOptionsStatus
    , cognitoOptionsStatus
    , cosOptions
    , cosStatus

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
    , edcLogPublishingOptions
    , edcElasticsearchClusterConfig
    , edcSnapshotOptions
    , edcCognitoOptions
    , edcEncryptionAtRestOptions
    , edcVPCOptions
    , edcAdvancedOptions
    , edcElasticsearchVersion

    -- * ElasticsearchDomainStatus
    , ElasticsearchDomainStatus
    , elasticsearchDomainStatus
    , edsEBSOptions
    , edsAccessPolicies
    , edsLogPublishingOptions
    , edsCreated
    , edsSnapshotOptions
    , edsCognitoOptions
    , edsEncryptionAtRestOptions
    , edsDeleted
    , edsVPCOptions
    , edsEndpoints
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

    -- * EncryptionAtRestOptions
    , EncryptionAtRestOptions
    , encryptionAtRestOptions
    , earoEnabled
    , earoKMSKeyId

    -- * EncryptionAtRestOptionsStatus
    , EncryptionAtRestOptionsStatus
    , encryptionAtRestOptionsStatus
    , earosOptions
    , earosStatus

    -- * InstanceCountLimits
    , InstanceCountLimits
    , instanceCountLimits
    , iclMaximumInstanceCount
    , iclMinimumInstanceCount

    -- * InstanceLimits
    , InstanceLimits
    , instanceLimits
    , ilInstanceCountLimits

    -- * Limits
    , Limits
    , limits
    , lInstanceLimits
    , lAdditionalLimits
    , lStorageTypes

    -- * LogPublishingOption
    , LogPublishingOption
    , logPublishingOption
    , lpoEnabled
    , lpoCloudWatchLogsLogGroupARN

    -- * LogPublishingOptionsStatus
    , LogPublishingOptionsStatus
    , logPublishingOptionsStatus
    , lposStatus
    , lposOptions

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReservedElasticsearchInstance
    , ReservedElasticsearchInstance
    , reservedElasticsearchInstance
    , reiState
    , reiCurrencyCode
    , reiStartTime
    , reiReservedElasticsearchInstanceOfferingId
    , reiReservedElasticsearchInstanceId
    , reiElasticsearchInstanceCount
    , reiReservationName
    , reiElasticsearchInstanceType
    , reiRecurringCharges
    , reiUsagePrice
    , reiFixedPrice
    , reiDuration
    , reiPaymentOption

    -- * ReservedElasticsearchInstanceOffering
    , ReservedElasticsearchInstanceOffering
    , reservedElasticsearchInstanceOffering
    , reioCurrencyCode
    , reioReservedElasticsearchInstanceOfferingId
    , reioElasticsearchInstanceType
    , reioRecurringCharges
    , reioUsagePrice
    , reioFixedPrice
    , reioDuration
    , reioPaymentOption

    -- * SnapshotOptions
    , SnapshotOptions
    , snapshotOptions
    , soAutomatedSnapshotStartHour

    -- * SnapshotOptionsStatus
    , SnapshotOptionsStatus
    , snapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- * StorageType
    , StorageType
    , storageType
    , stStorageTypeLimits
    , stStorageSubTypeName
    , stStorageTypeName

    -- * StorageTypeLimit
    , StorageTypeLimit
    , storageTypeLimit
    , stlLimitName
    , stlLimitValues

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * VPCDerivedInfo
    , VPCDerivedInfo
    , vpcDerivedInfo
    , vdiSecurityGroupIds
    , vdiSubnetIds
    , vdiVPCId
    , vdiAvailabilityZones

    -- * VPCDerivedInfoStatus
    , VPCDerivedInfoStatus
    , vpcDerivedInfoStatus
    , vdisOptions
    , vdisStatus

    -- * VPCOptions
    , VPCOptions
    , vpcOptions
    , voSecurityGroupIds
    , voSubnetIds
    ) where

import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.ElasticSearch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
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
  _MatchServiceError elasticSearch "ValidationException" . hasStatus 400


-- | An exception for creating a resource that already exists. Gives http status code of 400.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError elasticSearch "ResourceAlreadyExistsException" .
  hasStatus 409


-- | An error occurred while processing the request.
--
--
_BaseException :: AsError a => Getting (First ServiceError) a ServiceError
_BaseException = _MatchServiceError elasticSearch "BaseException"


-- | An error occured because the client wanted to access a not supported operation. Gives http status code of 409.
--
--
_DisabledOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledOperationException =
  _MatchServiceError elasticSearch "DisabledOperationException" . hasStatus 409


-- | The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException =
  _MatchServiceError elasticSearch "InternalException" . hasStatus 500


-- | An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.
--
--
_InvalidTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException =
  _MatchServiceError elasticSearch "InvalidTypeException" . hasStatus 409


-- | An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError elasticSearch "ResourceNotFoundException" . hasStatus 409


-- | An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError elasticSearch "LimitExceededException" . hasStatus 409

