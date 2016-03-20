{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types
    (
    -- * Service Configuration
      dms

    -- * Errors
    , _InvalidSubnet
    , _KMSKeyNotAccessibleFault
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _InvalidResourceStateFault
    , _ResourceAlreadyExistsFault
    , _InsufficientResourceCapacityFault
    , _ResourceQuotaExceededFault
    , _UpgradeDependencyFailureFault
    , _ResourceNotFoundFault
    , _StorageQuotaExceededFault
    , _AccessDeniedFault
    , _SubnetAlreadyInUse

    -- * MigrationTypeValue
    , MigrationTypeValue (..)

    -- * RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- * ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- * StartReplicationTaskTypeValue
    , StartReplicationTaskTypeValue (..)

    -- * AccountQuota
    , AccountQuota
    , accountQuota
    , aqMax
    , aqUsed
    , aqAccountQuotaName

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * Connection
    , Connection
    , connection
    , cStatus
    , cReplicationInstanceARN
    , cEndpointIdentifier
    , cReplicationInstanceIdentifier
    , cEndpointARN
    , cLastFailureMessage

    -- * Endpoint
    , Endpoint
    , endpoint
    , eStatus
    , eServerName
    , eExtraConnectionAttributes
    , eEndpointType
    , eUsername
    , eEngineName
    , eKMSKeyId
    , eDatabaseName
    , eEndpointIdentifier
    , eEndpointARN
    , ePort

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * OrderableReplicationInstance
    , OrderableReplicationInstance
    , orderableReplicationInstance
    , oriEngineVersion
    , oriMinAllocatedStorage
    , oriIncludedAllocatedStorage
    , oriMaxAllocatedStorage
    , oriReplicationInstanceClass
    , oriDefaultAllocatedStorage
    , oriStorageType

    -- * RefreshSchemasStatus
    , RefreshSchemasStatus
    , refreshSchemasStatus
    , rssStatus
    , rssLastRefreshDate
    , rssReplicationInstanceARN
    , rssEndpointARN
    , rssLastFailureMessage

    -- * ReplicationInstance
    , ReplicationInstance
    , replicationInstance
    , riEngineVersion
    , riPubliclyAccessible
    , riAutoMinorVersionUpgrade
    , riReplicationSubnetGroup
    , riInstanceCreateTime
    , riReplicationInstanceStatus
    , riPreferredMaintenanceWindow
    , riReplicationInstancePrivateIPAddress
    , riKMSKeyId
    , riAvailabilityZone
    , riReplicationInstanceARN
    , riAllocatedStorage
    , riReplicationInstancePublicIPAddress
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riPendingModifiedValues

    -- * ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues
    , replicationPendingModifiedValues
    , rpmvEngineVersion
    , rpmvAllocatedStorage
    , rpmvReplicationInstanceClass

    -- * ReplicationSubnetGroup
    , ReplicationSubnetGroup
    , replicationSubnetGroup
    , rsgVPCId
    , rsgSubnets
    , rsgReplicationSubnetGroupIdentifier
    , rsgSubnetGroupStatus
    , rsgReplicationSubnetGroupDescription

    -- * ReplicationTask
    , ReplicationTask
    , replicationTask
    , rtReplicationTaskSettings
    , rtStatus
    , rtTargetEndpointARN
    , rtReplicationTaskIdentifier
    , rtReplicationTaskStartDate
    , rtSourceEndpointARN
    , rtTableMappings
    , rtReplicationTaskCreationDate
    , rtMigrationType
    , rtReplicationTaskARN
    , rtReplicationTaskStats
    , rtReplicationInstanceARN
    , rtLastFailureMessage

    -- * ReplicationTaskStats
    , ReplicationTaskStats
    , replicationTaskStats
    , rtsFullLoadProgressPercent
    , rtsElapsedTimeMillis
    , rtsTablesErrored
    , rtsTablesLoaded
    , rtsTablesQueued
    , rtsTablesLoading

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SupportedEndpointType
    , SupportedEndpointType
    , supportedEndpointType
    , setEndpointType
    , setEngineName
    , setSupportsCDC

    -- * TableStatistics
    , TableStatistics
    , tableStatistics
    , tsFullLoadRows
    , tsInserts
    , tsSchemaName
    , tsTableState
    , tsDdls
    , tsDeletes
    , tsUpdates
    , tsLastUpdateTime
    , tsTableName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.DMS.Types.Product
import           Network.AWS.DMS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2016-01-01' of the Amazon Database Migration Service SDK configuration.
dms :: Service
dms =
    Service
    { _svcAbbrev = "DMS"
    , _svcSigner = v4
    , _svcPrefix = "dms"
    , _svcVersion = "2016-01-01"
    , _svcEndpoint = defaultEndpoint dms
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for InvalidSubnet' errors.
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasCode "InvalidSubnet"

-- | Prism for KMSKeyNotAccessibleFault' errors.
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault = _ServiceError . hasCode "KMSKeyNotAccessibleFault"

-- | Prism for ReplicationSubnetGroupDoesNotCoverEnoughAZs' errors.
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
    _ServiceError . hasCode "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | Prism for InvalidResourceStateFault' errors.
_InvalidResourceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateFault =
    _ServiceError . hasCode "InvalidResourceStateFault"

-- | Prism for ResourceAlreadyExistsFault' errors.
_ResourceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsFault =
    _ServiceError . hasCode "ResourceAlreadyExistsFault"

-- | Prism for InsufficientResourceCapacityFault' errors.
_InsufficientResourceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientResourceCapacityFault =
    _ServiceError . hasCode "InsufficientResourceCapacityFault"

-- | Prism for ResourceQuotaExceededFault' errors.
_ResourceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceQuotaExceededFault =
    _ServiceError . hasCode "ResourceQuotaExceededFault"

-- | Prism for UpgradeDependencyFailureFault' errors.
_UpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_UpgradeDependencyFailureFault =
    _ServiceError . hasCode "UpgradeDependencyFailureFault"

-- | Prism for ResourceNotFoundFault' errors.
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault = _ServiceError . hasCode "ResourceNotFoundFault"

-- | Prism for StorageQuotaExceededFault' errors.
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault =
    _ServiceError . hasCode "StorageQuotaExceededFault"

-- | Prism for AccessDeniedFault' errors.
_AccessDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedFault = _ServiceError . hasCode "AccessDeniedFault"

-- | Prism for SubnetAlreadyInUse' errors.
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse = _ServiceError . hasCode "SubnetAlreadyInUse"
