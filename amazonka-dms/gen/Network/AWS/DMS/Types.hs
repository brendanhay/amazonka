{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types
    (
    -- * Service Configuration
      dms

    -- * Errors
    , _KMSAccessDeniedFault
    , _KMSDisabledFault
    , _InvalidSubnet
    , _KMSKeyNotAccessibleFault
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _InvalidResourceStateFault
    , _InvalidCertificateFault
    , _SNSNoAuthorizationFault
    , _ResourceAlreadyExistsFault
    , _InsufficientResourceCapacityFault
    , _SNSInvalidTopicFault
    , _KMSNotFoundFault
    , _KMSThrottlingFault
    , _ResourceQuotaExceededFault
    , _UpgradeDependencyFailureFault
    , _ResourceNotFoundFault
    , _StorageQuotaExceededFault
    , _AccessDeniedFault
    , _SubnetAlreadyInUse
    , _KMSInvalidStateFault

    -- * AuthMechanismValue
    , AuthMechanismValue (..)

    -- * AuthTypeValue
    , AuthTypeValue (..)

    -- * CompressionTypeValue
    , CompressionTypeValue (..)

    -- * DataFormatValue
    , DataFormatValue (..)

    -- * DmsSSLModeValue
    , DmsSSLModeValue (..)

    -- * EncodingTypeValue
    , EncodingTypeValue (..)

    -- * EncryptionModeValue
    , EncryptionModeValue (..)

    -- * MessageFormatValue
    , MessageFormatValue (..)

    -- * MigrationTypeValue
    , MigrationTypeValue (..)

    -- * NestingLevelValue
    , NestingLevelValue (..)

    -- * ParquetVersionValue
    , ParquetVersionValue (..)

    -- * RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- * ReloadOptionValue
    , ReloadOptionValue (..)

    -- * ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- * SourceType
    , SourceType (..)

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

    -- * Certificate
    , Certificate
    , certificate
    , cCertificateOwner
    , cSigningAlgorithm
    , cValidFromDate
    , cCertificatePem
    , cCertificateARN
    , cCertificateCreationDate
    , cCertificateIdentifier
    , cCertificateWallet
    , cKeyLength
    , cValidToDate

    -- * Connection
    , Connection
    , connection
    , cStatus
    , cReplicationInstanceARN
    , cEndpointIdentifier
    , cReplicationInstanceIdentifier
    , cEndpointARN
    , cLastFailureMessage

    -- * DmsTransferSettings
    , DmsTransferSettings
    , dmsTransferSettings
    , dtsServiceAccessRoleARN
    , dtsBucketName

    -- * DynamoDBSettings
    , DynamoDBSettings
    , dynamoDBSettings
    , ddsServiceAccessRoleARN

    -- * ElasticsearchSettings
    , ElasticsearchSettings
    , elasticsearchSettings
    , esFullLoadErrorPercentage
    , esErrorRetryDuration
    , esServiceAccessRoleARN
    , esEndpointURI

    -- * Endpoint
    , Endpoint
    , endpoint
    , eStatus
    , eDmsTransferSettings
    , eServerName
    , eCertificateARN
    , eServiceAccessRoleARN
    , eEngineDisplayName
    , eExtraConnectionAttributes
    , eEndpointType
    , eRedshiftSettings
    , eElasticsearchSettings
    , eUsername
    , eExternalTableDefinition
    , eEngineName
    , eKMSKeyId
    , eMongoDBSettings
    , eSSLMode
    , eDatabaseName
    , eS3Settings
    , eKinesisSettings
    , eEndpointIdentifier
    , eExternalId
    , eDynamoDBSettings
    , eEndpointARN
    , ePort

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- * EventCategoryGroup
    , EventCategoryGroup
    , eventCategoryGroup
    , ecgSourceType
    , ecgEventCategories

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * KinesisSettings
    , KinesisSettings
    , kinesisSettings
    , ksServiceAccessRoleARN
    , ksStreamARN
    , ksMessageFormat

    -- * MongoDBSettings
    , MongoDBSettings
    , mongoDBSettings
    , mdsServerName
    , mdsAuthMechanism
    , mdsUsername
    , mdsKMSKeyId
    , mdsPassword
    , mdsNestingLevel
    , mdsDatabaseName
    , mdsDocsToInvestigate
    , mdsAuthSource
    , mdsExtractDocId
    , mdsAuthType
    , mdsPort

    -- * OrderableReplicationInstance
    , OrderableReplicationInstance
    , orderableReplicationInstance
    , oriEngineVersion
    , oriMinAllocatedStorage
    , oriIncludedAllocatedStorage
    , oriAvailabilityZones
    , oriMaxAllocatedStorage
    , oriReplicationInstanceClass
    , oriDefaultAllocatedStorage
    , oriStorageType

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- * RedshiftSettings
    , RedshiftSettings
    , redshiftSettings
    , rsEmptyAsNull
    , rsMaxFileSize
    , rsReplaceChars
    , rsServerName
    , rsConnectionTimeout
    , rsLoadTimeout
    , rsServiceAccessRoleARN
    , rsBucketFolder
    , rsTruncateColumns
    , rsReplaceInvalidChars
    , rsUsername
    , rsBucketName
    , rsEncryptionMode
    , rsDateFormat
    , rsRemoveQuotes
    , rsPassword
    , rsDatabaseName
    , rsAcceptAnyDate
    , rsAfterConnectScript
    , rsWriteBufferSize
    , rsTrimBlanks
    , rsTimeFormat
    , rsServerSideEncryptionKMSKeyId
    , rsPort
    , rsFileTransferUploadStreams

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
    , riReplicationInstancePublicIPAddresses
    , riReplicationSubnetGroup
    , riInstanceCreateTime
    , riFreeUntil
    , riReplicationInstanceStatus
    , riReplicationInstancePrivateIPAddresses
    , riPreferredMaintenanceWindow
    , riReplicationInstancePrivateIPAddress
    , riKMSKeyId
    , riAvailabilityZone
    , riVPCSecurityGroups
    , riMultiAZ
    , riSecondaryAvailabilityZone
    , riReplicationInstanceARN
    , riAllocatedStorage
    , riDNSNameServers
    , riReplicationInstancePublicIPAddress
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riPendingModifiedValues

    -- * ReplicationInstanceTaskLog
    , ReplicationInstanceTaskLog
    , replicationInstanceTaskLog
    , ritlReplicationTaskName
    , ritlReplicationTaskARN
    , ritlReplicationInstanceTaskLogSize

    -- * ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues
    , replicationPendingModifiedValues
    , rpmvEngineVersion
    , rpmvMultiAZ
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
    , rReplicationTaskSettings
    , rStatus
    , rStopReason
    , rTargetEndpointARN
    , rReplicationTaskIdentifier
    , rCdcStartPosition
    , rReplicationTaskStartDate
    , rSourceEndpointARN
    , rRecoveryCheckpoint
    , rTableMappings
    , rReplicationTaskCreationDate
    , rMigrationType
    , rReplicationTaskARN
    , rCdcStopPosition
    , rReplicationTaskStats
    , rReplicationInstanceARN
    , rLastFailureMessage

    -- * ReplicationTaskAssessmentResult
    , ReplicationTaskAssessmentResult
    , replicationTaskAssessmentResult
    , rtarAssessmentResults
    , rtarAssessmentResultsFile
    , rtarReplicationTaskIdentifier
    , rtarAssessmentStatus
    , rtarS3ObjectURL
    , rtarReplicationTaskLastAssessmentDate
    , rtarReplicationTaskARN

    -- * ReplicationTaskStats
    , ReplicationTaskStats
    , replicationTaskStats
    , rtsFullLoadProgressPercent
    , rtsElapsedTimeMillis
    , rtsTablesErrored
    , rtsTablesLoaded
    , rtsTablesQueued
    , rtsTablesLoading

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * S3Settings
    , S3Settings
    , s3Settings
    , ssParquetVersion
    , ssCSVDelimiter
    , ssServiceAccessRoleARN
    , ssBucketFolder
    , ssDataFormat
    , ssEncodingType
    , ssExternalTableDefinition
    , ssDictPageSizeLimit
    , ssBucketName
    , ssEncryptionMode
    , ssEnableStatistics
    , ssCdcInsertsOnly
    , ssCSVRowDelimiter
    , ssCompressionType
    , ssServerSideEncryptionKMSKeyId
    , ssDataPageSize
    , ssRowGroupLength

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SupportedEndpointType
    , SupportedEndpointType
    , supportedEndpointType
    , setEngineDisplayName
    , setEndpointType
    , setEngineName
    , setSupportsCDC

    -- * TableStatistics
    , TableStatistics
    , tableStatistics
    , tsValidationState
    , tsFullLoadRows
    , tsInserts
    , tsFullLoadCondtnlChkFailedRows
    , tsValidationFailedRecords
    , tsValidationSuspendedRecords
    , tsSchemaName
    , tsValidationStateDetails
    , tsTableState
    , tsFullLoadErrorRows
    , tsDdls
    , tsDeletes
    , tsUpdates
    , tsValidationPendingRecords
    , tsLastUpdateTime
    , tsTableName

    -- * TableToReload
    , TableToReload
    , tableToReload
    , ttrSchemaName
    , ttrTableName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.DMS.Types.Product
import Network.AWS.DMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
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
    , _svcError = parseJSONError "DMS"
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


-- | The ciphertext references a key that doesn't exist or DMS account doesn't have an access to
--
--
_KMSAccessDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSAccessDeniedFault = _MatchServiceError dms "KMSAccessDeniedFault"


-- | The specified master key (CMK) isn't enabled.
--
--
_KMSDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSDisabledFault = _MatchServiceError dms "KMSDisabledFault"


-- | The subnet provided is invalid.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError dms "InvalidSubnet"


-- | AWS DMS cannot access the KMS key.
--
--
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault = _MatchServiceError dms "KMSKeyNotAccessibleFault"


-- | The replication subnet group does not cover enough Availability Zones (AZs). Edit the replication subnet group and add more AZs.
--
--
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs =
  _MatchServiceError dms "ReplicationSubnetGroupDoesNotCoverEnoughAZs"


-- | The resource is in a state that prevents it from being used for database migration.
--
--
_InvalidResourceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateFault = _MatchServiceError dms "InvalidResourceStateFault"


-- | The certificate was not valid.
--
--
_InvalidCertificateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateFault = _MatchServiceError dms "InvalidCertificateFault"


-- | You are not authorized for the SNS subscription.
--
--
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault = _MatchServiceError dms "SNSNoAuthorizationFault"


-- | The resource you are attempting to create already exists.
--
--
_ResourceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsFault =
  _MatchServiceError dms "ResourceAlreadyExistsFault"


-- | There are not enough resources allocated to the database migration.
--
--
_InsufficientResourceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientResourceCapacityFault =
  _MatchServiceError dms "InsufficientResourceCapacityFault"


-- | The SNS topic is invalid.
--
--
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault = _MatchServiceError dms "SNSInvalidTopicFault"


-- | The specified KMS entity or resource can't be found.
--
--
_KMSNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSNotFoundFault = _MatchServiceError dms "KMSNotFoundFault"


-- | This request triggered KMS request throttling.
--
--
_KMSThrottlingFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSThrottlingFault = _MatchServiceError dms "KMSThrottlingFault"


-- | The quota for this resource quota has been exceeded.
--
--
_ResourceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceQuotaExceededFault =
  _MatchServiceError dms "ResourceQuotaExceededFault"


-- | An upgrade dependency is preventing the database migration.
--
--
_UpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_UpgradeDependencyFailureFault =
  _MatchServiceError dms "UpgradeDependencyFailureFault"


-- | The resource could not be found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault = _MatchServiceError dms "ResourceNotFoundFault"


-- | The storage quota has been exceeded.
--
--
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault = _MatchServiceError dms "StorageQuotaExceededFault"


-- | AWS DMS was denied access to the endpoint. Check that the role is correctly configured.
--
--
_AccessDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedFault = _MatchServiceError dms "AccessDeniedFault"


-- | The specified subnet is already in use.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse = _MatchServiceError dms "SubnetAlreadyInUse"


-- | The state of the specified KMS resource isn't valid for this request.
--
--
_KMSInvalidStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateFault = _MatchServiceError dms "KMSInvalidStateFault"

