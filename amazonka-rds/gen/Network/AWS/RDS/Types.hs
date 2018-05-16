{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types
    (
    -- * Service Configuration
      rds

    -- * Errors
    , _PointInTimeRestoreNotEnabledFault
    , _InvalidDBParameterGroupStateFault
    , _ReservedDBInstanceQuotaExceededFault
    , _SourceNotFoundFault
    , _CertificateNotFoundFault
    , _AuthorizationQuotaExceededFault
    , _DBClusterSnapshotAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupQuotaExceededFault
    , _InsufficientDBClusterCapacityFault
    , _ReservedDBInstanceAlreadyExistsFault
    , _ProvisionedIOPSNotAvailableInAZFault
    , _AuthorizationAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _SubscriptionNotFoundFault
    , _InvalidSubnet
    , _SharedSnapshotQuotaExceededFault
    , _DBSubnetQuotaExceededFault
    , _OptionGroupNotFoundFault
    , _DBClusterNotFoundFault
    , _DBLogFileNotFoundFault
    , _InvalidS3BucketFault
    , _DBClusterAlreadyExistsFault
    , _StorageTypeNotSupportedFault
    , _DBSecurityGroupQuotaExceededFault
    , _OptionGroupAlreadyExistsFault
    , _OptionGroupQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _SNSTopicARNNotFoundFault
    , _InvalidEventSubscriptionStateFault
    , _KMSKeyNotAccessibleFault
    , _DBSnapshotNotFoundFault
    , _DBClusterParameterGroupNotFoundFault
    , _DBClusterQuotaExceededFault
    , _SnapshotQuotaExceededFault
    , _DBSubnetGroupAlreadyExistsFault
    , _SNSNoAuthorizationFault
    , _DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotSupportedFault
    , _InstanceQuotaExceededFault
    , _DBClusterBacktrackNotFoundFault
    , _DomainNotFoundFault
    , _DBParameterGroupNotFoundFault
    , _InvalidDBSubnetGroupFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _InvalidDBSubnetStateFault
    , _DBClusterSnapshotNotFoundFault
    , _SNSInvalidTopicFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidDBClusterSnapshotStateFault
    , _SubscriptionAlreadyExistFault
    , _DBClusterRoleAlreadyExistsFault
    , _DBClusterRoleQuotaExceededFault
    , _InvalidVPCNetworkStateFault
    , _AuthorizationNotFoundFault
    , _ReservedDBInstanceNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupNotAllowedFault
    , _EventSubscriptionQuotaExceededFault
    , _InsufficientStorageClusterCapacityFault
    , _InvalidOptionGroupStateFault
    , _InvalidDBClusterStateFault
    , _DBInstanceAlreadyExistsFault
    , _InvalidRestoreFault
    , _InvalidDBSecurityGroupStateFault
    , _ResourceNotFoundFault
    , _DBSubnetGroupNotFoundFault
    , _DBUpgradeDependencyFailureFault
    , _InvalidDBInstanceStateFault
    , _DBSnapshotAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _StorageQuotaExceededFault
    , _InvalidDBSnapshotStateFault
    , _InvalidDBSubnetGroupStateFault
    , _DBSubnetGroupDoesNotCoverEnoughAZs
    , _SubnetAlreadyInUse
    , _DBClusterRoleNotFoundFault

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * SourceType
    , SourceType (..)

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
    , cCertificateType
    , cCertificateARN
    , cValidTill
    , cCertificateIdentifier
    , cThumbprint
    , cValidFrom

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- * CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration
    , cloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- * DBCluster
    , DBCluster
    , dbCluster
    , dcBacktrackConsumedChangeRecords
    , dcEngineVersion
    , dcStatus
    , dcStorageEncrypted
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcReadReplicaIdentifiers
    , dcReplicationSourceIdentifier
    , dcHostedZoneId
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcIAMDatabaseAuthenticationEnabled
    , dcEarliestBacktrackTime
    , dcBacktrackWindow
    , dcDBClusterResourceId
    , dcEarliestRestorableTime
    , dcEngine
    , dcDBClusterARN
    , dcCloneGroupId
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcAvailabilityZones
    , dcCharacterSetName
    , dcKMSKeyId
    , dcPreferredBackupWindow
    , dcAssociatedRoles
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDBSubnetGroup
    , dcDatabaseName
    , dcMultiAZ
    , dcAllocatedStorage
    , dcClusterCreateTime
    , dcEndpoint
    , dcPercentProgress
    , dcReaderEndpoint
    , dcPort
    , dcDBClusterOptionGroupMemberships

    -- * DBClusterBacktrack
    , DBClusterBacktrack
    , dbClusterBacktrack
    , dcbStatus
    , dcbBacktrackIdentifier
    , dcbBacktrackTo
    , dcbDBClusterIdentifier
    , dcbBacktrackedFrom
    , dcbBacktrackRequestCreationTime

    -- * DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmPromotionTier
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- * DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus
    , dbClusterOptionGroupStatus
    , dcogsStatus
    , dcogsDBClusterOptionGroupName

    -- * DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBClusterParameterGroupARN
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- * DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- * DBClusterRole
    , DBClusterRole
    , dbClusterRole
    , dcrStatus
    , dcrRoleARN

    -- * DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsStorageEncrypted
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsIAMDatabaseAuthenticationEnabled
    , dcsDBClusterSnapshotARN
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsAvailabilityZones
    , dcsSnapshotType
    , dcsKMSKeyId
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsSourceDBClusterSnapshotARN
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- * DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute
    , dbClusterSnapshotAttribute
    , dcsaAttributeValues
    , dcsaAttributeName

    -- * DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult
    , dbClusterSnapshotAttributesResult
    , dcsarDBClusterSnapshotIdentifier
    , dcsarDBClusterSnapshotAttributes

    -- * DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devEngineVersion
    , devDBEngineVersionDescription
    , devDefaultCharacterSet
    , devEngine
    , devDBParameterGroupFamily
    , devSupportedCharacterSets
    , devDBEngineDescription
    , devValidUpgradeTarget
    , devSupportsLogExportsToCloudwatchLogs
    , devSupportsReadReplica
    , devSupportedTimezones
    , devExportableLogTypes

    -- * DBInstance
    , DBInstance
    , dbInstance
    , diEngineVersion
    , diDBSecurityGroups
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diPubliclyAccessible
    , diAutoMinorVersionUpgrade
    , diDBInstanceARN
    , diMasterUsername
    , diReadReplicaDBInstanceIdentifiers
    , diIAMDatabaseAuthenticationEnabled
    , diMonitoringRoleARN
    , diIOPS
    , diInstanceCreateTime
    , diReadReplicaSourceDBInstanceIdentifier
    , diMonitoringInterval
    , diEngine
    , diLatestRestorableTime
    , diDBInstanceClass
    , diPromotionTier
    , diLicenseModel
    , diPreferredMaintenanceWindow
    , diCACertificateIdentifier
    , diDBInstanceIdentifier
    , diCharacterSetName
    , diKMSKeyId
    , diPreferredBackupWindow
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diPerformanceInsightsKMSKeyId
    , diDBSubnetGroup
    , diMultiAZ
    , diOptionGroupMemberships
    , diEnabledCloudwatchLogsExports
    , diEnhancedMonitoringResourceARN
    , diSecondaryAvailabilityZone
    , diPerformanceInsightsEnabled
    , diAllocatedStorage
    , diDBiResourceId
    , diDBParameterGroups
    , diCopyTagsToSnapshot
    , diTimezone
    , diTDECredentialARN
    , diEndpoint
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
    , diReadReplicaDBClusterIdentifiers
    , diStorageType
    , diStatusInfos
    , diDomainMemberships
    , diDBName

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- * DBParameterGroup
    , DBParameterGroup
    , dbParameterGroup
    , dpgDBParameterGroupARN
    , dpgDBParameterGroupFamily
    , dpgDBParameterGroupName
    , dpgDescription

    -- * DBParameterGroupNameMessage
    , DBParameterGroupNameMessage
    , dbParameterGroupNameMessage
    , dpgnmDBParameterGroupName

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus
    , dbParameterGroupStatus
    , dpgsDBParameterGroupName
    , dpgsParameterApplyStatus

    -- * DBSecurityGroup
    , DBSecurityGroup
    , dbSecurityGroup
    , dbsgVPCId
    , dbsgOwnerId
    , dbsgDBSecurityGroupARN
    , dbsgIPRanges
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgDBSecurityGroupDescription

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dbSecurityGroupMembership
    , dsgmStatus
    , dsgmDBSecurityGroupName

    -- * DBSnapshot
    , DBSnapshot
    , dbSnapshot
    , dsEngineVersion
    , dsStatus
    , dsDBSnapshotARN
    , dsMasterUsername
    , dsSourceRegion
    , dsIAMDatabaseAuthenticationEnabled
    , dsIOPS
    , dsVPCId
    , dsInstanceCreateTime
    , dsEngine
    , dsEncrypted
    , dsDBSnapshotIdentifier
    , dsLicenseModel
    , dsSourceDBSnapshotIdentifier
    , dsSnapshotType
    , dsDBInstanceIdentifier
    , dsKMSKeyId
    , dsAvailabilityZone
    , dsSnapshotCreateTime
    , dsAllocatedStorage
    , dsOptionGroupName
    , dsTimezone
    , dsTDECredentialARN
    , dsPercentProgress
    , dsPort
    , dsStorageType

    -- * DBSnapshotAttribute
    , DBSnapshotAttribute
    , dbSnapshotAttribute
    , dsaAttributeValues
    , dsaAttributeName

    -- * DBSnapshotAttributesResult
    , DBSnapshotAttributesResult
    , dbSnapshotAttributesResult
    , dsarDBSnapshotIdentifier
    , dsarDBSnapshotAttributes

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dsgDBSubnetGroupName
    , dsgVPCId
    , dsgSubnets
    , dsgDBSubnetGroupDescription
    , dsgDBSubnetGroupARN
    , dsgSubnetGroupStatus

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddlfdLastWritten
    , ddlfdSize
    , ddlfdLogFileName

    -- * DomainMembership
    , DomainMembership
    , domainMembership
    , dmStatus
    , dmFQDN
    , dmDomain
    , dmIAMRoleName

    -- * DoubleRange
    , DoubleRange
    , doubleRange
    , drTo
    , drFrom

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgEC2SecurityGroupId

    -- * Endpoint
    , Endpoint
    , endpoint
    , eHostedZoneId
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceARN
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEventSubscriptionARN
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

    -- * IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP

    -- * Option
    , Option
    , option
    , oOptionName
    , oPermanent
    , oPersistent
    , oOptionDescription
    , oOptionSettings
    , oVPCSecurityGroupMemberships
    , oDBSecurityGroupMemberships
    , oOptionVersion
    , oPort

    -- * OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionSettings
    , ocVPCSecurityGroupMemberships
    , ocDBSecurityGroupMemberships
    , ocOptionVersion
    , ocPort
    , ocOptionName

    -- * OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupDescription
    , ogVPCId
    , ogAllowsVPCAndNonVPCInstanceMemberships
    , ogEngineName
    , ogOptionGroupARN
    , ogMajorEngineVersion
    , ogOptions
    , ogOptionGroupName

    -- * OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmStatus
    , ogmOptionGroupName

    -- * OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoMinimumRequiredMinorEngineVersion
    , ogoOptionsConflictsWith
    , ogoPermanent
    , ogoPersistent
    , ogoOptionGroupOptionVersions
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoName
    , ogoSupportsOptionVersionDowngrade
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoRequiresAutoMinorEngineVersionUpgrade
    , ogoPortRequired
    , ogoDescription
    , ogoOptionsDependedOn
    , ogoVPCOnly

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosApplyType
    , ogosSettingName
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosSettingDescription
    , ogosAllowedValues

    -- * OptionSetting
    , OptionSetting
    , optionSetting
    , osIsCollection
    , osApplyType
    , osValue
    , osName
    , osDefaultValue
    , osIsModifiable
    , osDataType
    , osAllowedValues
    , osDescription

    -- * OptionVersion
    , OptionVersion
    , optionVersion
    , ovVersion
    , ovIsDefault

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioMinIOPSPerGib
    , odioSupportsIAMDatabaseAuthentication
    , odioMinIOPSPerDBInstance
    , odioMultiAZCapable
    , odioMaxStorageSize
    , odioEngine
    , odioMinStorageSize
    , odioSupportsIOPS
    , odioSupportsPerformanceInsights
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioSupportsStorageEncryption
    , odioReadReplicaCapable
    , odioMaxIOPSPerGib
    , odioVPC
    , odioSupportsEnhancedMonitoring
    , odioMaxIOPSPerDBInstance
    , odioStorageType

    -- * Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pApplyMethod
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription

    -- * PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports
    , pendingCloudwatchLogsExports
    , pcleLogTypesToEnable
    , pcleLogTypesToDisable

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvDBSubnetGroupName
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvLicenseModel
    , pmvCACertificateIdentifier
    , pmvDBInstanceIdentifier
    , pmvPendingCloudwatchLogsExports
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

    -- * Range
    , Range
    , range
    , rTo
    , rFrom
    , rStep

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdiDBInstanceCount
    , rdiState
    , rdiCurrencyCode
    , rdiStartTime
    , rdiProductDescription
    , rdiReservedDBInstanceId
    , rdiReservedDBInstanceARN
    , rdiDBInstanceClass
    , rdiMultiAZ
    , rdiReservedDBInstancesOfferingId
    , rdiRecurringCharges
    , rdiOfferingType
    , rdiUsagePrice
    , rdiFixedPrice
    , rdiDuration

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdioCurrencyCode
    , rdioProductDescription
    , rdioDBInstanceClass
    , rdioMultiAZ
    , rdioReservedDBInstancesOfferingId
    , rdioRecurringCharges
    , rdioOfferingType
    , rdioUsagePrice
    , rdioFixedPrice
    , rdioDuration

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * SourceRegion
    , SourceRegion
    , sourceRegion
    , srStatus
    , srRegionName
    , srEndpoint

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Timezone
    , Timezone
    , timezone
    , tTimezoneName

    -- * UpgradeTarget
    , UpgradeTarget
    , upgradeTarget
    , utEngineVersion
    , utIsMajorVersionUpgrade
    , utEngine
    , utAutoUpgrade
    , utDescription

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId

    -- * ValidDBInstanceModificationsMessage
    , ValidDBInstanceModificationsMessage
    , validDBInstanceModificationsMessage
    , vdimmStorage

    -- * ValidStorageOptions
    , ValidStorageOptions
    , validStorageOptions
    , vsoStorageSize
    , vsoProvisionedIOPS
    , vsoIOPSToStorageRatio
    , vsoStorageType
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Product
import Network.AWS.RDS.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
rds :: Service
rds =
  Service
    { _svcAbbrev = "RDS"
    , _svcSigner = v4
    , _svcPrefix = "rds"
    , _svcVersion = "2014-10-31"
    , _svcEndpoint = defaultEndpoint rds
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "RDS"
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


-- | /SourceDBInstanceIdentifier/ refers to a DB instance with /BackupRetentionPeriod/ equal to 0.
--
--
_PointInTimeRestoreNotEnabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_PointInTimeRestoreNotEnabledFault =
  _MatchServiceError rds "PointInTimeRestoreNotEnabled" . hasStatus 400


-- | The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you can't delete it when the parameter group is in this state.
--
--
_InvalidDBParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault =
  _MatchServiceError rds "InvalidDBParameterGroupState" . hasStatus 400


-- | Request would exceed the user's DB Instance quota.
--
--
_ReservedDBInstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceQuotaExceededFault =
  _MatchServiceError rds "ReservedDBInstanceQuotaExceeded" . hasStatus 400


-- | The requested source could not be found.
--
--
_SourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault = _MatchServiceError rds "SourceNotFound" . hasStatus 404


-- | /CertificateIdentifier/ doesn't refer to an existing certificate.
--
--
_CertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundFault =
  _MatchServiceError rds "CertificateNotFound" . hasStatus 404


-- | The DB security group authorization quota has been reached.
--
--
_AuthorizationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
  _MatchServiceError rds "AuthorizationQuotaExceeded" . hasStatus 400


-- | The user already has a DB cluster snapshot with the given identifier.
--
--
_DBClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  _MatchServiceError rds "DBClusterSnapshotAlreadyExistsFault" . hasStatus 400


-- | A DB parameter group with the same name exists.
--
--
_DBParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault =
  _MatchServiceError rds "DBParameterGroupAlreadyExists" . hasStatus 400


-- | The request would result in the user exceeding the allowed number of DB parameter groups.
--
--
_DBParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault =
  _MatchServiceError rds "DBParameterGroupQuotaExceeded" . hasStatus 400


-- | The DB cluster doesn't have enough capacity for the current operation.
--
--
_InsufficientDBClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBClusterCapacityFault =
  _MatchServiceError rds "InsufficientDBClusterCapacityFault" . hasStatus 403


-- | User already has a reservation with the given identifier.
--
--
_ReservedDBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceAlreadyExistsFault =
  _MatchServiceError rds "ReservedDBInstanceAlreadyExists" . hasStatus 404


-- | Provisioned IOPS not available in the specified Availability Zone.
--
--
_ProvisionedIOPSNotAvailableInAZFault :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedIOPSNotAvailableInAZFault =
  _MatchServiceError rds "ProvisionedIopsNotAvailableInAZFault" . hasStatus 400


-- | The specified CIDRIP or Amazon EC2 security group is already authorized for the specified DB security group.
--
--
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
  _MatchServiceError rds "AuthorizationAlreadyExists" . hasStatus 400


-- | The supplied category does not exist.
--
--
_SubscriptionCategoryNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
  _MatchServiceError rds "SubscriptionCategoryNotFound" . hasStatus 404


-- | The subscription name does not exist.
--
--
_SubscriptionNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
  _MatchServiceError rds "SubscriptionNotFound" . hasStatus 404


-- | The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError rds "InvalidSubnet" . hasStatus 400


-- | You have exceeded the maximum number of accounts that you can share a manual DB snapshot with.
--
--
_SharedSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SharedSnapshotQuotaExceededFault =
  _MatchServiceError rds "SharedSnapshotQuotaExceeded" . hasStatus 400


-- | The request would result in the user exceeding the allowed number of subnets in a DB subnet groups.
--
--
_DBSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault =
  _MatchServiceError rds "DBSubnetQuotaExceededFault" . hasStatus 400


-- | The specified option group could not be found.
--
--
_OptionGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupNotFoundFault =
  _MatchServiceError rds "OptionGroupNotFoundFault" . hasStatus 404


-- | /DBClusterIdentifier/ doesn't refer to an existing DB cluster.
--
--
_DBClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterNotFoundFault =
  _MatchServiceError rds "DBClusterNotFoundFault" . hasStatus 404


-- | /LogFileName/ doesn't refer to an existing DB log file.
--
--
_DBLogFileNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBLogFileNotFoundFault =
  _MatchServiceError rds "DBLogFileNotFoundFault" . hasStatus 404


-- | The specified Amazon S3 bucket name can't be found or Amazon RDS isn't authorized to access the specified Amazon S3 bucket. Verify the __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
--
--
_InvalidS3BucketFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketFault =
  _MatchServiceError rds "InvalidS3BucketFault" . hasStatus 400


-- | The user already has a DB cluster with the given identifier.
--
--
_DBClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterAlreadyExistsFault =
  _MatchServiceError rds "DBClusterAlreadyExistsFault" . hasStatus 400


-- | Storage of the /StorageType/ specified can't be associated with the DB instance.
--
--
_StorageTypeNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault =
  _MatchServiceError rds "StorageTypeNotSupported" . hasStatus 400


-- | The request would result in the user exceeding the allowed number of DB security groups.
--
--
_DBSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupQuotaExceededFault =
  _MatchServiceError rds "QuotaExceeded.DBSecurityGroup" . hasStatus 400


-- | The option group you are trying to create already exists.
--
--
_OptionGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupAlreadyExistsFault =
  _MatchServiceError rds "OptionGroupAlreadyExistsFault" . hasStatus 400


-- | The quota of 20 option groups was exceeded for this AWS account.
--
--
_OptionGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupQuotaExceededFault =
  _MatchServiceError rds "OptionGroupQuotaExceededFault" . hasStatus 400


-- | A DB security group with the name specified in /DBSecurityGroupName/ already exists.
--
--
_DBSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupAlreadyExistsFault =
  _MatchServiceError rds "DBSecurityGroupAlreadyExists" . hasStatus 400


-- | The SNS topic ARN does not exist.
--
--
_SNSTopicARNNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
  _MatchServiceError rds "SNSTopicArnNotFound" . hasStatus 404


-- | This error can occur if someone else is modifying a subscription. You should retry the action.
--
--
_InvalidEventSubscriptionStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventSubscriptionStateFault =
  _MatchServiceError rds "InvalidEventSubscriptionState" . hasStatus 400


-- | An error occurred accessing an AWS KMS key.
--
--
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault =
  _MatchServiceError rds "KMSKeyNotAccessibleFault" . hasStatus 400


-- | /DBSnapshotIdentifier/ doesn't refer to an existing DB snapshot.
--
--
_DBSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault =
  _MatchServiceError rds "DBSnapshotNotFound" . hasStatus 404


-- | /DBClusterParameterGroupName/ doesn't refer to an existing DB cluster parameter group.
--
--
_DBClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterParameterGroupNotFoundFault =
  _MatchServiceError rds "DBClusterParameterGroupNotFound" . hasStatus 404


-- | The user attempted to create a new DB cluster and the user has already reached the maximum allowed DB cluster quota.
--
--
_DBClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterQuotaExceededFault =
  _MatchServiceError rds "DBClusterQuotaExceededFault" . hasStatus 403


-- | The request would result in the user exceeding the allowed number of DB snapshots.
--
--
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
  _MatchServiceError rds "SnapshotQuotaExceeded" . hasStatus 400


-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
--
--
_DBSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault =
  _MatchServiceError rds "DBSubnetGroupAlreadyExists" . hasStatus 400


-- | You do not have permission to publish to the SNS topic ARN.
--
--
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
  _MatchServiceError rds "SNSNoAuthorization" . hasStatus 400


-- | /DBSecurityGroupName/ doesn't refer to an existing DB security group.
--
--
_DBSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault =
  _MatchServiceError rds "DBSecurityGroupNotFound" . hasStatus 404


-- | A DB security group isn't allowed for this action.
--
--
_DBSecurityGroupNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotSupportedFault =
  _MatchServiceError rds "DBSecurityGroupNotSupported" . hasStatus 400


-- | The request would result in the user exceeding the allowed number of DB instances.
--
--
_InstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault =
  _MatchServiceError rds "InstanceQuotaExceeded" . hasStatus 400


-- | /BacktrackIdentifier/ doesn't refer to an existing backtrack.
--
--
_DBClusterBacktrackNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterBacktrackNotFoundFault =
  _MatchServiceError rds "DBClusterBacktrackNotFoundFault" . hasStatus 404


-- | /Domain/ doesn't refer to an existing Active Directory domain.
--
--
_DomainNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DomainNotFoundFault =
  _MatchServiceError rds "DomainNotFoundFault" . hasStatus 404


-- | /DBParameterGroupName/ doesn't refer to an existing DB parameter group.
--
--
_DBParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault =
  _MatchServiceError rds "DBParameterGroupNotFound" . hasStatus 404


-- | The DBSubnetGroup doesn't belong to the same VPC as that of an existing cross-region read replica of the same source instance.
--
--
_InvalidDBSubnetGroupFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupFault =
  _MatchServiceError rds "InvalidDBSubnetGroupFault" . hasStatus 400


-- | Specified offering does not exist.
--
--
_ReservedDBInstancesOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
  _MatchServiceError rds "ReservedDBInstancesOfferingNotFound" . hasStatus 404


-- | The DB subnet isn't in the /available/ state.
--
--
_InvalidDBSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault =
  _MatchServiceError rds "InvalidDBSubnetStateFault" . hasStatus 400


-- | /DBClusterSnapshotIdentifier/ doesn't refer to an existing DB cluster snapshot.
--
--
_DBClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotNotFoundFault =
  _MatchServiceError rds "DBClusterSnapshotNotFoundFault" . hasStatus 404


-- | SNS has responded that there is a problem with the SND topic specified.
--
--
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault = _MatchServiceError rds "SNSInvalidTopic" . hasStatus 400


-- | The specified DB instance class isn't available in the specified Availability Zone.
--
--
_InsufficientDBInstanceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault =
  _MatchServiceError rds "InsufficientDBInstanceCapacity" . hasStatus 400


-- | The supplied value isn't a valid DB cluster snapshot state.
--
--
_InvalidDBClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterSnapshotStateFault =
  _MatchServiceError rds "InvalidDBClusterSnapshotStateFault" . hasStatus 400


-- | The supplied subscription name already exists.
--
--
_SubscriptionAlreadyExistFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
  _MatchServiceError rds "SubscriptionAlreadyExist" . hasStatus 400


-- | The specified IAM role Amazon Resource Name (ARN) is already associated with the specified DB cluster.
--
--
_DBClusterRoleAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleAlreadyExistsFault =
  _MatchServiceError rds "DBClusterRoleAlreadyExists" . hasStatus 400


-- | You have exceeded the maximum number of IAM roles that can be associated with the specified DB cluster.
--
--
_DBClusterRoleQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleQuotaExceededFault =
  _MatchServiceError rds "DBClusterRoleQuotaExceeded" . hasStatus 400


-- | The DB subnet group doesn't cover all Availability Zones after it's created because of users' change.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
  _MatchServiceError rds "InvalidVPCNetworkStateFault" . hasStatus 400


-- | The specified CIDRIP or Amazon EC2 security group isn't authorized for the specified DB security group.
--
--
-- RDS also may not be authorized by using IAM to perform necessary actions on your behalf.
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
  _MatchServiceError rds "AuthorizationNotFound" . hasStatus 404


-- | The specified reserved DB Instance not found.
--
--
_ReservedDBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceNotFoundFault =
  _MatchServiceError rds "ReservedDBInstanceNotFound" . hasStatus 404


-- | The request would result in the user exceeding the allowed number of DB subnet groups.
--
--
_DBSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault =
  _MatchServiceError rds "DBSubnetGroupQuotaExceeded" . hasStatus 400


-- | The DBSubnetGroup shouldn't be specified while creating read replicas that lie in the same region as the source instance.
--
--
_DBSubnetGroupNotAllowedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotAllowedFault =
  _MatchServiceError rds "DBSubnetGroupNotAllowedFault" . hasStatus 400


-- | You have reached the maximum number of event subscriptions.
--
--
_EventSubscriptionQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
  _MatchServiceError rds "EventSubscriptionQuotaExceeded" . hasStatus 400


-- | There is insufficient storage available for the current action. You might be able to resolve this error by updating your subnet group to use different Availability Zones that have more storage available.
--
--
_InsufficientStorageClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientStorageClusterCapacityFault =
  _MatchServiceError rds "InsufficientStorageClusterCapacity" . hasStatus 400


-- | The option group isn't in the /available/ state.
--
--
_InvalidOptionGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOptionGroupStateFault =
  _MatchServiceError rds "InvalidOptionGroupStateFault" . hasStatus 400


-- | The DB cluster isn't in a valid state.
--
--
_InvalidDBClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterStateFault =
  _MatchServiceError rds "InvalidDBClusterStateFault" . hasStatus 400


-- | The user already has a DB instance with the given identifier.
--
--
_DBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault =
  _MatchServiceError rds "DBInstanceAlreadyExists" . hasStatus 400


-- | Cannot restore from VPC backup to non-VPC DB instance.
--
--
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault =
  _MatchServiceError rds "InvalidRestoreFault" . hasStatus 400


-- | The state of the DB security group doesn't allow deletion.
--
--
_InvalidDBSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault =
  _MatchServiceError rds "InvalidDBSecurityGroupState" . hasStatus 400


-- | The specified resource ID was not found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
  _MatchServiceError rds "ResourceNotFoundFault" . hasStatus 404


-- | /DBSubnetGroupName/ doesn't refer to an existing DB subnet group.
--
--
_DBSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault =
  _MatchServiceError rds "DBSubnetGroupNotFoundFault" . hasStatus 404


-- | The DB upgrade failed because a resource the DB depends on can't be modified.
--
--
_DBUpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault =
  _MatchServiceError rds "DBUpgradeDependencyFailure" . hasStatus 400


-- | The specified DB instance isn't in the /available/ state.
--
--
_InvalidDBInstanceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault =
  _MatchServiceError rds "InvalidDBInstanceState" . hasStatus 400


-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
--
--
_DBSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault =
  _MatchServiceError rds "DBSnapshotAlreadyExists" . hasStatus 400


-- | /DBInstanceIdentifier/ doesn't refer to an existing DB instance.
--
--
_DBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault =
  _MatchServiceError rds "DBInstanceNotFound" . hasStatus 404


-- | The request would result in the user exceeding the allowed amount of storage available across all DB instances.
--
--
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault =
  _MatchServiceError rds "StorageQuotaExceeded" . hasStatus 400


-- | The state of the DB snapshot doesn't allow deletion.
--
--
_InvalidDBSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault =
  _MatchServiceError rds "InvalidDBSnapshotState" . hasStatus 400


-- | The DB subnet group cannot be deleted because it's in use.
--
--
_InvalidDBSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault =
  _MatchServiceError rds "InvalidDBSubnetGroupStateFault" . hasStatus 400


-- | Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.
--
--
_DBSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  _MatchServiceError rds "DBSubnetGroupDoesNotCoverEnoughAZs" . hasStatus 400


-- | The DB subnet is already in use in the Availability Zone.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
  _MatchServiceError rds "SubnetAlreadyInUse" . hasStatus 400


-- | The specified IAM role Amazon Resource Name (ARN) isn't associated with the specified DB cluster.
--
--
_DBClusterRoleNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleNotFoundFault =
  _MatchServiceError rds "DBClusterRoleNotFound" . hasStatus 404

