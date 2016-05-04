{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , cValidTill
    , cCertificateIdentifier
    , cThumbprint
    , cValidFrom

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- * DBCluster
    , DBCluster
    , dbCluster
    , dcEngineVersion
    , dcStatus
    , dcStorageEncrypted
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcHostedZoneId
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcDBClusterResourceId
    , dcEarliestRestorableTime
    , dcEngine
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcAvailabilityZones
    , dcCharacterSetName
    , dcKMSKeyId
    , dcPreferredBackupWindow
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDBSubnetGroup
    , dcDatabaseName
    , dcAllocatedStorage
    , dcEndpoint
    , dcPercentProgress
    , dcPort
    , dcDBClusterOptionGroupMemberships

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
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- * DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- * DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsStorageEncrypted
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsAvailabilityZones
    , dcsSnapshotType
    , dcsKMSKeyId
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

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

    -- * DBInstance
    , DBInstance
    , dbInstance
    , diEngineVersion
    , diDBSecurityGroups
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diPubliclyAccessible
    , diAutoMinorVersionUpgrade
    , diMasterUsername
    , diReadReplicaDBInstanceIdentifiers
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
    , diDBSubnetGroup
    , diMultiAZ
    , diOptionGroupMemberships
    , diEnhancedMonitoringResourceARN
    , diSecondaryAvailabilityZone
    , diAllocatedStorage
    , diDBiResourceId
    , diDBParameterGroups
    , diCopyTagsToSnapshot
    , diTDECredentialARN
    , diEndpoint
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
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
    , dsMasterUsername
    , dsSourceRegion
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
    , oPort

    -- * OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionSettings
    , ocVPCSecurityGroupMemberships
    , ocDBSecurityGroupMemberships
    , ocPort
    , ocOptionName

    -- * OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupDescription
    , ogVPCId
    , ogAllowsVPCAndNonVPCInstanceMemberships
    , ogEngineName
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
    , ogoPermanent
    , ogoPersistent
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoName
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoPortRequired
    , ogoDescription
    , ogoOptionsDependedOn

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

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioMultiAZCapable
    , odioEngine
    , odioSupportsIOPS
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioSupportsStorageEncryption
    , odioReadReplicaCapable
    , odioVPC
    , odioSupportsEnhancedMonitoring
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
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvCACertificateIdentifier
    , pmvDBInstanceIdentifier
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

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
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types.Product
import           Network.AWS.RDS.Types.Sum
import           Network.AWS.Sign.V4

-- | API version '2014-10-31' of the Amazon Relational Database Service SDK configuration.
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
    , _svcError = parseXMLError
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

-- | /SourceDBInstanceIdentifier/ refers to a DB instance with
-- /BackupRetentionPeriod/ equal to 0.
_PointInTimeRestoreNotEnabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_PointInTimeRestoreNotEnabledFault =
    _ServiceError . hasStatus 400 . hasCode "PointInTimeRestoreNotEnabled"

-- | The DB parameter group cannot be deleted because it is in use.
_InvalidDBParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBParameterGroupState"

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedDBInstanceQuotaExceeded"

-- | The requested source could not be found.
_SourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault = _ServiceError . hasStatus 404 . hasCode "SourceNotFound"

-- | /CertificateIdentifier/ does not refer to an existing certificate.
_CertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CertificateNotFound"

-- | DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationQuotaExceeded"

-- | User already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "DBClusterSnapshotAlreadyExistsFault"

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBParameterGroupAlreadyExists"

-- | Request would result in user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBParameterGroupQuotaExceeded"

-- | The DB cluster does not have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBClusterCapacityFault =
    _ServiceError .
    hasStatus 403 . hasCode "InsufficientDBClusterCapacityFault"

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedDBInstanceAlreadyExists"

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIOPSNotAvailableInAZFault :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedIOPSNotAvailableInAZFault =
    _ServiceError .
    hasStatus 400 . hasCode "ProvisionedIopsNotAvailableInAZFault"

-- | The specified CIDRIP or EC2 security group is already authorized for the
-- specified DB security group.
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionCategoryNotFound"

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionNotFound"

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | You have exceeded the maximum number of account ids that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SharedSnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "SharedSnapshotQuotaExceeded"

-- | Request would result in user exceeding the allowed number of subnets in
-- a DB subnet groups.
_DBSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetQuotaExceededFault"

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "OptionGroupNotFoundFault"

-- | /DBClusterIdentifier/ does not refer to an existing DB cluster.
_DBClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterNotFoundFault"

-- | /LogFileName/ does not refer to an existing DB log file.
_DBLogFileNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBLogFileNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBLogFileNotFoundFault"

-- | User already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBClusterAlreadyExistsFault"

-- | /StorageType/ specified cannot be associated with the DB Instance.
_StorageTypeNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "StorageTypeNotSupported"

-- | Request would result in user exceeding the allowed number of DB security
-- groups.
_DBSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "QuotaExceeded.DBSecurityGroup"

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "OptionGroupAlreadyExistsFault"

-- | The quota of 20 option groups was exceeded for this AWS account.
_OptionGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "OptionGroupQuotaExceededFault"

-- | A DB security group with the name specified in /DBSecurityGroupName/
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSecurityGroupAlreadyExists"

-- | The SNS topic ARN does not exist.
_SNSTopicARNNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SNSTopicArnNotFound"

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventSubscriptionStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidEventSubscriptionState"

-- | Error accessing KMS key.
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault =
    _ServiceError . hasStatus 400 . hasCode "KMSKeyNotAccessibleFault"

-- | /DBSnapshotIdentifier/ does not refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSnapshotNotFound"

-- | /DBClusterParameterGroupName/ does not refer to an existing DB Cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterParameterGroupNotFound"

-- | User attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterQuotaExceededFault =
    _ServiceError . hasStatus 403 . hasCode "DBClusterQuotaExceededFault"

-- | Request would result in user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotQuotaExceeded"

-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupAlreadyExists"

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
    _ServiceError . hasStatus 400 . hasCode "SNSNoAuthorization"

-- | /DBSecurityGroupName/ does not refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSecurityGroupNotFound"

-- | A DB security group is not allowed for this action.
_DBSecurityGroupNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "DBSecurityGroupNotSupported"

-- | Request would result in user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "InstanceQuotaExceeded"

-- | Prism for DomainNotFoundFault' errors.
_DomainNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DomainNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DomainNotFoundFault"

-- | /DBParameterGroupName/ does not refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBParameterGroupNotFound"

-- | Indicates the DBSubnetGroup does not belong to the same VPC as that of
-- an existing cross region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetGroupFault"

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
    _ServiceError .
    hasStatus 404 . hasCode "ReservedDBInstancesOfferingNotFound"

-- | The DB subnet is not in the /available/ state.
_InvalidDBSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetStateFault"

-- | /DBClusterSnapshotIdentifier/ does not refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterSnapshotNotFoundFault"

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault =
    _ServiceError . hasStatus 400 . hasCode "SNSInvalidTopic"

-- | Specified DB instance class is not available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientDBInstanceCapacity"

-- | The supplied value is not a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterSnapshotStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidDBClusterSnapshotStateFault"

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
    _ServiceError . hasStatus 400 . hasCode "SubscriptionAlreadyExist"

-- | DB subnet group does not cover all Availability Zones after it is
-- created because users\' change.
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | Specified CIDRIP or EC2 security group is not authorized for the
-- specified DB security group.
--
-- RDS may not also be authorized via IAM to perform necessary actions on
-- your behalf.
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedDBInstanceNotFound"

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupQuotaExceeded"

-- | Indicates that the DBSubnetGroup should not be specified while creating
-- read replicas that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotAllowedFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupNotAllowedFault"

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "EventSubscriptionQuotaExceeded"

-- | There is insufficient storage available for the current action. You may
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientStorageClusterCapacityFault =
    _ServiceError .
    hasStatus 400 . hasCode "InsufficientStorageClusterCapacity"

-- | The option group is not in the /available/ state.
_InvalidOptionGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOptionGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidOptionGroupStateFault"

-- | The supplied value is not a valid DB cluster state.
_InvalidDBClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBClusterStateFault"

-- | User already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBInstanceAlreadyExists"

-- | Cannot restore from vpc backup to non-vpc DB instance.
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidRestoreFault"

-- | The state of the DB security group does not allow deletion.
_InvalidDBSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSecurityGroupState"

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundFault"

-- | /DBSubnetGroupName/ does not refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSubnetGroupNotFoundFault"

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
_DBUpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault =
    _ServiceError . hasStatus 400 . hasCode "DBUpgradeDependencyFailure"

-- | The specified DB instance is not in the /available/ state.
_InvalidDBInstanceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBInstanceState"

-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSnapshotAlreadyExists"

-- | /DBInstanceIdentifier/ does not refer to an existing DB instance.
_DBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBInstanceNotFound"

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "StorageQuotaExceeded"

-- | The state of the DB snapshot does not allow deletion.
_InvalidDBSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSnapshotState"

-- | The DB subnet group cannot be deleted because it is in use.
_InvalidDBSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetGroupStateFault"

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
    _ServiceError .
    hasStatus 400 . hasCode "DBSubnetGroupDoesNotCoverEnoughAZs"

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
    _ServiceError . hasStatus 400 . hasCode "SubnetAlreadyInUse"
