{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types
    (
    -- * Service
      RDS

    -- * Errors
    , _CertificateNotFoundFault
    , _ReservedDBInstanceQuotaExceededFault
    , _DBClusterSnapshotAlreadyExistsFault
    , _AuthorizationQuotaExceededFault
    , _SourceNotFoundFault
    , _InvalidDBParameterGroupStateFault
    , _DBParameterGroupAlreadyExistsFault
    , _PointInTimeRestoreNotEnabledFault
    , _DBParameterGroupQuotaExceededFault
    , _ProvisionedIOPSNotAvailableInAZFault
    , _AuthorizationAlreadyExistsFault
    , _InsufficientDBClusterCapacityFault
    , _ReservedDBInstanceAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _DBSubnetQuotaExceededFault
    , _SubscriptionNotFoundFault
    , _InvalidSubnet
    , _DBClusterNotFoundFault
    , _OptionGroupNotFoundFault
    , _DBLogFileNotFoundFault
    , _OptionGroupAlreadyExistsFault
    , _DBClusterAlreadyExistsFault
    , _StorageTypeNotSupportedFault
    , _DBSecurityGroupQuotaExceededFault
    , _DBSnapshotNotFoundFault
    , _InvalidEventSubscriptionStateFault
    , _KMSKeyNotAccessibleFault
    , _OptionGroupQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _SnapshotQuotaExceededFault
    , _SNSTopicARNNotFoundFault
    , _DBClusterQuotaExceededFault
    , _DBClusterParameterGroupNotFoundFault
    , _DBSubnetGroupAlreadyExistsFault
    , _InstanceQuotaExceededFault
    , _SNSNoAuthorizationFault
    , _DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotSupportedFault
    , _DomainNotFoundFault
    , _DBClusterSnapshotNotFoundFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _InvalidDBSubnetGroupFault
    , _InvalidDBSubnetStateFault
    , _DBParameterGroupNotFoundFault
    , _SNSInvalidTopicFault
    , _SubscriptionAlreadyExistFault
    , _InvalidDBClusterSnapshotStateFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidVPCNetworkStateFault
    , _AuthorizationNotFoundFault
    , _ReservedDBInstanceNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupNotAllowedFault
    , _EventSubscriptionQuotaExceededFault
    , _InvalidOptionGroupStateFault
    , _InvalidDBClusterStateFault
    , _InsufficientStorageClusterCapacityFault
    , _DBInstanceAlreadyExistsFault
    , _ResourceNotFoundFault
    , _DBUpgradeDependencyFailureFault
    , _InvalidDBSecurityGroupStateFault
    , _InsufficientDomainCapacityFault
    , _DBSubnetGroupNotFoundFault
    , _InvalidRestoreFault
    , _InvalidDBInstanceStateFault
    , _InvalidDBSnapshotStateFault
    , _InvalidDBSubnetGroupStateFault
    , _StorageQuotaExceededFault
    , _DBSnapshotAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _SubnetAlreadyInUse
    , _DBSubnetGroupDoesNotCoverEnoughAZs

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
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcEarliestRestorableTime
    , dcEngine
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcCharacterSetName
    , dcAvailabilityZones
    , dcPreferredBackupWindow
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDatabaseName
    , dcDBSubnetGroup
    , dcAllocatedStorage
    , dcEndpoint
    , dcPercentProgress
    , dcPort
    , dcDBClusterOptionGroupMemberships

    -- * DBClusterMember
    , DBClusterMember
    , dbClusterMember
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
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsSnapshotType
    , dcsAvailabilityZones
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- * DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devDBEngineVersionDescription
    , devEngineVersion
    , devDefaultCharacterSet
    , devSupportedCharacterSets
    , devEngine
    , devDBParameterGroupFamily
    , devDBEngineDescription

    -- * DBInstance
    , DBInstance
    , dbInstance
    , diDBSecurityGroups
    , diEngineVersion
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diAutoMinorVersionUpgrade
    , diMasterUsername
    , diPubliclyAccessible
    , diReadReplicaDBInstanceIdentifiers
    , diIOPS
    , diInstanceCreateTime
    , diReadReplicaSourceDBInstanceIdentifier
    , diEngine
    , diLatestRestorableTime
    , diDBInstanceClass
    , diLicenseModel
    , diPreferredMaintenanceWindow
    , diCharacterSetName
    , diDBInstanceIdentifier
    , diCACertificateIdentifier
    , diPreferredBackupWindow
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diKMSKeyId
    , diDBSubnetGroup
    , diMultiAZ
    , diSecondaryAvailabilityZone
    , diOptionGroupMemberships
    , diDBiResourceId
    , diAllocatedStorage
    , diEndpoint
    , diDBParameterGroups
    , diTDECredentialARN
    , diCopyTagsToSnapshot
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
    , diStatusInfos
    , diDBName
    , diDomainMemberships
    , diStorageType

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
    , dsgVPCId
    , dsgOwnerId
    , dsgIPRanges
    , dsgDBSecurityGroupName
    , dsgEC2SecurityGroups
    , dsgDBSecurityGroupDescription

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
    , dsInstanceCreateTime
    , dsVPCId
    , dsEngine
    , dsEncrypted
    , dsDBSnapshotIdentifier
    , dsLicenseModel
    , dsSnapshotType
    , dsDBInstanceIdentifier
    , dsSourceDBSnapshotIdentifier
    , dsAvailabilityZone
    , dsKMSKeyId
    , dsSnapshotCreateTime
    , dsAllocatedStorage
    , dsTDECredentialARN
    , dsOptionGroupName
    , dsPercentProgress
    , dsPort
    , dsStorageType

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dbsgDBSubnetGroupName
    , dbsgVPCId
    , dbsgSubnets
    , dbsgDBSubnetGroupDescription
    , dbsgSubnetGroupStatus

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
    , dmDomain
    , dmConnectivity

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
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edParameters
    , edMarker

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
    , esCustomerAWSId
    , esStatus
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
    , ogoName
    , ogoMajorEngineVersion
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoPortRequired
    , ogoOptionsDependedOn
    , ogoDescription

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosApplyType
    , ogosSettingName
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosAllowedValues
    , ogosSettingDescription

    -- * OptionSetting
    , OptionSetting
    , optionSetting
    , osIsCollection
    , osApplyType
    , osValue
    , osName
    , osDefaultValue
    , osIsModifiable
    , osAllowedValues
    , osDataType
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
    , odioReadReplicaCapable
    , odioSupportsStorageEncryption
    , odioVPC
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
    , pAllowedValues
    , pDataType
    , pParameterName
    , pDescription

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaCurrentApplyDate
    , pmaForcedApplyDate

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvDBInstanceIdentifier
    , pmvCACertificateIdentifier
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
    , rdiProductDescription
    , rdiStartTime
    , rdiReservedDBInstanceId
    , rdiDBInstanceClass
    , rdiMultiAZ
    , rdiReservedDBInstancesOfferingId
    , rdiOfferingType
    , rdiUsagePrice
    , rdiRecurringCharges
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
    , rdioOfferingType
    , rdioUsagePrice
    , rdioRecurringCharges
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

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types.Product
import           Network.AWS.RDS.Types.Sum
import           Network.AWS.Sign.V4

-- | Version @2014-10-31@ of the Amazon Relational Database Service SDK.
data RDS

instance AWSService RDS where
    type Sg RDS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "RDS"
            , _svcPrefix = "rds"
            , _svcVersion = "2014-10-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | /CertificateIdentifier/ does not refer to an existing certificate.
_CertificateNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CertificateNotFound"

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedDBInstanceQuotaExceeded"

-- | User already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotAlreadyExistsFault =
    _ServiceError .
    hasStatus 400 . hasCode "DBClusterSnapshotAlreadyExistsFault"

-- | DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationQuotaExceeded"

-- | The requested source could not be found.
_SourceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault = _ServiceError . hasStatus 404 . hasCode "SourceNotFound"

-- | The DB parameter group cannot be deleted because it is in use.
_InvalidDBParameterGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBParameterGroupState"

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBParameterGroupAlreadyExists"

-- | /SourceDBInstanceIdentifier/ refers to a DB instance with
-- /BackupRetentionPeriod/ equal to 0.
_PointInTimeRestoreNotEnabledFault :: AWSError a => Getting (First ServiceError) a ServiceError
_PointInTimeRestoreNotEnabledFault =
    _ServiceError . hasStatus 400 . hasCode "PointInTimeRestoreNotEnabled"

-- | Request would result in user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBParameterGroupQuotaExceeded"

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIOPSNotAvailableInAZFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ProvisionedIOPSNotAvailableInAZFault =
    _ServiceError .
    hasStatus 400 . hasCode "ProvisionedIopsNotAvailableInAZFault"

-- | The specified CIDRIP or EC2 security group is already authorized for the
-- specified DB security group.
_AuthorizationAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The DB cluster does not have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientDBClusterCapacityFault =
    _ServiceError .
    hasStatus 403 . hasCode "InsufficientDBClusterCapacityFault"

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedDBInstanceAlreadyExists"

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionCategoryNotFound"

-- | Request would result in user exceeding the allowed number of subnets in
-- a DB subnet groups.
_DBSubnetQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetQuotaExceededFault"

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SubscriptionNotFound"

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | /DBClusterIdentifier/ does not refer to an existing DB cluster.
_DBClusterNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterNotFoundFault"

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_OptionGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "OptionGroupNotFoundFault"

-- | /LogFileName/ does not refer to an existing DB log file.
_DBLogFileNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBLogFileNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBLogFileNotFoundFault"

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_OptionGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "OptionGroupAlreadyExistsFault"

-- | User already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBClusterAlreadyExistsFault"

-- | /StorageType/ specified cannot be associated with the DB Instance.
_StorageTypeNotSupportedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "StorageTypeNotSupported"

-- | Request would result in user exceeding the allowed number of DB security
-- groups.
_DBSecurityGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "QuotaExceeded.DBSecurityGroup"

-- | /DBSnapshotIdentifier/ does not refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSnapshotNotFound"

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidEventSubscriptionStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidEventSubscriptionState"

-- | Error accessing KMS key.
_KMSKeyNotAccessibleFault :: AWSError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault =
    _ServiceError . hasStatus 400 . hasCode "KMSKeyNotAccessibleFault"

-- | The quota of 20 option groups was exceeded for this AWS account.
_OptionGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_OptionGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "OptionGroupQuotaExceededFault"

-- | A DB security group with the name specified in /DBSecurityGroupName/
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSecurityGroupAlreadyExists"

-- | Request would result in user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotQuotaExceeded"

-- | The SNS topic ARN does not exist.
_SNSTopicARNNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SNSTopicArnNotFound"

-- | User attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterQuotaExceededFault =
    _ServiceError . hasStatus 403 . hasCode "DBClusterQuotaExceededFault"

-- | /DBClusterParameterGroupName/ does not refer to an existing DB Cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterParameterGroupNotFound"

-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupAlreadyExists"

-- | Request would result in user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "InstanceQuotaExceeded"

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault =
    _ServiceError . hasStatus 400 . hasCode "SNSNoAuthorization"

-- | /DBSecurityGroupName/ does not refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSecurityGroupNotFound"

-- | A DB security group is not allowed for this action.
_DBSecurityGroupNotSupportedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "DBSecurityGroupNotSupported"

-- | /Domain/ does not refer to an existing Active Directory Domain.
_DomainNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DomainNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DomainNotFoundFault"

-- | /DBClusterSnapshotIdentifier/ does not refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBClusterSnapshotNotFoundFault"

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstancesOfferingNotFoundFault =
    _ServiceError .
    hasStatus 404 . hasCode "ReservedDBInstancesOfferingNotFound"

-- | Indicates the DBSubnetGroup does not belong to the same VPC as that of
-- an existing cross region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetGroupFault"

-- | The DB subnet is not in the /available/ state.
_InvalidDBSubnetStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetStateFault"

-- | /DBParameterGroupName/ does not refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBParameterGroupNotFound"

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault =
    _ServiceError . hasStatus 400 . hasCode "SNSInvalidTopic"

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault =
    _ServiceError . hasStatus 400 . hasCode "SubscriptionAlreadyExist"

-- | The supplied value is not a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterSnapshotStateFault =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidDBClusterSnapshotStateFault"

-- | Specified DB instance class is not available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientDBInstanceCapacity"

-- | DB subnet group does not cover all Availability Zones after it is
-- created because users\' change.
_InvalidVPCNetworkStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | Specified CIDRIP or EC2 security group is not authorized for the
-- specified DB security group.
--
-- RDS may not also be authorized via IAM to perform necessary actions on
-- your behalf.
_AuthorizationNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedDBInstanceNotFound"

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupQuotaExceeded"

-- | Indicates that the DBSubnetGroup should not be specified while creating
-- read replicas that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotAllowedFault =
    _ServiceError . hasStatus 400 . hasCode "DBSubnetGroupNotAllowedFault"

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "EventSubscriptionQuotaExceeded"

-- | The option group is not in the /available/ state.
_InvalidOptionGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidOptionGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidOptionGroupStateFault"

-- | The supplied value is not a valid DB cluster state.
_InvalidDBClusterStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBClusterStateFault"

-- | There is insufficient storage available for the current action. You may
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientStorageClusterCapacityFault =
    _ServiceError .
    hasStatus 400 . hasCode "InsufficientStorageClusterCapacity"

-- | User already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBInstanceAlreadyExists"

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundFault"

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
_DBUpgradeDependencyFailureFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault =
    _ServiceError . hasStatus 400 . hasCode "DBUpgradeDependencyFailure"

-- | The state of the DB security group does not allow deletion.
_InvalidDBSecurityGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSecurityGroupState"

-- | Requested Active Directory Domain has reached maximum number of
-- instances.
_InsufficientDomainCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientDomainCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientDomainCapacityFault"

-- | /DBSubnetGroupName/ does not refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBSubnetGroupNotFoundFault"

-- | Cannot restore from vpc backup to non-vpc DB instance.
_InvalidRestoreFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidRestoreFault"

-- | The specified DB instance is not in the /available/ state.
_InvalidDBInstanceStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBInstanceState"

-- | The state of the DB snapshot does not allow deletion.
_InvalidDBSnapshotStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSnapshotState"

-- | The DB subnet group cannot be deleted because it is in use.
_InvalidDBSubnetGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidDBSubnetGroupStateFault"

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
_StorageQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "StorageQuotaExceeded"

-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "DBSnapshotAlreadyExists"

-- | /DBInstanceIdentifier/ does not refer to an existing DB instance.
_DBInstanceNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "DBInstanceNotFound"

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse =
    _ServiceError . hasStatus 400 . hasCode "SubnetAlreadyInUse"

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: AWSError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
    _ServiceError .
    hasStatus 400 . hasCode "DBSubnetGroupDoesNotCoverEnoughAZs"
