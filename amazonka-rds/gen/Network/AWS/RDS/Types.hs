{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.RDS.Types
    (
    -- * Service
      RDS

    -- * Errors
    , _CertificateNotFoundFault
    , _ReservedDBInstanceQuotaExceededFault
    , _AuthorizationQuotaExceededFault
    , _SourceNotFoundFault
    , _InvalidDBParameterGroupStateFault
    , _DBParameterGroupAlreadyExistsFault
    , _PointInTimeRestoreNotEnabledFault
    , _DBParameterGroupQuotaExceededFault
    , _ProvisionedIOPSNotAvailableInAZFault
    , _AuthorizationAlreadyExistsFault
    , _ReservedDBInstanceAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _DBSubnetQuotaExceededFault
    , _SubscriptionNotFoundFault
    , _InvalidSubnet
    , _OptionGroupNotFoundFault
    , _OptionGroupAlreadyExistsFault
    , _StorageTypeNotSupportedFault
    , _DBSecurityGroupQuotaExceededFault
    , _DBSnapshotNotFoundFault
    , _InvalidEventSubscriptionStateFault
    , _KMSKeyNotAccessibleFault
    , _OptionGroupQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _SnapshotQuotaExceededFault
    , _SNSTopicARNNotFoundFault
    , _DBSubnetGroupAlreadyExistsFault
    , _InstanceQuotaExceededFault
    , _SNSNoAuthorizationFault
    , _DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotSupportedFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _InvalidDBSubnetGroupFault
    , _InvalidDBSubnetStateFault
    , _DBParameterGroupNotFoundFault
    , _SNSInvalidTopicFault
    , _SubscriptionAlreadyExistFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidVPCNetworkStateFault
    , _AuthorizationNotFoundFault
    , _ReservedDBInstanceNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupNotAllowedFault
    , _EventSubscriptionQuotaExceededFault
    , _InvalidOptionGroupStateFault
    , _DBInstanceAlreadyExistsFault
    , _ResourceNotFoundFault
    , _DBUpgradeDependencyFailureFault
    , _InvalidDBSecurityGroupStateFault
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
    , cerCertificateType
    , cerValidTill
    , cerCertificateIdentifier
    , cerThumbprint
    , cerValidFrom

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

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
    , diDBInstanceStatus
    , diPendingModifiedValues
    , diStatusInfos
    , diDBName
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
    , dbsDBSubnetGroupName
    , dbsVPCId
    , dbsSubnets
    , dbsDBSubnetGroupDescription
    , dbsSubnetGroupStatus

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddlfdLastWritten
    , ddlfdSize
    , ddlfdLogFileName

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
    , endAddress
    , endPort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edParameters
    , edMarker

    -- * Event
    , Event
    , event
    , eveSourceType
    , eveSourceIdentifier
    , eveDate
    , eveEventCategories
    , eveMessage

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
    , filName
    , filValues

    -- * IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIDRIP

    -- * Option
    , Option
    , option
    , optOptionName
    , optPermanent
    , optPersistent
    , optOptionDescription
    , optOptionSettings
    , optVPCSecurityGroupMemberships
    , optDBSecurityGroupMemberships
    , optPort

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
    , parApplyType
    , parParameterValue
    , parApplyMethod
    , parMinimumEngineVersion
    , parSource
    , parIsModifiable
    , parAllowedValues
    , parDataType
    , parParameterName
    , parDescription

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
    , subSubnetStatus
    , subSubnetIdentifier
    , subSubnetAvailabilityZone

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

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-10-31@ of the Amazon Relational Database Service SDK.
data RDS

instance AWSService RDS where
    type Sg RDS = V4

    service = const svc
      where
        svc :: Service RDS
        svc = Service
            { _svcAbbrev   = "RDS"
            , _svcPrefix   = "rds"
            , _svcVersion  = "2014-10-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout  = 80000000
            , _svcStatus   = statusSuccess
            , _svcError    = parseXMLError
            , _svcRetry    = retry
            }

        retry :: Retry
        retry = Exponential
            { _retryBase     = 0
            , _retryGrowth   = 0
            , _retryAttempts = 0
            , _retryCheck    = check
            }

        check :: ServiceError -> Bool
        check ServiceError'{..} = error "FIXME: Retry check not implemented."

-- | /CertificateIdentifier/ does not refer to an existing certificate.
_CertificateNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_CertificateNotFoundFault = _ServiceError . hasCode "CertificateNotFound" . hasStatus 400;

-- | Request would exceed the user\'s DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ReservedDBInstanceQuotaExceededFault = _ServiceError . hasCode "ReservedDBInstanceQuotaExceeded" . hasStatus 400;

-- | DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault = _ServiceError . hasCode "AuthorizationQuotaExceeded" . hasStatus 400;

-- | The requested source could not be found.
_SourceNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SourceNotFoundFault = _ServiceError . hasCode "SourceNotFound" . hasStatus 404;

-- | The DB parameter group cannot be deleted because it is in use.
_InvalidDBParameterGroupStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault = _ServiceError . hasCode "InvalidDBParameterGroupState" . hasStatus 400;

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault = _ServiceError . hasCode "DBParameterGroupAlreadyExists" . hasStatus 400;

-- | /SourceDBInstanceIdentifier/ refers to a DB instance with
-- /BackupRetentionPeriod/ equal to 0.
_PointInTimeRestoreNotEnabledFault :: AWSError a => Geting (First ServiceError) a ServiceError
_PointInTimeRestoreNotEnabledFault = _ServiceError . hasCode "PointInTimeRestoreNotEnabled" . hasStatus 400;

-- | Request would result in user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault = _ServiceError . hasCode "DBParameterGroupQuotaExceeded" . hasStatus 400;

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIOPSNotAvailableInAZFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ProvisionedIOPSNotAvailableInAZFault = _ServiceError . hasCode "ProvisionedIopsNotAvailableInAZFault" . hasStatus 400;

-- | The specified CIDRIP or EC2 security group is already authorized for the
-- specified DB security group.
_AuthorizationAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault = _ServiceError . hasCode "AuthorizationAlreadyExists" . hasStatus 400;

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ReservedDBInstanceAlreadyExistsFault = _ServiceError . hasCode "ReservedDBInstanceAlreadyExists" . hasStatus 404;

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault = _ServiceError . hasCode "SubscriptionCategoryNotFound" . hasStatus 404;

-- | Request would result in user exceeding the allowed number of subnets in
-- a DB subnet groups.
_DBSubnetQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault = _ServiceError . hasCode "DBSubnetQuotaExceededFault" . hasStatus 400;

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault = _ServiceError . hasCode "SubscriptionNotFound" . hasStatus 404;

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasCode "InvalidSubnet" . hasStatus 400;

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_OptionGroupNotFoundFault = _ServiceError . hasCode "OptionGroupNotFoundFault" . hasStatus 404;

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_OptionGroupAlreadyExistsFault = _ServiceError . hasCode "OptionGroupAlreadyExistsFault" . hasStatus 400;

-- | /StorageType/ specified cannot be associated with the DB Instance.
_StorageTypeNotSupportedFault :: AWSError a => Geting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault = _ServiceError . hasCode "StorageTypeNotSupported" . hasStatus 400;

-- | Request would result in user exceeding the allowed number of DB security
-- groups.
_DBSecurityGroupQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSecurityGroupQuotaExceededFault = _ServiceError . hasCode "QuotaExceeded.DBSecurityGroup" . hasStatus 400;

-- | /DBSnapshotIdentifier/ does not refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault = _ServiceError . hasCode "DBSnapshotNotFound" . hasStatus 404;

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
_InvalidEventSubscriptionStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidEventSubscriptionStateFault = _ServiceError . hasCode "InvalidEventSubscriptionState" . hasStatus 400;

-- | Error accessing KMS key.
_KMSKeyNotAccessibleFault :: AWSError a => Geting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault = _ServiceError . hasCode "KMSKeyNotAccessibleFault" . hasStatus 400;

-- | The quota of 20 option groups was exceeded for this AWS account.
_OptionGroupQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_OptionGroupQuotaExceededFault = _ServiceError . hasCode "OptionGroupQuotaExceededFault" . hasStatus 400;

-- | A DB security group with the name specified in /DBSecurityGroupName/
-- already exists.
_DBSecurityGroupAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSecurityGroupAlreadyExistsFault = _ServiceError . hasCode "DBSecurityGroupAlreadyExists" . hasStatus 400;

-- | Request would result in user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault = _ServiceError . hasCode "SnapshotQuotaExceeded" . hasStatus 400;

-- | The SNS topic ARN does not exist.
_SNSTopicARNNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault = _ServiceError . hasCode "SNSTopicArnNotFound" . hasStatus 404;

-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault = _ServiceError . hasCode "DBSubnetGroupAlreadyExists" . hasStatus 400;

-- | Request would result in user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault = _ServiceError . hasCode "InstanceQuotaExceeded" . hasStatus 400;

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault = _ServiceError . hasCode "SNSNoAuthorization" . hasStatus 400;

-- | /DBSecurityGroupName/ does not refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault = _ServiceError . hasCode "DBSecurityGroupNotFound" . hasStatus 404;

-- | A DB security group is not allowed for this action.
_DBSecurityGroupNotSupportedFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSecurityGroupNotSupportedFault = _ServiceError . hasCode "DBSecurityGroupNotSupported" . hasStatus 400;

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ReservedDBInstancesOfferingNotFoundFault = _ServiceError . hasCode "ReservedDBInstancesOfferingNotFound" . hasStatus 404;

-- | Indicates the DBSubnetGroup does not belong to the same VPC as that of
-- an existing cross region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupFault = _ServiceError . hasCode "InvalidDBSubnetGroupFault" . hasStatus 400;

-- | The DB subnet is not in the /available/ state.
_InvalidDBSubnetStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault = _ServiceError . hasCode "InvalidDBSubnetStateFault" . hasStatus 400;

-- | /DBParameterGroupName/ does not refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault = _ServiceError . hasCode "DBParameterGroupNotFound" . hasStatus 404;

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SNSInvalidTopicFault = _ServiceError . hasCode "SNSInvalidTopic" . hasStatus 400;

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: AWSError a => Geting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault = _ServiceError . hasCode "SubscriptionAlreadyExist" . hasStatus 400;

-- | Specified DB instance class is not available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault = _ServiceError . hasCode "InsufficientDBInstanceCapacity" . hasStatus 400;

-- | DB subnet group does not cover all Availability Zones after it is
-- created because users\' change.
_InvalidVPCNetworkStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault = _ServiceError . hasCode "InvalidVPCNetworkStateFault" . hasStatus 400;

-- | Specified CIDRIP or EC2 security group is not authorized for the
-- specified DB security group.
--
-- RDS may not also be authorized via IAM to perform necessary actions on
-- your behalf.
_AuthorizationNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault = _ServiceError . hasCode "AuthorizationNotFound" . hasStatus 404;

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ReservedDBInstanceNotFoundFault = _ServiceError . hasCode "ReservedDBInstanceNotFound" . hasStatus 404;

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault = _ServiceError . hasCode "DBSubnetGroupQuotaExceeded" . hasStatus 400;

-- | Indicates that the DBSubnetGroup should not be specified while creating
-- read replicas that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetGroupNotAllowedFault = _ServiceError . hasCode "DBSubnetGroupNotAllowedFault" . hasStatus 400;

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault = _ServiceError . hasCode "EventSubscriptionQuotaExceeded" . hasStatus 400;

-- | The option group is not in the /available/ state.
_InvalidOptionGroupStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidOptionGroupStateFault = _ServiceError . hasCode "InvalidOptionGroupStateFault" . hasStatus 400;

-- | User already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault = _ServiceError . hasCode "DBInstanceAlreadyExists" . hasStatus 400;

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_ResourceNotFoundFault = _ServiceError . hasCode "ResourceNotFoundFault" . hasStatus 404;

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
_DBUpgradeDependencyFailureFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault = _ServiceError . hasCode "DBUpgradeDependencyFailure" . hasStatus 400;

-- | The state of the DB security group does not allow deletion.
_InvalidDBSecurityGroupStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault = _ServiceError . hasCode "InvalidDBSecurityGroupState" . hasStatus 400;

-- | /DBSubnetGroupName/ does not refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault = _ServiceError . hasCode "DBSubnetGroupNotFoundFault" . hasStatus 404;

-- | Cannot restore from vpc backup to non-vpc DB instance.
_InvalidRestoreFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidRestoreFault = _ServiceError . hasCode "InvalidRestoreFault" . hasStatus 400;

-- | The specified DB instance is not in the /available/ state.
_InvalidDBInstanceStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault = _ServiceError . hasCode "InvalidDBInstanceState" . hasStatus 400;

-- | The state of the DB snapshot does not allow deletion.
_InvalidDBSnapshotStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault = _ServiceError . hasCode "InvalidDBSnapshotState" . hasStatus 400;

-- | The DB subnet group cannot be deleted because it is in use.
_InvalidDBSubnetGroupStateFault :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault = _ServiceError . hasCode "InvalidDBSubnetGroupStateFault" . hasStatus 400;

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
_StorageQuotaExceededFault :: AWSError a => Geting (First ServiceError) a ServiceError
_StorageQuotaExceededFault = _ServiceError . hasCode "StorageQuotaExceeded" . hasStatus 400;

-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault = _ServiceError . hasCode "DBSnapshotAlreadyExists" . hasStatus 400;

-- | /DBInstanceIdentifier/ does not refer to an existing DB instance.
_DBInstanceNotFoundFault :: AWSError a => Geting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault = _ServiceError . hasCode "DBInstanceNotFound" . hasStatus 404;

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: AWSError a => Geting (First ServiceError) a ServiceError
_SubnetAlreadyInUse = _ServiceError . hasCode "SubnetAlreadyInUse" . hasStatus 400;

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: AWSError a => Geting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs = _ServiceError . hasCode "DBSubnetGroupDoesNotCoverEnoughAZs" . hasStatus 400;

data ApplyMethod = PendingReboot | Immediate deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ApplyMethod where
    parser = takeLowerText >>= \case
        "immediate" -> pure Immediate
        "pending-reboot" -> pure PendingReboot
        e -> fail ("Failure parsing ApplyMethod from " ++ show e)

instance ToText ApplyMethod where
    toText = \case
        Immediate -> "immediate"
        PendingReboot -> "pending-reboot"

instance Hashable ApplyMethod
instance ToQuery ApplyMethod
instance ToHeader ApplyMethod

instance FromXML ApplyMethod where
    parseXML = parseXMLText "ApplyMethod"

data SourceType = DBSecurityGroup | DBSnapshot | DBParameterGroup | DBInstance deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "db-instance" -> pure DBInstance
        "db-parameter-group" -> pure DBParameterGroup
        "db-security-group" -> pure DBSecurityGroup
        "db-snapshot" -> pure DBSnapshot
        e -> fail ("Failure parsing SourceType from " ++ show e)

instance ToText SourceType where
    toText = \case
        DBInstance -> "db-instance"
        DBParameterGroup -> "db-parameter-group"
        DBSecurityGroup -> "db-security-group"
        DBSnapshot -> "db-snapshot"

instance Hashable SourceType
instance ToQuery SourceType
instance ToHeader SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

-- | Describes a quota for an AWS account, for example, the number of DB
-- instances allowed.
--
-- /See:/ 'accountQuota' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aqMax'
--
-- * 'aqUsed'
--
-- * 'aqAccountQuotaName'
data AccountQuota = AccountQuota'{_aqMax :: Maybe Integer, _aqUsed :: Maybe Integer, _aqAccountQuotaName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AccountQuota' smart constructor.
accountQuota :: AccountQuota
accountQuota = AccountQuota'{_aqMax = Nothing, _aqUsed = Nothing, _aqAccountQuotaName = Nothing};

-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\ s a -> s{_aqMax = a});

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\ s a -> s{_aqUsed = a});

-- | The name of the Amazon RDS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\ s a -> s{_aqAccountQuotaName = a});

instance FromXML AccountQuota where
        parseXML x
          = AccountQuota' <$>
              (x .@? "Max") <*> (x .@? "Used") <*>
                (x .@? "AccountQuotaName")

-- | Contains Availability Zone information.
--
-- This data type is used as an element in the following data type:
--
-- -   OrderableDBInstanceOption
--
-- /See:/ 'availabilityZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azName'
newtype AvailabilityZone = AvailabilityZone'{_azName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AvailabilityZone' smart constructor.
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone'{_azName = Nothing};

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a});

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

-- | A CA certificate for an AWS account.
--
-- /See:/ 'certificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerCertificateType'
--
-- * 'cerValidTill'
--
-- * 'cerCertificateIdentifier'
--
-- * 'cerThumbprint'
--
-- * 'cerValidFrom'
data Certificate = Certificate'{_cerCertificateType :: Maybe Text, _cerValidTill :: Maybe ISO8601, _cerCertificateIdentifier :: Maybe Text, _cerThumbprint :: Maybe Text, _cerValidFrom :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'Certificate' smart constructor.
certificate :: Certificate
certificate = Certificate'{_cerCertificateType = Nothing, _cerValidTill = Nothing, _cerCertificateIdentifier = Nothing, _cerThumbprint = Nothing, _cerValidFrom = Nothing};

-- | The type of the certificate.
cerCertificateType :: Lens' Certificate (Maybe Text)
cerCertificateType = lens _cerCertificateType (\ s a -> s{_cerCertificateType = a});

-- | The final date that the certificate continues to be valid.
cerValidTill :: Lens' Certificate (Maybe UTCTime)
cerValidTill = lens _cerValidTill (\ s a -> s{_cerValidTill = a}) . mapping _Time;

-- | The unique key that identifies a certificate.
cerCertificateIdentifier :: Lens' Certificate (Maybe Text)
cerCertificateIdentifier = lens _cerCertificateIdentifier (\ s a -> s{_cerCertificateIdentifier = a});

-- | The thumbprint of the certificate.
cerThumbprint :: Lens' Certificate (Maybe Text)
cerThumbprint = lens _cerThumbprint (\ s a -> s{_cerThumbprint = a});

-- | The starting date from which the certificate is valid.
cerValidFrom :: Lens' Certificate (Maybe UTCTime)
cerValidFrom = lens _cerValidFrom (\ s a -> s{_cerValidFrom = a}) . mapping _Time;

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateType") <*> (x .@? "ValidTill") <*>
                (x .@? "CertificateIdentifier")
                <*> (x .@? "Thumbprint")
                <*> (x .@? "ValidFrom")

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
--
-- /See:/ 'characterSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csCharacterSetName'
--
-- * 'csCharacterSetDescription'
data CharacterSet = CharacterSet'{_csCharacterSetName :: Maybe Text, _csCharacterSetDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CharacterSet' smart constructor.
characterSet :: CharacterSet
characterSet = CharacterSet'{_csCharacterSetName = Nothing, _csCharacterSetDescription = Nothing};

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName = lens _csCharacterSetName (\ s a -> s{_csCharacterSetName = a});

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription = lens _csCharacterSetDescription (\ s a -> s{_csCharacterSetDescription = a});

instance FromXML CharacterSet where
        parseXML x
          = CharacterSet' <$>
              (x .@? "CharacterSetName") <*>
                (x .@? "CharacterSetDescription")

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
--
-- /See:/ 'dbEngineVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'devDBEngineVersionDescription'
--
-- * 'devEngineVersion'
--
-- * 'devDefaultCharacterSet'
--
-- * 'devSupportedCharacterSets'
--
-- * 'devEngine'
--
-- * 'devDBParameterGroupFamily'
--
-- * 'devDBEngineDescription'
data DBEngineVersion = DBEngineVersion'{_devDBEngineVersionDescription :: Maybe Text, _devEngineVersion :: Maybe Text, _devDefaultCharacterSet :: Maybe CharacterSet, _devSupportedCharacterSets :: Maybe [CharacterSet], _devEngine :: Maybe Text, _devDBParameterGroupFamily :: Maybe Text, _devDBEngineDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBEngineVersion' smart constructor.
dbEngineVersion :: DBEngineVersion
dbEngineVersion = DBEngineVersion'{_devDBEngineVersionDescription = Nothing, _devEngineVersion = Nothing, _devDefaultCharacterSet = Nothing, _devSupportedCharacterSets = Nothing, _devEngine = Nothing, _devDBParameterGroupFamily = Nothing, _devDBEngineDescription = Nothing};

-- | The description of the database engine version.
devDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineVersionDescription = lens _devDBEngineVersionDescription (\ s a -> s{_devDBEngineVersionDescription = a});

-- | The version number of the database engine.
devEngineVersion :: Lens' DBEngineVersion (Maybe Text)
devEngineVersion = lens _devEngineVersion (\ s a -> s{_devEngineVersion = a});

-- | The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API is not
-- specified.
devDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
devDefaultCharacterSet = lens _devDefaultCharacterSet (\ s a -> s{_devDefaultCharacterSet = a});

-- | A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the CreateDBInstance API.
devSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
devSupportedCharacterSets = lens _devSupportedCharacterSets (\ s a -> s{_devSupportedCharacterSets = a}) . _Default;

-- | The name of the database engine.
devEngine :: Lens' DBEngineVersion (Maybe Text)
devEngine = lens _devEngine (\ s a -> s{_devEngine = a});

-- | The name of the DB parameter group family for the database engine.
devDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
devDBParameterGroupFamily = lens _devDBParameterGroupFamily (\ s a -> s{_devDBParameterGroupFamily = a});

-- | The description of the database engine.
devDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineDescription = lens _devDBEngineDescription (\ s a -> s{_devDBEngineDescription = a});

instance FromXML DBEngineVersion where
        parseXML x
          = DBEngineVersion' <$>
              (x .@? "DBEngineVersionDescription") <*>
                (x .@? "EngineVersion")
                <*> (x .@? "DefaultCharacterSet")
                <*>
                (x .@? "SupportedCharacterSets" .!@ mempty >>=
                   may (parseXMLList "CharacterSet"))
                <*> (x .@? "Engine")
                <*> (x .@? "DBParameterGroupFamily")
                <*> (x .@? "DBEngineDescription")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBInstance
-- -   DeleteDBInstance
-- -   ModifyDBInstance
--
-- This data type is used as a response element in the DescribeDBInstances
-- action.
--
-- /See:/ 'dbInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDBSecurityGroups'
--
-- * 'diEngineVersion'
--
-- * 'diStorageEncrypted'
--
-- * 'diAutoMinorVersionUpgrade'
--
-- * 'diMasterUsername'
--
-- * 'diPubliclyAccessible'
--
-- * 'diReadReplicaDBInstanceIdentifiers'
--
-- * 'diIOPS'
--
-- * 'diInstanceCreateTime'
--
-- * 'diReadReplicaSourceDBInstanceIdentifier'
--
-- * 'diEngine'
--
-- * 'diLatestRestorableTime'
--
-- * 'diDBInstanceClass'
--
-- * 'diLicenseModel'
--
-- * 'diPreferredMaintenanceWindow'
--
-- * 'diCharacterSetName'
--
-- * 'diDBInstanceIdentifier'
--
-- * 'diCACertificateIdentifier'
--
-- * 'diPreferredBackupWindow'
--
-- * 'diAvailabilityZone'
--
-- * 'diVPCSecurityGroups'
--
-- * 'diBackupRetentionPeriod'
--
-- * 'diKMSKeyId'
--
-- * 'diDBSubnetGroup'
--
-- * 'diMultiAZ'
--
-- * 'diSecondaryAvailabilityZone'
--
-- * 'diOptionGroupMemberships'
--
-- * 'diDBiResourceId'
--
-- * 'diAllocatedStorage'
--
-- * 'diEndpoint'
--
-- * 'diDBParameterGroups'
--
-- * 'diTDECredentialARN'
--
-- * 'diDBInstanceStatus'
--
-- * 'diPendingModifiedValues'
--
-- * 'diStatusInfos'
--
-- * 'diDBName'
--
-- * 'diStorageType'
data DBInstance = DBInstance'{_diDBSecurityGroups :: Maybe [DBSecurityGroupMembership], _diEngineVersion :: Maybe Text, _diStorageEncrypted :: Maybe Bool, _diAutoMinorVersionUpgrade :: Maybe Bool, _diMasterUsername :: Maybe Text, _diPubliclyAccessible :: Maybe Bool, _diReadReplicaDBInstanceIdentifiers :: Maybe [Text], _diIOPS :: Maybe Int, _diInstanceCreateTime :: Maybe ISO8601, _diReadReplicaSourceDBInstanceIdentifier :: Maybe Text, _diEngine :: Maybe Text, _diLatestRestorableTime :: Maybe ISO8601, _diDBInstanceClass :: Maybe Text, _diLicenseModel :: Maybe Text, _diPreferredMaintenanceWindow :: Maybe Text, _diCharacterSetName :: Maybe Text, _diDBInstanceIdentifier :: Maybe Text, _diCACertificateIdentifier :: Maybe Text, _diPreferredBackupWindow :: Maybe Text, _diAvailabilityZone :: Maybe Text, _diVPCSecurityGroups :: Maybe [VPCSecurityGroupMembership], _diBackupRetentionPeriod :: Maybe Int, _diKMSKeyId :: Maybe Text, _diDBSubnetGroup :: Maybe DBSubnetGroup, _diMultiAZ :: Maybe Bool, _diSecondaryAvailabilityZone :: Maybe Text, _diOptionGroupMemberships :: Maybe [OptionGroupMembership], _diDBiResourceId :: Maybe Text, _diAllocatedStorage :: Maybe Int, _diEndpoint :: Maybe Endpoint, _diDBParameterGroups :: Maybe [DBParameterGroupStatus], _diTDECredentialARN :: Maybe Text, _diDBInstanceStatus :: Maybe Text, _diPendingModifiedValues :: Maybe PendingModifiedValues, _diStatusInfos :: Maybe [DBInstanceStatusInfo], _diDBName :: Maybe Text, _diStorageType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBInstance' smart constructor.
dbInstance :: DBInstance
dbInstance = DBInstance'{_diDBSecurityGroups = Nothing, _diEngineVersion = Nothing, _diStorageEncrypted = Nothing, _diAutoMinorVersionUpgrade = Nothing, _diMasterUsername = Nothing, _diPubliclyAccessible = Nothing, _diReadReplicaDBInstanceIdentifiers = Nothing, _diIOPS = Nothing, _diInstanceCreateTime = Nothing, _diReadReplicaSourceDBInstanceIdentifier = Nothing, _diEngine = Nothing, _diLatestRestorableTime = Nothing, _diDBInstanceClass = Nothing, _diLicenseModel = Nothing, _diPreferredMaintenanceWindow = Nothing, _diCharacterSetName = Nothing, _diDBInstanceIdentifier = Nothing, _diCACertificateIdentifier = Nothing, _diPreferredBackupWindow = Nothing, _diAvailabilityZone = Nothing, _diVPCSecurityGroups = Nothing, _diBackupRetentionPeriod = Nothing, _diKMSKeyId = Nothing, _diDBSubnetGroup = Nothing, _diMultiAZ = Nothing, _diSecondaryAvailabilityZone = Nothing, _diOptionGroupMemberships = Nothing, _diDBiResourceId = Nothing, _diAllocatedStorage = Nothing, _diEndpoint = Nothing, _diDBParameterGroups = Nothing, _diTDECredentialARN = Nothing, _diDBInstanceStatus = Nothing, _diPendingModifiedValues = Nothing, _diStatusInfos = Nothing, _diDBName = Nothing, _diStorageType = Nothing};

-- | Provides List of DB security group elements containing only
-- @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
diDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
diDBSecurityGroups = lens _diDBSecurityGroups (\ s a -> s{_diDBSecurityGroups = a}) . _Default;

-- | Indicates the database engine version.
diEngineVersion :: Lens' DBInstance (Maybe Text)
diEngineVersion = lens _diEngineVersion (\ s a -> s{_diEngineVersion = a});

-- | Specifies whether the DB instance is encrypted.
diStorageEncrypted :: Lens' DBInstance (Maybe Bool)
diStorageEncrypted = lens _diStorageEncrypted (\ s a -> s{_diStorageEncrypted = a});

-- | Indicates that minor version patches are applied automatically.
diAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
diAutoMinorVersionUpgrade = lens _diAutoMinorVersionUpgrade (\ s a -> s{_diAutoMinorVersionUpgrade = a});

-- | Contains the master username for the DB instance.
diMasterUsername :: Lens' DBInstance (Maybe Text)
diMasterUsername = lens _diMasterUsername (\ s a -> s{_diMasterUsername = a});

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS
-- name, which resolves to a public IP address. A value of false specifies
-- an internal instance with a DNS name that resolves to a private IP
-- address.
--
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case.
--
-- -   __Default VPC:__true
-- -   __VPC:__false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
diPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
diPubliclyAccessible = lens _diPubliclyAccessible (\ s a -> s{_diPubliclyAccessible = a});

-- | Contains one or more identifiers of the Read Replicas associated with
-- this DB instance.
diReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBInstanceIdentifiers = lens _diReadReplicaDBInstanceIdentifiers (\ s a -> s{_diReadReplicaDBInstanceIdentifiers = a}) . _Default;

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
diIOPS :: Lens' DBInstance (Maybe Int)
diIOPS = lens _diIOPS (\ s a -> s{_diIOPS = a});

-- | Provides the date and time the DB instance was created.
diInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
diInstanceCreateTime = lens _diInstanceCreateTime (\ s a -> s{_diInstanceCreateTime = a}) . mapping _Time;

-- | Contains the identifier of the source DB instance if this DB instance is
-- a Read Replica.
diReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diReadReplicaSourceDBInstanceIdentifier = lens _diReadReplicaSourceDBInstanceIdentifier (\ s a -> s{_diReadReplicaSourceDBInstanceIdentifier = a});

-- | Provides the name of the database engine to be used for this DB
-- instance.
diEngine :: Lens' DBInstance (Maybe Text)
diEngine = lens _diEngine (\ s a -> s{_diEngine = a});

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
diLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
diLatestRestorableTime = lens _diLatestRestorableTime (\ s a -> s{_diLatestRestorableTime = a}) . mapping _Time;

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
diDBInstanceClass :: Lens' DBInstance (Maybe Text)
diDBInstanceClass = lens _diDBInstanceClass (\ s a -> s{_diDBInstanceClass = a});

-- | License model information for this DB instance.
diLicenseModel :: Lens' DBInstance (Maybe Text)
diLicenseModel = lens _diLicenseModel (\ s a -> s{_diLicenseModel = a});

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
diPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
diPreferredMaintenanceWindow = lens _diPreferredMaintenanceWindow (\ s a -> s{_diPreferredMaintenanceWindow = a});

-- | If present, specifies the name of the character set that this instance
-- is associated with.
diCharacterSetName :: Lens' DBInstance (Maybe Text)
diCharacterSetName = lens _diCharacterSetName (\ s a -> s{_diCharacterSetName = a});

-- | Contains a user-supplied database identifier. This is the unique key
-- that identifies a DB instance.
diDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diDBInstanceIdentifier = lens _diDBInstanceIdentifier (\ s a -> s{_diDBInstanceIdentifier = a});

-- | The identifier of the CA certificate for this DB instance.
diCACertificateIdentifier :: Lens' DBInstance (Maybe Text)
diCACertificateIdentifier = lens _diCACertificateIdentifier (\ s a -> s{_diCACertificateIdentifier = a});

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
diPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
diPreferredBackupWindow = lens _diPreferredBackupWindow (\ s a -> s{_diPreferredBackupWindow = a});

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
diAvailabilityZone :: Lens' DBInstance (Maybe Text)
diAvailabilityZone = lens _diAvailabilityZone (\ s a -> s{_diAvailabilityZone = a});

-- | Provides List of VPC security group elements that the DB instance
-- belongs to.
diVPCSecurityGroups :: Lens' DBInstance [VPCSecurityGroupMembership]
diVPCSecurityGroups = lens _diVPCSecurityGroups (\ s a -> s{_diVPCSecurityGroups = a}) . _Default;

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
diBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
diBackupRetentionPeriod = lens _diBackupRetentionPeriod (\ s a -> s{_diBackupRetentionPeriod = a});

-- | If @StorageEncrypted@ is true, the KMS key identifier for the encrypted
-- DB instance.
diKMSKeyId :: Lens' DBInstance (Maybe Text)
diKMSKeyId = lens _diKMSKeyId (\ s a -> s{_diKMSKeyId = a});

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
diDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
diDBSubnetGroup = lens _diDBSubnetGroup (\ s a -> s{_diDBSubnetGroup = a});

-- | Specifies if the DB instance is a Multi-AZ deployment.
diMultiAZ :: Lens' DBInstance (Maybe Bool)
diMultiAZ = lens _diMultiAZ (\ s a -> s{_diMultiAZ = a});

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
diSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
diSecondaryAvailabilityZone = lens _diSecondaryAvailabilityZone (\ s a -> s{_diSecondaryAvailabilityZone = a});

-- | Provides the list of option group memberships for this DB instance.
diOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
diOptionGroupMemberships = lens _diOptionGroupMemberships (\ s a -> s{_diOptionGroupMemberships = a}) . _Default;

-- | If @StorageEncrypted@ is true, the region-unique, immutable identifier
-- for the encrypted DB instance. This identifier is found in AWS
-- CloudTrail log entries whenever the KMS key for the DB instance is
-- accessed.
diDBiResourceId :: Lens' DBInstance (Maybe Text)
diDBiResourceId = lens _diDBiResourceId (\ s a -> s{_diDBiResourceId = a});

-- | Specifies the allocated storage size specified in gigabytes.
diAllocatedStorage :: Lens' DBInstance (Maybe Int)
diAllocatedStorage = lens _diAllocatedStorage (\ s a -> s{_diAllocatedStorage = a});

-- | Specifies the connection endpoint.
diEndpoint :: Lens' DBInstance (Maybe Endpoint)
diEndpoint = lens _diEndpoint (\ s a -> s{_diEndpoint = a});

-- | Provides the list of DB parameter groups applied to this DB instance.
diDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
diDBParameterGroups = lens _diDBParameterGroups (\ s a -> s{_diDBParameterGroups = a}) . _Default;

-- | The ARN from the Key Store with which the instance is associated for TDE
-- encryption.
diTDECredentialARN :: Lens' DBInstance (Maybe Text)
diTDECredentialARN = lens _diTDECredentialARN (\ s a -> s{_diTDECredentialARN = a});

-- | Specifies the current state of this database.
diDBInstanceStatus :: Lens' DBInstance (Maybe Text)
diDBInstanceStatus = lens _diDBInstanceStatus (\ s a -> s{_diDBInstanceStatus = a});

-- | Specifies that changes to the DB instance are pending. This element is
-- only included when changes are pending. Specific changes are identified
-- by subelements.
diPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
diPendingModifiedValues = lens _diPendingModifiedValues (\ s a -> s{_diPendingModifiedValues = a});

-- | The status of a Read Replica. If the instance is not a Read Replica,
-- this will be blank.
diStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
diStatusInfos = lens _diStatusInfos (\ s a -> s{_diStatusInfos = a}) . _Default;

-- | The meaning of this parameter differs according to the database engine
-- you use. For example, this value returns either MySQL or PostgreSQL
-- information when returning values from CreateDBInstanceReadReplica since
-- Read Replicas are only supported for MySQL and PostgreSQL.
--
-- __MySQL, SQL Server, PostgreSQL__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- Type: String
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
diDBName :: Lens' DBInstance (Maybe Text)
diDBName = lens _diDBName (\ s a -> s{_diDBName = a});

-- | Specifies the storage type associated with DB instance.
diStorageType :: Lens' DBInstance (Maybe Text)
diStorageType = lens _diStorageType (\ s a -> s{_diStorageType = a});

instance FromXML DBInstance where
        parseXML x
          = DBInstance' <$>
              (x .@? "DBSecurityGroups" .!@ mempty >>=
                 may (parseXMLList "DBSecurityGroup"))
                <*> (x .@? "EngineVersion")
                <*> (x .@? "StorageEncrypted")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "PubliclyAccessible")
                <*>
                (x .@? "ReadReplicaDBInstanceIdentifiers" .!@ mempty
                   >>=
                   may (parseXMLList "ReadReplicaDBInstanceIdentifier"))
                <*> (x .@? "Iops")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "ReadReplicaSourceDBInstanceIdentifier")
                <*> (x .@? "Engine")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "CharacterSetName")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "PreferredBackupWindow")
                <*> (x .@? "AvailabilityZone")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "SecondaryAvailabilityZone")
                <*>
                (x .@? "OptionGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "OptionGroupMembership"))
                <*> (x .@? "DbiResourceId")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Endpoint")
                <*>
                (x .@? "DBParameterGroups" .!@ mempty >>=
                   may (parseXMLList "DBParameterGroup"))
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "DBInstanceStatus")
                <*> (x .@? "PendingModifiedValues")
                <*>
                (x .@? "StatusInfos" .!@ mempty >>=
                   may (parseXMLList "DBInstanceStatusInfo"))
                <*> (x .@? "DBName")
                <*> (x .@? "StorageType")

-- | Provides a list of status information for a DB instance.
--
-- /See:/ 'dbInstanceStatusInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disiStatus'
--
-- * 'disiNormal'
--
-- * 'disiStatusType'
--
-- * 'disiMessage'
data DBInstanceStatusInfo = DBInstanceStatusInfo'{_disiStatus :: Maybe Text, _disiNormal :: Maybe Bool, _disiStatusType :: Maybe Text, _disiMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBInstanceStatusInfo' smart constructor.
dbInstanceStatusInfo :: DBInstanceStatusInfo
dbInstanceStatusInfo = DBInstanceStatusInfo'{_disiStatus = Nothing, _disiNormal = Nothing, _disiStatusType = Nothing, _disiMessage = Nothing};

-- | Status of the DB instance. For a StatusType of read replica, the values
-- can be replicating, error, stopped, or terminated.
disiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatus = lens _disiStatus (\ s a -> s{_disiStatus = a});

-- | Boolean value that is true if the instance is operating normally, or
-- false if the instance is in an error state.
disiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
disiNormal = lens _disiNormal (\ s a -> s{_disiNormal = a});

-- | This value is currently \"read replication.\"
disiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatusType = lens _disiStatusType (\ s a -> s{_disiStatusType = a});

-- | Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
disiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
disiMessage = lens _disiMessage (\ s a -> s{_disiMessage = a});

instance FromXML DBInstanceStatusInfo where
        parseXML x
          = DBInstanceStatusInfo' <$>
              (x .@? "Status") <*> (x .@? "Normal") <*>
                (x .@? "StatusType")
                <*> (x .@? "Message")

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action.
--
-- This data type is used as a request parameter in the
-- DeleteDBParameterGroup action, and as a response element in the
-- DescribeDBParameterGroups action.
--
-- /See:/ 'dbParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgDBParameterGroupFamily'
--
-- * 'dpgDBParameterGroupName'
--
-- * 'dpgDescription'
data DBParameterGroup = DBParameterGroup'{_dpgDBParameterGroupFamily :: Maybe Text, _dpgDBParameterGroupName :: Maybe Text, _dpgDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBParameterGroup' smart constructor.
dbParameterGroup :: DBParameterGroup
dbParameterGroup = DBParameterGroup'{_dpgDBParameterGroupFamily = Nothing, _dpgDBParameterGroupName = Nothing, _dpgDescription = Nothing};

-- | Provides the name of the DB parameter group family that this DB
-- parameter group is compatible with.
dpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupFamily = lens _dpgDBParameterGroupFamily (\ s a -> s{_dpgDBParameterGroupFamily = a});

-- | Provides the name of the DB parameter group.
dpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupName = lens _dpgDBParameterGroupName (\ s a -> s{_dpgDBParameterGroupName = a});

-- | Provides the customer-specified description for this DB parameter group.
dpgDescription :: Lens' DBParameterGroup (Maybe Text)
dpgDescription = lens _dpgDescription (\ s a -> s{_dpgDescription = a});

instance FromXML DBParameterGroup where
        parseXML x
          = DBParameterGroup' <$>
              (x .@? "DBParameterGroupFamily") <*>
                (x .@? "DBParameterGroupName")
                <*> (x .@? "Description")

-- | Contains the result of a successful invocation of the
-- ModifyDBParameterGroup or ResetDBParameterGroup action.
--
-- /See:/ 'dbParameterGroupNameMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgnmDBParameterGroupName'
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'{_dpgnmDBParameterGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBParameterGroupNameMessage' smart constructor.
dbParameterGroupNameMessage :: DBParameterGroupNameMessage
dbParameterGroupNameMessage = DBParameterGroupNameMessage'{_dpgnmDBParameterGroupName = Nothing};

-- | The name of the DB parameter group.
dpgnmDBParameterGroupName :: Lens' DBParameterGroupNameMessage (Maybe Text)
dpgnmDBParameterGroupName = lens _dpgnmDBParameterGroupName (\ s a -> s{_dpgnmDBParameterGroupName = a});

instance FromXML DBParameterGroupNameMessage where
        parseXML x
          = DBParameterGroupNameMessage' <$>
              (x .@? "DBParameterGroupName")

-- | The status of the DB parameter group.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateDBInstance
-- -   CreateDBInstanceReadReplica
-- -   DeleteDBInstance
-- -   ModifyDBInstance
-- -   RebootDBInstance
-- -   RestoreDBInstanceFromDBSnapshot
--
-- /See:/ 'dbParameterGroupStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgsDBParameterGroupName'
--
-- * 'dpgsParameterApplyStatus'
data DBParameterGroupStatus = DBParameterGroupStatus'{_dpgsDBParameterGroupName :: Maybe Text, _dpgsParameterApplyStatus :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBParameterGroupStatus' smart constructor.
dbParameterGroupStatus :: DBParameterGroupStatus
dbParameterGroupStatus = DBParameterGroupStatus'{_dpgsDBParameterGroupName = Nothing, _dpgsParameterApplyStatus = Nothing};

-- | The name of the DP parameter group.
dpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsDBParameterGroupName = lens _dpgsDBParameterGroupName (\ s a -> s{_dpgsDBParameterGroupName = a});

-- | The status of parameter updates.
dpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsParameterApplyStatus = lens _dpgsParameterApplyStatus (\ s a -> s{_dpgsParameterApplyStatus = a});

instance FromXML DBParameterGroupStatus where
        parseXML x
          = DBParameterGroupStatus' <$>
              (x .@? "DBParameterGroupName") <*>
                (x .@? "ParameterApplyStatus")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   DescribeDBSecurityGroups
-- -   AuthorizeDBSecurityGroupIngress
-- -   CreateDBSecurityGroup
-- -   RevokeDBSecurityGroupIngress
--
-- This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'dbSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgVPCId'
--
-- * 'dsgOwnerId'
--
-- * 'dsgIPRanges'
--
-- * 'dsgDBSecurityGroupName'
--
-- * 'dsgEC2SecurityGroups'
--
-- * 'dsgDBSecurityGroupDescription'
data DBSecurityGroup = DBSecurityGroup'{_dsgVPCId :: Maybe Text, _dsgOwnerId :: Maybe Text, _dsgIPRanges :: Maybe [IPRange], _dsgDBSecurityGroupName :: Maybe Text, _dsgEC2SecurityGroups :: Maybe [EC2SecurityGroup], _dsgDBSecurityGroupDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBSecurityGroup' smart constructor.
dbSecurityGroup :: DBSecurityGroup
dbSecurityGroup = DBSecurityGroup'{_dsgVPCId = Nothing, _dsgOwnerId = Nothing, _dsgIPRanges = Nothing, _dsgDBSecurityGroupName = Nothing, _dsgEC2SecurityGroups = Nothing, _dsgDBSecurityGroupDescription = Nothing};

-- | Provides the VpcId of the DB security group.
dsgVPCId :: Lens' DBSecurityGroup (Maybe Text)
dsgVPCId = lens _dsgVPCId (\ s a -> s{_dsgVPCId = a});

-- | Provides the AWS ID of the owner of a specific DB security group.
dsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dsgOwnerId = lens _dsgOwnerId (\ s a -> s{_dsgOwnerId = a});

-- | Contains a list of IPRange elements.
dsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dsgIPRanges = lens _dsgIPRanges (\ s a -> s{_dsgIPRanges = a}) . _Default;

-- | Specifies the name of the DB security group.
dsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dsgDBSecurityGroupName = lens _dsgDBSecurityGroupName (\ s a -> s{_dsgDBSecurityGroupName = a});

-- | Contains a list of EC2SecurityGroup elements.
dsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dsgEC2SecurityGroups = lens _dsgEC2SecurityGroups (\ s a -> s{_dsgEC2SecurityGroups = a}) . _Default;

-- | Provides the description of the DB security group.
dsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dsgDBSecurityGroupDescription = lens _dsgDBSecurityGroupDescription (\ s a -> s{_dsgDBSecurityGroupDescription = a});

instance FromXML DBSecurityGroup where
        parseXML x
          = DBSecurityGroup' <$>
              (x .@? "VpcId") <*> (x .@? "OwnerId") <*>
                (x .@? "IPRanges" .!@ mempty >>=
                   may (parseXMLList "IPRange"))
                <*> (x .@? "DBSecurityGroupName")
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "DBSecurityGroupDescription")

-- | This data type is used as a response element in the following actions:
--
-- -   ModifyDBInstance
-- -   RebootDBInstance
-- -   RestoreDBInstanceFromDBSnapshot
-- -   RestoreDBInstanceToPointInTime
--
-- /See:/ 'dbSecurityGroupMembership' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgmStatus'
--
-- * 'dsgmDBSecurityGroupName'
data DBSecurityGroupMembership = DBSecurityGroupMembership'{_dsgmStatus :: Maybe Text, _dsgmDBSecurityGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBSecurityGroupMembership' smart constructor.
dbSecurityGroupMembership :: DBSecurityGroupMembership
dbSecurityGroupMembership = DBSecurityGroupMembership'{_dsgmStatus = Nothing, _dsgmDBSecurityGroupName = Nothing};

-- | The status of the DB security group.
dsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmStatus = lens _dsgmStatus (\ s a -> s{_dsgmStatus = a});

-- | The name of the DB security group.
dsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmDBSecurityGroupName = lens _dsgmDBSecurityGroupName (\ s a -> s{_dsgmDBSecurityGroupName = a});

instance FromXML DBSecurityGroupMembership where
        parseXML x
          = DBSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "DBSecurityGroupName")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBSnapshot
-- -   DeleteDBSnapshot
--
-- This data type is used as a response element in the DescribeDBSnapshots
-- action.
--
-- /See:/ 'dbSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsEngineVersion'
--
-- * 'dsStatus'
--
-- * 'dsMasterUsername'
--
-- * 'dsSourceRegion'
--
-- * 'dsIOPS'
--
-- * 'dsInstanceCreateTime'
--
-- * 'dsVPCId'
--
-- * 'dsEngine'
--
-- * 'dsEncrypted'
--
-- * 'dsDBSnapshotIdentifier'
--
-- * 'dsLicenseModel'
--
-- * 'dsSnapshotType'
--
-- * 'dsDBInstanceIdentifier'
--
-- * 'dsAvailabilityZone'
--
-- * 'dsKMSKeyId'
--
-- * 'dsSnapshotCreateTime'
--
-- * 'dsAllocatedStorage'
--
-- * 'dsTDECredentialARN'
--
-- * 'dsOptionGroupName'
--
-- * 'dsPercentProgress'
--
-- * 'dsPort'
--
-- * 'dsStorageType'
data DBSnapshot = DBSnapshot'{_dsEngineVersion :: Maybe Text, _dsStatus :: Maybe Text, _dsMasterUsername :: Maybe Text, _dsSourceRegion :: Maybe Text, _dsIOPS :: Maybe Int, _dsInstanceCreateTime :: Maybe ISO8601, _dsVPCId :: Maybe Text, _dsEngine :: Maybe Text, _dsEncrypted :: Maybe Bool, _dsDBSnapshotIdentifier :: Maybe Text, _dsLicenseModel :: Maybe Text, _dsSnapshotType :: Maybe Text, _dsDBInstanceIdentifier :: Maybe Text, _dsAvailabilityZone :: Maybe Text, _dsKMSKeyId :: Maybe Text, _dsSnapshotCreateTime :: Maybe ISO8601, _dsAllocatedStorage :: Maybe Int, _dsTDECredentialARN :: Maybe Text, _dsOptionGroupName :: Maybe Text, _dsPercentProgress :: Maybe Int, _dsPort :: Maybe Int, _dsStorageType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBSnapshot' smart constructor.
dbSnapshot :: DBSnapshot
dbSnapshot = DBSnapshot'{_dsEngineVersion = Nothing, _dsStatus = Nothing, _dsMasterUsername = Nothing, _dsSourceRegion = Nothing, _dsIOPS = Nothing, _dsInstanceCreateTime = Nothing, _dsVPCId = Nothing, _dsEngine = Nothing, _dsEncrypted = Nothing, _dsDBSnapshotIdentifier = Nothing, _dsLicenseModel = Nothing, _dsSnapshotType = Nothing, _dsDBInstanceIdentifier = Nothing, _dsAvailabilityZone = Nothing, _dsKMSKeyId = Nothing, _dsSnapshotCreateTime = Nothing, _dsAllocatedStorage = Nothing, _dsTDECredentialARN = Nothing, _dsOptionGroupName = Nothing, _dsPercentProgress = Nothing, _dsPort = Nothing, _dsStorageType = Nothing};

-- | Specifies the version of the database engine.
dsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dsEngineVersion = lens _dsEngineVersion (\ s a -> s{_dsEngineVersion = a});

-- | Specifies the status of this DB snapshot.
dsStatus :: Lens' DBSnapshot (Maybe Text)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a});

-- | Provides the master username for the DB snapshot.
dsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dsMasterUsername = lens _dsMasterUsername (\ s a -> s{_dsMasterUsername = a});

-- | The region that the DB snapshot was created in or copied from.
dsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dsSourceRegion = lens _dsSourceRegion (\ s a -> s{_dsSourceRegion = a});

-- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
dsIOPS :: Lens' DBSnapshot (Maybe Int)
dsIOPS = lens _dsIOPS (\ s a -> s{_dsIOPS = a});

-- | Specifies the time (UTC) when the snapshot was taken.
dsInstanceCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsInstanceCreateTime = lens _dsInstanceCreateTime (\ s a -> s{_dsInstanceCreateTime = a}) . mapping _Time;

-- | Provides the Vpc Id associated with the DB snapshot.
dsVPCId :: Lens' DBSnapshot (Maybe Text)
dsVPCId = lens _dsVPCId (\ s a -> s{_dsVPCId = a});

-- | Specifies the name of the database engine.
dsEngine :: Lens' DBSnapshot (Maybe Text)
dsEngine = lens _dsEngine (\ s a -> s{_dsEngine = a});

-- | Specifies whether the DB snapshot is encrypted.
dsEncrypted :: Lens' DBSnapshot (Maybe Bool)
dsEncrypted = lens _dsEncrypted (\ s a -> s{_dsEncrypted = a});

-- | Specifies the identifier for the DB snapshot.
dsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotIdentifier = lens _dsDBSnapshotIdentifier (\ s a -> s{_dsDBSnapshotIdentifier = a});

-- | License model information for the restored DB instance.
dsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dsLicenseModel = lens _dsLicenseModel (\ s a -> s{_dsLicenseModel = a});

-- | Provides the type of the DB snapshot.
dsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dsSnapshotType = lens _dsSnapshotType (\ s a -> s{_dsSnapshotType = a});

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBInstanceIdentifier = lens _dsDBInstanceIdentifier (\ s a -> s{_dsDBInstanceIdentifier = a});

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dsAvailabilityZone = lens _dsAvailabilityZone (\ s a -> s{_dsAvailabilityZone = a});

-- | If @Encrypted@ is true, the KMS key identifier for the encrypted DB
-- snapshot.
dsKMSKeyId :: Lens' DBSnapshot (Maybe Text)
dsKMSKeyId = lens _dsKMSKeyId (\ s a -> s{_dsKMSKeyId = a});

-- | Provides the time (UTC) when the snapshot was taken.
dsSnapshotCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsSnapshotCreateTime = lens _dsSnapshotCreateTime (\ s a -> s{_dsSnapshotCreateTime = a}) . mapping _Time;

-- | Specifies the allocated storage size in gigabytes (GB).
dsAllocatedStorage :: Lens' DBSnapshot (Maybe Int)
dsAllocatedStorage = lens _dsAllocatedStorage (\ s a -> s{_dsAllocatedStorage = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
dsTDECredentialARN :: Lens' DBSnapshot (Maybe Text)
dsTDECredentialARN = lens _dsTDECredentialARN (\ s a -> s{_dsTDECredentialARN = a});

-- | Provides the option group name for the DB snapshot.
dsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dsOptionGroupName = lens _dsOptionGroupName (\ s a -> s{_dsOptionGroupName = a});

-- | The percentage of the estimated data that has been transferred.
dsPercentProgress :: Lens' DBSnapshot (Maybe Int)
dsPercentProgress = lens _dsPercentProgress (\ s a -> s{_dsPercentProgress = a});

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dsPort :: Lens' DBSnapshot (Maybe Int)
dsPort = lens _dsPort (\ s a -> s{_dsPort = a});

-- | Specifies the storage type associated with DB Snapshot.
dsStorageType :: Lens' DBSnapshot (Maybe Text)
dsStorageType = lens _dsStorageType (\ s a -> s{_dsStorageType = a});

instance FromXML DBSnapshot where
        parseXML x
          = DBSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "MasterUsername")
                <*> (x .@? "SourceRegion")
                <*> (x .@? "Iops")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "VpcId")
                <*> (x .@? "Engine")
                <*> (x .@? "Encrypted")
                <*> (x .@? "DBSnapshotIdentifier")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "SnapshotType")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "AvailabilityZone")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "OptionGroupName")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBSubnetGroup
-- -   ModifyDBSubnetGroup
-- -   DescribeDBSubnetGroups
-- -   DeleteDBSubnetGroup
--
-- This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'dbSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsDBSubnetGroupName'
--
-- * 'dbsVPCId'
--
-- * 'dbsSubnets'
--
-- * 'dbsDBSubnetGroupDescription'
--
-- * 'dbsSubnetGroupStatus'
data DBSubnetGroup = DBSubnetGroup'{_dbsDBSubnetGroupName :: Maybe Text, _dbsVPCId :: Maybe Text, _dbsSubnets :: Maybe [Subnet], _dbsDBSubnetGroupDescription :: Maybe Text, _dbsSubnetGroupStatus :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DBSubnetGroup' smart constructor.
dbSubnetGroup :: DBSubnetGroup
dbSubnetGroup = DBSubnetGroup'{_dbsDBSubnetGroupName = Nothing, _dbsVPCId = Nothing, _dbsSubnets = Nothing, _dbsDBSubnetGroupDescription = Nothing, _dbsSubnetGroupStatus = Nothing};

-- | Specifies the name of the DB subnet group.
dbsDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dbsDBSubnetGroupName = lens _dbsDBSubnetGroupName (\ s a -> s{_dbsDBSubnetGroupName = a});

-- | Provides the VpcId of the DB subnet group.
dbsVPCId :: Lens' DBSubnetGroup (Maybe Text)
dbsVPCId = lens _dbsVPCId (\ s a -> s{_dbsVPCId = a});

-- | Contains a list of Subnet elements.
dbsSubnets :: Lens' DBSubnetGroup [Subnet]
dbsSubnets = lens _dbsSubnets (\ s a -> s{_dbsSubnets = a}) . _Default;

-- | Provides the description of the DB subnet group.
dbsDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dbsDBSubnetGroupDescription = lens _dbsDBSubnetGroupDescription (\ s a -> s{_dbsDBSubnetGroupDescription = a});

-- | Provides the status of the DB subnet group.
dbsSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dbsSubnetGroupStatus = lens _dbsSubnetGroupStatus (\ s a -> s{_dbsSubnetGroupStatus = a});

instance FromXML DBSubnetGroup where
        parseXML x
          = DBSubnetGroup' <$>
              (x .@? "DBSubnetGroupName") <*> (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "DBSubnetGroupDescription")
                <*> (x .@? "SubnetGroupStatus")

-- | This data type is used as a response element to DescribeDBLogFiles.
--
-- /See:/ 'describeDBLogFilesDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddlfdLastWritten'
--
-- * 'ddlfdSize'
--
-- * 'ddlfdLogFileName'
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'{_ddlfdLastWritten :: Maybe Integer, _ddlfdSize :: Maybe Integer, _ddlfdLogFileName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBLogFilesDetails' smart constructor.
describeDBLogFilesDetails :: DescribeDBLogFilesDetails
describeDBLogFilesDetails = DescribeDBLogFilesDetails'{_ddlfdLastWritten = Nothing, _ddlfdSize = Nothing, _ddlfdLogFileName = Nothing};

-- | A POSIX timestamp when the last log entry was written.
ddlfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdLastWritten = lens _ddlfdLastWritten (\ s a -> s{_ddlfdLastWritten = a});

-- | The size, in bytes, of the log file for the specified DB instance.
ddlfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdSize = lens _ddlfdSize (\ s a -> s{_ddlfdSize = a});

-- | The name of the log file for the specified DB instance.
ddlfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddlfdLogFileName = lens _ddlfdLogFileName (\ s a -> s{_ddlfdLogFileName = a});

instance FromXML DescribeDBLogFilesDetails where
        parseXML x
          = DescribeDBLogFilesDetails' <$>
              (x .@? "LastWritten") <*> (x .@? "Size") <*>
                (x .@? "LogFileName")

-- | This data type is used as a response element in the following actions:
--
-- -   AuthorizeDBSecurityGroupIngress
-- -   DescribeDBSecurityGroups
-- -   RevokeDBSecurityGroupIngress
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esgStatus'
--
-- * 'esgEC2SecurityGroupOwnerId'
--
-- * 'esgEC2SecurityGroupName'
--
-- * 'esgEC2SecurityGroupId'
data EC2SecurityGroup = EC2SecurityGroup'{_esgStatus :: Maybe Text, _esgEC2SecurityGroupOwnerId :: Maybe Text, _esgEC2SecurityGroupName :: Maybe Text, _esgEC2SecurityGroupId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EC2SecurityGroup' smart constructor.
ec2SecurityGroup :: EC2SecurityGroup
ec2SecurityGroup = EC2SecurityGroup'{_esgStatus = Nothing, _esgEC2SecurityGroupOwnerId = Nothing, _esgEC2SecurityGroupName = Nothing, _esgEC2SecurityGroupId = Nothing};

-- | Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a});

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the @EC2SecurityGroupName@ field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a});

-- | Specifies the name of the EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a});

-- | Specifies the id of the EC2 security group.
esgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupId = lens _esgEC2SecurityGroupId (\ s a -> s{_esgEC2SecurityGroupId = a});

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")
                <*> (x .@? "EC2SecurityGroupId")

-- | This data type is used as a response element in the following actions:
--
-- -   CreateDBInstance
-- -   DescribeDBInstances
-- -   DeleteDBInstance
--
-- /See:/ 'endpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'endAddress'
--
-- * 'endPort'
data Endpoint = Endpoint'{_endAddress :: Maybe Text, _endPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'Endpoint' smart constructor.
endpoint :: Endpoint
endpoint = Endpoint'{_endAddress = Nothing, _endPort = Nothing};

-- | Specifies the DNS address of the DB instance.
endAddress :: Lens' Endpoint (Maybe Text)
endAddress = lens _endAddress (\ s a -> s{_endAddress = a});

-- | Specifies the port that the database engine is listening on.
endPort :: Lens' Endpoint (Maybe Int)
endPort = lens _endPort (\ s a -> s{_endPort = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$> (x .@? "Address") <*> (x .@? "Port")

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
--
-- /See:/ 'engineDefaults' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edDBParameterGroupFamily'
--
-- * 'edParameters'
--
-- * 'edMarker'
data EngineDefaults = EngineDefaults'{_edDBParameterGroupFamily :: Maybe Text, _edParameters :: Maybe [Parameter], _edMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EngineDefaults' smart constructor.
engineDefaults :: EngineDefaults
engineDefaults = EngineDefaults'{_edDBParameterGroupFamily = Nothing, _edParameters = Nothing, _edMarker = Nothing};

-- | Specifies the name of the DB parameter group family which the engine
-- default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\ s a -> s{_edDBParameterGroupFamily = a});

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default;

-- | An optional pagination token provided by a previous EngineDefaults
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a});

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "DBParameterGroupFamily") <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))
                <*> (x .@? "Marker")

-- | This data type is used as a response element in the DescribeEvents
-- action.
--
-- /See:/ 'event' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eveSourceType'
--
-- * 'eveSourceIdentifier'
--
-- * 'eveDate'
--
-- * 'eveEventCategories'
--
-- * 'eveMessage'
data Event = Event'{_eveSourceType :: Maybe SourceType, _eveSourceIdentifier :: Maybe Text, _eveDate :: Maybe ISO8601, _eveEventCategories :: Maybe [Text], _eveMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Event' smart constructor.
event :: Event
event = Event'{_eveSourceType = Nothing, _eveSourceIdentifier = Nothing, _eveDate = Nothing, _eveEventCategories = Nothing, _eveMessage = Nothing};

-- | Specifies the source type for this event.
eveSourceType :: Lens' Event (Maybe SourceType)
eveSourceType = lens _eveSourceType (\ s a -> s{_eveSourceType = a});

-- | Provides the identifier for the source of the event.
eveSourceIdentifier :: Lens' Event (Maybe Text)
eveSourceIdentifier = lens _eveSourceIdentifier (\ s a -> s{_eveSourceIdentifier = a});

-- | Specifies the date and time of the event.
eveDate :: Lens' Event (Maybe UTCTime)
eveDate = lens _eveDate (\ s a -> s{_eveDate = a}) . mapping _Time;

-- | Specifies the category for the event.
eveEventCategories :: Lens' Event [Text]
eveEventCategories = lens _eveEventCategories (\ s a -> s{_eveEventCategories = a}) . _Default;

-- | Provides the text of this event.
eveMessage :: Lens' Event (Maybe Text)
eveMessage = lens _eveMessage (\ s a -> s{_eveMessage = a});

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "Message")

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
--
-- /See:/ 'eventCategoriesMap' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmSourceType'
--
-- * 'ecmEventCategories'
data EventCategoriesMap = EventCategoriesMap'{_ecmSourceType :: Maybe Text, _ecmEventCategories :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'EventCategoriesMap' smart constructor.
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap = EventCategoriesMap'{_ecmSourceType = Nothing, _ecmEventCategories = Nothing};

-- | The source type that the returned categories belong to
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a});

-- | The event categories for the specified source type
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories = lens _ecmEventCategories (\ s a -> s{_ecmEventCategories = a}) . _Default;

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
--
-- /See:/ 'eventSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esCustomerAWSId'
--
-- * 'esStatus'
--
-- * 'esCustSubscriptionId'
--
-- * 'esSNSTopicARN'
--
-- * 'esEnabled'
--
-- * 'esSourceType'
--
-- * 'esSubscriptionCreationTime'
--
-- * 'esEventCategoriesList'
--
-- * 'esSourceIdsList'
data EventSubscription = EventSubscription'{_esCustomerAWSId :: Maybe Text, _esStatus :: Maybe Text, _esCustSubscriptionId :: Maybe Text, _esSNSTopicARN :: Maybe Text, _esEnabled :: Maybe Bool, _esSourceType :: Maybe Text, _esSubscriptionCreationTime :: Maybe Text, _esEventCategoriesList :: Maybe [Text], _esSourceIdsList :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'EventSubscription' smart constructor.
eventSubscription :: EventSubscription
eventSubscription = EventSubscription'{_esCustomerAWSId = Nothing, _esStatus = Nothing, _esCustSubscriptionId = Nothing, _esSNSTopicARN = Nothing, _esEnabled = Nothing, _esSourceType = Nothing, _esSubscriptionCreationTime = Nothing, _esEventCategoriesList = Nothing, _esSourceIdsList = Nothing};

-- | The AWS customer account associated with the RDS event notification
-- subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a});

-- | The status of the RDS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that RDS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a});

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a});

-- | The topic ARN of the RDS event notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a});

-- | A Boolean value indicating if the subscription is enabled. True
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a});

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a});

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a});

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default;

-- | A list of source IDs for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default;

instance FromXML EventSubscription where
        parseXML x
          = EventSubscription' <$>
              (x .@? "CustomerAwsId") <*> (x .@? "Status") <*>
                (x .@? "CustSubscriptionId")
                <*> (x .@? "SnsTopicArn")
                <*> (x .@? "Enabled")
                <*> (x .@? "SourceType")
                <*> (x .@? "SubscriptionCreationTime")
                <*>
                (x .@? "EventCategoriesList" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*>
                (x .@? "SourceIdsList" .!@ mempty >>=
                   may (parseXMLList "SourceId"))

-- | /See:/ 'filter'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'filName'
--
-- * 'filValues'
data Filter = Filter'{_filName :: Text, _filValues :: [Text]} deriving (Eq, Read, Show)

-- | 'Filter' smart constructor.
filter' :: Text -> Filter
filter' pName = Filter'{_filName = pName, _filValues = mempty};

-- | This parameter is not currently supported.
filName :: Lens' Filter Text
filName = lens _filName (\ s a -> s{_filName = a});

-- | This parameter is not currently supported.
filValues :: Lens' Filter [Text]
filValues = lens _filValues (\ s a -> s{_filValues = a});

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Name" =: _filName,
               "Values" =: toQueryList "Value" _filValues]

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'ipRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irStatus'
--
-- * 'irCIDRIP'
data IPRange = IPRange'{_irStatus :: Maybe Text, _irCIDRIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'IPRange' smart constructor.
ipRange :: IPRange
ipRange = IPRange'{_irStatus = Nothing, _irCIDRIP = Nothing};

-- | Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\ s a -> s{_irStatus = a});

-- | Specifies the IP range.
irCIDRIP :: Lens' IPRange (Maybe Text)
irCIDRIP = lens _irCIDRIP (\ s a -> s{_irCIDRIP = a});

instance FromXML IPRange where
        parseXML x
          = IPRange' <$> (x .@? "Status") <*> (x .@? "CIDRIP")

-- | Option details.
--
-- /See:/ 'option' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'optOptionName'
--
-- * 'optPermanent'
--
-- * 'optPersistent'
--
-- * 'optOptionDescription'
--
-- * 'optOptionSettings'
--
-- * 'optVPCSecurityGroupMemberships'
--
-- * 'optDBSecurityGroupMemberships'
--
-- * 'optPort'
data Option = Option'{_optOptionName :: Maybe Text, _optPermanent :: Maybe Bool, _optPersistent :: Maybe Bool, _optOptionDescription :: Maybe Text, _optOptionSettings :: Maybe [OptionSetting], _optVPCSecurityGroupMemberships :: Maybe [VPCSecurityGroupMembership], _optDBSecurityGroupMemberships :: Maybe [DBSecurityGroupMembership], _optPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'Option' smart constructor.
option :: Option
option = Option'{_optOptionName = Nothing, _optPermanent = Nothing, _optPersistent = Nothing, _optOptionDescription = Nothing, _optOptionSettings = Nothing, _optVPCSecurityGroupMemberships = Nothing, _optDBSecurityGroupMemberships = Nothing, _optPort = Nothing};

-- | The name of the option.
optOptionName :: Lens' Option (Maybe Text)
optOptionName = lens _optOptionName (\ s a -> s{_optOptionName = a});

-- | Indicate if this option is permanent.
optPermanent :: Lens' Option (Maybe Bool)
optPermanent = lens _optPermanent (\ s a -> s{_optPermanent = a});

-- | Indicate if this option is persistent.
optPersistent :: Lens' Option (Maybe Bool)
optPersistent = lens _optPersistent (\ s a -> s{_optPersistent = a});

-- | The description of the option.
optOptionDescription :: Lens' Option (Maybe Text)
optOptionDescription = lens _optOptionDescription (\ s a -> s{_optOptionDescription = a});

-- | The option settings for this option.
optOptionSettings :: Lens' Option [OptionSetting]
optOptionSettings = lens _optOptionSettings (\ s a -> s{_optOptionSettings = a}) . _Default;

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
optVPCSecurityGroupMemberships :: Lens' Option [VPCSecurityGroupMembership]
optVPCSecurityGroupMemberships = lens _optVPCSecurityGroupMemberships (\ s a -> s{_optVPCSecurityGroupMemberships = a}) . _Default;

-- | If the option requires access to a port, then this DB security group
-- allows access to the port.
optDBSecurityGroupMemberships :: Lens' Option [DBSecurityGroupMembership]
optDBSecurityGroupMemberships = lens _optDBSecurityGroupMemberships (\ s a -> s{_optDBSecurityGroupMemberships = a}) . _Default;

-- | If required, the port configured for this option to use.
optPort :: Lens' Option (Maybe Int)
optPort = lens _optPort (\ s a -> s{_optPort = a});

instance FromXML Option where
        parseXML x
          = Option' <$>
              (x .@? "OptionName") <*> (x .@? "Permanent") <*>
                (x .@? "Persistent")
                <*> (x .@? "OptionDescription")
                <*>
                (x .@? "OptionSettings" .!@ mempty >>=
                   may (parseXMLList "OptionSetting"))
                <*>
                (x .@? "VpcSecurityGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*>
                (x .@? "DBSecurityGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "DBSecurityGroup"))
                <*> (x .@? "Port")

-- | A list of all available options
--
-- /See:/ 'optionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ocOptionSettings'
--
-- * 'ocVPCSecurityGroupMemberships'
--
-- * 'ocDBSecurityGroupMemberships'
--
-- * 'ocPort'
--
-- * 'ocOptionName'
data OptionConfiguration = OptionConfiguration'{_ocOptionSettings :: Maybe [OptionSetting], _ocVPCSecurityGroupMemberships :: Maybe [Text], _ocDBSecurityGroupMemberships :: Maybe [Text], _ocPort :: Maybe Int, _ocOptionName :: Text} deriving (Eq, Read, Show)

-- | 'OptionConfiguration' smart constructor.
optionConfiguration :: Text -> OptionConfiguration
optionConfiguration pOptionName = OptionConfiguration'{_ocOptionSettings = Nothing, _ocVPCSecurityGroupMemberships = Nothing, _ocDBSecurityGroupMemberships = Nothing, _ocPort = Nothing, _ocOptionName = pOptionName};

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings = lens _ocOptionSettings (\ s a -> s{_ocOptionSettings = a}) . _Default;

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVPCSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVPCSecurityGroupMemberships = lens _ocVPCSecurityGroupMemberships (\ s a -> s{_ocVPCSecurityGroupMemberships = a}) . _Default;

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships = lens _ocDBSecurityGroupMemberships (\ s a -> s{_ocDBSecurityGroupMemberships = a}) . _Default;

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Int)
ocPort = lens _ocPort (\ s a -> s{_ocPort = a});

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\ s a -> s{_ocOptionName = a});

instance ToQuery OptionConfiguration where
        toQuery OptionConfiguration'{..}
          = mconcat
              ["OptionSettings" =:
                 toQuery
                   (toQueryList "OptionSetting" <$> _ocOptionSettings),
               "VpcSecurityGroupMemberships" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _ocVPCSecurityGroupMemberships),
               "DBSecurityGroupMemberships" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _ocDBSecurityGroupMemberships),
               "Port" =: _ocPort, "OptionName" =: _ocOptionName]

-- |
--
-- /See:/ 'optionGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogOptionGroupDescription'
--
-- * 'ogVPCId'
--
-- * 'ogAllowsVPCAndNonVPCInstanceMemberships'
--
-- * 'ogEngineName'
--
-- * 'ogMajorEngineVersion'
--
-- * 'ogOptions'
--
-- * 'ogOptionGroupName'
data OptionGroup = OptionGroup'{_ogOptionGroupDescription :: Maybe Text, _ogVPCId :: Maybe Text, _ogAllowsVPCAndNonVPCInstanceMemberships :: Maybe Bool, _ogEngineName :: Maybe Text, _ogMajorEngineVersion :: Maybe Text, _ogOptions :: Maybe [Option], _ogOptionGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionGroup' smart constructor.
optionGroup :: OptionGroup
optionGroup = OptionGroup'{_ogOptionGroupDescription = Nothing, _ogVPCId = Nothing, _ogAllowsVPCAndNonVPCInstanceMemberships = Nothing, _ogEngineName = Nothing, _ogMajorEngineVersion = Nothing, _ogOptions = Nothing, _ogOptionGroupName = Nothing};

-- | Provides a description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription = lens _ogOptionGroupDescription (\ s a -> s{_ogOptionGroupDescription = a});

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
-- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
-- field is blank, then this option group can be applied to both VPC and
-- non-VPC instances. If this field contains a value, then this option
-- group can only be applied to instances that are in the VPC indicated by
-- this field.
ogVPCId :: Lens' OptionGroup (Maybe Text)
ogVPCId = lens _ogVPCId (\ s a -> s{_ogVPCId = a});

-- | Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value @true@ indicates the option group can be
-- applied to both VPC and non-VPC instances.
ogAllowsVPCAndNonVPCInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVPCAndNonVPCInstanceMemberships = lens _ogAllowsVPCAndNonVPCInstanceMemberships (\ s a -> s{_ogAllowsVPCAndNonVPCInstanceMemberships = a});

-- | Engine name that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\ s a -> s{_ogEngineName = a});

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion = lens _ogMajorEngineVersion (\ s a -> s{_ogMajorEngineVersion = a});

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\ s a -> s{_ogOptions = a}) . _Default;

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName = lens _ogOptionGroupName (\ s a -> s{_ogOptionGroupName = a});

instance FromXML OptionGroup where
        parseXML x
          = OptionGroup' <$>
              (x .@? "OptionGroupDescription") <*> (x .@? "VpcId")
                <*> (x .@? "AllowsVpcAndNonVpcInstanceMemberships")
                <*> (x .@? "EngineName")
                <*> (x .@? "MajorEngineVersion")
                <*>
                (x .@? "Options" .!@ mempty >>=
                   may (parseXMLList "Option"))
                <*> (x .@? "OptionGroupName")

-- | Provides information on the option groups the DB instance is a member
-- of.
--
-- /See:/ 'optionGroupMembership' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogmStatus'
--
-- * 'ogmOptionGroupName'
data OptionGroupMembership = OptionGroupMembership'{_ogmStatus :: Maybe Text, _ogmOptionGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionGroupMembership' smart constructor.
optionGroupMembership :: OptionGroupMembership
optionGroupMembership = OptionGroupMembership'{_ogmStatus = Nothing, _ogmOptionGroupName = Nothing};

-- | The status of the DB instance\'s option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\ s a -> s{_ogmStatus = a});

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName = lens _ogmOptionGroupName (\ s a -> s{_ogmOptionGroupName = a});

instance FromXML OptionGroupMembership where
        parseXML x
          = OptionGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "OptionGroupName")

-- | Available option.
--
-- /See:/ 'optionGroupOption' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogoMinimumRequiredMinorEngineVersion'
--
-- * 'ogoPermanent'
--
-- * 'ogoPersistent'
--
-- * 'ogoEngineName'
--
-- * 'ogoName'
--
-- * 'ogoMajorEngineVersion'
--
-- * 'ogoDefaultPort'
--
-- * 'ogoOptionGroupOptionSettings'
--
-- * 'ogoPortRequired'
--
-- * 'ogoOptionsDependedOn'
--
-- * 'ogoDescription'
data OptionGroupOption = OptionGroupOption'{_ogoMinimumRequiredMinorEngineVersion :: Maybe Text, _ogoPermanent :: Maybe Bool, _ogoPersistent :: Maybe Bool, _ogoEngineName :: Maybe Text, _ogoName :: Maybe Text, _ogoMajorEngineVersion :: Maybe Text, _ogoDefaultPort :: Maybe Int, _ogoOptionGroupOptionSettings :: Maybe [OptionGroupOptionSetting], _ogoPortRequired :: Maybe Bool, _ogoOptionsDependedOn :: Maybe [Text], _ogoDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionGroupOption' smart constructor.
optionGroupOption :: OptionGroupOption
optionGroupOption = OptionGroupOption'{_ogoMinimumRequiredMinorEngineVersion = Nothing, _ogoPermanent = Nothing, _ogoPersistent = Nothing, _ogoEngineName = Nothing, _ogoName = Nothing, _ogoMajorEngineVersion = Nothing, _ogoDefaultPort = Nothing, _ogoOptionGroupOptionSettings = Nothing, _ogoPortRequired = Nothing, _ogoOptionsDependedOn = Nothing, _ogoDescription = Nothing};

-- | The minimum required engine version for the option to be applied.
ogoMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMinimumRequiredMinorEngineVersion = lens _ogoMinimumRequiredMinorEngineVersion (\ s a -> s{_ogoMinimumRequiredMinorEngineVersion = a});

-- | A permanent option cannot be removed from the option group once the
-- option group is used, and it cannot be removed from the db instance
-- after assigning an option group with this permanent option.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\ s a -> s{_ogoPermanent = a});

-- | A persistent option cannot be removed from the option group once the
-- option group is used, but this option can be removed from the db
-- instance while modifying the related data and assigning another option
-- group without this option.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\ s a -> s{_ogoPersistent = a});

-- | The name of the engine that this option can be applied to.
ogoEngineName :: Lens' OptionGroupOption (Maybe Text)
ogoEngineName = lens _ogoEngineName (\ s a -> s{_ogoEngineName = a});

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\ s a -> s{_ogoName = a});

-- | Indicates the major engine version that the option is available for.
ogoMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMajorEngineVersion = lens _ogoMajorEngineVersion (\ s a -> s{_ogoMajorEngineVersion = a});

-- | If the option requires a port, specifies the default port for the
-- option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Int)
ogoDefaultPort = lens _ogoDefaultPort (\ s a -> s{_ogoDefaultPort = a});

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings = lens _ogoOptionGroupOptionSettings (\ s a -> s{_ogoOptionGroupOptionSettings = a}) . _Default;

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\ s a -> s{_ogoPortRequired = a});

-- | List of all options that are prerequisites for this option.
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn = lens _ogoOptionsDependedOn (\ s a -> s{_ogoOptionsDependedOn = a}) . _Default;

-- | The description of the option.
ogoDescription :: Lens' OptionGroupOption (Maybe Text)
ogoDescription = lens _ogoDescription (\ s a -> s{_ogoDescription = a});

instance FromXML OptionGroupOption where
        parseXML x
          = OptionGroupOption' <$>
              (x .@? "MinimumRequiredMinorEngineVersion") <*>
                (x .@? "Permanent")
                <*> (x .@? "Persistent")
                <*> (x .@? "EngineName")
                <*> (x .@? "Name")
                <*> (x .@? "MajorEngineVersion")
                <*> (x .@? "DefaultPort")
                <*>
                (x .@? "OptionGroupOptionSettings" .!@ mempty >>=
                   may (parseXMLList "OptionGroupOptionSetting"))
                <*> (x .@? "PortRequired")
                <*>
                (x .@? "OptionsDependedOn" .!@ mempty >>=
                   may (parseXMLList "OptionName"))
                <*> (x .@? "Description")

-- | Option group option settings are used to display settings available for
-- each option with their default values and other information. These
-- values are used with the DescribeOptionGroupOptions action.
--
-- /See:/ 'optionGroupOptionSetting' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogosApplyType'
--
-- * 'ogosSettingName'
--
-- * 'ogosDefaultValue'
--
-- * 'ogosIsModifiable'
--
-- * 'ogosAllowedValues'
--
-- * 'ogosSettingDescription'
data OptionGroupOptionSetting = OptionGroupOptionSetting'{_ogosApplyType :: Maybe Text, _ogosSettingName :: Maybe Text, _ogosDefaultValue :: Maybe Text, _ogosIsModifiable :: Maybe Bool, _ogosAllowedValues :: Maybe Text, _ogosSettingDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionGroupOptionSetting' smart constructor.
optionGroupOptionSetting :: OptionGroupOptionSetting
optionGroupOptionSetting = OptionGroupOptionSetting'{_ogosApplyType = Nothing, _ogosSettingName = Nothing, _ogosDefaultValue = Nothing, _ogosIsModifiable = Nothing, _ogosAllowedValues = Nothing, _ogosSettingDescription = Nothing};

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\ s a -> s{_ogosApplyType = a});

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\ s a -> s{_ogosSettingName = a});

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\ s a -> s{_ogosDefaultValue = a});

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\ s a -> s{_ogosIsModifiable = a});

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues = lens _ogosAllowedValues (\ s a -> s{_ogosAllowedValues = a});

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription = lens _ogosSettingDescription (\ s a -> s{_ogosSettingDescription = a});

instance FromXML OptionGroupOptionSetting where
        parseXML x
          = OptionGroupOptionSetting' <$>
              (x .@? "ApplyType") <*> (x .@? "SettingName") <*>
                (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "SettingDescription")

-- | Option settings are the actual settings being applied or configured for
-- that option. It is used when you modify an option group or describe
-- option groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a
-- setting called SQLNET.ENCRYPTION_SERVER that can have several different
-- values.
--
-- /See:/ 'optionSetting' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osIsCollection'
--
-- * 'osApplyType'
--
-- * 'osValue'
--
-- * 'osName'
--
-- * 'osDefaultValue'
--
-- * 'osIsModifiable'
--
-- * 'osAllowedValues'
--
-- * 'osDataType'
--
-- * 'osDescription'
data OptionSetting = OptionSetting'{_osIsCollection :: Maybe Bool, _osApplyType :: Maybe Text, _osValue :: Maybe Text, _osName :: Maybe Text, _osDefaultValue :: Maybe Text, _osIsModifiable :: Maybe Bool, _osAllowedValues :: Maybe Text, _osDataType :: Maybe Text, _osDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionSetting' smart constructor.
optionSetting :: OptionSetting
optionSetting = OptionSetting'{_osIsCollection = Nothing, _osApplyType = Nothing, _osValue = Nothing, _osName = Nothing, _osDefaultValue = Nothing, _osIsModifiable = Nothing, _osAllowedValues = Nothing, _osDataType = Nothing, _osDescription = Nothing};

-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection = lens _osIsCollection (\ s a -> s{_osIsCollection = a});

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\ s a -> s{_osApplyType = a});

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\ s a -> s{_osValue = a});

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName = lens _osName (\ s a -> s{_osName = a});

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\ s a -> s{_osDefaultValue = a});

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\ s a -> s{_osIsModifiable = a});

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\ s a -> s{_osAllowedValues = a});

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\ s a -> s{_osDataType = a});

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\ s a -> s{_osDescription = a});

instance FromXML OptionSetting where
        parseXML x
          = OptionSetting' <$>
              (x .@? "IsCollection") <*> (x .@? "ApplyType") <*>
                (x .@? "Value")
                <*> (x .@? "Name")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "DataType")
                <*> (x .@? "Description")

instance ToQuery OptionSetting where
        toQuery OptionSetting'{..}
          = mconcat
              ["IsCollection" =: _osIsCollection,
               "ApplyType" =: _osApplyType, "Value" =: _osValue,
               "Name" =: _osName, "DefaultValue" =: _osDefaultValue,
               "IsModifiable" =: _osIsModifiable,
               "AllowedValues" =: _osAllowedValues,
               "DataType" =: _osDataType,
               "Description" =: _osDescription]

-- | Contains a list of available options for a DB instance
--
-- This data type is used as a response element in the
-- DescribeOrderableDBInstanceOptions action.
--
-- /See:/ 'orderableDBInstanceOption' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'odioEngineVersion'
--
-- * 'odioMultiAZCapable'
--
-- * 'odioEngine'
--
-- * 'odioSupportsIOPS'
--
-- * 'odioDBInstanceClass'
--
-- * 'odioLicenseModel'
--
-- * 'odioAvailabilityZones'
--
-- * 'odioReadReplicaCapable'
--
-- * 'odioSupportsStorageEncryption'
--
-- * 'odioVPC'
--
-- * 'odioStorageType'
data OrderableDBInstanceOption = OrderableDBInstanceOption'{_odioEngineVersion :: Maybe Text, _odioMultiAZCapable :: Maybe Bool, _odioEngine :: Maybe Text, _odioSupportsIOPS :: Maybe Bool, _odioDBInstanceClass :: Maybe Text, _odioLicenseModel :: Maybe Text, _odioAvailabilityZones :: Maybe [AvailabilityZone], _odioReadReplicaCapable :: Maybe Bool, _odioSupportsStorageEncryption :: Maybe Bool, _odioVPC :: Maybe Bool, _odioStorageType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OrderableDBInstanceOption' smart constructor.
orderableDBInstanceOption :: OrderableDBInstanceOption
orderableDBInstanceOption = OrderableDBInstanceOption'{_odioEngineVersion = Nothing, _odioMultiAZCapable = Nothing, _odioEngine = Nothing, _odioSupportsIOPS = Nothing, _odioDBInstanceClass = Nothing, _odioLicenseModel = Nothing, _odioAvailabilityZones = Nothing, _odioReadReplicaCapable = Nothing, _odioSupportsStorageEncryption = Nothing, _odioVPC = Nothing, _odioStorageType = Nothing};

-- | The engine version of the orderable DB instance.
odioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngineVersion = lens _odioEngineVersion (\ s a -> s{_odioEngineVersion = a});

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioMultiAZCapable = lens _odioMultiAZCapable (\ s a -> s{_odioMultiAZCapable = a});

-- | The engine type of the orderable DB instance.
odioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngine = lens _odioEngine (\ s a -> s{_odioEngine = a});

-- | Indicates whether this orderable DB instance supports provisioned IOPS.
odioSupportsIOPS :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIOPS = lens _odioSupportsIOPS (\ s a -> s{_odioSupportsIOPS = a});

-- | The DB instance Class for the orderable DB instance
odioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odioDBInstanceClass = lens _odioDBInstanceClass (\ s a -> s{_odioDBInstanceClass = a});

-- | The license model for the orderable DB instance.
odioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odioLicenseModel = lens _odioLicenseModel (\ s a -> s{_odioLicenseModel = a});

-- | A list of availability zones for the orderable DB instance.
odioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odioAvailabilityZones = lens _odioAvailabilityZones (\ s a -> s{_odioAvailabilityZones = a}) . _Default;

-- | Indicates whether this orderable DB instance can have a Read Replica.
odioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioReadReplicaCapable = lens _odioReadReplicaCapable (\ s a -> s{_odioReadReplicaCapable = a});

-- | Indicates whether this orderable DB instance supports encrypted storage.
odioSupportsStorageEncryption :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsStorageEncryption = lens _odioSupportsStorageEncryption (\ s a -> s{_odioSupportsStorageEncryption = a});

-- | Indicates whether this is a VPC orderable DB instance.
odioVPC :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioVPC = lens _odioVPC (\ s a -> s{_odioVPC = a});

-- | The storage type for this orderable DB instance.
odioStorageType :: Lens' OrderableDBInstanceOption (Maybe Text)
odioStorageType = lens _odioStorageType (\ s a -> s{_odioStorageType = a});

instance FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' <$>
              (x .@? "EngineVersion") <*> (x .@? "MultiAZCapable")
                <*> (x .@? "Engine")
                <*> (x .@? "SupportsIops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "ReadReplicaCapable")
                <*> (x .@? "SupportsStorageEncryption")
                <*> (x .@? "Vpc")
                <*> (x .@? "StorageType")

-- | This data type is used as a request parameter in the
-- ModifyDBParameterGroup and ResetDBParameterGroup actions.
--
-- This data type is used as a response element in the
-- DescribeEngineDefaultParameters and DescribeDBParameters actions.
--
-- /See:/ 'parameter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parApplyType'
--
-- * 'parParameterValue'
--
-- * 'parApplyMethod'
--
-- * 'parMinimumEngineVersion'
--
-- * 'parSource'
--
-- * 'parIsModifiable'
--
-- * 'parAllowedValues'
--
-- * 'parDataType'
--
-- * 'parParameterName'
--
-- * 'parDescription'
data Parameter = Parameter'{_parApplyType :: Maybe Text, _parParameterValue :: Maybe Text, _parApplyMethod :: Maybe ApplyMethod, _parMinimumEngineVersion :: Maybe Text, _parSource :: Maybe Text, _parIsModifiable :: Maybe Bool, _parAllowedValues :: Maybe Text, _parDataType :: Maybe Text, _parParameterName :: Maybe Text, _parDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Parameter' smart constructor.
parameter :: Parameter
parameter = Parameter'{_parApplyType = Nothing, _parParameterValue = Nothing, _parApplyMethod = Nothing, _parMinimumEngineVersion = Nothing, _parSource = Nothing, _parIsModifiable = Nothing, _parAllowedValues = Nothing, _parDataType = Nothing, _parParameterName = Nothing, _parDescription = Nothing};

-- | Specifies the engine specific parameters type.
parApplyType :: Lens' Parameter (Maybe Text)
parApplyType = lens _parApplyType (\ s a -> s{_parApplyType = a});

-- | Specifies the value of the parameter.
parParameterValue :: Lens' Parameter (Maybe Text)
parParameterValue = lens _parParameterValue (\ s a -> s{_parParameterValue = a});

-- | Indicates when to apply parameter updates.
parApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
parApplyMethod = lens _parApplyMethod (\ s a -> s{_parApplyMethod = a});

-- | The earliest engine version to which the parameter can apply.
parMinimumEngineVersion :: Lens' Parameter (Maybe Text)
parMinimumEngineVersion = lens _parMinimumEngineVersion (\ s a -> s{_parMinimumEngineVersion = a});

-- | Indicates the source of the parameter value.
parSource :: Lens' Parameter (Maybe Text)
parSource = lens _parSource (\ s a -> s{_parSource = a});

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parIsModifiable :: Lens' Parameter (Maybe Bool)
parIsModifiable = lens _parIsModifiable (\ s a -> s{_parIsModifiable = a});

-- | Specifies the valid range of values for the parameter.
parAllowedValues :: Lens' Parameter (Maybe Text)
parAllowedValues = lens _parAllowedValues (\ s a -> s{_parAllowedValues = a});

-- | Specifies the valid data type for the parameter.
parDataType :: Lens' Parameter (Maybe Text)
parDataType = lens _parDataType (\ s a -> s{_parDataType = a});

-- | Specifies the name of the parameter.
parParameterName :: Lens' Parameter (Maybe Text)
parParameterName = lens _parParameterName (\ s a -> s{_parParameterName = a});

-- | Provides a description of the parameter.
parDescription :: Lens' Parameter (Maybe Text)
parDescription = lens _parDescription (\ s a -> s{_parDescription = a});

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ApplyType") <*> (x .@? "ParameterValue") <*>
                (x .@? "ApplyMethod")
                <*> (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "DataType")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ApplyType" =: _parApplyType,
               "ParameterValue" =: _parParameterValue,
               "ApplyMethod" =: _parApplyMethod,
               "MinimumEngineVersion" =: _parMinimumEngineVersion,
               "Source" =: _parSource,
               "IsModifiable" =: _parIsModifiable,
               "AllowedValues" =: _parAllowedValues,
               "DataType" =: _parDataType,
               "ParameterName" =: _parParameterName,
               "Description" =: _parDescription]

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmaAutoAppliedAfterDate'
--
-- * 'pmaAction'
--
-- * 'pmaOptInStatus'
--
-- * 'pmaDescription'
--
-- * 'pmaCurrentApplyDate'
--
-- * 'pmaForcedApplyDate'
data PendingMaintenanceAction = PendingMaintenanceAction'{_pmaAutoAppliedAfterDate :: Maybe ISO8601, _pmaAction :: Maybe Text, _pmaOptInStatus :: Maybe Text, _pmaDescription :: Maybe Text, _pmaCurrentApplyDate :: Maybe ISO8601, _pmaForcedApplyDate :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'PendingMaintenanceAction' smart constructor.
pendingMaintenanceAction :: PendingMaintenanceAction
pendingMaintenanceAction = PendingMaintenanceAction'{_pmaAutoAppliedAfterDate = Nothing, _pmaAction = Nothing, _pmaOptInStatus = Nothing, _pmaDescription = Nothing, _pmaCurrentApplyDate = Nothing, _pmaForcedApplyDate = Nothing};

-- | The date of the maintenance window when the action will be applied. The
-- maintenance action will be applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- @next-maintenance@ opt-in requests are ignored.
pmaAutoAppliedAfterDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaAutoAppliedAfterDate = lens _pmaAutoAppliedAfterDate (\ s a -> s{_pmaAutoAppliedAfterDate = a}) . mapping _Time;

-- | The type of pending maintenance action that is available for the
-- resource.
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\ s a -> s{_pmaAction = a});

-- | Indicates the type of opt-in request that has been received for the
-- resource.
pmaOptInStatus :: Lens' PendingMaintenanceAction (Maybe Text)
pmaOptInStatus = lens _pmaOptInStatus (\ s a -> s{_pmaOptInStatus = a});

-- | A description providing more detail about the maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\ s a -> s{_pmaDescription = a});

-- | The effective date when the pending maintenance action will be applied
-- to the resource. This date takes into account opt-in requests received
-- from the ApplyPendingMaintenanceAction API, the @AutoAppliedAfterDate@,
-- and the @ForcedApplyDate@. This value is blank if an opt-in request has
-- not been received and nothing has been specified as
-- @AutoAppliedAfterDate@ or @ForcedApplyDate@.
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\ s a -> s{_pmaCurrentApplyDate = a}) . mapping _Time;

-- | The date when the maintenance action will be automatically applied. The
-- maintenance action will be applied to the resource on this date
-- regardless of the maintenance window for the resource. If this date is
-- specified, any @immediate@ opt-in requests are ignored.
pmaForcedApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaForcedApplyDate = lens _pmaForcedApplyDate (\ s a -> s{_pmaForcedApplyDate = a}) . mapping _Time;

instance FromXML PendingMaintenanceAction where
        parseXML x
          = PendingMaintenanceAction' <$>
              (x .@? "AutoAppliedAfterDate") <*> (x .@? "Action")
                <*> (x .@? "OptInStatus")
                <*> (x .@? "Description")
                <*> (x .@? "CurrentApplyDate")
                <*> (x .@? "ForcedApplyDate")

-- | This data type is used as a response element in the ModifyDBInstance
-- action.
--
-- /See:/ 'pendingModifiedValues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmvEngineVersion'
--
-- * 'pmvMasterUserPassword'
--
-- * 'pmvIOPS'
--
-- * 'pmvDBInstanceClass'
--
-- * 'pmvDBInstanceIdentifier'
--
-- * 'pmvCACertificateIdentifier'
--
-- * 'pmvBackupRetentionPeriod'
--
-- * 'pmvMultiAZ'
--
-- * 'pmvAllocatedStorage'
--
-- * 'pmvPort'
--
-- * 'pmvStorageType'
data PendingModifiedValues = PendingModifiedValues'{_pmvEngineVersion :: Maybe Text, _pmvMasterUserPassword :: Maybe Text, _pmvIOPS :: Maybe Int, _pmvDBInstanceClass :: Maybe Text, _pmvDBInstanceIdentifier :: Maybe Text, _pmvCACertificateIdentifier :: Maybe Text, _pmvBackupRetentionPeriod :: Maybe Int, _pmvMultiAZ :: Maybe Bool, _pmvAllocatedStorage :: Maybe Int, _pmvPort :: Maybe Int, _pmvStorageType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PendingModifiedValues' smart constructor.
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues'{_pmvEngineVersion = Nothing, _pmvMasterUserPassword = Nothing, _pmvIOPS = Nothing, _pmvDBInstanceClass = Nothing, _pmvDBInstanceIdentifier = Nothing, _pmvCACertificateIdentifier = Nothing, _pmvBackupRetentionPeriod = Nothing, _pmvMultiAZ = Nothing, _pmvAllocatedStorage = Nothing, _pmvPort = Nothing, _pmvStorageType = Nothing};

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a});

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a});

-- | Specifies the new Provisioned IOPS value for the DB instance that will
-- be applied or is being applied.
pmvIOPS :: Lens' PendingModifiedValues (Maybe Int)
pmvIOPS = lens _pmvIOPS (\ s a -> s{_pmvIOPS = a});

-- | Contains the new @DBInstanceClass@ for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass = lens _pmvDBInstanceClass (\ s a -> s{_pmvDBInstanceClass = a});

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\ s a -> s{_pmvDBInstanceIdentifier = a});

-- | Specifies the identifier of the CA certificate for the DB instance.
pmvCACertificateIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvCACertificateIdentifier = lens _pmvCACertificateIdentifier (\ s a -> s{_pmvCACertificateIdentifier = a});

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\ s a -> s{_pmvBackupRetentionPeriod = a});

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\ s a -> s{_pmvMultiAZ = a});

-- | Contains the new @AllocatedStorage@ size for the DB instance that will
-- be applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\ s a -> s{_pmvAllocatedStorage = a});

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\ s a -> s{_pmvPort = a});

-- | Specifies the storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\ s a -> s{_pmvStorageType = a});

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "MasterUserPassword")
                <*> (x .@? "Iops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and DescribeReservedDBInstancesOfferings
-- actions.
--
-- /See:/ 'recurringCharge' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRecurringChargeFrequency'
--
-- * 'rcRecurringChargeAmount'
data RecurringCharge = RecurringCharge'{_rcRecurringChargeFrequency :: Maybe Text, _rcRecurringChargeAmount :: Maybe Double} deriving (Eq, Read, Show)

-- | 'RecurringCharge' smart constructor.
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge'{_rcRecurringChargeFrequency = Nothing, _rcRecurringChargeAmount = Nothing};

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a});

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a});

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
--
-- /See:/ 'reservedDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdiDBInstanceCount'
--
-- * 'rdiState'
--
-- * 'rdiCurrencyCode'
--
-- * 'rdiProductDescription'
--
-- * 'rdiStartTime'
--
-- * 'rdiReservedDBInstanceId'
--
-- * 'rdiDBInstanceClass'
--
-- * 'rdiMultiAZ'
--
-- * 'rdiReservedDBInstancesOfferingId'
--
-- * 'rdiOfferingType'
--
-- * 'rdiUsagePrice'
--
-- * 'rdiRecurringCharges'
--
-- * 'rdiFixedPrice'
--
-- * 'rdiDuration'
data ReservedDBInstance = ReservedDBInstance'{_rdiDBInstanceCount :: Maybe Int, _rdiState :: Maybe Text, _rdiCurrencyCode :: Maybe Text, _rdiProductDescription :: Maybe Text, _rdiStartTime :: Maybe ISO8601, _rdiReservedDBInstanceId :: Maybe Text, _rdiDBInstanceClass :: Maybe Text, _rdiMultiAZ :: Maybe Bool, _rdiReservedDBInstancesOfferingId :: Maybe Text, _rdiOfferingType :: Maybe Text, _rdiUsagePrice :: Maybe Double, _rdiRecurringCharges :: Maybe [RecurringCharge], _rdiFixedPrice :: Maybe Double, _rdiDuration :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ReservedDBInstance' smart constructor.
reservedDBInstance :: ReservedDBInstance
reservedDBInstance = ReservedDBInstance'{_rdiDBInstanceCount = Nothing, _rdiState = Nothing, _rdiCurrencyCode = Nothing, _rdiProductDescription = Nothing, _rdiStartTime = Nothing, _rdiReservedDBInstanceId = Nothing, _rdiDBInstanceClass = Nothing, _rdiMultiAZ = Nothing, _rdiReservedDBInstancesOfferingId = Nothing, _rdiOfferingType = Nothing, _rdiUsagePrice = Nothing, _rdiRecurringCharges = Nothing, _rdiFixedPrice = Nothing, _rdiDuration = Nothing};

-- | The number of reserved DB instances.
rdiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Int)
rdiDBInstanceCount = lens _rdiDBInstanceCount (\ s a -> s{_rdiDBInstanceCount = a});

-- | The state of the reserved DB instance.
rdiState :: Lens' ReservedDBInstance (Maybe Text)
rdiState = lens _rdiState (\ s a -> s{_rdiState = a});

-- | The currency code for the reserved DB instance.
rdiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdiCurrencyCode = lens _rdiCurrencyCode (\ s a -> s{_rdiCurrencyCode = a});

-- | The description of the reserved DB instance.
rdiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdiProductDescription = lens _rdiProductDescription (\ s a -> s{_rdiProductDescription = a});

-- | The time the reservation started.
rdiStartTime :: Lens' ReservedDBInstance (Maybe UTCTime)
rdiStartTime = lens _rdiStartTime (\ s a -> s{_rdiStartTime = a}) . mapping _Time;

-- | The unique identifier for the reservation.
rdiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceId = lens _rdiReservedDBInstanceId (\ s a -> s{_rdiReservedDBInstanceId = a});

-- | The DB instance class for the reserved DB instance.
rdiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdiDBInstanceClass = lens _rdiDBInstanceClass (\ s a -> s{_rdiDBInstanceClass = a});

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdiMultiAZ = lens _rdiMultiAZ (\ s a -> s{_rdiMultiAZ = a});

-- | The offering identifier.
rdiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstancesOfferingId = lens _rdiReservedDBInstancesOfferingId (\ s a -> s{_rdiReservedDBInstancesOfferingId = a});

-- | The offering type of this reserved DB instance.
rdiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdiOfferingType = lens _rdiOfferingType (\ s a -> s{_rdiOfferingType = a});

-- | The hourly price charged for this reserved DB instance.
rdiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdiUsagePrice = lens _rdiUsagePrice (\ s a -> s{_rdiUsagePrice = a});

-- | The recurring price charged to run this reserved DB instance.
rdiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdiRecurringCharges = lens _rdiRecurringCharges (\ s a -> s{_rdiRecurringCharges = a}) . _Default;

-- | The fixed price charged for this reserved DB instance.
rdiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdiFixedPrice = lens _rdiFixedPrice (\ s a -> s{_rdiFixedPrice = a});

-- | The duration of the reservation in seconds.
rdiDuration :: Lens' ReservedDBInstance (Maybe Int)
rdiDuration = lens _rdiDuration (\ s a -> s{_rdiDuration = a});

instance FromXML ReservedDBInstance where
        parseXML x
          = ReservedDBInstance' <$>
              (x .@? "DBInstanceCount") <*> (x .@? "State") <*>
                (x .@? "CurrencyCode")
                <*> (x .@? "ProductDescription")
                <*> (x .@? "StartTime")
                <*> (x .@? "ReservedDBInstanceId")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "ReservedDBInstancesOfferingId")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
--
-- /See:/ 'reservedDBInstancesOffering' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdioCurrencyCode'
--
-- * 'rdioProductDescription'
--
-- * 'rdioDBInstanceClass'
--
-- * 'rdioMultiAZ'
--
-- * 'rdioReservedDBInstancesOfferingId'
--
-- * 'rdioOfferingType'
--
-- * 'rdioUsagePrice'
--
-- * 'rdioRecurringCharges'
--
-- * 'rdioFixedPrice'
--
-- * 'rdioDuration'
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'{_rdioCurrencyCode :: Maybe Text, _rdioProductDescription :: Maybe Text, _rdioDBInstanceClass :: Maybe Text, _rdioMultiAZ :: Maybe Bool, _rdioReservedDBInstancesOfferingId :: Maybe Text, _rdioOfferingType :: Maybe Text, _rdioUsagePrice :: Maybe Double, _rdioRecurringCharges :: Maybe [RecurringCharge], _rdioFixedPrice :: Maybe Double, _rdioDuration :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ReservedDBInstancesOffering' smart constructor.
reservedDBInstancesOffering :: ReservedDBInstancesOffering
reservedDBInstancesOffering = ReservedDBInstancesOffering'{_rdioCurrencyCode = Nothing, _rdioProductDescription = Nothing, _rdioDBInstanceClass = Nothing, _rdioMultiAZ = Nothing, _rdioReservedDBInstancesOfferingId = Nothing, _rdioOfferingType = Nothing, _rdioUsagePrice = Nothing, _rdioRecurringCharges = Nothing, _rdioFixedPrice = Nothing, _rdioDuration = Nothing};

-- | The currency code for the reserved DB instance offering.
rdioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioCurrencyCode = lens _rdioCurrencyCode (\ s a -> s{_rdioCurrencyCode = a});

-- | The database engine used by the offering.
rdioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioProductDescription = lens _rdioProductDescription (\ s a -> s{_rdioProductDescription = a});

-- | The DB instance class for the reserved DB instance.
rdioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioDBInstanceClass = lens _rdioDBInstanceClass (\ s a -> s{_rdioDBInstanceClass = a});

-- | Indicates if the offering applies to Multi-AZ deployments.
rdioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdioMultiAZ = lens _rdioMultiAZ (\ s a -> s{_rdioMultiAZ = a});

-- | The offering identifier.
rdioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioReservedDBInstancesOfferingId = lens _rdioReservedDBInstancesOfferingId (\ s a -> s{_rdioReservedDBInstancesOfferingId = a});

-- | The offering type.
rdioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioOfferingType = lens _rdioOfferingType (\ s a -> s{_rdioOfferingType = a});

-- | The hourly price charged for this offering.
rdioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioUsagePrice = lens _rdioUsagePrice (\ s a -> s{_rdioUsagePrice = a});

-- | The recurring price charged to run this reserved DB instance.
rdioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdioRecurringCharges = lens _rdioRecurringCharges (\ s a -> s{_rdioRecurringCharges = a}) . _Default;

-- | The fixed price charged for this offering.
rdioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioFixedPrice = lens _rdioFixedPrice (\ s a -> s{_rdioFixedPrice = a});

-- | The duration of the offering in seconds.
rdioDuration :: Lens' ReservedDBInstancesOffering (Maybe Int)
rdioDuration = lens _rdioDuration (\ s a -> s{_rdioDuration = a});

instance FromXML ReservedDBInstancesOffering where
        parseXML x
          = ReservedDBInstancesOffering' <$>
              (x .@? "CurrencyCode") <*>
                (x .@? "ProductDescription")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "ReservedDBInstancesOfferingId")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

-- | Describes the pending maintenance actions for a resource.
--
-- /See:/ 'resourcePendingMaintenanceActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpmaPendingMaintenanceActionDetails'
--
-- * 'rpmaResourceIdentifier'
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'{_rpmaPendingMaintenanceActionDetails :: Maybe [PendingMaintenanceAction], _rpmaResourceIdentifier :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ResourcePendingMaintenanceActions' smart constructor.
resourcePendingMaintenanceActions :: ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'{_rpmaPendingMaintenanceActionDetails = Nothing, _rpmaResourceIdentifier = Nothing};

-- | A list that provides details about the pending maintenance actions for
-- the resource.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\ s a -> s{_rpmaPendingMaintenanceActionDetails = a}) . _Default;

-- | The ARN of the resource that has pending maintenance actions.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\ s a -> s{_rpmaResourceIdentifier = a});

instance FromXML ResourcePendingMaintenanceActions
         where
        parseXML x
          = ResourcePendingMaintenanceActions' <$>
              (x .@? "PendingMaintenanceActionDetails" .!@ mempty
                 >>= may (parseXMLList "PendingMaintenanceAction"))
                <*> (x .@? "ResourceIdentifier")

-- | This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'subnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'subSubnetStatus'
--
-- * 'subSubnetIdentifier'
--
-- * 'subSubnetAvailabilityZone'
data Subnet = Subnet'{_subSubnetStatus :: Maybe Text, _subSubnetIdentifier :: Maybe Text, _subSubnetAvailabilityZone :: Maybe AvailabilityZone} deriving (Eq, Read, Show)

-- | 'Subnet' smart constructor.
subnet :: Subnet
subnet = Subnet'{_subSubnetStatus = Nothing, _subSubnetIdentifier = Nothing, _subSubnetAvailabilityZone = Nothing};

-- | Specifies the status of the subnet.
subSubnetStatus :: Lens' Subnet (Maybe Text)
subSubnetStatus = lens _subSubnetStatus (\ s a -> s{_subSubnetStatus = a});

-- | Specifies the identifier of the subnet.
subSubnetIdentifier :: Lens' Subnet (Maybe Text)
subSubnetIdentifier = lens _subSubnetIdentifier (\ s a -> s{_subSubnetIdentifier = a});

-- | FIXME: Undocumented member.
subSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
subSubnetAvailabilityZone = lens _subSubnetAvailabilityZone (\ s a -> s{_subSubnetAvailabilityZone = a});

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value
-- pair.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'{_tagValue :: Maybe Text, _tagKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag = Tag'{_tagValue = Nothing, _tagKey = Nothing};

-- | A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and cannot be prefixed with \"aws:\"
-- or \"rds:\". The string may only contain only the set of Unicode
-- letters, digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\'
-- (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and cannot be prefixed with \"aws:\" or
-- \"rds:\". The string may only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | This data type is used as a response element for queries on VPC security
-- group membership.
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsgmStatus'
--
-- * 'vsgmVPCSecurityGroupId'
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'{_vsgmStatus :: Maybe Text, _vsgmVPCSecurityGroupId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VPCSecurityGroupMembership' smart constructor.
vpcSecurityGroupMembership :: VPCSecurityGroupMembership
vpcSecurityGroupMembership = VPCSecurityGroupMembership'{_vsgmStatus = Nothing, _vsgmVPCSecurityGroupId = Nothing};

-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a});

-- | The name of the VPC security group.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a});

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")
