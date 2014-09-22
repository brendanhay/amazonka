{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Relational Database Service (Amazon RDS) is a web service that makes
-- it easy to set up, operate, and scale a relational database in the cloud.
-- It provides cost-efficient and resizable capacity while managing
-- time-consuming database administration tasks, freeing you up to focus on
-- your applications and business.
module Network.AWS.RDS.Types
    (
    -- * Service
      RDS
    -- ** Errors
    , RDSError (..)
    , _AuthorizationAlreadyExistsFault
    , _AuthorizationNotFoundFault
    , _AuthorizationQuotaExceededFault
    , _DBInstanceAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupNotFoundFault
    , _DBParameterGroupQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotSupportedFault
    , _DBSecurityGroupQuotaExceededFault
    , _DBSnapshotAlreadyExistsFault
    , _DBSnapshotNotFoundFault
    , _DBSubnetGroupAlreadyExistsFault
    , _DBSubnetGroupDoesNotCoverEnoughAZs
    , _DBSubnetGroupNotAllowedFault
    , _DBSubnetGroupNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _DBSubnetQuotaExceededFault
    , _DBUpgradeDependencyFailureFault
    , _EventSubscriptionQuotaExceededFault
    , _InstanceQuotaExceededFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidDBInstanceStateFault
    , _InvalidDBParameterGroupStateFault
    , _InvalidDBSecurityGroupStateFault
    , _InvalidDBSnapshotStateFault
    , _InvalidDBSubnetGroupFault
    , _InvalidDBSubnetGroupStateFault
    , _InvalidDBSubnetStateFault
    , _InvalidEventSubscriptionStateFault
    , _InvalidOptionGroupStateFault
    , _InvalidRestoreFault
    , _InvalidSubnet
    , _InvalidVPCNetworkStateFault
    , _OptionGroupAlreadyExistsFault
    , _OptionGroupNotFoundFault
    , _OptionGroupQuotaExceededFault
    , _PointInTimeRestoreNotEnabledFault
    , _ProvisionedIopsNotAvailableInAZFault
    , _RDSClient
    , _RDSSerializer
    , _RDSService
    , _ReservedDBInstanceAlreadyExistsFault
    , _ReservedDBInstanceNotFoundFault
    , _ReservedDBInstanceQuotaExceededFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _SNSInvalidTopicFault
    , _SNSNoAuthorizationFault
    , _SNSTopicArnNotFoundFault
    , _SnapshotQuotaExceededFault
    , _SourceNotFoundFault
    , _StorageQuotaExceededFault
    , _SubnetAlreadyInUse
    , _SubscriptionAlreadyExistFault
    , _SubscriptionCategoryNotFoundFault
    , _SubscriptionNotFoundFault
    -- ** XML
    , xmlOptions

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * SourceType
    , SourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName
    , azProvisionedIopsCapable

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- * DBEngineVersion
    , DBEngineVersion
    , dBEngineVersion
    , dbevEngine
    , dbevEngineVersion
    , dbevDBParameterGroupFamily
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDefaultCharacterSet
    , dbevCharacterSet

    -- * DBInstance
    , DBInstance
    , dBInstance
    , dbiDBInstanceIdentifier
    , dbiDBInstanceClass
    , dbiEngine
    , dbiDBInstanceStatus
    , dbiMasterUsername
    , dbiDBName
    , dbiEndpoint
    , dbiAllocatedStorage
    , dbiInstanceCreateTime
    , dbiPreferredBackupWindow
    , dbiBackupRetentionPeriod
    , dbiDBSecurityGroup
    , dbiVpcSecurityGroupMembership
    , dbiDBParameterGroup
    , dbiAvailabilityZone
    , dbiDBSubnetGroup
    , dbiPreferredMaintenanceWindow
    , dbiPendingModifiedValues
    , dbiLatestRestorableTime
    , dbiMultiAZ
    , dbiEngineVersion
    , dbiAutoMinorVersionUpgrade
    , dbiReadReplicaSourceDBInstanceIdentifier
    , dbiReadReplicaDBInstanceIdentifier
    , dbiLicenseModel
    , dbiIops
    , dbiOptionGroupMembership
    , dbiCharacterSetName
    , dbiSecondaryAvailabilityZone
    , dbiPubliclyAccessible
    , dbiDBInstanceStatusInfo

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dBInstanceStatusInfo
    , dbisiStatusType
    , dbisiNormal
    , dbisiStatus
    , dbisiMessage

    -- * DBParameterGroup
    , DBParameterGroup
    , dBParameterGroup
    , dbpgDBParameterGroupName
    , dbpgDBParameterGroupFamily
    , dbpgDescription

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus
    , dBParameterGroupStatus
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- * DBSecurityGroup
    , DBSecurityGroup
    , dBSecurityGroup
    , dbsgOwnerId
    , dbsgDBSecurityGroupName
    , dbsgDBSecurityGroupDescription
    , dbsgVpcId
    , dbsgEC2SecurityGroup
    , dbsgIPRange

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dBSecurityGroupMembership
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- * DBSnapshot
    , DBSnapshot
    , dBSnapshot
    , dbsDBSnapshotIdentifier
    , dbsDBInstanceIdentifier
    , dbsSnapshotCreateTime
    , dbsEngine
    , dbsAllocatedStorage
    , dbsStatus
    , dbsPort
    , dbsAvailabilityZone
    , dbsVpcId
    , dbsInstanceCreateTime
    , dbsMasterUsername
    , dbsEngineVersion
    , dbsLicenseModel
    , dbsSnapshotType
    , dbsIops
    , dbsOptionGroupName
    , dbsPercentProgress
    , dbsSourceRegion

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dBSubnetGroup
    , dbsgrDBSubnetGroupName
    , dbsgrDBSubnetGroupDescription
    , dbsgrVpcId
    , dbsgrSubnetGroupStatus
    , dbsgrSubnet

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddblfdLogFileName
    , ddblfdLastWritten
    , ddblfdSize

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , eC2SecurityGroup
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupOwnerId

    -- * Endpoint'
    , Endpoint'
    , endpoint'
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameter

    -- * Event
    , Event
    , event
    , erSourceIdentifier
    , erSourceType
    , erMessage
    , erEventCategory
    , erDate

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategory

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esCustomerAwsId
    , esCustSubscriptionId
    , esSnsTopicArn
    , esStatus
    , esSubscriptionCreationTime
    , esSourceType
    , esSourceId
    , esEventCategory
    , esEnabled

    -- * IPRange
    , IPRange
    , iPRange
    , iprStatus
    , iprCIDRIP

    -- * Option
    , Option
    , option
    , oOptionName
    , oOptionDescription
    , oPersistent
    , oPermanent
    , oPort
    , oOptionSetting
    , oDBSecurityGroup
    , oVpcSecurityGroupMembership

    -- * OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionName
    , ocPort
    , ocDBSecurityGroupName
    , ocVpcSecurityGroupId
    , ocOptionSetting

    -- * OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupName
    , ogOptionGroupDescription
    , ogEngineName
    , ogMajorEngineVersion
    , ogOption
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogVpcId

    -- * OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmOptionGroupName
    , ogmStatus

    -- * OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoName
    , ogoDescription
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoMinimumRequiredMinorEngineVersion
    , ogoPortRequired
    , ogoDefaultPort
    , ogoOptionName
    , ogoPersistent
    , ogoPermanent
    , ogoOptionGroupOptionSetting

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosSettingName
    , ogosSettingDescription
    , ogosDefaultValue
    , ogosApplyType
    , ogosAllowedValues
    , ogosIsModifiable

    -- * OptionSetting
    , OptionSetting
    , optionSetting
    , osName
    , osValue
    , osDefaultValue
    , osDescription
    , osApplyType
    , osDataType
    , osAllowedValues
    , osIsModifiable
    , osIsCollection

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odbioEngine
    , odbioEngineVersion
    , odbioDBInstanceClass
    , odbioLicenseModel
    , odbioAvailabilityZone
    , odbioMultiAZCapable
    , odbioReadReplicaCapable
    , odbioVpc

    -- * Parameter
    , Parameter
    , parameter
    , pParameterName
    , pParameterValue
    , pDescription
    , pSource
    , pApplyType
    , pDataType
    , pAllowedValues
    , pIsModifiable
    , pMinimumEngineVersion
    , pApplyMethod

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvDBInstanceClass
    , pmvAllocatedStorage
    , pmvMasterUserPassword
    , pmvPort
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvEngineVersion
    , pmvIops
    , pmvDBInstanceIdentifier

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdbiReservedDBInstanceId
    , rdbiReservedDBInstancesOfferingId
    , rdbiDBInstanceClass
    , rdbiStartTime
    , rdbiDuration
    , rdbiFixedPrice
    , rdbiUsagePrice
    , rdbiCurrencyCode
    , rdbiDBInstanceCount
    , rdbiProductDescription
    , rdbiOfferingType
    , rdbiMultiAZ
    , rdbiState
    , rdbiRecurringCharge

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdbioReservedDBInstancesOfferingId
    , rdbioDBInstanceClass
    , rdbioDuration
    , rdbioFixedPrice
    , rdbioUsagePrice
    , rdbioCurrencyCode
    , rdbioProductDescription
    , rdbioOfferingType
    , rdbioMultiAZ
    , rdbioRecurringCharge

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone
    , sSubnetStatus

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmVpcSecurityGroupId
    , vsgmStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-09-09@) of the
-- @Amazon Relational Database Service@ service.
data RDS deriving (Typeable)

instance AWSService RDS where
    type Sg RDS = V4
    type Er RDS = RDSError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "rds"
        , _svcVersion  = "2013-09-09"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'RDS' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data RDSError
      -- | The specified CIDRIP or EC2 security group is already authorized
      -- for the specified DB security group.
    = AuthorizationAlreadyExistsFault
      -- | Specified CIDRIP or EC2 security group is not authorized for the
      -- specified DB security group.
    | AuthorizationNotFoundFault
      -- | DB security group authorization quota has been reached.
    | AuthorizationQuotaExceededFault
      -- | User already has a DB instance with the given identifier.
    | DBInstanceAlreadyExistsFault
      -- | DBInstanceIdentifier does not refer to an existing DB instance.
    | DBInstanceNotFoundFault
      -- | A DB parameter group with the same name exists.
    | DBParameterGroupAlreadyExistsFault
      -- | DBParameterGroupName does not refer to an existing DB parameter
      -- group.
    | DBParameterGroupNotFoundFault
      -- | Request would result in user exceeding the allowed number of DB
      -- parameter groups.
    | DBParameterGroupQuotaExceededFault
      -- | A DB security group with the name specified in
      -- DBSecurityGroupName already exists.
    | DBSecurityGroupAlreadyExistsFault
      -- | DBSecurityGroupName does not refer to an existing DB security
      -- group.
    | DBSecurityGroupNotFoundFault
      -- | A DB security group is not allowed for this action.
    | DBSecurityGroupNotSupportedFault
      -- | Request would result in user exceeding the allowed number of DB
      -- security groups.
    | DBSecurityGroupQuotaExceededFault
      -- | DBSnapshotIdentifier is already used by an existing snapshot.
    | DBSnapshotAlreadyExistsFault
      -- | DBSnapshotIdentifier does not refer to an existing DB snapshot.
    | DBSnapshotNotFoundFault
      -- | DBSubnetGroupName is already used by an existing DB subnet group.
    | DBSubnetGroupAlreadyExistsFault
      -- | Subnets in the DB subnet group should cover at least 2
      -- Availability Zones unless there is only 1 availablility zone.
    | DBSubnetGroupDoesNotCoverEnoughAZs
      -- | Indicates that the DBSubnetGroup should not be specified while
      -- creating read replicas that lie in the same region as the source
      -- instance.
    | DBSubnetGroupNotAllowedFault
      -- | DBSubnetGroupName does not refer to an existing DB subnet group.
    | DBSubnetGroupNotFoundFault
      -- | Request would result in user exceeding the allowed number of DB
      -- subnet groups.
    | DBSubnetGroupQuotaExceededFault
      -- | Request would result in user exceeding the allowed number of
      -- subnets in a DB subnet groups.
    | DBSubnetQuotaExceededFault
      -- | The DB upgrade failed because a resource the DB depends on could
      -- not be modified.
    | DBUpgradeDependencyFailureFault
      -- | You have reached the maximum number of event subscriptions.
    | EventSubscriptionQuotaExceededFault
      -- | Request would result in user exceeding the allowed number of DB
      -- instances.
    | InstanceQuotaExceededFault
      -- | Specified DB instance class is not available in the specified
      -- Availability Zone.
    | InsufficientDBInstanceCapacityFault
      -- | The specified DB instance is not in the available state.
    | InvalidDBInstanceStateFault
      -- | The DB parameter group cannot be deleted because it is in use.
    | InvalidDBParameterGroupStateFault
      -- | The state of the DB security group does not allow deletion.
    | InvalidDBSecurityGroupStateFault
      -- | The state of the DB snapshot does not allow deletion.
    | InvalidDBSnapshotStateFault
      -- | Indicates the DBSubnetGroup does not belong to the same VPC as
      -- that of an existing cross region read replica of the same source
      -- instance.
    | InvalidDBSubnetGroupFault
      -- | The DB subnet group cannot be deleted because it is in use.
    | InvalidDBSubnetGroupStateFault
      -- | The DB subnet is not in the available state.
    | InvalidDBSubnetStateFault
      -- | This error can occur if someone else is modifying a subscription.
      -- You should retry the action.
    | InvalidEventSubscriptionStateFault
      -- | The option group is not in the available state.
    | InvalidOptionGroupStateFault
      -- | Cannot restore from vpc backup to non-vpc DB instance.
    | InvalidRestoreFault
      -- | The requested subnet is invalid, or multiple subnets were
      -- requested that are not all in a common VPC.
    | InvalidSubnet
      -- | DB subnet group does not cover all Availability Zones after it is
      -- created because users' change.
    | InvalidVPCNetworkStateFault
      -- | The option group you are trying to create already exists.
    | OptionGroupAlreadyExistsFault
      -- | The specified option group could not be found.
    | OptionGroupNotFoundFault
      -- | The quota of 20 option groups was exceeded for this AWS account.
    | OptionGroupQuotaExceededFault
      -- | SourceDBInstanceIdentifier refers to a DB instance with
      -- BackupRetentionPeriod equal to 0.
    | PointInTimeRestoreNotEnabledFault
      -- | Provisioned IOPS not available in the specified Availability
      -- Zone.
    | ProvisionedIopsNotAvailableInAZFault
    | RDSClient HttpException
    | RDSSerializer String
    | RDSService String
      -- | User already has a reservation with the given identifier.
    | ReservedDBInstanceAlreadyExistsFault
      -- | The specified reserved DB Instance not found.
    | ReservedDBInstanceNotFoundFault
      -- | Request would exceed the user's DB Instance quota.
    | ReservedDBInstanceQuotaExceededFault
      -- | Specified offering does not exist.
    | ReservedDBInstancesOfferingNotFoundFault
      -- | SNS has responded that there is a problem with the SND topic
      -- specified.
    | SNSInvalidTopicFault
      -- | You do not have permission to publish to the SNS topic ARN.
    | SNSNoAuthorizationFault
      -- | The SNS topic ARN does not exist.
    | SNSTopicArnNotFoundFault
      -- | Request would result in user exceeding the allowed number of DB
      -- snapshots.
    | SnapshotQuotaExceededFault
      -- | The requested source could not be found.
    | SourceNotFoundFault
      -- | Request would result in user exceeding the allowed amount of
      -- storage available across all DB instances.
    | StorageQuotaExceededFault
      -- | The DB subnet is already in use in the Availability Zone.
    | SubnetAlreadyInUse
      -- | The supplied subscription name already exists.
    | SubscriptionAlreadyExistFault
      -- | The supplied category does not exist.
    | SubscriptionCategoryNotFoundFault
      -- | The subscription name does not exist.
    | SubscriptionNotFoundFault
      deriving (Show, Typeable, Generic)

instance AWSError RDSError where
    awsError = const "RDSError"

instance AWSServiceError RDSError where
    serviceError    = RDSService
    clientError     = RDSClient
    serializerError = RDSSerializer

instance Exception RDSError

-- | The specified CIDRIP or EC2 security group is already authorized for the
-- specified DB security group.
--
-- See: 'AuthorizationAlreadyExistsFault'
_AuthorizationAlreadyExistsFault :: Prism' RDSError ()
_AuthorizationAlreadyExistsFault = prism
    (const AuthorizationAlreadyExistsFault)
    (\case
        AuthorizationAlreadyExistsFault -> Right ()
        x -> Left x)

-- | Specified CIDRIP or EC2 security group is not authorized for the specified
-- DB security group.
--
-- See: 'AuthorizationNotFoundFault'
_AuthorizationNotFoundFault :: Prism' RDSError ()
_AuthorizationNotFoundFault = prism
    (const AuthorizationNotFoundFault)
    (\case
        AuthorizationNotFoundFault -> Right ()
        x -> Left x)

-- | DB security group authorization quota has been reached.
--
-- See: 'AuthorizationQuotaExceededFault'
_AuthorizationQuotaExceededFault :: Prism' RDSError ()
_AuthorizationQuotaExceededFault = prism
    (const AuthorizationQuotaExceededFault)
    (\case
        AuthorizationQuotaExceededFault -> Right ()
        x -> Left x)

-- | User already has a DB instance with the given identifier.
--
-- See: 'DBInstanceAlreadyExistsFault'
_DBInstanceAlreadyExistsFault :: Prism' RDSError ()
_DBInstanceAlreadyExistsFault = prism
    (const DBInstanceAlreadyExistsFault)
    (\case
        DBInstanceAlreadyExistsFault -> Right ()
        x -> Left x)

-- | DBInstanceIdentifier does not refer to an existing DB instance.
--
-- See: 'DBInstanceNotFoundFault'
_DBInstanceNotFoundFault :: Prism' RDSError ()
_DBInstanceNotFoundFault = prism
    (const DBInstanceNotFoundFault)
    (\case
        DBInstanceNotFoundFault -> Right ()
        x -> Left x)

-- | A DB parameter group with the same name exists.
--
-- See: 'DBParameterGroupAlreadyExistsFault'
_DBParameterGroupAlreadyExistsFault :: Prism' RDSError ()
_DBParameterGroupAlreadyExistsFault = prism
    (const DBParameterGroupAlreadyExistsFault)
    (\case
        DBParameterGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | DBParameterGroupName does not refer to an existing DB parameter group.
--
-- See: 'DBParameterGroupNotFoundFault'
_DBParameterGroupNotFoundFault :: Prism' RDSError ()
_DBParameterGroupNotFoundFault = prism
    (const DBParameterGroupNotFoundFault)
    (\case
        DBParameterGroupNotFoundFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of DB parameter
-- groups.
--
-- See: 'DBParameterGroupQuotaExceededFault'
_DBParameterGroupQuotaExceededFault :: Prism' RDSError ()
_DBParameterGroupQuotaExceededFault = prism
    (const DBParameterGroupQuotaExceededFault)
    (\case
        DBParameterGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | A DB security group with the name specified in DBSecurityGroupName already
-- exists.
--
-- See: 'DBSecurityGroupAlreadyExistsFault'
_DBSecurityGroupAlreadyExistsFault :: Prism' RDSError ()
_DBSecurityGroupAlreadyExistsFault = prism
    (const DBSecurityGroupAlreadyExistsFault)
    (\case
        DBSecurityGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | DBSecurityGroupName does not refer to an existing DB security group.
--
-- See: 'DBSecurityGroupNotFoundFault'
_DBSecurityGroupNotFoundFault :: Prism' RDSError ()
_DBSecurityGroupNotFoundFault = prism
    (const DBSecurityGroupNotFoundFault)
    (\case
        DBSecurityGroupNotFoundFault -> Right ()
        x -> Left x)

-- | A DB security group is not allowed for this action.
--
-- See: 'DBSecurityGroupNotSupportedFault'
_DBSecurityGroupNotSupportedFault :: Prism' RDSError ()
_DBSecurityGroupNotSupportedFault = prism
    (const DBSecurityGroupNotSupportedFault)
    (\case
        DBSecurityGroupNotSupportedFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of DB security
-- groups.
--
-- See: 'DBSecurityGroupQuotaExceededFault'
_DBSecurityGroupQuotaExceededFault :: Prism' RDSError ()
_DBSecurityGroupQuotaExceededFault = prism
    (const DBSecurityGroupQuotaExceededFault)
    (\case
        DBSecurityGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | DBSnapshotIdentifier is already used by an existing snapshot.
--
-- See: 'DBSnapshotAlreadyExistsFault'
_DBSnapshotAlreadyExistsFault :: Prism' RDSError ()
_DBSnapshotAlreadyExistsFault = prism
    (const DBSnapshotAlreadyExistsFault)
    (\case
        DBSnapshotAlreadyExistsFault -> Right ()
        x -> Left x)

-- | DBSnapshotIdentifier does not refer to an existing DB snapshot.
--
-- See: 'DBSnapshotNotFoundFault'
_DBSnapshotNotFoundFault :: Prism' RDSError ()
_DBSnapshotNotFoundFault = prism
    (const DBSnapshotNotFoundFault)
    (\case
        DBSnapshotNotFoundFault -> Right ()
        x -> Left x)

-- | DBSubnetGroupName is already used by an existing DB subnet group.
--
-- See: 'DBSubnetGroupAlreadyExistsFault'
_DBSubnetGroupAlreadyExistsFault :: Prism' RDSError ()
_DBSubnetGroupAlreadyExistsFault = prism
    (const DBSubnetGroupAlreadyExistsFault)
    (\case
        DBSubnetGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | Subnets in the DB subnet group should cover at least 2 Availability Zones
-- unless there is only 1 availablility zone.
--
-- See: 'DBSubnetGroupDoesNotCoverEnoughAZs'
_DBSubnetGroupDoesNotCoverEnoughAZs :: Prism' RDSError ()
_DBSubnetGroupDoesNotCoverEnoughAZs = prism
    (const DBSubnetGroupDoesNotCoverEnoughAZs)
    (\case
        DBSubnetGroupDoesNotCoverEnoughAZs -> Right ()
        x -> Left x)

-- | Indicates that the DBSubnetGroup should not be specified while creating
-- read replicas that lie in the same region as the source instance.
--
-- See: 'DBSubnetGroupNotAllowedFault'
_DBSubnetGroupNotAllowedFault :: Prism' RDSError ()
_DBSubnetGroupNotAllowedFault = prism
    (const DBSubnetGroupNotAllowedFault)
    (\case
        DBSubnetGroupNotAllowedFault -> Right ()
        x -> Left x)

-- | DBSubnetGroupName does not refer to an existing DB subnet group.
--
-- See: 'DBSubnetGroupNotFoundFault'
_DBSubnetGroupNotFoundFault :: Prism' RDSError ()
_DBSubnetGroupNotFoundFault = prism
    (const DBSubnetGroupNotFoundFault)
    (\case
        DBSubnetGroupNotFoundFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
--
-- See: 'DBSubnetGroupQuotaExceededFault'
_DBSubnetGroupQuotaExceededFault :: Prism' RDSError ()
_DBSubnetGroupQuotaExceededFault = prism
    (const DBSubnetGroupQuotaExceededFault)
    (\case
        DBSubnetGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of subnets in a
-- DB subnet groups.
--
-- See: 'DBSubnetQuotaExceededFault'
_DBSubnetQuotaExceededFault :: Prism' RDSError ()
_DBSubnetQuotaExceededFault = prism
    (const DBSubnetQuotaExceededFault)
    (\case
        DBSubnetQuotaExceededFault -> Right ()
        x -> Left x)

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
--
-- See: 'DBUpgradeDependencyFailureFault'
_DBUpgradeDependencyFailureFault :: Prism' RDSError ()
_DBUpgradeDependencyFailureFault = prism
    (const DBUpgradeDependencyFailureFault)
    (\case
        DBUpgradeDependencyFailureFault -> Right ()
        x -> Left x)

-- | You have reached the maximum number of event subscriptions.
--
-- See: 'EventSubscriptionQuotaExceededFault'
_EventSubscriptionQuotaExceededFault :: Prism' RDSError ()
_EventSubscriptionQuotaExceededFault = prism
    (const EventSubscriptionQuotaExceededFault)
    (\case
        EventSubscriptionQuotaExceededFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of DB instances.
--
-- See: 'InstanceQuotaExceededFault'
_InstanceQuotaExceededFault :: Prism' RDSError ()
_InstanceQuotaExceededFault = prism
    (const InstanceQuotaExceededFault)
    (\case
        InstanceQuotaExceededFault -> Right ()
        x -> Left x)

-- | Specified DB instance class is not available in the specified Availability
-- Zone.
--
-- See: 'InsufficientDBInstanceCapacityFault'
_InsufficientDBInstanceCapacityFault :: Prism' RDSError ()
_InsufficientDBInstanceCapacityFault = prism
    (const InsufficientDBInstanceCapacityFault)
    (\case
        InsufficientDBInstanceCapacityFault -> Right ()
        x -> Left x)

-- | The specified DB instance is not in the available state.
--
-- See: 'InvalidDBInstanceStateFault'
_InvalidDBInstanceStateFault :: Prism' RDSError ()
_InvalidDBInstanceStateFault = prism
    (const InvalidDBInstanceStateFault)
    (\case
        InvalidDBInstanceStateFault -> Right ()
        x -> Left x)

-- | The DB parameter group cannot be deleted because it is in use.
--
-- See: 'InvalidDBParameterGroupStateFault'
_InvalidDBParameterGroupStateFault :: Prism' RDSError ()
_InvalidDBParameterGroupStateFault = prism
    (const InvalidDBParameterGroupStateFault)
    (\case
        InvalidDBParameterGroupStateFault -> Right ()
        x -> Left x)

-- | The state of the DB security group does not allow deletion.
--
-- See: 'InvalidDBSecurityGroupStateFault'
_InvalidDBSecurityGroupStateFault :: Prism' RDSError ()
_InvalidDBSecurityGroupStateFault = prism
    (const InvalidDBSecurityGroupStateFault)
    (\case
        InvalidDBSecurityGroupStateFault -> Right ()
        x -> Left x)

-- | The state of the DB snapshot does not allow deletion.
--
-- See: 'InvalidDBSnapshotStateFault'
_InvalidDBSnapshotStateFault :: Prism' RDSError ()
_InvalidDBSnapshotStateFault = prism
    (const InvalidDBSnapshotStateFault)
    (\case
        InvalidDBSnapshotStateFault -> Right ()
        x -> Left x)

-- | Indicates the DBSubnetGroup does not belong to the same VPC as that of an
-- existing cross region read replica of the same source instance.
--
-- See: 'InvalidDBSubnetGroupFault'
_InvalidDBSubnetGroupFault :: Prism' RDSError ()
_InvalidDBSubnetGroupFault = prism
    (const InvalidDBSubnetGroupFault)
    (\case
        InvalidDBSubnetGroupFault -> Right ()
        x -> Left x)

-- | The DB subnet group cannot be deleted because it is in use.
--
-- See: 'InvalidDBSubnetGroupStateFault'
_InvalidDBSubnetGroupStateFault :: Prism' RDSError ()
_InvalidDBSubnetGroupStateFault = prism
    (const InvalidDBSubnetGroupStateFault)
    (\case
        InvalidDBSubnetGroupStateFault -> Right ()
        x -> Left x)

-- | The DB subnet is not in the available state.
--
-- See: 'InvalidDBSubnetStateFault'
_InvalidDBSubnetStateFault :: Prism' RDSError ()
_InvalidDBSubnetStateFault = prism
    (const InvalidDBSubnetStateFault)
    (\case
        InvalidDBSubnetStateFault -> Right ()
        x -> Left x)

-- | This error can occur if someone else is modifying a subscription. You
-- should retry the action.
--
-- See: 'InvalidEventSubscriptionStateFault'
_InvalidEventSubscriptionStateFault :: Prism' RDSError ()
_InvalidEventSubscriptionStateFault = prism
    (const InvalidEventSubscriptionStateFault)
    (\case
        InvalidEventSubscriptionStateFault -> Right ()
        x -> Left x)

-- | The option group is not in the available state.
--
-- See: 'InvalidOptionGroupStateFault'
_InvalidOptionGroupStateFault :: Prism' RDSError ()
_InvalidOptionGroupStateFault = prism
    (const InvalidOptionGroupStateFault)
    (\case
        InvalidOptionGroupStateFault -> Right ()
        x -> Left x)

-- | Cannot restore from vpc backup to non-vpc DB instance.
--
-- See: 'InvalidRestoreFault'
_InvalidRestoreFault :: Prism' RDSError ()
_InvalidRestoreFault = prism
    (const InvalidRestoreFault)
    (\case
        InvalidRestoreFault -> Right ()
        x -> Left x)

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
--
-- See: 'InvalidSubnet'
_InvalidSubnet :: Prism' RDSError ()
_InvalidSubnet = prism
    (const InvalidSubnet)
    (\case
        InvalidSubnet -> Right ()
        x -> Left x)

-- | DB subnet group does not cover all Availability Zones after it is created
-- because users' change.
--
-- See: 'InvalidVPCNetworkStateFault'
_InvalidVPCNetworkStateFault :: Prism' RDSError ()
_InvalidVPCNetworkStateFault = prism
    (const InvalidVPCNetworkStateFault)
    (\case
        InvalidVPCNetworkStateFault -> Right ()
        x -> Left x)

-- | The option group you are trying to create already exists.
--
-- See: 'OptionGroupAlreadyExistsFault'
_OptionGroupAlreadyExistsFault :: Prism' RDSError ()
_OptionGroupAlreadyExistsFault = prism
    (const OptionGroupAlreadyExistsFault)
    (\case
        OptionGroupAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The specified option group could not be found.
--
-- See: 'OptionGroupNotFoundFault'
_OptionGroupNotFoundFault :: Prism' RDSError ()
_OptionGroupNotFoundFault = prism
    (const OptionGroupNotFoundFault)
    (\case
        OptionGroupNotFoundFault -> Right ()
        x -> Left x)

-- | The quota of 20 option groups was exceeded for this AWS account.
--
-- See: 'OptionGroupQuotaExceededFault'
_OptionGroupQuotaExceededFault :: Prism' RDSError ()
_OptionGroupQuotaExceededFault = prism
    (const OptionGroupQuotaExceededFault)
    (\case
        OptionGroupQuotaExceededFault -> Right ()
        x -> Left x)

-- | SourceDBInstanceIdentifier refers to a DB instance with
-- BackupRetentionPeriod equal to 0.
--
-- See: 'PointInTimeRestoreNotEnabledFault'
_PointInTimeRestoreNotEnabledFault :: Prism' RDSError ()
_PointInTimeRestoreNotEnabledFault = prism
    (const PointInTimeRestoreNotEnabledFault)
    (\case
        PointInTimeRestoreNotEnabledFault -> Right ()
        x -> Left x)

-- | Provisioned IOPS not available in the specified Availability Zone.
--
-- See: 'ProvisionedIopsNotAvailableInAZFault'
_ProvisionedIopsNotAvailableInAZFault :: Prism' RDSError ()
_ProvisionedIopsNotAvailableInAZFault = prism
    (const ProvisionedIopsNotAvailableInAZFault)
    (\case
        ProvisionedIopsNotAvailableInAZFault -> Right ()
        x -> Left x)

-- | See: 'RDSClient'
_RDSClient :: Prism' RDSError HttpException
_RDSClient = prism
    RDSClient
    (\case
        RDSClient p1 -> Right p1
        x -> Left x)

-- | See: 'RDSSerializer'
_RDSSerializer :: Prism' RDSError String
_RDSSerializer = prism
    RDSSerializer
    (\case
        RDSSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'RDSService'
_RDSService :: Prism' RDSError String
_RDSService = prism
    RDSService
    (\case
        RDSService p1 -> Right p1
        x -> Left x)

-- | User already has a reservation with the given identifier.
--
-- See: 'ReservedDBInstanceAlreadyExistsFault'
_ReservedDBInstanceAlreadyExistsFault :: Prism' RDSError ()
_ReservedDBInstanceAlreadyExistsFault = prism
    (const ReservedDBInstanceAlreadyExistsFault)
    (\case
        ReservedDBInstanceAlreadyExistsFault -> Right ()
        x -> Left x)

-- | The specified reserved DB Instance not found.
--
-- See: 'ReservedDBInstanceNotFoundFault'
_ReservedDBInstanceNotFoundFault :: Prism' RDSError ()
_ReservedDBInstanceNotFoundFault = prism
    (const ReservedDBInstanceNotFoundFault)
    (\case
        ReservedDBInstanceNotFoundFault -> Right ()
        x -> Left x)

-- | Request would exceed the user's DB Instance quota.
--
-- See: 'ReservedDBInstanceQuotaExceededFault'
_ReservedDBInstanceQuotaExceededFault :: Prism' RDSError ()
_ReservedDBInstanceQuotaExceededFault = prism
    (const ReservedDBInstanceQuotaExceededFault)
    (\case
        ReservedDBInstanceQuotaExceededFault -> Right ()
        x -> Left x)

-- | Specified offering does not exist.
--
-- See: 'ReservedDBInstancesOfferingNotFoundFault'
_ReservedDBInstancesOfferingNotFoundFault :: Prism' RDSError ()
_ReservedDBInstancesOfferingNotFoundFault = prism
    (const ReservedDBInstancesOfferingNotFoundFault)
    (\case
        ReservedDBInstancesOfferingNotFoundFault -> Right ()
        x -> Left x)

-- | SNS has responded that there is a problem with the SND topic specified.
--
-- See: 'SNSInvalidTopicFault'
_SNSInvalidTopicFault :: Prism' RDSError ()
_SNSInvalidTopicFault = prism
    (const SNSInvalidTopicFault)
    (\case
        SNSInvalidTopicFault -> Right ()
        x -> Left x)

-- | You do not have permission to publish to the SNS topic ARN.
--
-- See: 'SNSNoAuthorizationFault'
_SNSNoAuthorizationFault :: Prism' RDSError ()
_SNSNoAuthorizationFault = prism
    (const SNSNoAuthorizationFault)
    (\case
        SNSNoAuthorizationFault -> Right ()
        x -> Left x)

-- | The SNS topic ARN does not exist.
--
-- See: 'SNSTopicArnNotFoundFault'
_SNSTopicArnNotFoundFault :: Prism' RDSError ()
_SNSTopicArnNotFoundFault = prism
    (const SNSTopicArnNotFoundFault)
    (\case
        SNSTopicArnNotFoundFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed number of DB snapshots.
--
-- See: 'SnapshotQuotaExceededFault'
_SnapshotQuotaExceededFault :: Prism' RDSError ()
_SnapshotQuotaExceededFault = prism
    (const SnapshotQuotaExceededFault)
    (\case
        SnapshotQuotaExceededFault -> Right ()
        x -> Left x)

-- | The requested source could not be found.
--
-- See: 'SourceNotFoundFault'
_SourceNotFoundFault :: Prism' RDSError ()
_SourceNotFoundFault = prism
    (const SourceNotFoundFault)
    (\case
        SourceNotFoundFault -> Right ()
        x -> Left x)

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
--
-- See: 'StorageQuotaExceededFault'
_StorageQuotaExceededFault :: Prism' RDSError ()
_StorageQuotaExceededFault = prism
    (const StorageQuotaExceededFault)
    (\case
        StorageQuotaExceededFault -> Right ()
        x -> Left x)

-- | The DB subnet is already in use in the Availability Zone.
--
-- See: 'SubnetAlreadyInUse'
_SubnetAlreadyInUse :: Prism' RDSError ()
_SubnetAlreadyInUse = prism
    (const SubnetAlreadyInUse)
    (\case
        SubnetAlreadyInUse -> Right ()
        x -> Left x)

-- | The supplied subscription name already exists.
--
-- See: 'SubscriptionAlreadyExistFault'
_SubscriptionAlreadyExistFault :: Prism' RDSError ()
_SubscriptionAlreadyExistFault = prism
    (const SubscriptionAlreadyExistFault)
    (\case
        SubscriptionAlreadyExistFault -> Right ()
        x -> Left x)

-- | The supplied category does not exist.
--
-- See: 'SubscriptionCategoryNotFoundFault'
_SubscriptionCategoryNotFoundFault :: Prism' RDSError ()
_SubscriptionCategoryNotFoundFault = prism
    (const SubscriptionCategoryNotFoundFault)
    (\case
        SubscriptionCategoryNotFoundFault -> Right ()
        x -> Left x)

-- | The subscription name does not exist.
--
-- See: 'SubscriptionNotFoundFault'
_SubscriptionNotFoundFault :: Prism' RDSError ()
_SubscriptionNotFoundFault = prism
    (const SubscriptionNotFoundFault)
    (\case
        SubscriptionNotFoundFault -> Right ()
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data ApplyMethod
    = ApplyMethodImmediate -- ^ immediate
    | ApplyMethodPendingReboot -- ^ pending-reboot
      deriving (Eq, Ord, Show, Generic)

instance Hashable ApplyMethod

instance FromText ApplyMethod where
    parser = match "immediate" ApplyMethodImmediate
         <|> match "pending-reboot" ApplyMethodPendingReboot

instance ToText ApplyMethod where
    toText ApplyMethodImmediate = "immediate"
    toText ApplyMethodPendingReboot = "pending-reboot"

instance ToByteString ApplyMethod

instance FromXML ApplyMethod where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplyMethod"

instance ToQuery ApplyMethod where
    toQuery = genericQuery def

data SourceType
    = SourceTypeDbInstance -- ^ db-instance
    | SourceTypeDbParameterGroup -- ^ db-parameter-group
    | SourceTypeDbSecurityGroup -- ^ db-security-group
    | SourceTypeDbSnapshot -- ^ db-snapshot
      deriving (Eq, Ord, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "db-instance" SourceTypeDbInstance
         <|> match "db-parameter-group" SourceTypeDbParameterGroup
         <|> match "db-security-group" SourceTypeDbSecurityGroup
         <|> match "db-snapshot" SourceTypeDbSnapshot

instance ToText SourceType where
    toText SourceTypeDbInstance = "db-instance"
    toText SourceTypeDbParameterGroup = "db-parameter-group"
    toText SourceTypeDbSecurityGroup = "db-security-group"
    toText SourceTypeDbSnapshot = "db-snapshot"

instance ToByteString SourceType

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType where
    toQuery = genericQuery def

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
data AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
    , _azProvisionedIopsCapable :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @ProvisionedIopsCapable ::@ @Maybe Bool@
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azName = Nothing
    , _azProvisionedIopsCapable = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

-- | True indicates the availability zone is capable of provisioned IOPs.
azProvisionedIopsCapable :: Lens' AvailabilityZone (Maybe Bool)
azProvisionedIopsCapable =
    lens _azProvisionedIopsCapable
         (\s a -> s { _azProvisionedIopsCapable = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
data CharacterSet = CharacterSet
    { _csCharacterSetName :: Maybe Text
    , _csCharacterSetDescription :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CharacterSet' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CharacterSetName ::@ @Maybe Text@
--
-- * @CharacterSetDescription ::@ @Maybe Text@
--
characterSet :: CharacterSet
characterSet = CharacterSet
    { _csCharacterSetName = Nothing
    , _csCharacterSetDescription = Nothing
    }

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName =
    lens _csCharacterSetName (\s a -> s { _csCharacterSetName = a })

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription =
    lens _csCharacterSetDescription
         (\s a -> s { _csCharacterSetDescription = a })

instance FromXML CharacterSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CharacterSet"

instance ToQuery CharacterSet where
    toQuery = genericQuery def

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
data DBEngineVersion = DBEngineVersion
    { _dbevEngine :: Maybe Text
    , _dbevEngineVersion :: Maybe Text
    , _dbevDBParameterGroupFamily :: Maybe Text
    , _dbevDBEngineDescription :: Maybe Text
    , _dbevDBEngineVersionDescription :: Maybe Text
    , _dbevDefaultCharacterSet :: Maybe CharacterSet
    , _dbevCharacterSet :: [CharacterSet]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBEngineVersion' data type.
--
-- 'DBEngineVersion' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @DBParameterGroupFamily ::@ @Maybe Text@
--
-- * @DBEngineDescription ::@ @Maybe Text@
--
-- * @DBEngineVersionDescription ::@ @Maybe Text@
--
-- * @DefaultCharacterSet ::@ @Maybe CharacterSet@
--
-- * @CharacterSet ::@ @[CharacterSet]@
--
dBEngineVersion :: DBEngineVersion
dBEngineVersion = DBEngineVersion
    { _dbevEngine = Nothing
    , _dbevEngineVersion = Nothing
    , _dbevDBParameterGroupFamily = Nothing
    , _dbevDBEngineDescription = Nothing
    , _dbevDBEngineVersionDescription = Nothing
    , _dbevDefaultCharacterSet = Nothing
    , _dbevCharacterSet = mempty
    }

-- | The name of the database engine.
dbevEngine :: Lens' DBEngineVersion (Maybe Text)
dbevEngine = lens _dbevEngine (\s a -> s { _dbevEngine = a })

-- | The version number of the database engine.
dbevEngineVersion :: Lens' DBEngineVersion (Maybe Text)
dbevEngineVersion =
    lens _dbevEngineVersion (\s a -> s { _dbevEngineVersion = a })

-- | The name of the DB parameter group family for the database engine.
dbevDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
dbevDBParameterGroupFamily =
    lens _dbevDBParameterGroupFamily
         (\s a -> s { _dbevDBParameterGroupFamily = a })

-- | The description of the database engine.
dbevDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineDescription =
    lens _dbevDBEngineDescription
         (\s a -> s { _dbevDBEngineDescription = a })

-- | The description of the database engine version.
dbevDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineVersionDescription =
    lens _dbevDBEngineVersionDescription
         (\s a -> s { _dbevDBEngineVersionDescription = a })

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
dbevDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
dbevDefaultCharacterSet =
    lens _dbevDefaultCharacterSet
         (\s a -> s { _dbevDefaultCharacterSet = a })

-- | A list of the character sets supported by this engine for the
-- CharacterSetName parameter of the CreateDBInstance API.
dbevCharacterSet :: Lens' DBEngineVersion [CharacterSet]
dbevCharacterSet =
    lens _dbevCharacterSet (\s a -> s { _dbevCharacterSet = a })

instance FromXML DBEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBEngineVersion"

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
data DBInstance = DBInstance
    { _dbiDBInstanceIdentifier :: Maybe Text
    , _dbiDBInstanceClass :: Maybe Text
    , _dbiEngine :: Maybe Text
    , _dbiDBInstanceStatus :: Maybe Text
    , _dbiMasterUsername :: Maybe Text
    , _dbiDBName :: Maybe Text
    , _dbiEndpoint :: Maybe Endpoint'
    , _dbiAllocatedStorage :: Maybe Integer
    , _dbiInstanceCreateTime :: Maybe ISO8601
    , _dbiPreferredBackupWindow :: Maybe Text
    , _dbiBackupRetentionPeriod :: Maybe Integer
    , _dbiDBSecurityGroup :: [DBSecurityGroupMembership]
    , _dbiVpcSecurityGroupMembership :: [VpcSecurityGroupMembership]
    , _dbiDBParameterGroup :: [DBParameterGroupStatus]
    , _dbiAvailabilityZone :: Maybe Text
    , _dbiDBSubnetGroup :: Maybe DBSubnetGroup
    , _dbiPreferredMaintenanceWindow :: Maybe Text
    , _dbiPendingModifiedValues :: Maybe PendingModifiedValues
    , _dbiLatestRestorableTime :: Maybe ISO8601
    , _dbiMultiAZ :: Maybe Bool
    , _dbiEngineVersion :: Maybe Text
    , _dbiAutoMinorVersionUpgrade :: Maybe Bool
    , _dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
    , _dbiReadReplicaDBInstanceIdentifier :: [Text]
    , _dbiLicenseModel :: Maybe Text
    , _dbiIops :: Maybe Integer
    , _dbiOptionGroupMembership :: [OptionGroupMembership]
    , _dbiCharacterSetName :: Maybe Text
    , _dbiSecondaryAvailabilityZone :: Maybe Text
    , _dbiPubliclyAccessible :: Maybe Bool
    , _dbiDBInstanceStatusInfo :: [DBInstanceStatusInfo]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBInstance' data type.
--
-- 'DBInstance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Maybe Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @DBInstanceStatus ::@ @Maybe Text@
--
-- * @MasterUsername ::@ @Maybe Text@
--
-- * @DBName ::@ @Maybe Text@
--
-- * @Endpoint ::@ @Maybe Endpoint'@
--
-- * @AllocatedStorage ::@ @Maybe Integer@
--
-- * @InstanceCreateTime ::@ @Maybe ISO8601@
--
-- * @PreferredBackupWindow ::@ @Maybe Text@
--
-- * @BackupRetentionPeriod ::@ @Maybe Integer@
--
-- * @DBSecurityGroup ::@ @[DBSecurityGroupMembership]@
--
-- * @VpcSecurityGroupMembership ::@ @[VpcSecurityGroupMembership]@
--
-- * @DBParameterGroup ::@ @[DBParameterGroupStatus]@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @DBSubnetGroup ::@ @Maybe DBSubnetGroup@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @PendingModifiedValues ::@ @Maybe PendingModifiedValues@
--
-- * @LatestRestorableTime ::@ @Maybe ISO8601@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @ReadReplicaSourceDBInstanceIdentifier ::@ @Maybe Text@
--
-- * @ReadReplicaDBInstanceIdentifier ::@ @[Text]@
--
-- * @LicenseModel ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @OptionGroupMembership ::@ @[OptionGroupMembership]@
--
-- * @CharacterSetName ::@ @Maybe Text@
--
-- * @SecondaryAvailabilityZone ::@ @Maybe Text@
--
-- * @PubliclyAccessible ::@ @Maybe Bool@
--
-- * @DBInstanceStatusInfo ::@ @[DBInstanceStatusInfo]@
--
dBInstance :: DBInstance
dBInstance = DBInstance
    { _dbiDBInstanceIdentifier = Nothing
    , _dbiDBInstanceClass = Nothing
    , _dbiEngine = Nothing
    , _dbiDBInstanceStatus = Nothing
    , _dbiMasterUsername = Nothing
    , _dbiDBName = Nothing
    , _dbiEndpoint = Nothing
    , _dbiAllocatedStorage = Nothing
    , _dbiInstanceCreateTime = Nothing
    , _dbiPreferredBackupWindow = Nothing
    , _dbiBackupRetentionPeriod = Nothing
    , _dbiDBSecurityGroup = mempty
    , _dbiVpcSecurityGroupMembership = mempty
    , _dbiDBParameterGroup = mempty
    , _dbiAvailabilityZone = Nothing
    , _dbiDBSubnetGroup = Nothing
    , _dbiPreferredMaintenanceWindow = Nothing
    , _dbiPendingModifiedValues = Nothing
    , _dbiLatestRestorableTime = Nothing
    , _dbiMultiAZ = Nothing
    , _dbiEngineVersion = Nothing
    , _dbiAutoMinorVersionUpgrade = Nothing
    , _dbiReadReplicaSourceDBInstanceIdentifier = Nothing
    , _dbiReadReplicaDBInstanceIdentifier = mempty
    , _dbiLicenseModel = Nothing
    , _dbiIops = Nothing
    , _dbiOptionGroupMembership = mempty
    , _dbiCharacterSetName = Nothing
    , _dbiSecondaryAvailabilityZone = Nothing
    , _dbiPubliclyAccessible = Nothing
    , _dbiDBInstanceStatusInfo = mempty
    }

-- | Contains a user-supplied database identifier. This is the unique key that
-- identifies a DB instance.
dbiDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiDBInstanceIdentifier =
    lens _dbiDBInstanceIdentifier
         (\s a -> s { _dbiDBInstanceIdentifier = a })

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbiDBInstanceClass :: Lens' DBInstance (Maybe Text)
dbiDBInstanceClass =
    lens _dbiDBInstanceClass (\s a -> s { _dbiDBInstanceClass = a })

-- | Provides the name of the database engine to be used for this DB instance.
dbiEngine :: Lens' DBInstance (Maybe Text)
dbiEngine = lens _dbiEngine (\s a -> s { _dbiEngine = a })

-- | Specifies the current state of this database.
dbiDBInstanceStatus :: Lens' DBInstance (Maybe Text)
dbiDBInstanceStatus =
    lens _dbiDBInstanceStatus (\s a -> s { _dbiDBInstanceStatus = a })

-- | Contains the master username for the DB instance.
dbiMasterUsername :: Lens' DBInstance (Maybe Text)
dbiMasterUsername =
    lens _dbiMasterUsername (\s a -> s { _dbiMasterUsername = a })

-- | The meaning of this parameter differs according to the database engine you
-- use. For example, this value returns only MySQL information when returning
-- values from CreateDBInstanceReadReplica since read replicas are only
-- supported for MySQL. MySQL Contains the name of the initial database of
-- this instance that was provided at create time, if one was specified when
-- the DB instance was created. This same name is returned for the life of the
-- DB instance. Type: String Oracle Contains the Oracle System ID (SID) of the
-- created DB instance. Not shown when the returned parameters do not apply to
-- an Oracle DB instance.
dbiDBName :: Lens' DBInstance (Maybe Text)
dbiDBName = lens _dbiDBName (\s a -> s { _dbiDBName = a })

-- | Specifies the connection endpoint.
dbiEndpoint :: Lens' DBInstance (Maybe Endpoint')
dbiEndpoint = lens _dbiEndpoint (\s a -> s { _dbiEndpoint = a })

-- | Specifies the allocated storage size specified in gigabytes.
dbiAllocatedStorage :: Lens' DBInstance (Maybe Integer)
dbiAllocatedStorage =
    lens _dbiAllocatedStorage (\s a -> s { _dbiAllocatedStorage = a })

-- | Provides the date and time the DB instance was created.
dbiInstanceCreateTime :: Lens' DBInstance (Maybe ISO8601)
dbiInstanceCreateTime =
    lens _dbiInstanceCreateTime (\s a -> s { _dbiInstanceCreateTime = a })

-- | Specifies the daily time range during which automated backups are created
-- if automated backups are enabled, as determined by the
-- BackupRetentionPeriod.
dbiPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredBackupWindow =
    lens _dbiPreferredBackupWindow
         (\s a -> s { _dbiPreferredBackupWindow = a })

-- | Specifies the number of days for which automatic DB snapshots are retained.
dbiBackupRetentionPeriod :: Lens' DBInstance (Maybe Integer)
dbiBackupRetentionPeriod =
    lens _dbiBackupRetentionPeriod
         (\s a -> s { _dbiBackupRetentionPeriod = a })

-- | Provides List of DB security group elements containing only
-- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
dbiDBSecurityGroup :: Lens' DBInstance [DBSecurityGroupMembership]
dbiDBSecurityGroup =
    lens _dbiDBSecurityGroup (\s a -> s { _dbiDBSecurityGroup = a })

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroupMembership :: Lens' DBInstance [VpcSecurityGroupMembership]
dbiVpcSecurityGroupMembership =
    lens _dbiVpcSecurityGroupMembership
         (\s a -> s { _dbiVpcSecurityGroupMembership = a })

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroup :: Lens' DBInstance [DBParameterGroupStatus]
dbiDBParameterGroup =
    lens _dbiDBParameterGroup (\s a -> s { _dbiDBParameterGroup = a })

-- | Specifies the name of the Availability Zone the DB instance is located in.
dbiAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiAvailabilityZone =
    lens _dbiAvailabilityZone (\s a -> s { _dbiAvailabilityZone = a })

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
dbiDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
dbiDBSubnetGroup =
    lens _dbiDBSubnetGroup (\s a -> s { _dbiDBSubnetGroup = a })

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
dbiPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredMaintenanceWindow =
    lens _dbiPreferredMaintenanceWindow
         (\s a -> s { _dbiPreferredMaintenanceWindow = a })

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
dbiPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
dbiPendingModifiedValues =
    lens _dbiPendingModifiedValues
         (\s a -> s { _dbiPendingModifiedValues = a })

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbiLatestRestorableTime :: Lens' DBInstance (Maybe ISO8601)
dbiLatestRestorableTime =
    lens _dbiLatestRestorableTime
         (\s a -> s { _dbiLatestRestorableTime = a })

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbiMultiAZ :: Lens' DBInstance (Maybe Bool)
dbiMultiAZ = lens _dbiMultiAZ (\s a -> s { _dbiMultiAZ = a })

-- | Indicates the database engine version.
dbiEngineVersion :: Lens' DBInstance (Maybe Text)
dbiEngineVersion =
    lens _dbiEngineVersion (\s a -> s { _dbiEngineVersion = a })

-- | Indicates that minor version patches are applied automatically.
dbiAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
dbiAutoMinorVersionUpgrade =
    lens _dbiAutoMinorVersionUpgrade
         (\s a -> s { _dbiAutoMinorVersionUpgrade = a })

-- | Contains the identifier of the source DB instance if this DB instance is a
-- read replica.
dbiReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiReadReplicaSourceDBInstanceIdentifier =
    lens _dbiReadReplicaSourceDBInstanceIdentifier
         (\s a -> s { _dbiReadReplicaSourceDBInstanceIdentifier = a })

-- | Contains one or more identifiers of the read replicas associated with this
-- DB instance.
dbiReadReplicaDBInstanceIdentifier :: Lens' DBInstance [Text]
dbiReadReplicaDBInstanceIdentifier =
    lens _dbiReadReplicaDBInstanceIdentifier
         (\s a -> s { _dbiReadReplicaDBInstanceIdentifier = a })

-- | License model information for this DB instance.
dbiLicenseModel :: Lens' DBInstance (Maybe Text)
dbiLicenseModel = lens _dbiLicenseModel (\s a -> s { _dbiLicenseModel = a })

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops :: Lens' DBInstance (Maybe Integer)
dbiIops = lens _dbiIops (\s a -> s { _dbiIops = a })

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMembership :: Lens' DBInstance [OptionGroupMembership]
dbiOptionGroupMembership =
    lens _dbiOptionGroupMembership
         (\s a -> s { _dbiOptionGroupMembership = a })

-- | If present, specifies the name of the character set that this instance is
-- associated with.
dbiCharacterSetName :: Lens' DBInstance (Maybe Text)
dbiCharacterSetName =
    lens _dbiCharacterSetName (\s a -> s { _dbiCharacterSetName = a })

-- | If present, specifies the name of the secondary Availability Zone for a DB
-- instance with multi-AZ support.
dbiSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiSecondaryAvailabilityZone =
    lens _dbiSecondaryAvailabilityZone
         (\s a -> s { _dbiSecondaryAvailabilityZone = a })

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS name,
-- which resolves to a public IP address. A value of false specifies an
-- internal instance with a DNS name that resolves to a private IP address.
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case. Default VPC:true VPC:false If no DB subnet group has been specified
-- as part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be publicly accessible. If a specific DB subnet group
-- has been specified as part of the request and the PubliclyAccessible value
-- has not been set, the DB instance will be private.
dbiPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
dbiPubliclyAccessible =
    lens _dbiPubliclyAccessible (\s a -> s { _dbiPubliclyAccessible = a })

-- | The status of a read replica. If the instance is not a read replica, this
-- will be blank.
dbiDBInstanceStatusInfo :: Lens' DBInstance [DBInstanceStatusInfo]
dbiDBInstanceStatusInfo =
    lens _dbiDBInstanceStatusInfo
         (\s a -> s { _dbiDBInstanceStatusInfo = a })

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstance"

-- | Provides a list of status information for a DB instance.
data DBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType :: Maybe Text
    , _dbisiNormal :: Maybe Bool
    , _dbisiStatus :: Maybe Text
    , _dbisiMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBInstanceStatusInfo' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StatusType ::@ @Maybe Text@
--
-- * @Normal ::@ @Maybe Bool@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Message ::@ @Maybe Text@
--
dBInstanceStatusInfo :: DBInstanceStatusInfo
dBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType = Nothing
    , _dbisiNormal = Nothing
    , _dbisiStatus = Nothing
    , _dbisiMessage = Nothing
    }

-- | This value is currently "read replication.".
dbisiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatusType = lens _dbisiStatusType (\s a -> s { _dbisiStatusType = a })

-- | Boolean value that is true if the instance is operating normally, or false
-- if the instance is in an error state.
dbisiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
dbisiNormal = lens _dbisiNormal (\s a -> s { _dbisiNormal = a })

-- | Status of the DB instance. For a StatusType of read replica, the values can
-- be replicating, error, stopped, or terminated.
dbisiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatus = lens _dbisiStatus (\s a -> s { _dbisiStatus = a })

-- | Details of the error if there is an error for the instance. If the instance
-- is not in an error state, this value is blank.
dbisiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiMessage = lens _dbisiMessage (\s a -> s { _dbisiMessage = a })

instance FromXML DBInstanceStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstanceStatusInfo"

instance ToQuery DBInstanceStatusInfo where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action. This data type is used as a request
-- parameter in the DeleteDBParameterGroup action, and as a response element
-- in the DescribeDBParameterGroups action.
data DBParameterGroup = DBParameterGroup
    { _dbpgDBParameterGroupName :: Maybe Text
    , _dbpgDBParameterGroupFamily :: Maybe Text
    , _dbpgDescription :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBParameterGroup' data type.
--
-- 'DBParameterGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Maybe Text@
--
-- * @DBParameterGroupFamily ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
dBParameterGroup :: DBParameterGroup
dBParameterGroup = DBParameterGroup
    { _dbpgDBParameterGroupName = Nothing
    , _dbpgDBParameterGroupFamily = Nothing
    , _dbpgDescription = Nothing
    }

-- | Provides the name of the DB parameter group.
dbpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupName =
    lens _dbpgDBParameterGroupName
         (\s a -> s { _dbpgDBParameterGroupName = a })

-- | Provides the name of the DB parameter group family that this DB parameter
-- group is compatible with.
dbpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupFamily =
    lens _dbpgDBParameterGroupFamily
         (\s a -> s { _dbpgDBParameterGroupFamily = a })

-- | Provides the customer-specified description for this DB parameter group.
dbpgDescription :: Lens' DBParameterGroup (Maybe Text)
dbpgDescription = lens _dbpgDescription (\s a -> s { _dbpgDescription = a })

instance FromXML DBParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

-- | The status of the DB parameter group. This data type is used as a response
-- element in the following actions: CreateDBInstance
-- CreateDBInstanceReadReplica DeleteDBInstance ModifyDBInstance
-- RebootDBInstance RestoreDBInstanceFromDBSnapshot.
data DBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName :: Maybe Text
    , _dbpgsParameterApplyStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBParameterGroupStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterApplyStatus ::@ @Maybe Text@
--
dBParameterGroupStatus :: DBParameterGroupStatus
dBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName = Nothing
    , _dbpgsParameterApplyStatus = Nothing
    }

-- | The name of the DP parameter group.
dbpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dbpgsDBParameterGroupName =
    lens _dbpgsDBParameterGroupName
         (\s a -> s { _dbpgsDBParameterGroupName = a })

-- | The status of parameter updates.
dbpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dbpgsParameterApplyStatus =
    lens _dbpgsParameterApplyStatus
         (\s a -> s { _dbpgsParameterApplyStatus = a })

instance FromXML DBParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

instance ToQuery DBParameterGroupStatus where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
data DBSecurityGroup = DBSecurityGroup
    { _dbsgOwnerId :: Maybe Text
    , _dbsgDBSecurityGroupName :: Maybe Text
    , _dbsgDBSecurityGroupDescription :: Maybe Text
    , _dbsgVpcId :: Maybe Text
    , _dbsgEC2SecurityGroup :: [EC2SecurityGroup]
    , _dbsgIPRange :: [IPRange]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSecurityGroup' data type.
--
-- 'DBSecurityGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @DBSecurityGroupName ::@ @Maybe Text@
--
-- * @DBSecurityGroupDescription ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @EC2SecurityGroup ::@ @[EC2SecurityGroup]@
--
-- * @IPRange ::@ @[IPRange]@
--
dBSecurityGroup :: DBSecurityGroup
dBSecurityGroup = DBSecurityGroup
    { _dbsgOwnerId = Nothing
    , _dbsgDBSecurityGroupName = Nothing
    , _dbsgDBSecurityGroupDescription = Nothing
    , _dbsgVpcId = Nothing
    , _dbsgEC2SecurityGroup = mempty
    , _dbsgIPRange = mempty
    }

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId = lens _dbsgOwnerId (\s a -> s { _dbsgOwnerId = a })

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName =
    lens _dbsgDBSecurityGroupName
         (\s a -> s { _dbsgDBSecurityGroupName = a })

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription =
    lens _dbsgDBSecurityGroupDescription
         (\s a -> s { _dbsgDBSecurityGroupDescription = a })

-- | Provides the VpcId of the DB security group.
dbsgVpcId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVpcId = lens _dbsgVpcId (\s a -> s { _dbsgVpcId = a })

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroup :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroup =
    lens _dbsgEC2SecurityGroup (\s a -> s { _dbsgEC2SecurityGroup = a })

-- | Contains a list of IPRange elements.
dbsgIPRange :: Lens' DBSecurityGroup [IPRange]
dbsgIPRange = lens _dbsgIPRange (\s a -> s { _dbsgIPRange = a })

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

-- | This data type is used as a response element in the following actions:
-- ModifyDBInstance RebootDBInstance RestoreDBInstanceFromDBSnapshot
-- RestoreDBInstanceToPointInTime.
data DBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName :: Maybe Text
    , _dbsgmStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroupName ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
dBSecurityGroupMembership :: DBSecurityGroupMembership
dBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName = Nothing
    , _dbsgmStatus = Nothing
    }

-- | The name of the DB security group.
dbsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dbsgmDBSecurityGroupName =
    lens _dbsgmDBSecurityGroupName
         (\s a -> s { _dbsgmDBSecurityGroupName = a })

-- | The status of the DB security group.
dbsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dbsgmStatus = lens _dbsgmStatus (\s a -> s { _dbsgmStatus = a })

instance FromXML DBSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

instance ToQuery DBSecurityGroupMembership where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
data DBSnapshot = DBSnapshot
    { _dbsDBSnapshotIdentifier :: Maybe Text
    , _dbsDBInstanceIdentifier :: Maybe Text
    , _dbsSnapshotCreateTime :: Maybe ISO8601
    , _dbsEngine :: Maybe Text
    , _dbsAllocatedStorage :: Maybe Integer
    , _dbsStatus :: Maybe Text
    , _dbsPort :: Maybe Integer
    , _dbsAvailabilityZone :: Maybe Text
    , _dbsVpcId :: Maybe Text
    , _dbsInstanceCreateTime :: Maybe ISO8601
    , _dbsMasterUsername :: Maybe Text
    , _dbsEngineVersion :: Maybe Text
    , _dbsLicenseModel :: Maybe Text
    , _dbsSnapshotType :: Maybe Text
    , _dbsIops :: Maybe Integer
    , _dbsOptionGroupName :: Maybe Text
    , _dbsPercentProgress :: Maybe Integer
    , _dbsSourceRegion :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSnapshot' data type.
--
-- 'DBSnapshot' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSnapshotIdentifier ::@ @Maybe Text@
--
-- * @DBInstanceIdentifier ::@ @Maybe Text@
--
-- * @SnapshotCreateTime ::@ @Maybe ISO8601@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @AllocatedStorage ::@ @Maybe Integer@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @InstanceCreateTime ::@ @Maybe ISO8601@
--
-- * @MasterUsername ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @LicenseModel ::@ @Maybe Text@
--
-- * @SnapshotType ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @PercentProgress ::@ @Maybe Integer@
--
-- * @SourceRegion ::@ @Maybe Text@
--
dBSnapshot :: DBSnapshot
dBSnapshot = DBSnapshot
    { _dbsDBSnapshotIdentifier = Nothing
    , _dbsDBInstanceIdentifier = Nothing
    , _dbsSnapshotCreateTime = Nothing
    , _dbsEngine = Nothing
    , _dbsAllocatedStorage = Nothing
    , _dbsStatus = Nothing
    , _dbsPort = Nothing
    , _dbsAvailabilityZone = Nothing
    , _dbsVpcId = Nothing
    , _dbsInstanceCreateTime = Nothing
    , _dbsMasterUsername = Nothing
    , _dbsEngineVersion = Nothing
    , _dbsLicenseModel = Nothing
    , _dbsSnapshotType = Nothing
    , _dbsIops = Nothing
    , _dbsOptionGroupName = Nothing
    , _dbsPercentProgress = Nothing
    , _dbsSourceRegion = Nothing
    }

-- | Specifies the identifier for the DB snapshot.
dbsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBSnapshotIdentifier =
    lens _dbsDBSnapshotIdentifier
         (\s a -> s { _dbsDBSnapshotIdentifier = a })

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBInstanceIdentifier =
    lens _dbsDBInstanceIdentifier
         (\s a -> s { _dbsDBInstanceIdentifier = a })

-- | Provides the time (UTC) when the snapshot was taken.
dbsSnapshotCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsSnapshotCreateTime =
    lens _dbsSnapshotCreateTime (\s a -> s { _dbsSnapshotCreateTime = a })

-- | Specifies the name of the database engine.
dbsEngine :: Lens' DBSnapshot (Maybe Text)
dbsEngine = lens _dbsEngine (\s a -> s { _dbsEngine = a })

-- | Specifies the allocated storage size in gigabytes (GB).
dbsAllocatedStorage :: Lens' DBSnapshot (Maybe Integer)
dbsAllocatedStorage =
    lens _dbsAllocatedStorage (\s a -> s { _dbsAllocatedStorage = a })

-- | Specifies the status of this DB snapshot.
dbsStatus :: Lens' DBSnapshot (Maybe Text)
dbsStatus = lens _dbsStatus (\s a -> s { _dbsStatus = a })

-- | Specifies the port that the database engine was listening on at the time of
-- the snapshot.
dbsPort :: Lens' DBSnapshot (Maybe Integer)
dbsPort = lens _dbsPort (\s a -> s { _dbsPort = a })

-- | Specifies the name of the Availability Zone the DB instance was located in
-- at the time of the DB snapshot.
dbsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dbsAvailabilityZone =
    lens _dbsAvailabilityZone (\s a -> s { _dbsAvailabilityZone = a })

-- | Provides the Vpc Id associated with the DB snapshot.
dbsVpcId :: Lens' DBSnapshot (Maybe Text)
dbsVpcId = lens _dbsVpcId (\s a -> s { _dbsVpcId = a })

-- | Specifies the time (UTC) when the snapshot was taken.
dbsInstanceCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsInstanceCreateTime =
    lens _dbsInstanceCreateTime (\s a -> s { _dbsInstanceCreateTime = a })

-- | Provides the master username for the DB snapshot.
dbsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dbsMasterUsername =
    lens _dbsMasterUsername (\s a -> s { _dbsMasterUsername = a })

-- | Specifies the version of the database engine.
dbsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dbsEngineVersion =
    lens _dbsEngineVersion (\s a -> s { _dbsEngineVersion = a })

-- | License model information for the restored DB instance.
dbsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dbsLicenseModel = lens _dbsLicenseModel (\s a -> s { _dbsLicenseModel = a })

-- | Provides the type of the DB snapshot.
dbsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dbsSnapshotType = lens _dbsSnapshotType (\s a -> s { _dbsSnapshotType = a })

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB
-- instance at the time of the snapshot.
dbsIops :: Lens' DBSnapshot (Maybe Integer)
dbsIops = lens _dbsIops (\s a -> s { _dbsIops = a })

-- | Provides the option group name for the DB snapshot.
dbsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dbsOptionGroupName =
    lens _dbsOptionGroupName (\s a -> s { _dbsOptionGroupName = a })

-- | The percentage of the estimated data that has been transferred.
dbsPercentProgress :: Lens' DBSnapshot (Maybe Integer)
dbsPercentProgress =
    lens _dbsPercentProgress (\s a -> s { _dbsPercentProgress = a })

-- | The region that the DB snapshot was created in or copied from.
dbsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dbsSourceRegion = lens _dbsSourceRegion (\s a -> s { _dbsSourceRegion = a })

instance FromXML DBSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSnapshot"

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSubnetGroup ModifyDBSubnetGroup DescribeDBSubnetGroups
-- DeleteDBSubnetGroup This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
data DBSubnetGroup = DBSubnetGroup
    { _dbsgrDBSubnetGroupName :: Maybe Text
    , _dbsgrDBSubnetGroupDescription :: Maybe Text
    , _dbsgrVpcId :: Maybe Text
    , _dbsgrSubnetGroupStatus :: Maybe Text
    , _dbsgrSubnet :: [Subnet]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSubnetGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSubnetGroupName ::@ @Maybe Text@
--
-- * @DBSubnetGroupDescription ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @SubnetGroupStatus ::@ @Maybe Text@
--
-- * @Subnet ::@ @[Subnet]@
--
dBSubnetGroup :: DBSubnetGroup
dBSubnetGroup = DBSubnetGroup
    { _dbsgrDBSubnetGroupName = Nothing
    , _dbsgrDBSubnetGroupDescription = Nothing
    , _dbsgrVpcId = Nothing
    , _dbsgrSubnetGroupStatus = Nothing
    , _dbsgrSubnet = mempty
    }

-- | Specifies the name of the DB subnet group.
dbsgrDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dbsgrDBSubnetGroupName =
    lens _dbsgrDBSubnetGroupName (\s a -> s { _dbsgrDBSubnetGroupName = a })

-- | Provides the description of the DB subnet group.
dbsgrDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dbsgrDBSubnetGroupDescription =
    lens _dbsgrDBSubnetGroupDescription
         (\s a -> s { _dbsgrDBSubnetGroupDescription = a })

-- | Provides the VpcId of the DB subnet group.
dbsgrVpcId :: Lens' DBSubnetGroup (Maybe Text)
dbsgrVpcId = lens _dbsgrVpcId (\s a -> s { _dbsgrVpcId = a })

-- | Provides the status of the DB subnet group.
dbsgrSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dbsgrSubnetGroupStatus =
    lens _dbsgrSubnetGroupStatus (\s a -> s { _dbsgrSubnetGroupStatus = a })

-- | Contains a list of Subnet elements.
dbsgrSubnet :: Lens' DBSubnetGroup [Subnet]
dbsgrSubnet = lens _dbsgrSubnet (\s a -> s { _dbsgrSubnet = a })

instance FromXML DBSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSubnetGroup"

instance ToQuery DBSubnetGroup where
    toQuery = genericQuery def

-- | This data type is used as a response element to DescribeDBLogFiles.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLogFileName :: Maybe Text
    , _ddblfdLastWritten :: Maybe Integer
    , _ddblfdSize :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DescribeDBLogFilesDetails' data type.
--
-- 'DescribeDBLogFilesDetails' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogFileName ::@ @Maybe Text@
--
-- * @LastWritten ::@ @Maybe Integer@
--
-- * @Size ::@ @Maybe Integer@
--
describeDBLogFilesDetails :: DescribeDBLogFilesDetails
describeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLogFileName = Nothing
    , _ddblfdLastWritten = Nothing
    , _ddblfdSize = Nothing
    }

-- | The name of the log file for the specified DB instance.
ddblfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddblfdLogFileName =
    lens _ddblfdLogFileName (\s a -> s { _ddblfdLogFileName = a })

-- | A POSIX timestamp when the last log entry was written.
ddblfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdLastWritten =
    lens _ddblfdLastWritten (\s a -> s { _ddblfdLastWritten = a })

-- | The size, in bytes, of the log file for the specified DB instance.
ddblfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdSize = lens _ddblfdSize (\s a -> s { _ddblfdSize = a })

instance FromXML DescribeDBLogFilesDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDBLogFilesDetails"

-- | This data type is used as a response element in the following actions:
-- AuthorizeDBSecurityGroupIngress DescribeDBSecurityGroups
-- RevokeDBSecurityGroupIngress.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
    , _ecsgEC2SecurityGroupName :: Maybe Text
    , _ecsgEC2SecurityGroupId :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @EC2SecurityGroupName ::@ @Maybe Text@
--
-- * @EC2SecurityGroupId ::@ @Maybe Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Maybe Text@
--
eC2SecurityGroup :: EC2SecurityGroup
eC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupId = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }

-- | Provides the status of the EC2 security group. Status can be "authorizing",
-- "authorized", "revoking", and "revoked".
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

-- | Specifies the name of the EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName =
    lens _ecsgEC2SecurityGroupName
         (\s a -> s { _ecsgEC2SecurityGroupName = a })

-- | Specifies the id of the EC2 security group.
ecsgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupId =
    lens _ecsgEC2SecurityGroupId (\s a -> s { _ecsgEC2SecurityGroupId = a })

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId =
    lens _ecsgEC2SecurityGroupOwnerId
         (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Specifies the connection endpoint.
data Endpoint' = Endpoint'
    { _eAddress :: Maybe Text
    , _ePort :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint'' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Address ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
endpoint' :: Endpoint'
endpoint' = Endpoint'
    { _eAddress = Nothing
    , _ePort = Nothing
    }

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint' (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint' (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint' where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint'"

instance ToQuery Endpoint' where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
data EngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily :: Maybe Text
    , _edMarker :: Maybe Text
    , _edParameter :: [Parameter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EngineDefaults' data type.
--
-- 'EngineDefaults' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupFamily ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Parameter ::@ @[Parameter]@
--
engineDefaults :: EngineDefaults
engineDefaults = EngineDefaults
    { _edDBParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameter = mempty
    }

-- | Specifies the name of the DB parameter group family which the engine
-- default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily =
    lens _edDBParameterGroupFamily
         (\s a -> s { _edDBParameterGroupFamily = a })

-- | An optional pagination token provided by a previous EngineDefaults request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s { _edMarker = a })

-- | Contains a list of engine default parameters.
edParameter :: Lens' EngineDefaults [Parameter]
edParameter = lens _edParameter (\s a -> s { _edParameter = a })

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | This data type is used as a response element in the DescribeEvents action.
data Event = Event
    { _erSourceIdentifier :: Maybe Text
    , _erSourceType :: Maybe SourceType
    , _erMessage :: Maybe Text
    , _erEventCategory :: [Text]
    , _erDate :: Maybe ISO8601
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Event' data type.
--
-- 'Event' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceIdentifier ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe SourceType@
--
-- * @Message ::@ @Maybe Text@
--
-- * @EventCategory ::@ @[Text]@
--
-- * @Date ::@ @Maybe ISO8601@
--
event :: Event
event = Event
    { _erSourceIdentifier = Nothing
    , _erSourceType = Nothing
    , _erMessage = Nothing
    , _erEventCategory = mempty
    , _erDate = Nothing
    }

-- | Provides the identifier for the source of the event.
erSourceIdentifier :: Lens' Event (Maybe Text)
erSourceIdentifier =
    lens _erSourceIdentifier (\s a -> s { _erSourceIdentifier = a })

-- | Specifies the source type for this event.
erSourceType :: Lens' Event (Maybe SourceType)
erSourceType = lens _erSourceType (\s a -> s { _erSourceType = a })

-- | Provides the text of this event.
erMessage :: Lens' Event (Maybe Text)
erMessage = lens _erMessage (\s a -> s { _erMessage = a })

-- | Specifies the category for the event.
erEventCategory :: Lens' Event [Text]
erEventCategory = lens _erEventCategory (\s a -> s { _erEventCategory = a })

-- | Specifies the date and time of the event.
erDate :: Lens' Event (Maybe ISO8601)
erDate = lens _erDate (\s a -> s { _erDate = a })

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
data EventCategoriesMap = EventCategoriesMap
    { _ecmSourceType :: Maybe Text
    , _ecmEventCategory :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventCategoriesMap' data type.
--
-- 'EventCategoriesMap' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @EventCategory ::@ @[Text]@
--
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap = EventCategoriesMap
    { _ecmSourceType = Nothing
    , _ecmEventCategory = mempty
    }

-- | The source type that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s { _ecmSourceType = a })

-- | The event categories for the specified source type.
ecmEventCategory :: Lens' EventCategoriesMap [Text]
ecmEventCategory =
    lens _ecmEventCategory (\s a -> s { _ecmEventCategory = a })

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
data EventSubscription = EventSubscription
    { _esCustomerAwsId :: Maybe Text
    , _esCustSubscriptionId :: Maybe Text
    , _esSnsTopicArn :: Maybe Text
    , _esStatus :: Maybe Text
    , _esSubscriptionCreationTime :: Maybe Text
    , _esSourceType :: Maybe Text
    , _esSourceId :: [Text]
    , _esEventCategory :: [Text]
    , _esEnabled :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventSubscription' data type.
--
-- 'EventSubscription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CustomerAwsId ::@ @Maybe Text@
--
-- * @CustSubscriptionId ::@ @Maybe Text@
--
-- * @SnsTopicArn ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @SubscriptionCreationTime ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @SourceId ::@ @[Text]@
--
-- * @EventCategory ::@ @[Text]@
--
-- * @Enabled ::@ @Maybe Bool@
--
eventSubscription :: EventSubscription
eventSubscription = EventSubscription
    { _esCustomerAwsId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSnsTopicArn = Nothing
    , _esStatus = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esSourceType = Nothing
    , _esSourceId = mempty
    , _esEventCategory = mempty
    , _esEnabled = Nothing
    }

-- | The AWS customer account associated with the RDS event notification
-- subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId =
    lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })

-- | The topic ARN of the RDS event notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })

-- | The status of the RDS event notification subscription. Constraints: Can be
-- one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist The status "no-permission" indicates that
-- RDS no longer has permission to post to the SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\s a -> s { _esStatus = a })

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime =
    lens _esSubscriptionCreationTime
         (\s a -> s { _esSubscriptionCreationTime = a })

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })

-- | A list of source Ids for the RDS event notification subscription.
esSourceId :: Lens' EventSubscription [Text]
esSourceId = lens _esSourceId (\s a -> s { _esSourceId = a })

-- | A list of event categories for the RDS event notification subscription.
esEventCategory :: Lens' EventSubscription [Text]
esEventCategory = lens _esEventCategory (\s a -> s { _esEventCategory = a })

-- | A Boolean value indicating if the subscription is enabled. True indicates
-- the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
data IPRange = IPRange
    { _iprStatus :: Maybe Text
    , _iprCIDRIP :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IPRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @CIDRIP ::@ @Maybe Text@
--
iPRange :: IPRange
iPRange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    }

-- | Specifies the status of the IP range. Status can be "authorizing",
-- "authorized", "revoking", and "revoked".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })

-- | Specifies the IP range.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericQuery def

-- | Option details.
data Option = Option
    { _oOptionName :: Maybe Text
    , _oOptionDescription :: Maybe Text
    , _oPersistent :: Maybe Bool
    , _oPermanent :: Maybe Bool
    , _oPort :: Maybe Integer
    , _oOptionSetting :: [OptionSetting]
    , _oDBSecurityGroup :: [DBSecurityGroupMembership]
    , _oVpcSecurityGroupMembership :: [VpcSecurityGroupMembership]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Option' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionName ::@ @Maybe Text@
--
-- * @OptionDescription ::@ @Maybe Text@
--
-- * @Persistent ::@ @Maybe Bool@
--
-- * @Permanent ::@ @Maybe Bool@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @OptionSetting ::@ @[OptionSetting]@
--
-- * @DBSecurityGroup ::@ @[DBSecurityGroupMembership]@
--
-- * @VpcSecurityGroupMembership ::@ @[VpcSecurityGroupMembership]@
--
option :: Option
option = Option
    { _oOptionName = Nothing
    , _oOptionDescription = Nothing
    , _oPersistent = Nothing
    , _oPermanent = Nothing
    , _oPort = Nothing
    , _oOptionSetting = mempty
    , _oDBSecurityGroup = mempty
    , _oVpcSecurityGroupMembership = mempty
    }

-- | The name of the option.
oOptionName :: Lens' Option (Maybe Text)
oOptionName = lens _oOptionName (\s a -> s { _oOptionName = a })

-- | The description of the option.
oOptionDescription :: Lens' Option (Maybe Text)
oOptionDescription =
    lens _oOptionDescription (\s a -> s { _oOptionDescription = a })

-- | Indicate if this option is persistent.
oPersistent :: Lens' Option (Maybe Bool)
oPersistent = lens _oPersistent (\s a -> s { _oPersistent = a })

-- | Indicate if this option is permanent.
oPermanent :: Lens' Option (Maybe Bool)
oPermanent = lens _oPermanent (\s a -> s { _oPermanent = a })

-- | If required, the port configured for this option to use.
oPort :: Lens' Option (Maybe Integer)
oPort = lens _oPort (\s a -> s { _oPort = a })

-- | The option settings for this option.
oOptionSetting :: Lens' Option [OptionSetting]
oOptionSetting = lens _oOptionSetting (\s a -> s { _oOptionSetting = a })

-- | If the option requires access to a port, then this DB security group allows
-- access to the port.
oDBSecurityGroup :: Lens' Option [DBSecurityGroupMembership]
oDBSecurityGroup =
    lens _oDBSecurityGroup (\s a -> s { _oDBSecurityGroup = a })

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
oVpcSecurityGroupMembership :: Lens' Option [VpcSecurityGroupMembership]
oVpcSecurityGroupMembership =
    lens _oVpcSecurityGroupMembership
         (\s a -> s { _oVpcSecurityGroupMembership = a })

instance FromXML Option where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Option"

instance ToQuery Option where
    toQuery = genericQuery def

-- | A list of all available options.
data OptionConfiguration = OptionConfiguration
    { _ocOptionName :: Text
    , _ocPort :: Maybe Integer
    , _ocDBSecurityGroupName :: [Text]
    , _ocVpcSecurityGroupId :: [Text]
    , _ocOptionSetting :: [OptionSetting]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionName ::@ @Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @DBSecurityGroupName ::@ @[Text]@
--
-- * @VpcSecurityGroupId ::@ @[Text]@
--
-- * @OptionSetting ::@ @[OptionSetting]@
--
optionConfiguration :: Text -- ^ 'ocOptionName'
                    -> OptionConfiguration
optionConfiguration p1 = OptionConfiguration
    { _ocOptionName = p1
    , _ocPort = Nothing
    , _ocDBSecurityGroupName = mempty
    , _ocVpcSecurityGroupId = mempty
    , _ocOptionSetting = mempty
    }

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\s a -> s { _ocOptionName = a })

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Integer)
ocPort = lens _ocPort (\s a -> s { _ocPort = a })

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupName :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupName =
    lens _ocDBSecurityGroupName (\s a -> s { _ocDBSecurityGroupName = a })

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupId :: Lens' OptionConfiguration [Text]
ocVpcSecurityGroupId =
    lens _ocVpcSecurityGroupId (\s a -> s { _ocVpcSecurityGroupId = a })

-- | The option settings to include in an option group.
ocOptionSetting :: Lens' OptionConfiguration [OptionSetting]
ocOptionSetting = lens _ocOptionSetting (\s a -> s { _ocOptionSetting = a })

instance ToQuery OptionConfiguration where
    toQuery = genericQuery def

-- | 
data OptionGroup = OptionGroup
    { _ogOptionGroupName :: Maybe Text
    , _ogOptionGroupDescription :: Maybe Text
    , _ogEngineName :: Maybe Text
    , _ogMajorEngineVersion :: Maybe Text
    , _ogOption :: [Option]
    , _ogAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
    , _ogVpcId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroup' data type.
--
-- 'OptionGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @OptionGroupDescription ::@ @Maybe Text@
--
-- * @EngineName ::@ @Maybe Text@
--
-- * @MajorEngineVersion ::@ @Maybe Text@
--
-- * @Option ::@ @[Option]@
--
-- * @AllowsVpcAndNonVpcInstanceMemberships ::@ @Maybe Bool@
--
-- * @VpcId ::@ @Maybe Text@
--
optionGroup :: OptionGroup
optionGroup = OptionGroup
    { _ogOptionGroupName = Nothing
    , _ogOptionGroupDescription = Nothing
    , _ogEngineName = Nothing
    , _ogMajorEngineVersion = Nothing
    , _ogOption = mempty
    , _ogAllowsVpcAndNonVpcInstanceMemberships = Nothing
    , _ogVpcId = Nothing
    }

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName =
    lens _ogOptionGroupName (\s a -> s { _ogOptionGroupName = a })

-- | Provides the description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription =
    lens _ogOptionGroupDescription
         (\s a -> s { _ogOptionGroupDescription = a })

-- | Engine name that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\s a -> s { _ogEngineName = a })

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion =
    lens _ogMajorEngineVersion (\s a -> s { _ogMajorEngineVersion = a })

-- | Indicates what options are available in the option group.
ogOption :: Lens' OptionGroup [Option]
ogOption = lens _ogOption (\s a -> s { _ogOption = a })

-- | Indicates whether this option group can be applied to both VPC and non-VPC
-- instances. The value 'true' indicates the option group can be applied to
-- both VPC and non-VPC instances.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVpcAndNonVpcInstanceMemberships =
    lens _ogAllowsVpcAndNonVpcInstanceMemberships
         (\s a -> s { _ogAllowsVpcAndNonVpcInstanceMemberships = a })

-- | If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
-- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is blank,
-- then this option group can be applied to both VPC and non-VPC instances. If
-- this field contains a value, then this option group can only be applied to
-- instances that are in the VPC indicated by this field.
ogVpcId :: Lens' OptionGroup (Maybe Text)
ogVpcId = lens _ogVpcId (\s a -> s { _ogVpcId = a })

instance FromXML OptionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroup"

-- | Provides information on the option groups the DB instance is a member of.
data OptionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName :: Maybe Text
    , _ogmStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
optionGroupMembership :: OptionGroupMembership
optionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName = Nothing
    , _ogmStatus = Nothing
    }

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName =
    lens _ogmOptionGroupName (\s a -> s { _ogmOptionGroupName = a })

-- | The status of the DB instance's option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\s a -> s { _ogmStatus = a })

instance FromXML OptionGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupMembership"

instance ToQuery OptionGroupMembership where
    toQuery = genericQuery def

-- | Available option.
data OptionGroupOption = OptionGroupOption
    { _ogoName :: Maybe Text
    , _ogoDescription :: Maybe Text
    , _ogoEngineName :: Maybe Text
    , _ogoMajorEngineVersion :: Maybe Text
    , _ogoMinimumRequiredMinorEngineVersion :: Maybe Text
    , _ogoPortRequired :: Maybe Bool
    , _ogoDefaultPort :: Maybe Integer
    , _ogoOptionName :: [Text]
    , _ogoPersistent :: Maybe Bool
    , _ogoPermanent :: Maybe Bool
    , _ogoOptionGroupOptionSetting :: [OptionGroupOptionSetting]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupOption' data type.
--
-- 'OptionGroupOption' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EngineName ::@ @Maybe Text@
--
-- * @MajorEngineVersion ::@ @Maybe Text@
--
-- * @MinimumRequiredMinorEngineVersion ::@ @Maybe Text@
--
-- * @PortRequired ::@ @Maybe Bool@
--
-- * @DefaultPort ::@ @Maybe Integer@
--
-- * @OptionName ::@ @[Text]@
--
-- * @Persistent ::@ @Maybe Bool@
--
-- * @Permanent ::@ @Maybe Bool@
--
-- * @OptionGroupOptionSetting ::@ @[OptionGroupOptionSetting]@
--
optionGroupOption :: OptionGroupOption
optionGroupOption = OptionGroupOption
    { _ogoName = Nothing
    , _ogoDescription = Nothing
    , _ogoEngineName = Nothing
    , _ogoMajorEngineVersion = Nothing
    , _ogoMinimumRequiredMinorEngineVersion = Nothing
    , _ogoPortRequired = Nothing
    , _ogoDefaultPort = Nothing
    , _ogoOptionName = mempty
    , _ogoPersistent = Nothing
    , _ogoPermanent = Nothing
    , _ogoOptionGroupOptionSetting = mempty
    }

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\s a -> s { _ogoName = a })

-- | The description of the option.
ogoDescription :: Lens' OptionGroupOption (Maybe Text)
ogoDescription = lens _ogoDescription (\s a -> s { _ogoDescription = a })

-- | Engine name that this option can be applied to.
ogoEngineName :: Lens' OptionGroupOption (Maybe Text)
ogoEngineName = lens _ogoEngineName (\s a -> s { _ogoEngineName = a })

-- | Indicates the major engine version that the option is available for.
ogoMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMajorEngineVersion =
    lens _ogoMajorEngineVersion (\s a -> s { _ogoMajorEngineVersion = a })

-- | The minimum required engine version for the option to be applied.
ogoMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMinimumRequiredMinorEngineVersion =
    lens _ogoMinimumRequiredMinorEngineVersion
         (\s a -> s { _ogoMinimumRequiredMinorEngineVersion = a })

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\s a -> s { _ogoPortRequired = a })

-- | If the option requires a port, specifies the default port for the option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Integer)
ogoDefaultPort = lens _ogoDefaultPort (\s a -> s { _ogoDefaultPort = a })

-- | List of all options that are prerequisites for this option.
ogoOptionName :: Lens' OptionGroupOption [Text]
ogoOptionName = lens _ogoOptionName (\s a -> s { _ogoOptionName = a })

-- | A persistent option cannot be removed from the option group once the option
-- group is used, but this option can be removed from the db instance while
-- modifying the related data and assigning another option group without this
-- option.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\s a -> s { _ogoPersistent = a })

-- | A permanent option cannot be removed from the option group once the option
-- group is used, and it cannot be removed from the db instance after
-- assigning an option group with this permanent option.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\s a -> s { _ogoPermanent = a })

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogoOptionGroupOptionSetting :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSetting =
    lens _ogoOptionGroupOptionSetting
         (\s a -> s { _ogoOptionGroupOptionSetting = a })

instance FromXML OptionGroupOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOption"

-- | option group option settings are used to display settings available for
-- each option with their default values and other information. These values
-- are used with the DescribeOptionGroupOptions action.
data OptionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosSettingName :: Maybe Text
    , _ogosSettingDescription :: Maybe Text
    , _ogosDefaultValue :: Maybe Text
    , _ogosApplyType :: Maybe Text
    , _ogosAllowedValues :: Maybe Text
    , _ogosIsModifiable :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupOptionSetting' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SettingName ::@ @Maybe Text@
--
-- * @SettingDescription ::@ @Maybe Text@
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @ApplyType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
optionGroupOptionSetting :: OptionGroupOptionSetting
optionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosSettingName = Nothing
    , _ogosSettingDescription = Nothing
    , _ogosDefaultValue = Nothing
    , _ogosApplyType = Nothing
    , _ogosAllowedValues = Nothing
    , _ogosIsModifiable = Nothing
    }

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\s a -> s { _ogosSettingName = a })

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription =
    lens _ogosSettingDescription (\s a -> s { _ogosSettingDescription = a })

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue =
    lens _ogosDefaultValue (\s a -> s { _ogosDefaultValue = a })

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\s a -> s { _ogosApplyType = a })

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues =
    lens _ogosAllowedValues (\s a -> s { _ogosAllowedValues = a })

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable =
    lens _ogosIsModifiable (\s a -> s { _ogosIsModifiable = a })

instance FromXML OptionGroupOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOptionSetting"

instance ToQuery OptionGroupOptionSetting where
    toQuery = genericQuery def

-- | Option settings are the actual settings being applied or configured for
-- that option. It is used when you modify an option group or describe option
-- groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting
-- called SQLNET.ENCRYPTION_SERVER that can have several different values.
data OptionSetting = OptionSetting
    { _osName :: Maybe Text
    , _osValue :: Maybe Text
    , _osDefaultValue :: Maybe Text
    , _osDescription :: Maybe Text
    , _osApplyType :: Maybe Text
    , _osDataType :: Maybe Text
    , _osAllowedValues :: Maybe Text
    , _osIsModifiable :: Maybe Bool
    , _osIsCollection :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionSetting' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @ApplyType ::@ @Maybe Text@
--
-- * @DataType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
-- * @IsCollection ::@ @Maybe Bool@
--
optionSetting :: OptionSetting
optionSetting = OptionSetting
    { _osName = Nothing
    , _osValue = Nothing
    , _osDefaultValue = Nothing
    , _osDescription = Nothing
    , _osApplyType = Nothing
    , _osDataType = Nothing
    , _osAllowedValues = Nothing
    , _osIsModifiable = Nothing
    , _osIsCollection = Nothing
    }

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName = lens _osName (\s a -> s { _osName = a })

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\s a -> s { _osValue = a })

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\s a -> s { _osDefaultValue = a })

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\s a -> s { _osDescription = a })

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\s a -> s { _osApplyType = a })

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\s a -> s { _osDataType = a })

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\s a -> s { _osAllowedValues = a })

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\s a -> s { _osIsModifiable = a })

-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection = lens _osIsCollection (\s a -> s { _osIsCollection = a })

instance FromXML OptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionSetting"

instance ToQuery OptionSetting where
    toQuery = genericQuery def

-- | Contains a list of available options for a DB instance This data type is
-- used as a response element in the DescribeOrderableDBInstanceOptions
-- action.
data OrderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioEngine :: Maybe Text
    , _odbioEngineVersion :: Maybe Text
    , _odbioDBInstanceClass :: Maybe Text
    , _odbioLicenseModel :: Maybe Text
    , _odbioAvailabilityZone :: [AvailabilityZone]
    , _odbioMultiAZCapable :: Maybe Bool
    , _odbioReadReplicaCapable :: Maybe Bool
    , _odbioVpc :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OrderableDBInstanceOption' data type.
--
-- 'OrderableDBInstanceOption' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @LicenseModel ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @[AvailabilityZone]@
--
-- * @MultiAZCapable ::@ @Maybe Bool@
--
-- * @ReadReplicaCapable ::@ @Maybe Bool@
--
-- * @Vpc ::@ @Maybe Bool@
--
orderableDBInstanceOption :: OrderableDBInstanceOption
orderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioEngine = Nothing
    , _odbioEngineVersion = Nothing
    , _odbioDBInstanceClass = Nothing
    , _odbioLicenseModel = Nothing
    , _odbioAvailabilityZone = mempty
    , _odbioMultiAZCapable = Nothing
    , _odbioReadReplicaCapable = Nothing
    , _odbioVpc = Nothing
    }

-- | The engine type of the orderable DB instance.
odbioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngine = lens _odbioEngine (\s a -> s { _odbioEngine = a })

-- | The engine version of the orderable DB instance.
odbioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngineVersion =
    lens _odbioEngineVersion (\s a -> s { _odbioEngineVersion = a })

-- | The DB instance Class for the orderable DB instance.
odbioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioDBInstanceClass =
    lens _odbioDBInstanceClass (\s a -> s { _odbioDBInstanceClass = a })

-- | The license model for the orderable DB instance.
odbioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioLicenseModel =
    lens _odbioLicenseModel (\s a -> s { _odbioLicenseModel = a })

-- | A list of availability zones for the orderable DB instance.
odbioAvailabilityZone :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odbioAvailabilityZone =
    lens _odbioAvailabilityZone (\s a -> s { _odbioAvailabilityZone = a })

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odbioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioMultiAZCapable =
    lens _odbioMultiAZCapable (\s a -> s { _odbioMultiAZCapable = a })

-- | Indicates whether this orderable DB instance can have a read replica.
odbioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioReadReplicaCapable =
    lens _odbioReadReplicaCapable
         (\s a -> s { _odbioReadReplicaCapable = a })

-- | Indicates whether this is a VPC orderable DB instance.
odbioVpc :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioVpc = lens _odbioVpc (\s a -> s { _odbioVpc = a })

instance FromXML OrderableDBInstanceOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableDBInstanceOption"

-- | This data type is used as a request parameter in the ModifyDBParameterGroup
-- and ResetDBParameterGroup actions. This data type is used as a response
-- element in the DescribeEngineDefaultParameters and DescribeDBParameters
-- actions.
data Parameter = Parameter
    { _pParameterName :: Maybe Text
    , _pParameterValue :: Maybe Text
    , _pDescription :: Maybe Text
    , _pSource :: Maybe Text
    , _pApplyType :: Maybe Text
    , _pDataType :: Maybe Text
    , _pAllowedValues :: Maybe Text
    , _pIsModifiable :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    , _pApplyMethod :: Maybe ApplyMethod
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterName ::@ @Maybe Text@
--
-- * @ParameterValue ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @ApplyType ::@ @Maybe Text@
--
-- * @DataType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
-- * @MinimumEngineVersion ::@ @Maybe Text@
--
-- * @ApplyMethod ::@ @Maybe ApplyMethod@
--
parameter :: Parameter
parameter = Parameter
    { _pParameterName = Nothing
    , _pParameterValue = Nothing
    , _pDescription = Nothing
    , _pSource = Nothing
    , _pApplyType = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pIsModifiable = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pApplyMethod = Nothing
    }

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

-- | Specifies the engine specific parameters type.
pApplyType :: Lens' Parameter (Maybe Text)
pApplyType = lens _pApplyType (\s a -> s { _pApplyType = a })

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
pApplyMethod = lens _pApplyMethod (\s a -> s { _pApplyMethod = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericQuery def

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
data PendingModifiedValues = PendingModifiedValues
    { _pmvDBInstanceClass :: Maybe Text
    , _pmvAllocatedStorage :: Maybe Integer
    , _pmvMasterUserPassword :: Maybe Text
    , _pmvPort :: Maybe Integer
    , _pmvBackupRetentionPeriod :: Maybe Integer
    , _pmvMultiAZ :: Maybe Bool
    , _pmvEngineVersion :: Maybe Text
    , _pmvIops :: Maybe Integer
    , _pmvDBInstanceIdentifier :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @AllocatedStorage ::@ @Maybe Integer@
--
-- * @MasterUserPassword ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @BackupRetentionPeriod ::@ @Maybe Integer@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @DBInstanceIdentifier ::@ @Maybe Text@
--
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues
    { _pmvDBInstanceClass = Nothing
    , _pmvAllocatedStorage = Nothing
    , _pmvMasterUserPassword = Nothing
    , _pmvPort = Nothing
    , _pmvBackupRetentionPeriod = Nothing
    , _pmvMultiAZ = Nothing
    , _pmvEngineVersion = Nothing
    , _pmvIops = Nothing
    , _pmvDBInstanceIdentifier = Nothing
    }

-- | Contains the new DBInstanceClass for the DB instance that will be applied
-- or is in progress.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass =
    lens _pmvDBInstanceClass (\s a -> s { _pmvDBInstanceClass = a })

-- | Contains the new AllocatedStorage size for the DB instance that will be
-- applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Integer)
pmvAllocatedStorage =
    lens _pmvAllocatedStorage (\s a -> s { _pmvAllocatedStorage = a })

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword =
    lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Integer)
pmvPort = lens _pmvPort (\s a -> s { _pmvPort = a })

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvBackupRetentionPeriod =
    lens _pmvBackupRetentionPeriod
         (\s a -> s { _pmvBackupRetentionPeriod = a })

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\s a -> s { _pmvMultiAZ = a })

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion =
    lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })

-- | Specifies the new Provisioned IOPS value for the DB instance that will be
-- applied or is being applied.
pmvIops :: Lens' PendingModifiedValues (Maybe Integer)
pmvIops = lens _pmvIops (\s a -> s { _pmvIops = a })

-- | Contains the new DBInstanceIdentifier for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier =
    lens _pmvDBInstanceIdentifier
         (\s a -> s { _pmvDBInstanceIdentifier = a })

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and DescribeReservedDBInstancesOfferings
-- actions.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RecurringChargeAmount ::@ @Maybe Double@
--
-- * @RecurringChargeFrequency ::@ @Maybe Text@
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount
         (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
         (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
data ReservedDBInstance = ReservedDBInstance
    { _rdbiReservedDBInstanceId :: Maybe Text
    , _rdbiReservedDBInstancesOfferingId :: Maybe Text
    , _rdbiDBInstanceClass :: Maybe Text
    , _rdbiStartTime :: Maybe ISO8601
    , _rdbiDuration :: Maybe Integer
    , _rdbiFixedPrice :: Maybe Double
    , _rdbiUsagePrice :: Maybe Double
    , _rdbiCurrencyCode :: Maybe Text
    , _rdbiDBInstanceCount :: Maybe Integer
    , _rdbiProductDescription :: Maybe Text
    , _rdbiOfferingType :: Maybe Text
    , _rdbiMultiAZ :: Maybe Bool
    , _rdbiState :: Maybe Text
    , _rdbiRecurringCharge :: [RecurringCharge]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedDBInstance' data type.
--
-- 'ReservedDBInstance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedDBInstanceId ::@ @Maybe Text@
--
-- * @ReservedDBInstancesOfferingId ::@ @Maybe Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe Text@
--
-- * @DBInstanceCount ::@ @Maybe Integer@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @State ::@ @Maybe Text@
--
-- * @RecurringCharge ::@ @[RecurringCharge]@
--
reservedDBInstance :: ReservedDBInstance
reservedDBInstance = ReservedDBInstance
    { _rdbiReservedDBInstanceId = Nothing
    , _rdbiReservedDBInstancesOfferingId = Nothing
    , _rdbiDBInstanceClass = Nothing
    , _rdbiStartTime = Nothing
    , _rdbiDuration = Nothing
    , _rdbiFixedPrice = Nothing
    , _rdbiUsagePrice = Nothing
    , _rdbiCurrencyCode = Nothing
    , _rdbiDBInstanceCount = Nothing
    , _rdbiProductDescription = Nothing
    , _rdbiOfferingType = Nothing
    , _rdbiMultiAZ = Nothing
    , _rdbiState = Nothing
    , _rdbiRecurringCharge = mempty
    }

-- | The unique identifier for the reservation.
rdbiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdbiReservedDBInstanceId =
    lens _rdbiReservedDBInstanceId
         (\s a -> s { _rdbiReservedDBInstanceId = a })

-- | The offering identifier.
rdbiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdbiReservedDBInstancesOfferingId =
    lens _rdbiReservedDBInstancesOfferingId
         (\s a -> s { _rdbiReservedDBInstancesOfferingId = a })

-- | The DB instance class for the reserved DB instance.
rdbiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdbiDBInstanceClass =
    lens _rdbiDBInstanceClass (\s a -> s { _rdbiDBInstanceClass = a })

-- | The time the reservation started.
rdbiStartTime :: Lens' ReservedDBInstance (Maybe ISO8601)
rdbiStartTime = lens _rdbiStartTime (\s a -> s { _rdbiStartTime = a })

-- | The duration of the reservation in seconds.
rdbiDuration :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDuration = lens _rdbiDuration (\s a -> s { _rdbiDuration = a })

-- | The fixed price charged for this reserved DB instance.
rdbiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiFixedPrice = lens _rdbiFixedPrice (\s a -> s { _rdbiFixedPrice = a })

-- | The hourly price charged for this reserved DB instance.
rdbiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiUsagePrice = lens _rdbiUsagePrice (\s a -> s { _rdbiUsagePrice = a })

-- | The currency code for the reserved DB instance.
rdbiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdbiCurrencyCode =
    lens _rdbiCurrencyCode (\s a -> s { _rdbiCurrencyCode = a })

-- | The number of reserved DB instances.
rdbiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDBInstanceCount =
    lens _rdbiDBInstanceCount (\s a -> s { _rdbiDBInstanceCount = a })

-- | The description of the reserved DB instance.
rdbiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdbiProductDescription =
    lens _rdbiProductDescription (\s a -> s { _rdbiProductDescription = a })

-- | The offering type of this reserved DB instance.
rdbiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdbiOfferingType =
    lens _rdbiOfferingType (\s a -> s { _rdbiOfferingType = a })

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdbiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdbiMultiAZ = lens _rdbiMultiAZ (\s a -> s { _rdbiMultiAZ = a })

-- | The state of the reserved DB instance.
rdbiState :: Lens' ReservedDBInstance (Maybe Text)
rdbiState = lens _rdbiState (\s a -> s { _rdbiState = a })

-- | The recurring price charged to run this reserved DB instance.
rdbiRecurringCharge :: Lens' ReservedDBInstance [RecurringCharge]
rdbiRecurringCharge =
    lens _rdbiRecurringCharge (\s a -> s { _rdbiRecurringCharge = a })

instance FromXML ReservedDBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstance"

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioReservedDBInstancesOfferingId :: Maybe Text
    , _rdbioDBInstanceClass :: Maybe Text
    , _rdbioDuration :: Maybe Integer
    , _rdbioFixedPrice :: Maybe Double
    , _rdbioUsagePrice :: Maybe Double
    , _rdbioCurrencyCode :: Maybe Text
    , _rdbioProductDescription :: Maybe Text
    , _rdbioOfferingType :: Maybe Text
    , _rdbioMultiAZ :: Maybe Bool
    , _rdbioRecurringCharge :: [RecurringCharge]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedDBInstancesOffering' data type.
--
-- 'ReservedDBInstancesOffering' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedDBInstancesOfferingId ::@ @Maybe Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @CurrencyCode ::@ @Maybe Text@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @RecurringCharge ::@ @[RecurringCharge]@
--
reservedDBInstancesOffering :: ReservedDBInstancesOffering
reservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioReservedDBInstancesOfferingId = Nothing
    , _rdbioDBInstanceClass = Nothing
    , _rdbioDuration = Nothing
    , _rdbioFixedPrice = Nothing
    , _rdbioUsagePrice = Nothing
    , _rdbioCurrencyCode = Nothing
    , _rdbioProductDescription = Nothing
    , _rdbioOfferingType = Nothing
    , _rdbioMultiAZ = Nothing
    , _rdbioRecurringCharge = mempty
    }

-- | The offering identifier.
rdbioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioReservedDBInstancesOfferingId =
    lens _rdbioReservedDBInstancesOfferingId
         (\s a -> s { _rdbioReservedDBInstancesOfferingId = a })

-- | The DB instance class for the reserved DB instance.
rdbioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioDBInstanceClass =
    lens _rdbioDBInstanceClass (\s a -> s { _rdbioDBInstanceClass = a })

-- | The duration of the offering in seconds.
rdbioDuration :: Lens' ReservedDBInstancesOffering (Maybe Integer)
rdbioDuration = lens _rdbioDuration (\s a -> s { _rdbioDuration = a })

-- | The fixed price charged for this offering.
rdbioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioFixedPrice = lens _rdbioFixedPrice (\s a -> s { _rdbioFixedPrice = a })

-- | The hourly price charged for this offering.
rdbioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioUsagePrice = lens _rdbioUsagePrice (\s a -> s { _rdbioUsagePrice = a })

-- | The currency code for the reserved DB instance offering.
rdbioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioCurrencyCode =
    lens _rdbioCurrencyCode (\s a -> s { _rdbioCurrencyCode = a })

-- | The database engine used by the offering.
rdbioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioProductDescription =
    lens _rdbioProductDescription
         (\s a -> s { _rdbioProductDescription = a })

-- | The offering type.
rdbioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioOfferingType =
    lens _rdbioOfferingType (\s a -> s { _rdbioOfferingType = a })

-- | Indicates if the offering applies to Multi-AZ deployments.
rdbioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdbioMultiAZ = lens _rdbioMultiAZ (\s a -> s { _rdbioMultiAZ = a })

-- | The recurring price charged to run this reserved DB instance.
rdbioRecurringCharge :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdbioRecurringCharge =
    lens _rdbioRecurringCharge (\s a -> s { _rdbioRecurringCharge = a })

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOffering"

-- | This data type is used as a response element in the DescribeDBSubnetGroups
-- action.
data Subnet = Subnet
    { _sSubnetIdentifier :: Maybe Text
    , _sSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _sSubnetStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetIdentifier ::@ @Maybe Text@
--
-- * @SubnetAvailabilityZone ::@ @Maybe AvailabilityZone@
--
-- * @SubnetStatus ::@ @Maybe Text@
--
subnet :: Subnet
subnet = Subnet
    { _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    , _sSubnetStatus = Nothing
    }

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier =
    lens _sSubnetIdentifier (\s a -> s { _sSubnetIdentifier = a })

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone =
    lens _sSubnetAvailabilityZone
         (\s a -> s { _sSubnetAvailabilityZone = a })

-- | Specifies the status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\s a -> s { _sSubnetStatus = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value pair.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
tag :: Tag
tag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | A value is the optional value of the tag. The string value can be from 1 to
-- 256 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | This data type is used as a response element for queries on VPC security
-- group membership.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId :: Maybe Text
    , _vsgmStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcSecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcSecurityGroupId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
vpcSecurityGroupMembership :: VpcSecurityGroupMembership
vpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus = Nothing
    }

-- | The name of the VPC security group.
vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId =
    lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })

-- | The status of the VPC security group.
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
