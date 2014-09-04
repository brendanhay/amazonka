{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.Types
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
module Network.AWS.RDS.V2013_09_09.Types
    (
    -- * Service
      RDS
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * SourceType
    , SourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , azName
    , azProvisionedIopsCapable

    -- * CharacterSet
    , CharacterSet (..)
    , csCharacterSetName
    , csCharacterSetDescription

    -- * DBEngineVersion
    , DBEngineVersion (..)
    , dbevEngine
    , dbevEngineVersion
    , dbevDBParameterGroupFamily
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDefaultCharacterSet
    , dbevSupportedCharacterSets

    -- * DBInstance
    , DBInstance (..)
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
    , dbiDBSecurityGroups
    , dbiVpcSecurityGroups
    , dbiDBParameterGroups
    , dbiAvailabilityZone
    , dbiDBSubnetGroup
    , dbiPreferredMaintenanceWindow
    , dbiPendingModifiedValues
    , dbiLatestRestorableTime
    , dbiMultiAZ
    , dbiEngineVersion
    , dbiAutoMinorVersionUpgrade
    , dbiReadReplicaSourceDBInstanceIdentifier
    , dbiReadReplicaDBInstanceIdentifiers
    , dbiLicenseModel
    , dbiIops
    , dbiOptionGroupMemberships
    , dbiCharacterSetName
    , dbiSecondaryAvailabilityZone
    , dbiPubliclyAccessible
    , dbiStatusInfos

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo (..)
    , dbisiStatusType
    , dbisiNormal
    , dbisiStatus
    , dbisiMessage

    -- * DBParameterGroup
    , DBParameterGroup (..)
    , dbpgDBParameterGroupName
    , dbpgDBParameterGroupFamily
    , dbpgDescription

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus (..)
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- * DBSecurityGroup
    , DBSecurityGroup (..)
    , dbsgOwnerId
    , dbsgDBSecurityGroupName
    , dbsgDBSecurityGroupDescription
    , dbsgVpcId
    , dbsgEC2SecurityGroups
    , dbsgIPRanges

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership (..)
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- * DBSnapshot
    , DBSnapshot (..)
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
    , DBSubnetGroup (..)
    , dbshDBSubnetGroupName
    , dbshDBSubnetGroupDescription
    , dbshVpcId
    , dbshSubnetGroupStatus
    , dbshSubnets

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails (..)
    , ddblfdLogFileName
    , ddblfdLastWritten
    , ddblfdSize

    -- * EC2SecurityGroup
    , EC2SecurityGroup (..)
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupOwnerId

    -- * Endpoint
    , Endpoint (..)
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults (..)
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event (..)
    , eySourceIdentifier
    , eySourceType
    , eyMessage
    , eyEventCategories
    , eyDate

    -- * EventCategoriesMap
    , EventCategoriesMap (..)
    , ecqSourceType
    , ecqEventCategories

    -- * EventSubscription
    , EventSubscription (..)
    , esCustomerAwsId
    , esCustSubscriptionId
    , esSnsTopicArn
    , esStatus
    , esSubscriptionCreationTime
    , esSourceType
    , esSourceIdsList
    , esEventCategoriesList
    , esEnabled

    -- * IPRange
    , IPRange (..)
    , iprStatus
    , iprCIDRIP

    -- * Option
    , Option (..)
    , onOptionName
    , onOptionDescription
    , onPersistent
    , onPermanent
    , onPort
    , onOptionSettings
    , onDBSecurityGroupMemberships
    , onVpcSecurityGroupMemberships

    -- * OptionConfiguration
    , OptionConfiguration (..)
    , ocOptionName
    , ocPort
    , ocDBSecurityGroupMemberships
    , ocVpcSecurityGroupMemberships
    , ocOptionSettings

    -- * OptionGroup
    , OptionGroup (..)
    , ogOptionGroupName
    , ogOptionGroupDescription
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptions
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogVpcId

    -- * OptionGroupMembership
    , OptionGroupMembership (..)
    , ogmOptionGroupName
    , ogmStatus

    -- * OptionGroupOption
    , OptionGroupOption (..)
    , ogqName
    , ogqDescription
    , ogqEngineName
    , ogqMajorEngineVersion
    , ogqMinimumRequiredMinorEngineVersion
    , ogqPortRequired
    , ogqDefaultPort
    , ogqOptionsDependedOn
    , ogqPersistent
    , ogqPermanent
    , ogqOptionGroupOptionSettings

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting (..)
    , ogosSettingName
    , ogosSettingDescription
    , ogosDefaultValue
    , ogosApplyType
    , ogosAllowedValues
    , ogosIsModifiable

    -- * OptionSetting
    , OptionSetting (..)
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
    , OrderableDBInstanceOption (..)
    , odbioEngine
    , odbioEngineVersion
    , odbioDBInstanceClass
    , odbioLicenseModel
    , odbioAvailabilityZones
    , odbioMultiAZCapable
    , odbioReadReplicaCapable
    , odbioVpc

    -- * Parameter
    , Parameter (..)
    , prParameterName
    , prParameterValue
    , prDescription
    , prSource
    , prApplyType
    , prDataType
    , prAllowedValues
    , prIsModifiable
    , prMinimumEngineVersion
    , prApplyMethod

    -- * PendingModifiedValues
    , PendingModifiedValues (..)
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
    , RecurringCharge (..)
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedDBInstance
    , ReservedDBInstance (..)
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
    , rdbiRecurringCharges

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering (..)
    , rdbioReservedDBInstancesOfferingId
    , rdbioDBInstanceClass
    , rdbioDuration
    , rdbioFixedPrice
    , rdbioUsagePrice
    , rdbioCurrencyCode
    , rdbioProductDescription
    , rdbioOfferingType
    , rdbioMultiAZ
    , rdbioRecurringCharges

    -- * Subnet
    , Subnet (..)
    , ssssstSubnetIdentifier
    , ssssstSubnetAvailabilityZone
    , ssssstSubnetStatus

    -- * Tag
    , Tag (..)
    , tgKey
    , tgValue

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
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
    data Er RDS
        = AuthorizationAlreadyExistsFault
        | AuthorizationNotFoundFault
        | AuthorizationQuotaExceededFault
        | DBInstanceAlreadyExistsFault
        | DBInstanceNotFoundFault
        | DBParameterGroupAlreadyExistsFault
        | DBParameterGroupNotFoundFault
        | DBParameterGroupQuotaExceededFault
        | DBSecurityGroupAlreadyExistsFault
        | DBSecurityGroupNotFoundFault
        | DBSecurityGroupNotSupportedFault
        | DBSecurityGroupQuotaExceededFault
        | DBSnapshotAlreadyExistsFault
        | DBSnapshotNotFoundFault
        | DBSubnetGroupAlreadyExistsFault
        | DBSubnetGroupDoesNotCoverEnoughAZs
        | DBSubnetGroupNotAllowedFault
        | DBSubnetGroupNotFoundFault
        | DBSubnetGroupQuotaExceededFault
        | DBSubnetQuotaExceededFault
        | DBUpgradeDependencyFailureFault
        | EventSubscriptionQuotaExceededFault
        | InstanceQuotaExceededFault
        | InsufficientDBInstanceCapacityFault
        | InvalidDBInstanceStateFault
        | InvalidDBParameterGroupStateFault
        | InvalidDBSecurityGroupStateFault
        | InvalidDBSnapshotStateFault
        | InvalidDBSubnetGroupFault
        | InvalidDBSubnetGroupStateFault
        | InvalidDBSubnetStateFault
        | InvalidEventSubscriptionStateFault
        | InvalidOptionGroupStateFault
        | InvalidRestoreFault
        | InvalidSubnet
        | InvalidVPCNetworkStateFault
        | OptionGroupAlreadyExistsFault
        | OptionGroupNotFoundFault
        | OptionGroupQuotaExceededFault
        | PointInTimeRestoreNotEnabledFault
        | ProvisionedIopsNotAvailableInAZFault
        | RDSClient HttpException
        | RDSSerializer String
        | RDSService String
        | ReservedDBInstanceAlreadyExistsFault
        | ReservedDBInstanceNotFoundFault
        | ReservedDBInstanceQuotaExceededFault
        | ReservedDBInstancesOfferingNotFoundFault
        | SNSInvalidTopicFault
        | SNSNoAuthorizationFault
        | SNSTopicArnNotFoundFault
        | SnapshotQuotaExceededFault
        | SourceNotFoundFault
        | StorageQuotaExceededFault
        | SubnetAlreadyInUse
        | SubscriptionAlreadyExistFault
        | SubscriptionCategoryNotFoundFault
        | SubscriptionNotFoundFault

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "rds"
        , _svcVersion  = "2013-09-09"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er RDS)
deriving instance Generic (Er RDS)

instance AWSError (Er RDS) where
    awsError = const "RDSError"

instance AWSServiceError (Er RDS) where
    serviceError    = RDSService
    clientError     = RDSClient
    serializerError = RDSSerializer

instance Exception (Er RDS)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://rds.amazonaws.com/doc/2013-09-09/"
    }

-- | Indicates when to apply parameter updates.
data ApplyMethod
    = ApplyMethodImmediate -- ^ immediate
    | ApplyMethodPendingReboot -- ^ pending-reboot
      deriving (Eq, Show, Generic)

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

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
data SourceType
    = SourceTypeDbInstance -- ^ db-instance
    | SourceTypeDbParameterGroup -- ^ db-parameter-group
    | SourceTypeDbSecurityGroup -- ^ db-security-group
    | SourceTypeDbSnapshot -- ^ db-snapshot
      deriving (Eq, Show, Generic)

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
      -- ^ The name of the availability zone.
    , _azProvisionedIopsCapable :: Maybe Bool
      -- ^ True indicates the availability zone is capable of provisioned
      -- IOPs.
    } deriving (Show, Generic)

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName f x =
    f (_azName x)
        <&> \y -> x { _azName = y }
{-# INLINE azName #-}

-- | True indicates the availability zone is capable of provisioned IOPs.
azProvisionedIopsCapable :: Lens' AvailabilityZone (Maybe Bool)
azProvisionedIopsCapable f x =
    f (_azProvisionedIopsCapable x)
        <&> \y -> x { _azProvisionedIopsCapable = y }
{-# INLINE azProvisionedIopsCapable #-}

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
data CharacterSet = CharacterSet
    { _csCharacterSetName :: Maybe Text
      -- ^ The name of the character set.
    , _csCharacterSetDescription :: Maybe Text
      -- ^ The description of the character set.
    } deriving (Show, Generic)

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName f x =
    f (_csCharacterSetName x)
        <&> \y -> x { _csCharacterSetName = y }
{-# INLINE csCharacterSetName #-}

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription f x =
    f (_csCharacterSetDescription x)
        <&> \y -> x { _csCharacterSetDescription = y }
{-# INLINE csCharacterSetDescription #-}

instance FromXML CharacterSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CharacterSet"

instance ToQuery CharacterSet where
    toQuery = genericQuery def

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
data DBEngineVersion = DBEngineVersion
    { _dbevEngine :: Maybe Text
      -- ^ The name of the database engine.
    , _dbevEngineVersion :: Maybe Text
      -- ^ The version number of the database engine.
    , _dbevDBParameterGroupFamily :: Maybe Text
      -- ^ The name of the DB parameter group family for the database
      -- engine.
    , _dbevDBEngineDescription :: Maybe Text
      -- ^ The description of the database engine.
    , _dbevDBEngineVersionDescription :: Maybe Text
      -- ^ The description of the database engine version.
    , _dbevDefaultCharacterSet :: Maybe CharacterSet
      -- ^ The default character set for new instances of this engine
      -- version, if the CharacterSetName parameter of the
      -- CreateDBInstance API is not specified.
    , _dbevSupportedCharacterSets :: [CharacterSet]
      -- ^ A list of the character sets supported by this engine for the
      -- CharacterSetName parameter of the CreateDBInstance API.
    } deriving (Show, Generic)

-- | The name of the database engine.
dbevEngine :: Lens' DBEngineVersion (Maybe Text)
dbevEngine f x =
    f (_dbevEngine x)
        <&> \y -> x { _dbevEngine = y }
{-# INLINE dbevEngine #-}

-- | The version number of the database engine.
dbevEngineVersion :: Lens' DBEngineVersion (Maybe Text)
dbevEngineVersion f x =
    f (_dbevEngineVersion x)
        <&> \y -> x { _dbevEngineVersion = y }
{-# INLINE dbevEngineVersion #-}

-- | The name of the DB parameter group family for the database engine.
dbevDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
dbevDBParameterGroupFamily f x =
    f (_dbevDBParameterGroupFamily x)
        <&> \y -> x { _dbevDBParameterGroupFamily = y }
{-# INLINE dbevDBParameterGroupFamily #-}

-- | The description of the database engine.
dbevDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineDescription f x =
    f (_dbevDBEngineDescription x)
        <&> \y -> x { _dbevDBEngineDescription = y }
{-# INLINE dbevDBEngineDescription #-}

-- | The description of the database engine version.
dbevDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineVersionDescription f x =
    f (_dbevDBEngineVersionDescription x)
        <&> \y -> x { _dbevDBEngineVersionDescription = y }
{-# INLINE dbevDBEngineVersionDescription #-}

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
dbevDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
dbevDefaultCharacterSet f x =
    f (_dbevDefaultCharacterSet x)
        <&> \y -> x { _dbevDefaultCharacterSet = y }
{-# INLINE dbevDefaultCharacterSet #-}

-- | A list of the character sets supported by this engine for the
-- CharacterSetName parameter of the CreateDBInstance API.
dbevSupportedCharacterSets :: Lens' DBEngineVersion ([CharacterSet])
dbevSupportedCharacterSets f x =
    f (_dbevSupportedCharacterSets x)
        <&> \y -> x { _dbevSupportedCharacterSets = y }
{-# INLINE dbevSupportedCharacterSets #-}

instance FromXML DBEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBEngineVersion"

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
data DBInstance = DBInstance
    { _dbiDBInstanceIdentifier :: Maybe Text
      -- ^ Contains a user-supplied database identifier. This is the unique
      -- key that identifies a DB instance.
    , _dbiDBInstanceClass :: Maybe Text
      -- ^ Contains the name of the compute and memory capacity class of the
      -- DB instance.
    , _dbiEngine :: Maybe Text
      -- ^ Provides the name of the database engine to be used for this DB
      -- instance.
    , _dbiDBInstanceStatus :: Maybe Text
      -- ^ Specifies the current state of this database.
    , _dbiMasterUsername :: Maybe Text
      -- ^ Contains the master username for the DB instance.
    , _dbiDBName :: Maybe Text
      -- ^ The meaning of this parameter differs according to the database
      -- engine you use. For example, this value returns only MySQL
      -- information when returning values from
      -- CreateDBInstanceReadReplica since read replicas are only
      -- supported for MySQL. MySQL Contains the name of the initial
      -- database of this instance that was provided at create time, if
      -- one was specified when the DB instance was created. This same
      -- name is returned for the life of the DB instance. Type: String
      -- Oracle Contains the Oracle System ID (SID) of the created DB
      -- instance. Not shown when the returned parameters do not apply to
      -- an Oracle DB instance.
    , _dbiEndpoint :: Maybe Endpoint
      -- ^ Specifies the connection endpoint.
    , _dbiAllocatedStorage :: Maybe Integer
      -- ^ Specifies the allocated storage size specified in gigabytes.
    , _dbiInstanceCreateTime :: Maybe ISO8601
      -- ^ Provides the date and time the DB instance was created.
    , _dbiPreferredBackupWindow :: Maybe Text
      -- ^ Specifies the daily time range during which automated backups are
      -- created if automated backups are enabled, as determined by the
      -- BackupRetentionPeriod.
    , _dbiBackupRetentionPeriod :: Maybe Integer
      -- ^ Specifies the number of days for which automatic DB snapshots are
      -- retained.
    , _dbiDBSecurityGroups :: [DBSecurityGroupMembership]
      -- ^ Provides List of DB security group elements containing only
      -- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
    , _dbiVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ Provides List of VPC security group elements that the DB instance
      -- belongs to.
    , _dbiDBParameterGroups :: [DBParameterGroupStatus]
      -- ^ Provides the list of DB parameter groups applied to this DB
      -- instance.
    , _dbiAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance is
      -- located in.
    , _dbiDBSubnetGroup :: Maybe DBSubnetGroup
      -- ^ Specifies information on the subnet group associated with the DB
      -- instance, including the name, description, and subnets in the
      -- subnet group.
    , _dbiPreferredMaintenanceWindow :: Maybe Text
      -- ^ Specifies the weekly time range (in UTC) during which system
      -- maintenance can occur.
    , _dbiPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ Specifies that changes to the DB instance are pending. This
      -- element is only included when changes are pending. Specific
      -- changes are identified by subelements.
    , _dbiLatestRestorableTime :: Maybe ISO8601
      -- ^ Specifies the latest time to which a database can be restored
      -- with point-in-time restore.
    , _dbiMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
    , _dbiEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , _dbiAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version patches are applied automatically.
    , _dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the identifier of the source DB instance if this DB
      -- instance is a read replica.
    , _dbiReadReplicaDBInstanceIdentifiers :: [Text]
      -- ^ Contains one or more identifiers of the read replicas associated
      -- with this DB instance.
    , _dbiLicenseModel :: Maybe Text
      -- ^ License model information for this DB instance.
    , _dbiIops :: Maybe Integer
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value.
    , _dbiOptionGroupMemberships :: [OptionGroupMembership]
      -- ^ Provides the list of option group memberships for this DB
      -- instance.
    , _dbiCharacterSetName :: Maybe Text
      -- ^ If present, specifies the name of the character set that this
      -- instance is associated with.
    , _dbiSecondaryAvailabilityZone :: Maybe Text
      -- ^ If present, specifies the name of the secondary Availability Zone
      -- for a DB instance with multi-AZ support.
    , _dbiPubliclyAccessible :: Maybe Bool
      -- ^ Specifies the accessibility options for the DB instance. A value
      -- of true specifies an Internet-facing instance with a publicly
      -- resolvable DNS name, which resolves to a public IP address. A
      -- value of false specifies an internal instance with a DNS name
      -- that resolves to a private IP address. Default: The default
      -- behavior varies depending on whether a VPC has been requested or
      -- not. The following list shows the default behavior in each case.
      -- Default VPC:true VPC:false If no DB subnet group has been
      -- specified as part of the request and the PubliclyAccessible value
      -- has not been set, the DB instance will be publicly accessible. If
      -- a specific DB subnet group has been specified as part of the
      -- request and the PubliclyAccessible value has not been set, the DB
      -- instance will be private.
    , _dbiStatusInfos :: [DBInstanceStatusInfo]
      -- ^ The status of a read replica. If the instance is not a read
      -- replica, this will be blank.
    } deriving (Show, Generic)

-- | Contains a user-supplied database identifier. This is the unique key that
-- identifies a DB instance.
dbiDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiDBInstanceIdentifier f x =
    f (_dbiDBInstanceIdentifier x)
        <&> \y -> x { _dbiDBInstanceIdentifier = y }
{-# INLINE dbiDBInstanceIdentifier #-}

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbiDBInstanceClass :: Lens' DBInstance (Maybe Text)
dbiDBInstanceClass f x =
    f (_dbiDBInstanceClass x)
        <&> \y -> x { _dbiDBInstanceClass = y }
{-# INLINE dbiDBInstanceClass #-}

-- | Provides the name of the database engine to be used for this DB instance.
dbiEngine :: Lens' DBInstance (Maybe Text)
dbiEngine f x =
    f (_dbiEngine x)
        <&> \y -> x { _dbiEngine = y }
{-# INLINE dbiEngine #-}

-- | Specifies the current state of this database.
dbiDBInstanceStatus :: Lens' DBInstance (Maybe Text)
dbiDBInstanceStatus f x =
    f (_dbiDBInstanceStatus x)
        <&> \y -> x { _dbiDBInstanceStatus = y }
{-# INLINE dbiDBInstanceStatus #-}

-- | Contains the master username for the DB instance.
dbiMasterUsername :: Lens' DBInstance (Maybe Text)
dbiMasterUsername f x =
    f (_dbiMasterUsername x)
        <&> \y -> x { _dbiMasterUsername = y }
{-# INLINE dbiMasterUsername #-}

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
dbiDBName f x =
    f (_dbiDBName x)
        <&> \y -> x { _dbiDBName = y }
{-# INLINE dbiDBName #-}

-- | Specifies the connection endpoint.
dbiEndpoint :: Lens' DBInstance (Maybe Endpoint)
dbiEndpoint f x =
    f (_dbiEndpoint x)
        <&> \y -> x { _dbiEndpoint = y }
{-# INLINE dbiEndpoint #-}

-- | Specifies the allocated storage size specified in gigabytes.
dbiAllocatedStorage :: Lens' DBInstance (Maybe Integer)
dbiAllocatedStorage f x =
    f (_dbiAllocatedStorage x)
        <&> \y -> x { _dbiAllocatedStorage = y }
{-# INLINE dbiAllocatedStorage #-}

-- | Provides the date and time the DB instance was created.
dbiInstanceCreateTime :: Lens' DBInstance (Maybe ISO8601)
dbiInstanceCreateTime f x =
    f (_dbiInstanceCreateTime x)
        <&> \y -> x { _dbiInstanceCreateTime = y }
{-# INLINE dbiInstanceCreateTime #-}

-- | Specifies the daily time range during which automated backups are created
-- if automated backups are enabled, as determined by the
-- BackupRetentionPeriod.
dbiPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredBackupWindow f x =
    f (_dbiPreferredBackupWindow x)
        <&> \y -> x { _dbiPreferredBackupWindow = y }
{-# INLINE dbiPreferredBackupWindow #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
dbiBackupRetentionPeriod :: Lens' DBInstance (Maybe Integer)
dbiBackupRetentionPeriod f x =
    f (_dbiBackupRetentionPeriod x)
        <&> \y -> x { _dbiBackupRetentionPeriod = y }
{-# INLINE dbiBackupRetentionPeriod #-}

-- | Provides List of DB security group elements containing only
-- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
dbiDBSecurityGroups :: Lens' DBInstance ([DBSecurityGroupMembership])
dbiDBSecurityGroups f x =
    f (_dbiDBSecurityGroups x)
        <&> \y -> x { _dbiDBSecurityGroups = y }
{-# INLINE dbiDBSecurityGroups #-}

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroups :: Lens' DBInstance ([VpcSecurityGroupMembership])
dbiVpcSecurityGroups f x =
    f (_dbiVpcSecurityGroups x)
        <&> \y -> x { _dbiVpcSecurityGroups = y }
{-# INLINE dbiVpcSecurityGroups #-}

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroups :: Lens' DBInstance ([DBParameterGroupStatus])
dbiDBParameterGroups f x =
    f (_dbiDBParameterGroups x)
        <&> \y -> x { _dbiDBParameterGroups = y }
{-# INLINE dbiDBParameterGroups #-}

-- | Specifies the name of the Availability Zone the DB instance is located in.
dbiAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiAvailabilityZone f x =
    f (_dbiAvailabilityZone x)
        <&> \y -> x { _dbiAvailabilityZone = y }
{-# INLINE dbiAvailabilityZone #-}

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
dbiDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
dbiDBSubnetGroup f x =
    f (_dbiDBSubnetGroup x)
        <&> \y -> x { _dbiDBSubnetGroup = y }
{-# INLINE dbiDBSubnetGroup #-}

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
dbiPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredMaintenanceWindow f x =
    f (_dbiPreferredMaintenanceWindow x)
        <&> \y -> x { _dbiPreferredMaintenanceWindow = y }
{-# INLINE dbiPreferredMaintenanceWindow #-}

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
dbiPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
dbiPendingModifiedValues f x =
    f (_dbiPendingModifiedValues x)
        <&> \y -> x { _dbiPendingModifiedValues = y }
{-# INLINE dbiPendingModifiedValues #-}

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbiLatestRestorableTime :: Lens' DBInstance (Maybe ISO8601)
dbiLatestRestorableTime f x =
    f (_dbiLatestRestorableTime x)
        <&> \y -> x { _dbiLatestRestorableTime = y }
{-# INLINE dbiLatestRestorableTime #-}

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbiMultiAZ :: Lens' DBInstance (Maybe Bool)
dbiMultiAZ f x =
    f (_dbiMultiAZ x)
        <&> \y -> x { _dbiMultiAZ = y }
{-# INLINE dbiMultiAZ #-}

-- | Indicates the database engine version.
dbiEngineVersion :: Lens' DBInstance (Maybe Text)
dbiEngineVersion f x =
    f (_dbiEngineVersion x)
        <&> \y -> x { _dbiEngineVersion = y }
{-# INLINE dbiEngineVersion #-}

-- | Indicates that minor version patches are applied automatically.
dbiAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
dbiAutoMinorVersionUpgrade f x =
    f (_dbiAutoMinorVersionUpgrade x)
        <&> \y -> x { _dbiAutoMinorVersionUpgrade = y }
{-# INLINE dbiAutoMinorVersionUpgrade #-}

-- | Contains the identifier of the source DB instance if this DB instance is a
-- read replica.
dbiReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiReadReplicaSourceDBInstanceIdentifier f x =
    f (_dbiReadReplicaSourceDBInstanceIdentifier x)
        <&> \y -> x { _dbiReadReplicaSourceDBInstanceIdentifier = y }
{-# INLINE dbiReadReplicaSourceDBInstanceIdentifier #-}

-- | Contains one or more identifiers of the read replicas associated with this
-- DB instance.
dbiReadReplicaDBInstanceIdentifiers :: Lens' DBInstance ([Text])
dbiReadReplicaDBInstanceIdentifiers f x =
    f (_dbiReadReplicaDBInstanceIdentifiers x)
        <&> \y -> x { _dbiReadReplicaDBInstanceIdentifiers = y }
{-# INLINE dbiReadReplicaDBInstanceIdentifiers #-}

-- | License model information for this DB instance.
dbiLicenseModel :: Lens' DBInstance (Maybe Text)
dbiLicenseModel f x =
    f (_dbiLicenseModel x)
        <&> \y -> x { _dbiLicenseModel = y }
{-# INLINE dbiLicenseModel #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops :: Lens' DBInstance (Maybe Integer)
dbiIops f x =
    f (_dbiIops x)
        <&> \y -> x { _dbiIops = y }
{-# INLINE dbiIops #-}

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMemberships :: Lens' DBInstance ([OptionGroupMembership])
dbiOptionGroupMemberships f x =
    f (_dbiOptionGroupMemberships x)
        <&> \y -> x { _dbiOptionGroupMemberships = y }
{-# INLINE dbiOptionGroupMemberships #-}

-- | If present, specifies the name of the character set that this instance is
-- associated with.
dbiCharacterSetName :: Lens' DBInstance (Maybe Text)
dbiCharacterSetName f x =
    f (_dbiCharacterSetName x)
        <&> \y -> x { _dbiCharacterSetName = y }
{-# INLINE dbiCharacterSetName #-}

-- | If present, specifies the name of the secondary Availability Zone for a DB
-- instance with multi-AZ support.
dbiSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiSecondaryAvailabilityZone f x =
    f (_dbiSecondaryAvailabilityZone x)
        <&> \y -> x { _dbiSecondaryAvailabilityZone = y }
{-# INLINE dbiSecondaryAvailabilityZone #-}

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
dbiPubliclyAccessible f x =
    f (_dbiPubliclyAccessible x)
        <&> \y -> x { _dbiPubliclyAccessible = y }
{-# INLINE dbiPubliclyAccessible #-}

-- | The status of a read replica. If the instance is not a read replica, this
-- will be blank.
dbiStatusInfos :: Lens' DBInstance ([DBInstanceStatusInfo])
dbiStatusInfos f x =
    f (_dbiStatusInfos x)
        <&> \y -> x { _dbiStatusInfos = y }
{-# INLINE dbiStatusInfos #-}

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstance"

-- | Provides a list of status information for a DB instance.
data DBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType :: Maybe Text
      -- ^ This value is currently "read replication.".
    , _dbisiNormal :: Maybe Bool
      -- ^ Boolean value that is true if the instance is operating normally,
      -- or false if the instance is in an error state.
    , _dbisiStatus :: Maybe Text
      -- ^ Status of the DB instance. For a StatusType of read replica, the
      -- values can be replicating, error, stopped, or terminated.
    , _dbisiMessage :: Maybe Text
      -- ^ Details of the error if there is an error for the instance. If
      -- the instance is not in an error state, this value is blank.
    } deriving (Show, Generic)

-- | This value is currently "read replication.".
dbisiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatusType f x =
    f (_dbisiStatusType x)
        <&> \y -> x { _dbisiStatusType = y }
{-# INLINE dbisiStatusType #-}

-- | Boolean value that is true if the instance is operating normally, or false
-- if the instance is in an error state.
dbisiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
dbisiNormal f x =
    f (_dbisiNormal x)
        <&> \y -> x { _dbisiNormal = y }
{-# INLINE dbisiNormal #-}

-- | Status of the DB instance. For a StatusType of read replica, the values can
-- be replicating, error, stopped, or terminated.
dbisiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatus f x =
    f (_dbisiStatus x)
        <&> \y -> x { _dbisiStatus = y }
{-# INLINE dbisiStatus #-}

-- | Details of the error if there is an error for the instance. If the instance
-- is not in an error state, this value is blank.
dbisiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiMessage f x =
    f (_dbisiMessage x)
        <&> \y -> x { _dbisiMessage = y }
{-# INLINE dbisiMessage #-}

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
      -- ^ Provides the name of the DB parameter group.
    , _dbpgDBParameterGroupFamily :: Maybe Text
      -- ^ Provides the name of the DB parameter group family that this DB
      -- parameter group is compatible with.
    , _dbpgDescription :: Maybe Text
      -- ^ Provides the customer-specified description for this DB parameter
      -- group.
    } deriving (Show, Generic)

-- | Provides the name of the DB parameter group.
dbpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupName f x =
    f (_dbpgDBParameterGroupName x)
        <&> \y -> x { _dbpgDBParameterGroupName = y }
{-# INLINE dbpgDBParameterGroupName #-}

-- | Provides the name of the DB parameter group family that this DB parameter
-- group is compatible with.
dbpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupFamily f x =
    f (_dbpgDBParameterGroupFamily x)
        <&> \y -> x { _dbpgDBParameterGroupFamily = y }
{-# INLINE dbpgDBParameterGroupFamily #-}

-- | Provides the customer-specified description for this DB parameter group.
dbpgDescription :: Lens' DBParameterGroup (Maybe Text)
dbpgDescription f x =
    f (_dbpgDescription x)
        <&> \y -> x { _dbpgDescription = y }
{-# INLINE dbpgDescription #-}

instance FromXML DBParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

-- | The status of the DB parameter group. This data type is used as a response
-- element in the following actions: CreateDBInstance
-- CreateDBInstanceReadReplica DeleteDBInstance ModifyDBInstance
-- RebootDBInstance RestoreDBInstanceFromDBSnapshot.
data DBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName :: Maybe Text
      -- ^ The name of the DP parameter group.
    , _dbpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Show, Generic)

-- | The name of the DP parameter group.
dbpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dbpgsDBParameterGroupName f x =
    f (_dbpgsDBParameterGroupName x)
        <&> \y -> x { _dbpgsDBParameterGroupName = y }
{-# INLINE dbpgsDBParameterGroupName #-}

-- | The status of parameter updates.
dbpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dbpgsParameterApplyStatus f x =
    f (_dbpgsParameterApplyStatus x)
        <&> \y -> x { _dbpgsParameterApplyStatus = y }
{-# INLINE dbpgsParameterApplyStatus #-}

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
      -- ^ Provides the AWS ID of the owner of a specific DB security group.
    , _dbsgDBSecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the DB security group.
    , _dbsgDBSecurityGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB security group.
    , _dbsgVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB security group.
    , _dbsgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ Contains a list of EC2SecurityGroup elements.
    , _dbsgIPRanges :: [IPRange]
      -- ^ Contains a list of IPRange elements.
    } deriving (Show, Generic)

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId f x =
    f (_dbsgOwnerId x)
        <&> \y -> x { _dbsgOwnerId = y }
{-# INLINE dbsgOwnerId #-}

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName f x =
    f (_dbsgDBSecurityGroupName x)
        <&> \y -> x { _dbsgDBSecurityGroupName = y }
{-# INLINE dbsgDBSecurityGroupName #-}

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription f x =
    f (_dbsgDBSecurityGroupDescription x)
        <&> \y -> x { _dbsgDBSecurityGroupDescription = y }
{-# INLINE dbsgDBSecurityGroupDescription #-}

-- | Provides the VpcId of the DB security group.
dbsgVpcId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVpcId f x =
    f (_dbsgVpcId x)
        <&> \y -> x { _dbsgVpcId = y }
{-# INLINE dbsgVpcId #-}

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup ([EC2SecurityGroup])
dbsgEC2SecurityGroups f x =
    f (_dbsgEC2SecurityGroups x)
        <&> \y -> x { _dbsgEC2SecurityGroups = y }
{-# INLINE dbsgEC2SecurityGroups #-}

-- | Contains a list of IPRange elements.
dbsgIPRanges :: Lens' DBSecurityGroup ([IPRange])
dbsgIPRanges f x =
    f (_dbsgIPRanges x)
        <&> \y -> x { _dbsgIPRanges = y }
{-# INLINE dbsgIPRanges #-}

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

-- | This data type is used as a response element in the following actions:
-- ModifyDBInstance RebootDBInstance RestoreDBInstanceFromDBSnapshot
-- RestoreDBInstanceToPointInTime.
data DBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group.
    , _dbsgmStatus :: Maybe Text
      -- ^ The status of the DB security group.
    } deriving (Show, Generic)

-- | The name of the DB security group.
dbsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dbsgmDBSecurityGroupName f x =
    f (_dbsgmDBSecurityGroupName x)
        <&> \y -> x { _dbsgmDBSecurityGroupName = y }
{-# INLINE dbsgmDBSecurityGroupName #-}

-- | The status of the DB security group.
dbsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dbsgmStatus f x =
    f (_dbsgmStatus x)
        <&> \y -> x { _dbsgmStatus = y }
{-# INLINE dbsgmStatus #-}

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
      -- ^ Specifies the identifier for the DB snapshot.
    , _dbsDBInstanceIdentifier :: Maybe Text
      -- ^ Specifies the DB instance identifier of the DB instance this DB
      -- snapshot was created from.
    , _dbsSnapshotCreateTime :: Maybe ISO8601
      -- ^ Provides the time (UTC) when the snapshot was taken.
    , _dbsEngine :: Maybe Text
      -- ^ Specifies the name of the database engine.
    , _dbsAllocatedStorage :: Maybe Integer
      -- ^ Specifies the allocated storage size in gigabytes (GB).
    , _dbsStatus :: Maybe Text
      -- ^ Specifies the status of this DB snapshot.
    , _dbsPort :: Maybe Integer
      -- ^ Specifies the port that the database engine was listening on at
      -- the time of the snapshot.
    , _dbsAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance was
      -- located in at the time of the DB snapshot.
    , _dbsVpcId :: Maybe Text
      -- ^ Provides the Vpc Id associated with the DB snapshot.
    , _dbsInstanceCreateTime :: Maybe ISO8601
      -- ^ Specifies the time (UTC) when the snapshot was taken.
    , _dbsMasterUsername :: Maybe Text
      -- ^ Provides the master username for the DB snapshot.
    , _dbsEngineVersion :: Maybe Text
      -- ^ Specifies the version of the database engine.
    , _dbsLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance.
    , _dbsSnapshotType :: Maybe Text
      -- ^ Provides the type of the DB snapshot.
    , _dbsIops :: Maybe Integer
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value
      -- of the DB instance at the time of the snapshot.
    , _dbsOptionGroupName :: Maybe Text
      -- ^ Provides the option group name for the DB snapshot.
    , _dbsPercentProgress :: Maybe Integer
      -- ^ The percentage of the estimated data that has been transferred.
    , _dbsSourceRegion :: Maybe Text
      -- ^ The region that the DB snapshot was created in or copied from.
    } deriving (Show, Generic)

-- | Specifies the identifier for the DB snapshot.
dbsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBSnapshotIdentifier f x =
    f (_dbsDBSnapshotIdentifier x)
        <&> \y -> x { _dbsDBSnapshotIdentifier = y }
{-# INLINE dbsDBSnapshotIdentifier #-}

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBInstanceIdentifier f x =
    f (_dbsDBInstanceIdentifier x)
        <&> \y -> x { _dbsDBInstanceIdentifier = y }
{-# INLINE dbsDBInstanceIdentifier #-}

-- | Provides the time (UTC) when the snapshot was taken.
dbsSnapshotCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsSnapshotCreateTime f x =
    f (_dbsSnapshotCreateTime x)
        <&> \y -> x { _dbsSnapshotCreateTime = y }
{-# INLINE dbsSnapshotCreateTime #-}

-- | Specifies the name of the database engine.
dbsEngine :: Lens' DBSnapshot (Maybe Text)
dbsEngine f x =
    f (_dbsEngine x)
        <&> \y -> x { _dbsEngine = y }
{-# INLINE dbsEngine #-}

-- | Specifies the allocated storage size in gigabytes (GB).
dbsAllocatedStorage :: Lens' DBSnapshot (Maybe Integer)
dbsAllocatedStorage f x =
    f (_dbsAllocatedStorage x)
        <&> \y -> x { _dbsAllocatedStorage = y }
{-# INLINE dbsAllocatedStorage #-}

-- | Specifies the status of this DB snapshot.
dbsStatus :: Lens' DBSnapshot (Maybe Text)
dbsStatus f x =
    f (_dbsStatus x)
        <&> \y -> x { _dbsStatus = y }
{-# INLINE dbsStatus #-}

-- | Specifies the port that the database engine was listening on at the time of
-- the snapshot.
dbsPort :: Lens' DBSnapshot (Maybe Integer)
dbsPort f x =
    f (_dbsPort x)
        <&> \y -> x { _dbsPort = y }
{-# INLINE dbsPort #-}

-- | Specifies the name of the Availability Zone the DB instance was located in
-- at the time of the DB snapshot.
dbsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dbsAvailabilityZone f x =
    f (_dbsAvailabilityZone x)
        <&> \y -> x { _dbsAvailabilityZone = y }
{-# INLINE dbsAvailabilityZone #-}

-- | Provides the Vpc Id associated with the DB snapshot.
dbsVpcId :: Lens' DBSnapshot (Maybe Text)
dbsVpcId f x =
    f (_dbsVpcId x)
        <&> \y -> x { _dbsVpcId = y }
{-# INLINE dbsVpcId #-}

-- | Specifies the time (UTC) when the snapshot was taken.
dbsInstanceCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsInstanceCreateTime f x =
    f (_dbsInstanceCreateTime x)
        <&> \y -> x { _dbsInstanceCreateTime = y }
{-# INLINE dbsInstanceCreateTime #-}

-- | Provides the master username for the DB snapshot.
dbsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dbsMasterUsername f x =
    f (_dbsMasterUsername x)
        <&> \y -> x { _dbsMasterUsername = y }
{-# INLINE dbsMasterUsername #-}

-- | Specifies the version of the database engine.
dbsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dbsEngineVersion f x =
    f (_dbsEngineVersion x)
        <&> \y -> x { _dbsEngineVersion = y }
{-# INLINE dbsEngineVersion #-}

-- | License model information for the restored DB instance.
dbsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dbsLicenseModel f x =
    f (_dbsLicenseModel x)
        <&> \y -> x { _dbsLicenseModel = y }
{-# INLINE dbsLicenseModel #-}

-- | Provides the type of the DB snapshot.
dbsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dbsSnapshotType f x =
    f (_dbsSnapshotType x)
        <&> \y -> x { _dbsSnapshotType = y }
{-# INLINE dbsSnapshotType #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB
-- instance at the time of the snapshot.
dbsIops :: Lens' DBSnapshot (Maybe Integer)
dbsIops f x =
    f (_dbsIops x)
        <&> \y -> x { _dbsIops = y }
{-# INLINE dbsIops #-}

-- | Provides the option group name for the DB snapshot.
dbsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dbsOptionGroupName f x =
    f (_dbsOptionGroupName x)
        <&> \y -> x { _dbsOptionGroupName = y }
{-# INLINE dbsOptionGroupName #-}

-- | The percentage of the estimated data that has been transferred.
dbsPercentProgress :: Lens' DBSnapshot (Maybe Integer)
dbsPercentProgress f x =
    f (_dbsPercentProgress x)
        <&> \y -> x { _dbsPercentProgress = y }
{-# INLINE dbsPercentProgress #-}

-- | The region that the DB snapshot was created in or copied from.
dbsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dbsSourceRegion f x =
    f (_dbsSourceRegion x)
        <&> \y -> x { _dbsSourceRegion = y }
{-# INLINE dbsSourceRegion #-}

instance FromXML DBSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSnapshot"

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
data DBSubnetGroup = DBSubnetGroup
    { _dbshDBSubnetGroupName :: Maybe Text
      -- ^ Specifies the name of the DB subnet group.
    , _dbshDBSubnetGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB subnet group.
    , _dbshVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB subnet group.
    , _dbshSubnetGroupStatus :: Maybe Text
      -- ^ Provides the status of the DB subnet group.
    , _dbshSubnets :: [Subnet]
      -- ^ Contains a list of Subnet elements.
    } deriving (Show, Generic)

-- | Specifies the name of the DB subnet group.
dbshDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dbshDBSubnetGroupName f x =
    f (_dbshDBSubnetGroupName x)
        <&> \y -> x { _dbshDBSubnetGroupName = y }
{-# INLINE dbshDBSubnetGroupName #-}

-- | Provides the description of the DB subnet group.
dbshDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dbshDBSubnetGroupDescription f x =
    f (_dbshDBSubnetGroupDescription x)
        <&> \y -> x { _dbshDBSubnetGroupDescription = y }
{-# INLINE dbshDBSubnetGroupDescription #-}

-- | Provides the VpcId of the DB subnet group.
dbshVpcId :: Lens' DBSubnetGroup (Maybe Text)
dbshVpcId f x =
    f (_dbshVpcId x)
        <&> \y -> x { _dbshVpcId = y }
{-# INLINE dbshVpcId #-}

-- | Provides the status of the DB subnet group.
dbshSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dbshSubnetGroupStatus f x =
    f (_dbshSubnetGroupStatus x)
        <&> \y -> x { _dbshSubnetGroupStatus = y }
{-# INLINE dbshSubnetGroupStatus #-}

-- | Contains a list of Subnet elements.
dbshSubnets :: Lens' DBSubnetGroup ([Subnet])
dbshSubnets f x =
    f (_dbshSubnets x)
        <&> \y -> x { _dbshSubnets = y }
{-# INLINE dbshSubnets #-}

instance FromXML DBSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSubnetGroup"

instance ToQuery DBSubnetGroup where
    toQuery = genericQuery def

-- | This data type is used as a response element to DescribeDBLogFiles.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLogFileName :: Maybe Text
      -- ^ The name of the log file for the specified DB instance.
    , _ddblfdLastWritten :: Maybe Integer
      -- ^ A POSIX timestamp when the last log entry was written.
    , _ddblfdSize :: Maybe Integer
      -- ^ The size, in bytes, of the log file for the specified DB
      -- instance.
    } deriving (Show, Generic)

-- | The name of the log file for the specified DB instance.
ddblfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddblfdLogFileName f x =
    f (_ddblfdLogFileName x)
        <&> \y -> x { _ddblfdLogFileName = y }
{-# INLINE ddblfdLogFileName #-}

-- | A POSIX timestamp when the last log entry was written.
ddblfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdLastWritten f x =
    f (_ddblfdLastWritten x)
        <&> \y -> x { _ddblfdLastWritten = y }
{-# INLINE ddblfdLastWritten #-}

-- | The size, in bytes, of the log file for the specified DB instance.
ddblfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdSize f x =
    f (_ddblfdSize x)
        <&> \y -> x { _ddblfdSize = y }
{-# INLINE ddblfdSize #-}

instance FromXML DescribeDBLogFilesDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDBLogFilesDetails"

-- | This data type is used as a response element in the following actions:
-- AuthorizeDBSecurityGroupIngress DescribeDBSecurityGroups
-- RevokeDBSecurityGroupIngress.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ Provides the status of the EC2 security group. Status can be
      -- "authorizing", "authorized", "revoking", and "revoked".
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the EC2 security group.
    , _ecsgEC2SecurityGroupId :: Maybe Text
      -- ^ Specifies the id of the EC2 security group.
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ Specifies the AWS ID of the owner of the EC2 security group
      -- specified in the EC2SecurityGroupName field.
    } deriving (Show, Generic)

-- | Provides the status of the EC2 security group. Status can be "authorizing",
-- "authorized", "revoking", and "revoked".
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus f x =
    f (_ecsgStatus x)
        <&> \y -> x { _ecsgStatus = y }
{-# INLINE ecsgStatus #-}

-- | Specifies the name of the EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName f x =
    f (_ecsgEC2SecurityGroupName x)
        <&> \y -> x { _ecsgEC2SecurityGroupName = y }
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | Specifies the id of the EC2 security group.
ecsgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupId f x =
    f (_ecsgEC2SecurityGroupId x)
        <&> \y -> x { _ecsgEC2SecurityGroupId = y }
{-# INLINE ecsgEC2SecurityGroupId #-}

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId f x =
    f (_ecsgEC2SecurityGroupOwnerId x)
        <&> \y -> x { _ecsgEC2SecurityGroupOwnerId = y }
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Specifies the connection endpoint.
data Endpoint = Endpoint
    { _eAddress :: Maybe Text
      -- ^ Specifies the DNS address of the DB instance.
    , _ePort :: Maybe Integer
      -- ^ Specifies the port that the database engine is listening on.
    } deriving (Show, Generic)

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress f x =
    f (_eAddress x)
        <&> \y -> x { _eAddress = y }
{-# INLINE eAddress #-}

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort f x =
    f (_ePort x)
        <&> \y -> x { _ePort = y }
{-# INLINE ePort #-}

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
data EngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the DB parameter group family which the
      -- engine default parameters apply to.
    , _edMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- EngineDefaults request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords .
    , _edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    } deriving (Show, Generic)

-- | Specifies the name of the DB parameter group family which the engine
-- default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily f x =
    f (_edDBParameterGroupFamily x)
        <&> \y -> x { _edDBParameterGroupFamily = y }
{-# INLINE edDBParameterGroupFamily #-}

-- | An optional pagination token provided by a previous EngineDefaults request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker f x =
    f (_edMarker x)
        <&> \y -> x { _edMarker = y }
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults ([Parameter])
edParameters f x =
    f (_edParameters x)
        <&> \y -> x { _edParameters = y }
{-# INLINE edParameters #-}

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | This data type is used as a response element in the DescribeEvents action.
data Event = Event
    { _eySourceIdentifier :: Maybe Text
      -- ^ Provides the identifier for the source of the event.
    , _eySourceType :: Maybe SourceType
      -- ^ Specifies the source type for this event.
    , _eyMessage :: Maybe Text
      -- ^ Provides the text of this event.
    , _eyEventCategories :: [Text]
      -- ^ Specifies the category for the event.
    , _eyDate :: Maybe ISO8601
      -- ^ Specifies the date and time of the event.
    } deriving (Show, Generic)

-- | Provides the identifier for the source of the event.
eySourceIdentifier :: Lens' Event (Maybe Text)
eySourceIdentifier f x =
    f (_eySourceIdentifier x)
        <&> \y -> x { _eySourceIdentifier = y }
{-# INLINE eySourceIdentifier #-}

-- | Specifies the source type for this event.
eySourceType :: Lens' Event (Maybe SourceType)
eySourceType f x =
    f (_eySourceType x)
        <&> \y -> x { _eySourceType = y }
{-# INLINE eySourceType #-}

-- | Provides the text of this event.
eyMessage :: Lens' Event (Maybe Text)
eyMessage f x =
    f (_eyMessage x)
        <&> \y -> x { _eyMessage = y }
{-# INLINE eyMessage #-}

-- | Specifies the category for the event.
eyEventCategories :: Lens' Event ([Text])
eyEventCategories f x =
    f (_eyEventCategories x)
        <&> \y -> x { _eyEventCategories = y }
{-# INLINE eyEventCategories #-}

-- | Specifies the date and time of the event.
eyDate :: Lens' Event (Maybe ISO8601)
eyDate f x =
    f (_eyDate x)
        <&> \y -> x { _eyDate = y }
{-# INLINE eyDate #-}

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
data EventCategoriesMap = EventCategoriesMap
    { _ecqSourceType :: Maybe Text
      -- ^ The source type that the returned categories belong to.
    , _ecqEventCategories :: [Text]
      -- ^ The event categories for the specified source type.
    } deriving (Show, Generic)

-- | The source type that the returned categories belong to.
ecqSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecqSourceType f x =
    f (_ecqSourceType x)
        <&> \y -> x { _ecqSourceType = y }
{-# INLINE ecqSourceType #-}

-- | The event categories for the specified source type.
ecqEventCategories :: Lens' EventCategoriesMap ([Text])
ecqEventCategories f x =
    f (_ecqEventCategories x)
        <&> \y -> x { _ecqEventCategories = y }
{-# INLINE ecqEventCategories #-}

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
data EventSubscription = EventSubscription
    { _esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the RDS event
      -- notification subscription.
    , _esCustSubscriptionId :: Maybe Text
      -- ^ The RDS event notification subscription Id.
    , _esSnsTopicArn :: Maybe Text
      -- ^ The topic ARN of the RDS event notification subscription.
    , _esStatus :: Maybe Text
      -- ^ The status of the RDS event notification subscription.
      -- Constraints: Can be one of the following: creating | modifying |
      -- deleting | active | no-permission | topic-not-exist The status
      -- "no-permission" indicates that RDS no longer has permission to
      -- post to the SNS topic. The status "topic-not-exist" indicates
      -- that the topic was deleted after the subscription was created.
    , _esSubscriptionCreationTime :: Maybe Text
      -- ^ The time the RDS event notification subscription was created.
    , _esSourceType :: Maybe Text
      -- ^ The source type for the RDS event notification subscription.
    , _esSourceIdsList :: [Text]
      -- ^ A list of source Ids for the RDS event notification subscription.
    , _esEventCategoriesList :: [Text]
      -- ^ A list of event categories for the RDS event notification
      -- subscription.
    , _esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating if the subscription is enabled. True
      -- indicates the subscription is enabled.
    } deriving (Show, Generic)

-- | The AWS customer account associated with the RDS event notification
-- subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId f x =
    f (_esCustomerAwsId x)
        <&> \y -> x { _esCustomerAwsId = y }
{-# INLINE esCustomerAwsId #-}

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId f x =
    f (_esCustSubscriptionId x)
        <&> \y -> x { _esCustSubscriptionId = y }
{-# INLINE esCustSubscriptionId #-}

-- | The topic ARN of the RDS event notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn f x =
    f (_esSnsTopicArn x)
        <&> \y -> x { _esSnsTopicArn = y }
{-# INLINE esSnsTopicArn #-}

-- | The status of the RDS event notification subscription. Constraints: Can be
-- one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist The status "no-permission" indicates that
-- RDS no longer has permission to post to the SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus f x =
    f (_esStatus x)
        <&> \y -> x { _esStatus = y }
{-# INLINE esStatus #-}

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime f x =
    f (_esSubscriptionCreationTime x)
        <&> \y -> x { _esSubscriptionCreationTime = y }
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType f x =
    f (_esSourceType x)
        <&> \y -> x { _esSourceType = y }
{-# INLINE esSourceType #-}

-- | A list of source Ids for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription ([Text])
esSourceIdsList f x =
    f (_esSourceIdsList x)
        <&> \y -> x { _esSourceIdsList = y }
{-# INLINE esSourceIdsList #-}

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription ([Text])
esEventCategoriesList f x =
    f (_esEventCategoriesList x)
        <&> \y -> x { _esEventCategoriesList = y }
{-# INLINE esEventCategoriesList #-}

-- | A Boolean value indicating if the subscription is enabled. True indicates
-- the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled f x =
    f (_esEnabled x)
        <&> \y -> x { _esEnabled = y }
{-# INLINE esEnabled #-}

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
data IPRange = IPRange
    { _iprStatus :: Maybe Text
      -- ^ Specifies the status of the IP range. Status can be
      -- "authorizing", "authorized", "revoking", and "revoked".
    , _iprCIDRIP :: Maybe Text
      -- ^ Specifies the IP range.
    } deriving (Show, Generic)

-- | Specifies the status of the IP range. Status can be "authorizing",
-- "authorized", "revoking", and "revoked".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus f x =
    f (_iprStatus x)
        <&> \y -> x { _iprStatus = y }
{-# INLINE iprStatus #-}

-- | Specifies the IP range.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP f x =
    f (_iprCIDRIP x)
        <&> \y -> x { _iprCIDRIP = y }
{-# INLINE iprCIDRIP #-}

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericQuery def

-- | Option details.
data Option = Option
    { _onOptionName :: Maybe Text
      -- ^ The name of the option.
    , _onOptionDescription :: Maybe Text
      -- ^ The description of the option.
    , _onPersistent :: Maybe Bool
      -- ^ Indicate if this option is persistent.
    , _onPermanent :: Maybe Bool
      -- ^ Indicate if this option is permanent.
    , _onPort :: Maybe Integer
      -- ^ If required, the port configured for this option to use.
    , _onOptionSettings :: [OptionSetting]
      -- ^ The option settings for this option.
    , _onDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this DB security
      -- group allows access to the port.
    , _onVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this VPC security
      -- group allows access to the port.
    } deriving (Show, Generic)

-- | The name of the option.
onOptionName :: Lens' Option (Maybe Text)
onOptionName f x =
    f (_onOptionName x)
        <&> \y -> x { _onOptionName = y }
{-# INLINE onOptionName #-}

-- | The description of the option.
onOptionDescription :: Lens' Option (Maybe Text)
onOptionDescription f x =
    f (_onOptionDescription x)
        <&> \y -> x { _onOptionDescription = y }
{-# INLINE onOptionDescription #-}

-- | Indicate if this option is persistent.
onPersistent :: Lens' Option (Maybe Bool)
onPersistent f x =
    f (_onPersistent x)
        <&> \y -> x { _onPersistent = y }
{-# INLINE onPersistent #-}

-- | Indicate if this option is permanent.
onPermanent :: Lens' Option (Maybe Bool)
onPermanent f x =
    f (_onPermanent x)
        <&> \y -> x { _onPermanent = y }
{-# INLINE onPermanent #-}

-- | If required, the port configured for this option to use.
onPort :: Lens' Option (Maybe Integer)
onPort f x =
    f (_onPort x)
        <&> \y -> x { _onPort = y }
{-# INLINE onPort #-}

-- | The option settings for this option.
onOptionSettings :: Lens' Option ([OptionSetting])
onOptionSettings f x =
    f (_onOptionSettings x)
        <&> \y -> x { _onOptionSettings = y }
{-# INLINE onOptionSettings #-}

-- | If the option requires access to a port, then this DB security group allows
-- access to the port.
onDBSecurityGroupMemberships :: Lens' Option ([DBSecurityGroupMembership])
onDBSecurityGroupMemberships f x =
    f (_onDBSecurityGroupMemberships x)
        <&> \y -> x { _onDBSecurityGroupMemberships = y }
{-# INLINE onDBSecurityGroupMemberships #-}

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
onVpcSecurityGroupMemberships :: Lens' Option ([VpcSecurityGroupMembership])
onVpcSecurityGroupMemberships f x =
    f (_onVpcSecurityGroupMemberships x)
        <&> \y -> x { _onVpcSecurityGroupMemberships = y }
{-# INLINE onVpcSecurityGroupMemberships #-}

instance FromXML Option where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Option"

instance ToQuery Option where
    toQuery = genericQuery def

-- | A list of all available options.
data OptionConfiguration = OptionConfiguration
    { _ocOptionName :: Text
      -- ^ The configuration of options to include in a group.
    , _ocPort :: Maybe Integer
      -- ^ The optional port for the option.
    , _ocDBSecurityGroupMemberships :: [Text]
      -- ^ A list of DBSecurityGroupMemebrship name strings used for this
      -- option.
    , _ocVpcSecurityGroupMemberships :: [Text]
      -- ^ A list of VpcSecurityGroupMemebrship name strings used for this
      -- option.
    , _ocOptionSettings :: [OptionSetting]
      -- ^ The option settings to include in an option group.
    } deriving (Show, Generic)

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration (Text)
ocOptionName f x =
    f (_ocOptionName x)
        <&> \y -> x { _ocOptionName = y }
{-# INLINE ocOptionName #-}

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Integer)
ocPort f x =
    f (_ocPort x)
        <&> \y -> x { _ocPort = y }
{-# INLINE ocPort #-}

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration ([Text])
ocDBSecurityGroupMemberships f x =
    f (_ocDBSecurityGroupMemberships x)
        <&> \y -> x { _ocDBSecurityGroupMemberships = y }
{-# INLINE ocDBSecurityGroupMemberships #-}

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupMemberships :: Lens' OptionConfiguration ([Text])
ocVpcSecurityGroupMemberships f x =
    f (_ocVpcSecurityGroupMemberships x)
        <&> \y -> x { _ocVpcSecurityGroupMemberships = y }
{-# INLINE ocVpcSecurityGroupMemberships #-}

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration ([OptionSetting])
ocOptionSettings f x =
    f (_ocOptionSettings x)
        <&> \y -> x { _ocOptionSettings = y }
{-# INLINE ocOptionSettings #-}

instance ToQuery OptionConfiguration where
    toQuery = genericQuery def

-- | 
data OptionGroup = OptionGroup
    { _ogOptionGroupName :: Maybe Text
      -- ^ Specifies the name of the option group.
    , _ogOptionGroupDescription :: Maybe Text
      -- ^ Provides the description of the option group.
    , _ogEngineName :: Maybe Text
      -- ^ Engine name that this option group can be applied to.
    , _ogMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version associated with this option
      -- group.
    , _ogOptions :: [Option]
      -- ^ Indicates what options are available in the option group.
    , _ogAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
      -- ^ Indicates whether this option group can be applied to both VPC
      -- and non-VPC instances. The value 'true' indicates the option
      -- group can be applied to both VPC and non-VPC instances.
    , _ogVpcId :: Maybe Text
      -- ^ If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field
      -- is blank. If AllowsVpcAndNonVpcInstanceMemberships is 'true' and
      -- this field is blank, then this option group can be applied to
      -- both VPC and non-VPC instances. If this field contains a value,
      -- then this option group can only be applied to instances that are
      -- in the VPC indicated by this field.
    } deriving (Show, Generic)

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName f x =
    f (_ogOptionGroupName x)
        <&> \y -> x { _ogOptionGroupName = y }
{-# INLINE ogOptionGroupName #-}

-- | Provides the description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription f x =
    f (_ogOptionGroupDescription x)
        <&> \y -> x { _ogOptionGroupDescription = y }
{-# INLINE ogOptionGroupDescription #-}

-- | Engine name that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName f x =
    f (_ogEngineName x)
        <&> \y -> x { _ogEngineName = y }
{-# INLINE ogEngineName #-}

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion f x =
    f (_ogMajorEngineVersion x)
        <&> \y -> x { _ogMajorEngineVersion = y }
{-# INLINE ogMajorEngineVersion #-}

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup ([Option])
ogOptions f x =
    f (_ogOptions x)
        <&> \y -> x { _ogOptions = y }
{-# INLINE ogOptions #-}

-- | Indicates whether this option group can be applied to both VPC and non-VPC
-- instances. The value 'true' indicates the option group can be applied to
-- both VPC and non-VPC instances.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVpcAndNonVpcInstanceMemberships f x =
    f (_ogAllowsVpcAndNonVpcInstanceMemberships x)
        <&> \y -> x { _ogAllowsVpcAndNonVpcInstanceMemberships = y }
{-# INLINE ogAllowsVpcAndNonVpcInstanceMemberships #-}

-- | If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
-- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is blank,
-- then this option group can be applied to both VPC and non-VPC instances. If
-- this field contains a value, then this option group can only be applied to
-- instances that are in the VPC indicated by this field.
ogVpcId :: Lens' OptionGroup (Maybe Text)
ogVpcId f x =
    f (_ogVpcId x)
        <&> \y -> x { _ogVpcId = y }
{-# INLINE ogVpcId #-}

instance FromXML OptionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroup"

-- | Provides information on the option groups the DB instance is a member of.
data OptionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName :: Maybe Text
      -- ^ The name of the option group that the instance belongs to.
    , _ogmStatus :: Maybe Text
      -- ^ The status of the DB instance's option group membership (e.g.
      -- in-sync, pending, pending-maintenance, applying).
    } deriving (Show, Generic)

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName f x =
    f (_ogmOptionGroupName x)
        <&> \y -> x { _ogmOptionGroupName = y }
{-# INLINE ogmOptionGroupName #-}

-- | The status of the DB instance's option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus f x =
    f (_ogmStatus x)
        <&> \y -> x { _ogmStatus = y }
{-# INLINE ogmStatus #-}

instance FromXML OptionGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupMembership"

instance ToQuery OptionGroupMembership where
    toQuery = genericQuery def

-- | Available option.
data OptionGroupOption = OptionGroupOption
    { _ogqName :: Maybe Text
      -- ^ The name of the option.
    , _ogqDescription :: Maybe Text
      -- ^ The description of the option.
    , _ogqEngineName :: Maybe Text
      -- ^ Engine name that this option can be applied to.
    , _ogqMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version that the option is available
      -- for.
    , _ogqMinimumRequiredMinorEngineVersion :: Maybe Text
      -- ^ The minimum required engine version for the option to be applied.
    , _ogqPortRequired :: Maybe Bool
      -- ^ Specifies whether the option requires a port.
    , _ogqDefaultPort :: Maybe Integer
      -- ^ If the option requires a port, specifies the default port for the
      -- option.
    , _ogqOptionsDependedOn :: [Text]
      -- ^ List of all options that are prerequisites for this option.
    , _ogqPersistent :: Maybe Bool
      -- ^ A persistent option cannot be removed from the option group once
      -- the option group is used, but this option can be removed from the
      -- db instance while modifying the related data and assigning
      -- another option group without this option.
    , _ogqPermanent :: Maybe Bool
      -- ^ A permanent option cannot be removed from the option group once
      -- the option group is used, and it cannot be removed from the db
      -- instance after assigning an option group with this permanent
      -- option.
    , _ogqOptionGroupOptionSettings :: [OptionGroupOptionSetting]
      -- ^ Specifies the option settings that are available (and the default
      -- value) for each option in an option group.
    } deriving (Show, Generic)

-- | The name of the option.
ogqName :: Lens' OptionGroupOption (Maybe Text)
ogqName f x =
    f (_ogqName x)
        <&> \y -> x { _ogqName = y }
{-# INLINE ogqName #-}

-- | The description of the option.
ogqDescription :: Lens' OptionGroupOption (Maybe Text)
ogqDescription f x =
    f (_ogqDescription x)
        <&> \y -> x { _ogqDescription = y }
{-# INLINE ogqDescription #-}

-- | Engine name that this option can be applied to.
ogqEngineName :: Lens' OptionGroupOption (Maybe Text)
ogqEngineName f x =
    f (_ogqEngineName x)
        <&> \y -> x { _ogqEngineName = y }
{-# INLINE ogqEngineName #-}

-- | Indicates the major engine version that the option is available for.
ogqMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogqMajorEngineVersion f x =
    f (_ogqMajorEngineVersion x)
        <&> \y -> x { _ogqMajorEngineVersion = y }
{-# INLINE ogqMajorEngineVersion #-}

-- | The minimum required engine version for the option to be applied.
ogqMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogqMinimumRequiredMinorEngineVersion f x =
    f (_ogqMinimumRequiredMinorEngineVersion x)
        <&> \y -> x { _ogqMinimumRequiredMinorEngineVersion = y }
{-# INLINE ogqMinimumRequiredMinorEngineVersion #-}

-- | Specifies whether the option requires a port.
ogqPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogqPortRequired f x =
    f (_ogqPortRequired x)
        <&> \y -> x { _ogqPortRequired = y }
{-# INLINE ogqPortRequired #-}

-- | If the option requires a port, specifies the default port for the option.
ogqDefaultPort :: Lens' OptionGroupOption (Maybe Integer)
ogqDefaultPort f x =
    f (_ogqDefaultPort x)
        <&> \y -> x { _ogqDefaultPort = y }
{-# INLINE ogqDefaultPort #-}

-- | List of all options that are prerequisites for this option.
ogqOptionsDependedOn :: Lens' OptionGroupOption ([Text])
ogqOptionsDependedOn f x =
    f (_ogqOptionsDependedOn x)
        <&> \y -> x { _ogqOptionsDependedOn = y }
{-# INLINE ogqOptionsDependedOn #-}

-- | A persistent option cannot be removed from the option group once the option
-- group is used, but this option can be removed from the db instance while
-- modifying the related data and assigning another option group without this
-- option.
ogqPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogqPersistent f x =
    f (_ogqPersistent x)
        <&> \y -> x { _ogqPersistent = y }
{-# INLINE ogqPersistent #-}

-- | A permanent option cannot be removed from the option group once the option
-- group is used, and it cannot be removed from the db instance after
-- assigning an option group with this permanent option.
ogqPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogqPermanent f x =
    f (_ogqPermanent x)
        <&> \y -> x { _ogqPermanent = y }
{-# INLINE ogqPermanent #-}

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogqOptionGroupOptionSettings :: Lens' OptionGroupOption ([OptionGroupOptionSetting])
ogqOptionGroupOptionSettings f x =
    f (_ogqOptionGroupOptionSettings x)
        <&> \y -> x { _ogqOptionGroupOptionSettings = y }
{-# INLINE ogqOptionGroupOptionSettings #-}

instance FromXML OptionGroupOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOption"

-- | option group option settings are used to display settings available for
-- each option with their default values and other information. These values
-- are used with the DescribeOptionGroupOptions action.
data OptionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosSettingName :: Maybe Text
      -- ^ The name of the option group option.
    , _ogosSettingDescription :: Maybe Text
      -- ^ The description of the option group option.
    , _ogosDefaultValue :: Maybe Text
      -- ^ The default value for the option group option.
    , _ogosApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type for the option group
      -- option.
    , _ogosAllowedValues :: Maybe Text
      -- ^ Indicates the acceptable values for the option group option.
    , _ogosIsModifiable :: Maybe Bool
      -- ^ Boolean value where true indicates that this option group option
      -- can be changed from the default value.
    } deriving (Show, Generic)

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName f x =
    f (_ogosSettingName x)
        <&> \y -> x { _ogosSettingName = y }
{-# INLINE ogosSettingName #-}

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription f x =
    f (_ogosSettingDescription x)
        <&> \y -> x { _ogosSettingDescription = y }
{-# INLINE ogosSettingDescription #-}

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue f x =
    f (_ogosDefaultValue x)
        <&> \y -> x { _ogosDefaultValue = y }
{-# INLINE ogosDefaultValue #-}

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType f x =
    f (_ogosApplyType x)
        <&> \y -> x { _ogosApplyType = y }
{-# INLINE ogosApplyType #-}

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues f x =
    f (_ogosAllowedValues x)
        <&> \y -> x { _ogosAllowedValues = y }
{-# INLINE ogosAllowedValues #-}

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable f x =
    f (_ogosIsModifiable x)
        <&> \y -> x { _ogosIsModifiable = y }
{-# INLINE ogosIsModifiable #-}

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
      -- ^ The name of the option that has settings that you can set.
    , _osValue :: Maybe Text
      -- ^ The current value of the option setting.
    , _osDefaultValue :: Maybe Text
      -- ^ The default value of the option setting.
    , _osDescription :: Maybe Text
      -- ^ The description of the option setting.
    , _osApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type.
    , _osDataType :: Maybe Text
      -- ^ The data type of the option setting.
    , _osAllowedValues :: Maybe Text
      -- ^ The allowed values of the option setting.
    , _osIsModifiable :: Maybe Bool
      -- ^ A Boolean value that, when true, indicates the option setting can
      -- be modified from the default.
    , _osIsCollection :: Maybe Bool
      -- ^ Indicates if the option setting is part of a collection.
    } deriving (Show, Generic)

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName f x =
    f (_osName x)
        <&> \y -> x { _osName = y }
{-# INLINE osName #-}

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue f x =
    f (_osValue x)
        <&> \y -> x { _osValue = y }
{-# INLINE osValue #-}

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue f x =
    f (_osDefaultValue x)
        <&> \y -> x { _osDefaultValue = y }
{-# INLINE osDefaultValue #-}

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription f x =
    f (_osDescription x)
        <&> \y -> x { _osDescription = y }
{-# INLINE osDescription #-}

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType f x =
    f (_osApplyType x)
        <&> \y -> x { _osApplyType = y }
{-# INLINE osApplyType #-}

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType f x =
    f (_osDataType x)
        <&> \y -> x { _osDataType = y }
{-# INLINE osDataType #-}

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues f x =
    f (_osAllowedValues x)
        <&> \y -> x { _osAllowedValues = y }
{-# INLINE osAllowedValues #-}

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable f x =
    f (_osIsModifiable x)
        <&> \y -> x { _osIsModifiable = y }
{-# INLINE osIsModifiable #-}

-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection f x =
    f (_osIsCollection x)
        <&> \y -> x { _osIsCollection = y }
{-# INLINE osIsCollection #-}

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
      -- ^ The engine type of the orderable DB instance.
    , _odbioEngineVersion :: Maybe Text
      -- ^ The engine version of the orderable DB instance.
    , _odbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance Class for the orderable DB instance.
    , _odbioLicenseModel :: Maybe Text
      -- ^ The license model for the orderable DB instance.
    , _odbioAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable DB instance.
    , _odbioMultiAZCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance is multi-AZ capable.
    , _odbioReadReplicaCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance can have a read
      -- replica.
    , _odbioVpc :: Maybe Bool
      -- ^ Indicates whether this is a VPC orderable DB instance.
    } deriving (Show, Generic)

-- | The engine type of the orderable DB instance.
odbioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngine f x =
    f (_odbioEngine x)
        <&> \y -> x { _odbioEngine = y }
{-# INLINE odbioEngine #-}

-- | The engine version of the orderable DB instance.
odbioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngineVersion f x =
    f (_odbioEngineVersion x)
        <&> \y -> x { _odbioEngineVersion = y }
{-# INLINE odbioEngineVersion #-}

-- | The DB instance Class for the orderable DB instance.
odbioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioDBInstanceClass f x =
    f (_odbioDBInstanceClass x)
        <&> \y -> x { _odbioDBInstanceClass = y }
{-# INLINE odbioDBInstanceClass #-}

-- | The license model for the orderable DB instance.
odbioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioLicenseModel f x =
    f (_odbioLicenseModel x)
        <&> \y -> x { _odbioLicenseModel = y }
{-# INLINE odbioLicenseModel #-}

-- | A list of availability zones for the orderable DB instance.
odbioAvailabilityZones :: Lens' OrderableDBInstanceOption ([AvailabilityZone])
odbioAvailabilityZones f x =
    f (_odbioAvailabilityZones x)
        <&> \y -> x { _odbioAvailabilityZones = y }
{-# INLINE odbioAvailabilityZones #-}

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odbioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioMultiAZCapable f x =
    f (_odbioMultiAZCapable x)
        <&> \y -> x { _odbioMultiAZCapable = y }
{-# INLINE odbioMultiAZCapable #-}

-- | Indicates whether this orderable DB instance can have a read replica.
odbioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioReadReplicaCapable f x =
    f (_odbioReadReplicaCapable x)
        <&> \y -> x { _odbioReadReplicaCapable = y }
{-# INLINE odbioReadReplicaCapable #-}

-- | Indicates whether this is a VPC orderable DB instance.
odbioVpc :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioVpc f x =
    f (_odbioVpc x)
        <&> \y -> x { _odbioVpc = y }
{-# INLINE odbioVpc #-}

instance FromXML OrderableDBInstanceOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableDBInstanceOption"

-- | This data type is used as a request parameter in the ModifyDBParameterGroup
-- and ResetDBParameterGroup actions. This data type is used as a response
-- element in the DescribeEngineDefaultParameters and DescribeDBParameters
-- actions.
data Parameter = Parameter
    { _prParameterName :: Maybe Text
      -- ^ Specifies the name of the parameter.
    , _prParameterValue :: Maybe Text
      -- ^ Specifies the value of the parameter.
    , _prDescription :: Maybe Text
      -- ^ Provides a description of the parameter.
    , _prSource :: Maybe Text
      -- ^ Indicates the source of the parameter value.
    , _prApplyType :: Maybe Text
      -- ^ Specifies the engine specific parameters type.
    , _prDataType :: Maybe Text
      -- ^ Specifies the valid data type for the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ Specifies the valid range of values for the parameter.
    , _prIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    , _prApplyMethod :: Maybe ApplyMethod
      -- ^ Indicates when to apply parameter updates.
    } deriving (Show, Generic)

-- | Specifies the name of the parameter.
prParameterName :: Lens' Parameter (Maybe Text)
prParameterName f x =
    f (_prParameterName x)
        <&> \y -> x { _prParameterName = y }
{-# INLINE prParameterName #-}

-- | Specifies the value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue f x =
    f (_prParameterValue x)
        <&> \y -> x { _prParameterValue = y }
{-# INLINE prParameterValue #-}

-- | Provides a description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription f x =
    f (_prDescription x)
        <&> \y -> x { _prDescription = y }
{-# INLINE prDescription #-}

-- | Indicates the source of the parameter value.
prSource :: Lens' Parameter (Maybe Text)
prSource f x =
    f (_prSource x)
        <&> \y -> x { _prSource = y }
{-# INLINE prSource #-}

-- | Specifies the engine specific parameters type.
prApplyType :: Lens' Parameter (Maybe Text)
prApplyType f x =
    f (_prApplyType x)
        <&> \y -> x { _prApplyType = y }
{-# INLINE prApplyType #-}

-- | Specifies the valid data type for the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType f x =
    f (_prDataType x)
        <&> \y -> x { _prDataType = y }
{-# INLINE prDataType #-}

-- | Specifies the valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues f x =
    f (_prAllowedValues x)
        <&> \y -> x { _prAllowedValues = y }
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable f x =
    f (_prIsModifiable x)
        <&> \y -> x { _prIsModifiable = y }
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion f x =
    f (_prMinimumEngineVersion x)
        <&> \y -> x { _prMinimumEngineVersion = y }
{-# INLINE prMinimumEngineVersion #-}

-- | Indicates when to apply parameter updates.
prApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
prApplyMethod f x =
    f (_prApplyMethod x)
        <&> \y -> x { _prApplyMethod = y }
{-# INLINE prApplyMethod #-}

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
      -- ^ Contains the new DBInstanceClass for the DB instance that will be
      -- applied or is in progress.
    , _pmvAllocatedStorage :: Maybe Integer
      -- ^ Contains the new AllocatedStorage size for the DB instance that
      -- will be applied or is in progress.
    , _pmvMasterUserPassword :: Maybe Text
      -- ^ Contains the pending or in-progress change of the master
      -- credentials for the DB instance.
    , _pmvPort :: Maybe Integer
      -- ^ Specifies the pending port for the DB instance.
    , _pmvBackupRetentionPeriod :: Maybe Integer
      -- ^ Specifies the pending number of days for which automated backups
      -- are retained.
    , _pmvMultiAZ :: Maybe Bool
      -- ^ Indicates that the Single-AZ DB instance is to change to a
      -- Multi-AZ deployment.
    , _pmvEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , _pmvIops :: Maybe Integer
      -- ^ Specifies the new Provisioned IOPS value for the DB instance that
      -- will be applied or is being applied.
    , _pmvDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the new DBInstanceIdentifier for the DB instance that
      -- will be applied or is in progress.
    } deriving (Show, Generic)

-- | Contains the new DBInstanceClass for the DB instance that will be applied
-- or is in progress.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass f x =
    f (_pmvDBInstanceClass x)
        <&> \y -> x { _pmvDBInstanceClass = y }
{-# INLINE pmvDBInstanceClass #-}

-- | Contains the new AllocatedStorage size for the DB instance that will be
-- applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Integer)
pmvAllocatedStorage f x =
    f (_pmvAllocatedStorage x)
        <&> \y -> x { _pmvAllocatedStorage = y }
{-# INLINE pmvAllocatedStorage #-}

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword f x =
    f (_pmvMasterUserPassword x)
        <&> \y -> x { _pmvMasterUserPassword = y }
{-# INLINE pmvMasterUserPassword #-}

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Integer)
pmvPort f x =
    f (_pmvPort x)
        <&> \y -> x { _pmvPort = y }
{-# INLINE pmvPort #-}

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvBackupRetentionPeriod f x =
    f (_pmvBackupRetentionPeriod x)
        <&> \y -> x { _pmvBackupRetentionPeriod = y }
{-# INLINE pmvBackupRetentionPeriod #-}

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ f x =
    f (_pmvMultiAZ x)
        <&> \y -> x { _pmvMultiAZ = y }
{-# INLINE pmvMultiAZ #-}

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion f x =
    f (_pmvEngineVersion x)
        <&> \y -> x { _pmvEngineVersion = y }
{-# INLINE pmvEngineVersion #-}

-- | Specifies the new Provisioned IOPS value for the DB instance that will be
-- applied or is being applied.
pmvIops :: Lens' PendingModifiedValues (Maybe Integer)
pmvIops f x =
    f (_pmvIops x)
        <&> \y -> x { _pmvIops = y }
{-# INLINE pmvIops #-}

-- | Contains the new DBInstanceIdentifier for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier f x =
    f (_pmvDBInstanceIdentifier x)
        <&> \y -> x { _pmvDBInstanceIdentifier = y }
{-# INLINE pmvDBInstanceIdentifier #-}

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
      -- ^ The amount of the recurring charge.
    , _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    } deriving (Show, Generic)

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount f x =
    f (_rcRecurringChargeAmount x)
        <&> \y -> x { _rcRecurringChargeAmount = y }
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency f x =
    f (_rcRecurringChargeFrequency x)
        <&> \y -> x { _rcRecurringChargeFrequency = y }
{-# INLINE rcRecurringChargeFrequency #-}

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
      -- ^ The unique identifier for the reservation.
    , _rdbiReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rdbiDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , _rdbiStartTime :: Maybe ISO8601
      -- ^ The time the reservation started.
    , _rdbiDuration :: Maybe Integer
      -- ^ The duration of the reservation in seconds.
    , _rdbiFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved DB instance.
    , _rdbiUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved DB instance.
    , _rdbiCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance.
    , _rdbiDBInstanceCount :: Maybe Integer
      -- ^ The number of reserved DB instances.
    , _rdbiProductDescription :: Maybe Text
      -- ^ The description of the reserved DB instance.
    , _rdbiOfferingType :: Maybe Text
      -- ^ The offering type of this reserved DB instance.
    , _rdbiMultiAZ :: Maybe Bool
      -- ^ Indicates if the reservation applies to Multi-AZ deployments.
    , _rdbiState :: Maybe Text
      -- ^ The state of the reserved DB instance.
    , _rdbiRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    } deriving (Show, Generic)

-- | The unique identifier for the reservation.
rdbiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdbiReservedDBInstanceId f x =
    f (_rdbiReservedDBInstanceId x)
        <&> \y -> x { _rdbiReservedDBInstanceId = y }
{-# INLINE rdbiReservedDBInstanceId #-}

-- | The offering identifier.
rdbiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdbiReservedDBInstancesOfferingId f x =
    f (_rdbiReservedDBInstancesOfferingId x)
        <&> \y -> x { _rdbiReservedDBInstancesOfferingId = y }
{-# INLINE rdbiReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdbiDBInstanceClass f x =
    f (_rdbiDBInstanceClass x)
        <&> \y -> x { _rdbiDBInstanceClass = y }
{-# INLINE rdbiDBInstanceClass #-}

-- | The time the reservation started.
rdbiStartTime :: Lens' ReservedDBInstance (Maybe ISO8601)
rdbiStartTime f x =
    f (_rdbiStartTime x)
        <&> \y -> x { _rdbiStartTime = y }
{-# INLINE rdbiStartTime #-}

-- | The duration of the reservation in seconds.
rdbiDuration :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDuration f x =
    f (_rdbiDuration x)
        <&> \y -> x { _rdbiDuration = y }
{-# INLINE rdbiDuration #-}

-- | The fixed price charged for this reserved DB instance.
rdbiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiFixedPrice f x =
    f (_rdbiFixedPrice x)
        <&> \y -> x { _rdbiFixedPrice = y }
{-# INLINE rdbiFixedPrice #-}

-- | The hourly price charged for this reserved DB instance.
rdbiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiUsagePrice f x =
    f (_rdbiUsagePrice x)
        <&> \y -> x { _rdbiUsagePrice = y }
{-# INLINE rdbiUsagePrice #-}

-- | The currency code for the reserved DB instance.
rdbiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdbiCurrencyCode f x =
    f (_rdbiCurrencyCode x)
        <&> \y -> x { _rdbiCurrencyCode = y }
{-# INLINE rdbiCurrencyCode #-}

-- | The number of reserved DB instances.
rdbiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDBInstanceCount f x =
    f (_rdbiDBInstanceCount x)
        <&> \y -> x { _rdbiDBInstanceCount = y }
{-# INLINE rdbiDBInstanceCount #-}

-- | The description of the reserved DB instance.
rdbiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdbiProductDescription f x =
    f (_rdbiProductDescription x)
        <&> \y -> x { _rdbiProductDescription = y }
{-# INLINE rdbiProductDescription #-}

-- | The offering type of this reserved DB instance.
rdbiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdbiOfferingType f x =
    f (_rdbiOfferingType x)
        <&> \y -> x { _rdbiOfferingType = y }
{-# INLINE rdbiOfferingType #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdbiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdbiMultiAZ f x =
    f (_rdbiMultiAZ x)
        <&> \y -> x { _rdbiMultiAZ = y }
{-# INLINE rdbiMultiAZ #-}

-- | The state of the reserved DB instance.
rdbiState :: Lens' ReservedDBInstance (Maybe Text)
rdbiState f x =
    f (_rdbiState x)
        <&> \y -> x { _rdbiState = y }
{-# INLINE rdbiState #-}

-- | The recurring price charged to run this reserved DB instance.
rdbiRecurringCharges :: Lens' ReservedDBInstance ([RecurringCharge])
rdbiRecurringCharges f x =
    f (_rdbiRecurringCharges x)
        <&> \y -> x { _rdbiRecurringCharges = y }
{-# INLINE rdbiRecurringCharges #-}

instance FromXML ReservedDBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstance"

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rdbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , _rdbioDuration :: Maybe Integer
      -- ^ The duration of the offering in seconds.
    , _rdbioFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , _rdbioUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    , _rdbioCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance offering.
    , _rdbioProductDescription :: Maybe Text
      -- ^ The database engine used by the offering.
    , _rdbioOfferingType :: Maybe Text
      -- ^ The offering type.
    , _rdbioMultiAZ :: Maybe Bool
      -- ^ Indicates if the offering applies to Multi-AZ deployments.
    , _rdbioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    } deriving (Show, Generic)

-- | The offering identifier.
rdbioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioReservedDBInstancesOfferingId f x =
    f (_rdbioReservedDBInstancesOfferingId x)
        <&> \y -> x { _rdbioReservedDBInstancesOfferingId = y }
{-# INLINE rdbioReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioDBInstanceClass f x =
    f (_rdbioDBInstanceClass x)
        <&> \y -> x { _rdbioDBInstanceClass = y }
{-# INLINE rdbioDBInstanceClass #-}

-- | The duration of the offering in seconds.
rdbioDuration :: Lens' ReservedDBInstancesOffering (Maybe Integer)
rdbioDuration f x =
    f (_rdbioDuration x)
        <&> \y -> x { _rdbioDuration = y }
{-# INLINE rdbioDuration #-}

-- | The fixed price charged for this offering.
rdbioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioFixedPrice f x =
    f (_rdbioFixedPrice x)
        <&> \y -> x { _rdbioFixedPrice = y }
{-# INLINE rdbioFixedPrice #-}

-- | The hourly price charged for this offering.
rdbioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioUsagePrice f x =
    f (_rdbioUsagePrice x)
        <&> \y -> x { _rdbioUsagePrice = y }
{-# INLINE rdbioUsagePrice #-}

-- | The currency code for the reserved DB instance offering.
rdbioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioCurrencyCode f x =
    f (_rdbioCurrencyCode x)
        <&> \y -> x { _rdbioCurrencyCode = y }
{-# INLINE rdbioCurrencyCode #-}

-- | The database engine used by the offering.
rdbioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioProductDescription f x =
    f (_rdbioProductDescription x)
        <&> \y -> x { _rdbioProductDescription = y }
{-# INLINE rdbioProductDescription #-}

-- | The offering type.
rdbioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioOfferingType f x =
    f (_rdbioOfferingType x)
        <&> \y -> x { _rdbioOfferingType = y }
{-# INLINE rdbioOfferingType #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
rdbioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdbioMultiAZ f x =
    f (_rdbioMultiAZ x)
        <&> \y -> x { _rdbioMultiAZ = y }
{-# INLINE rdbioMultiAZ #-}

-- | The recurring price charged to run this reserved DB instance.
rdbioRecurringCharges :: Lens' ReservedDBInstancesOffering ([RecurringCharge])
rdbioRecurringCharges f x =
    f (_rdbioRecurringCharges x)
        <&> \y -> x { _rdbioRecurringCharges = y }
{-# INLINE rdbioRecurringCharges #-}

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOffering"

-- | This data type is used as a response element in the DescribeDBSubnetGroups
-- action.
data Subnet = Subnet
    { _ssssstSubnetIdentifier :: Maybe Text
      -- ^ Specifies the identifier of the subnet.
    , _ssssstSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Contains Availability Zone information. This data type is used as
      -- an element in the following data type: OrderableDBInstanceOption.
    , _ssssstSubnetStatus :: Maybe Text
      -- ^ Specifies the status of the subnet.
    } deriving (Show, Generic)

-- | Specifies the identifier of the subnet.
ssssstSubnetIdentifier :: Lens' Subnet (Maybe Text)
ssssstSubnetIdentifier f x =
    f (_ssssstSubnetIdentifier x)
        <&> \y -> x { _ssssstSubnetIdentifier = y }
{-# INLINE ssssstSubnetIdentifier #-}

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
ssssstSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
ssssstSubnetAvailabilityZone f x =
    f (_ssssstSubnetAvailabilityZone x)
        <&> \y -> x { _ssssstSubnetAvailabilityZone = y }
{-# INLINE ssssstSubnetAvailabilityZone #-}

-- | Specifies the status of the subnet.
ssssstSubnetStatus :: Lens' Subnet (Maybe Text)
ssssstSubnetStatus f x =
    f (_ssssstSubnetStatus x)
        <&> \y -> x { _ssssstSubnetStatus = y }
{-# INLINE ssssstSubnetStatus #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value pair.
data Tag = Tag
    { _tgKey :: Maybe Text
      -- ^ A key is the required name of the tag. The string value can be
      -- from 1 to 128 Unicode characters in length and cannot be prefixed
      -- with "aws:" or "rds:". The string may only contain only the set
      -- of Unicode letters, digits, white-space, '_', '.', '/', '=', '+',
      -- '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    , _tgValue :: Maybe Text
      -- ^ A value is the optional value of the tag. The string value can be
      -- from 1 to 256 Unicode characters in length and cannot be prefixed
      -- with "aws:" or "rds:". The string may only contain only the set
      -- of Unicode letters, digits, white-space, '_', '.', '/', '=', '+',
      -- '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    } deriving (Show, Generic)

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tgKey :: Lens' Tag (Maybe Text)
tgKey f x =
    f (_tgKey x)
        <&> \y -> x { _tgKey = y }
{-# INLINE tgKey #-}

-- | A value is the optional value of the tag. The string value can be from 1 to
-- 256 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tgValue :: Lens' Tag (Maybe Text)
tgValue f x =
    f (_tgValue x)
        <&> \y -> x { _tgValue = y }
{-# INLINE tgValue #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | This data type is used as a response element for queries on VPC security
-- group membership.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId :: Maybe Text
      -- ^ The name of the VPC security group.
    , _vsgmStatus :: Maybe Text
      -- ^ The status of the VPC security group.
    } deriving (Show, Generic)

-- | The name of the VPC security group.
vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId f x =
    f (_vsgmVpcSecurityGroupId x)
        <&> \y -> x { _vsgmVpcSecurityGroupId = y }
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | The status of the VPC security group.
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus f x =
    f (_vsgmStatus x)
        <&> \y -> x { _vsgmStatus = y }
{-# INLINE vsgmStatus #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
