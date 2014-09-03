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
azName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZone
    -> f AvailabilityZone
azName f x =
    (\y -> x { _azName = y })
       <$> f (_azName x)
{-# INLINE azName #-}

-- | True indicates the availability zone is capable of provisioned IOPs.
azProvisionedIopsCapable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> AvailabilityZone
    -> f AvailabilityZone
azProvisionedIopsCapable f x =
    (\y -> x { _azProvisionedIopsCapable = y })
       <$> f (_azProvisionedIopsCapable x)
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
csCharacterSetName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CharacterSet
    -> f CharacterSet
csCharacterSetName f x =
    (\y -> x { _csCharacterSetName = y })
       <$> f (_csCharacterSetName x)
{-# INLINE csCharacterSetName #-}

-- | The description of the character set.
csCharacterSetDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CharacterSet
    -> f CharacterSet
csCharacterSetDescription f x =
    (\y -> x { _csCharacterSetDescription = y })
       <$> f (_csCharacterSetDescription x)
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
dbevEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevEngine f x =
    (\y -> x { _dbevEngine = y })
       <$> f (_dbevEngine x)
{-# INLINE dbevEngine #-}

-- | The version number of the database engine.
dbevEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevEngineVersion f x =
    (\y -> x { _dbevEngineVersion = y })
       <$> f (_dbevEngineVersion x)
{-# INLINE dbevEngineVersion #-}

-- | The name of the DB parameter group family for the database engine.
dbevDBParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevDBParameterGroupFamily f x =
    (\y -> x { _dbevDBParameterGroupFamily = y })
       <$> f (_dbevDBParameterGroupFamily x)
{-# INLINE dbevDBParameterGroupFamily #-}

-- | The description of the database engine.
dbevDBEngineDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevDBEngineDescription f x =
    (\y -> x { _dbevDBEngineDescription = y })
       <$> f (_dbevDBEngineDescription x)
{-# INLINE dbevDBEngineDescription #-}

-- | The description of the database engine version.
dbevDBEngineVersionDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevDBEngineVersionDescription f x =
    (\y -> x { _dbevDBEngineVersionDescription = y })
       <$> f (_dbevDBEngineVersionDescription x)
{-# INLINE dbevDBEngineVersionDescription #-}

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
dbevDefaultCharacterSet
    :: Functor f
    => (Maybe CharacterSet
    -> f (Maybe CharacterSet))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevDefaultCharacterSet f x =
    (\y -> x { _dbevDefaultCharacterSet = y })
       <$> f (_dbevDefaultCharacterSet x)
{-# INLINE dbevDefaultCharacterSet #-}

-- | A list of the character sets supported by this engine for the
-- CharacterSetName parameter of the CreateDBInstance API.
dbevSupportedCharacterSets
    :: Functor f
    => ([CharacterSet]
    -> f ([CharacterSet]))
    -> DBEngineVersion
    -> f DBEngineVersion
dbevSupportedCharacterSets f x =
    (\y -> x { _dbevSupportedCharacterSets = y })
       <$> f (_dbevSupportedCharacterSets x)
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
dbiDBInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiDBInstanceIdentifier f x =
    (\y -> x { _dbiDBInstanceIdentifier = y })
       <$> f (_dbiDBInstanceIdentifier x)
{-# INLINE dbiDBInstanceIdentifier #-}

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbiDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiDBInstanceClass f x =
    (\y -> x { _dbiDBInstanceClass = y })
       <$> f (_dbiDBInstanceClass x)
{-# INLINE dbiDBInstanceClass #-}

-- | Provides the name of the database engine to be used for this DB instance.
dbiEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiEngine f x =
    (\y -> x { _dbiEngine = y })
       <$> f (_dbiEngine x)
{-# INLINE dbiEngine #-}

-- | Specifies the current state of this database.
dbiDBInstanceStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiDBInstanceStatus f x =
    (\y -> x { _dbiDBInstanceStatus = y })
       <$> f (_dbiDBInstanceStatus x)
{-# INLINE dbiDBInstanceStatus #-}

-- | Contains the master username for the DB instance.
dbiMasterUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiMasterUsername f x =
    (\y -> x { _dbiMasterUsername = y })
       <$> f (_dbiMasterUsername x)
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
dbiDBName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiDBName f x =
    (\y -> x { _dbiDBName = y })
       <$> f (_dbiDBName x)
{-# INLINE dbiDBName #-}

-- | Specifies the connection endpoint.
dbiEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> DBInstance
    -> f DBInstance
dbiEndpoint f x =
    (\y -> x { _dbiEndpoint = y })
       <$> f (_dbiEndpoint x)
{-# INLINE dbiEndpoint #-}

-- | Specifies the allocated storage size specified in gigabytes.
dbiAllocatedStorage
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBInstance
    -> f DBInstance
dbiAllocatedStorage f x =
    (\y -> x { _dbiAllocatedStorage = y })
       <$> f (_dbiAllocatedStorage x)
{-# INLINE dbiAllocatedStorage #-}

-- | Provides the date and time the DB instance was created.
dbiInstanceCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DBInstance
    -> f DBInstance
dbiInstanceCreateTime f x =
    (\y -> x { _dbiInstanceCreateTime = y })
       <$> f (_dbiInstanceCreateTime x)
{-# INLINE dbiInstanceCreateTime #-}

-- | Specifies the daily time range during which automated backups are created
-- if automated backups are enabled, as determined by the
-- BackupRetentionPeriod.
dbiPreferredBackupWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiPreferredBackupWindow f x =
    (\y -> x { _dbiPreferredBackupWindow = y })
       <$> f (_dbiPreferredBackupWindow x)
{-# INLINE dbiPreferredBackupWindow #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
dbiBackupRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBInstance
    -> f DBInstance
dbiBackupRetentionPeriod f x =
    (\y -> x { _dbiBackupRetentionPeriod = y })
       <$> f (_dbiBackupRetentionPeriod x)
{-# INLINE dbiBackupRetentionPeriod #-}

-- | Provides List of DB security group elements containing only
-- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
dbiDBSecurityGroups
    :: Functor f
    => ([DBSecurityGroupMembership]
    -> f ([DBSecurityGroupMembership]))
    -> DBInstance
    -> f DBInstance
dbiDBSecurityGroups f x =
    (\y -> x { _dbiDBSecurityGroups = y })
       <$> f (_dbiDBSecurityGroups x)
{-# INLINE dbiDBSecurityGroups #-}

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroups
    :: Functor f
    => ([VpcSecurityGroupMembership]
    -> f ([VpcSecurityGroupMembership]))
    -> DBInstance
    -> f DBInstance
dbiVpcSecurityGroups f x =
    (\y -> x { _dbiVpcSecurityGroups = y })
       <$> f (_dbiVpcSecurityGroups x)
{-# INLINE dbiVpcSecurityGroups #-}

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroups
    :: Functor f
    => ([DBParameterGroupStatus]
    -> f ([DBParameterGroupStatus]))
    -> DBInstance
    -> f DBInstance
dbiDBParameterGroups f x =
    (\y -> x { _dbiDBParameterGroups = y })
       <$> f (_dbiDBParameterGroups x)
{-# INLINE dbiDBParameterGroups #-}

-- | Specifies the name of the Availability Zone the DB instance is located in.
dbiAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiAvailabilityZone f x =
    (\y -> x { _dbiAvailabilityZone = y })
       <$> f (_dbiAvailabilityZone x)
{-# INLINE dbiAvailabilityZone #-}

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
dbiDBSubnetGroup
    :: Functor f
    => (Maybe DBSubnetGroup
    -> f (Maybe DBSubnetGroup))
    -> DBInstance
    -> f DBInstance
dbiDBSubnetGroup f x =
    (\y -> x { _dbiDBSubnetGroup = y })
       <$> f (_dbiDBSubnetGroup x)
{-# INLINE dbiDBSubnetGroup #-}

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
dbiPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiPreferredMaintenanceWindow f x =
    (\y -> x { _dbiPreferredMaintenanceWindow = y })
       <$> f (_dbiPreferredMaintenanceWindow x)
{-# INLINE dbiPreferredMaintenanceWindow #-}

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
dbiPendingModifiedValues
    :: Functor f
    => (Maybe PendingModifiedValues
    -> f (Maybe PendingModifiedValues))
    -> DBInstance
    -> f DBInstance
dbiPendingModifiedValues f x =
    (\y -> x { _dbiPendingModifiedValues = y })
       <$> f (_dbiPendingModifiedValues x)
{-# INLINE dbiPendingModifiedValues #-}

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbiLatestRestorableTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DBInstance
    -> f DBInstance
dbiLatestRestorableTime f x =
    (\y -> x { _dbiLatestRestorableTime = y })
       <$> f (_dbiLatestRestorableTime x)
{-# INLINE dbiLatestRestorableTime #-}

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbiMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DBInstance
    -> f DBInstance
dbiMultiAZ f x =
    (\y -> x { _dbiMultiAZ = y })
       <$> f (_dbiMultiAZ x)
{-# INLINE dbiMultiAZ #-}

-- | Indicates the database engine version.
dbiEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiEngineVersion f x =
    (\y -> x { _dbiEngineVersion = y })
       <$> f (_dbiEngineVersion x)
{-# INLINE dbiEngineVersion #-}

-- | Indicates that minor version patches are applied automatically.
dbiAutoMinorVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DBInstance
    -> f DBInstance
dbiAutoMinorVersionUpgrade f x =
    (\y -> x { _dbiAutoMinorVersionUpgrade = y })
       <$> f (_dbiAutoMinorVersionUpgrade x)
{-# INLINE dbiAutoMinorVersionUpgrade #-}

-- | Contains the identifier of the source DB instance if this DB instance is a
-- read replica.
dbiReadReplicaSourceDBInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiReadReplicaSourceDBInstanceIdentifier f x =
    (\y -> x { _dbiReadReplicaSourceDBInstanceIdentifier = y })
       <$> f (_dbiReadReplicaSourceDBInstanceIdentifier x)
{-# INLINE dbiReadReplicaSourceDBInstanceIdentifier #-}

-- | Contains one or more identifiers of the read replicas associated with this
-- DB instance.
dbiReadReplicaDBInstanceIdentifiers
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DBInstance
    -> f DBInstance
dbiReadReplicaDBInstanceIdentifiers f x =
    (\y -> x { _dbiReadReplicaDBInstanceIdentifiers = y })
       <$> f (_dbiReadReplicaDBInstanceIdentifiers x)
{-# INLINE dbiReadReplicaDBInstanceIdentifiers #-}

-- | License model information for this DB instance.
dbiLicenseModel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiLicenseModel f x =
    (\y -> x { _dbiLicenseModel = y })
       <$> f (_dbiLicenseModel x)
{-# INLINE dbiLicenseModel #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBInstance
    -> f DBInstance
dbiIops f x =
    (\y -> x { _dbiIops = y })
       <$> f (_dbiIops x)
{-# INLINE dbiIops #-}

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMemberships
    :: Functor f
    => ([OptionGroupMembership]
    -> f ([OptionGroupMembership]))
    -> DBInstance
    -> f DBInstance
dbiOptionGroupMemberships f x =
    (\y -> x { _dbiOptionGroupMemberships = y })
       <$> f (_dbiOptionGroupMemberships x)
{-# INLINE dbiOptionGroupMemberships #-}

-- | If present, specifies the name of the character set that this instance is
-- associated with.
dbiCharacterSetName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiCharacterSetName f x =
    (\y -> x { _dbiCharacterSetName = y })
       <$> f (_dbiCharacterSetName x)
{-# INLINE dbiCharacterSetName #-}

-- | If present, specifies the name of the secondary Availability Zone for a DB
-- instance with multi-AZ support.
dbiSecondaryAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstance
    -> f DBInstance
dbiSecondaryAvailabilityZone f x =
    (\y -> x { _dbiSecondaryAvailabilityZone = y })
       <$> f (_dbiSecondaryAvailabilityZone x)
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
dbiPubliclyAccessible
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DBInstance
    -> f DBInstance
dbiPubliclyAccessible f x =
    (\y -> x { _dbiPubliclyAccessible = y })
       <$> f (_dbiPubliclyAccessible x)
{-# INLINE dbiPubliclyAccessible #-}

-- | The status of a read replica. If the instance is not a read replica, this
-- will be blank.
dbiStatusInfos
    :: Functor f
    => ([DBInstanceStatusInfo]
    -> f ([DBInstanceStatusInfo]))
    -> DBInstance
    -> f DBInstance
dbiStatusInfos f x =
    (\y -> x { _dbiStatusInfos = y })
       <$> f (_dbiStatusInfos x)
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
dbisiStatusType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstanceStatusInfo
    -> f DBInstanceStatusInfo
dbisiStatusType f x =
    (\y -> x { _dbisiStatusType = y })
       <$> f (_dbisiStatusType x)
{-# INLINE dbisiStatusType #-}

-- | Boolean value that is true if the instance is operating normally, or false
-- if the instance is in an error state.
dbisiNormal
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DBInstanceStatusInfo
    -> f DBInstanceStatusInfo
dbisiNormal f x =
    (\y -> x { _dbisiNormal = y })
       <$> f (_dbisiNormal x)
{-# INLINE dbisiNormal #-}

-- | Status of the DB instance. For a StatusType of read replica, the values can
-- be replicating, error, stopped, or terminated.
dbisiStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstanceStatusInfo
    -> f DBInstanceStatusInfo
dbisiStatus f x =
    (\y -> x { _dbisiStatus = y })
       <$> f (_dbisiStatus x)
{-# INLINE dbisiStatus #-}

-- | Details of the error if there is an error for the instance. If the instance
-- is not in an error state, this value is blank.
dbisiMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBInstanceStatusInfo
    -> f DBInstanceStatusInfo
dbisiMessage f x =
    (\y -> x { _dbisiMessage = y })
       <$> f (_dbisiMessage x)
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
dbpgDBParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBParameterGroup
    -> f DBParameterGroup
dbpgDBParameterGroupName f x =
    (\y -> x { _dbpgDBParameterGroupName = y })
       <$> f (_dbpgDBParameterGroupName x)
{-# INLINE dbpgDBParameterGroupName #-}

-- | Provides the name of the DB parameter group family that this DB parameter
-- group is compatible with.
dbpgDBParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBParameterGroup
    -> f DBParameterGroup
dbpgDBParameterGroupFamily f x =
    (\y -> x { _dbpgDBParameterGroupFamily = y })
       <$> f (_dbpgDBParameterGroupFamily x)
{-# INLINE dbpgDBParameterGroupFamily #-}

-- | Provides the customer-specified description for this DB parameter group.
dbpgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBParameterGroup
    -> f DBParameterGroup
dbpgDescription f x =
    (\y -> x { _dbpgDescription = y })
       <$> f (_dbpgDescription x)
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
dbpgsDBParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBParameterGroupStatus
    -> f DBParameterGroupStatus
dbpgsDBParameterGroupName f x =
    (\y -> x { _dbpgsDBParameterGroupName = y })
       <$> f (_dbpgsDBParameterGroupName x)
{-# INLINE dbpgsDBParameterGroupName #-}

-- | The status of parameter updates.
dbpgsParameterApplyStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBParameterGroupStatus
    -> f DBParameterGroupStatus
dbpgsParameterApplyStatus f x =
    (\y -> x { _dbpgsParameterApplyStatus = y })
       <$> f (_dbpgsParameterApplyStatus x)
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
dbsgOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgOwnerId f x =
    (\y -> x { _dbsgOwnerId = y })
       <$> f (_dbsgOwnerId x)
{-# INLINE dbsgOwnerId #-}

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgDBSecurityGroupName f x =
    (\y -> x { _dbsgDBSecurityGroupName = y })
       <$> f (_dbsgDBSecurityGroupName x)
{-# INLINE dbsgDBSecurityGroupName #-}

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgDBSecurityGroupDescription f x =
    (\y -> x { _dbsgDBSecurityGroupDescription = y })
       <$> f (_dbsgDBSecurityGroupDescription x)
{-# INLINE dbsgDBSecurityGroupDescription #-}

-- | Provides the VpcId of the DB security group.
dbsgVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgVpcId f x =
    (\y -> x { _dbsgVpcId = y })
       <$> f (_dbsgVpcId x)
{-# INLINE dbsgVpcId #-}

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroups
    :: Functor f
    => ([EC2SecurityGroup]
    -> f ([EC2SecurityGroup]))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgEC2SecurityGroups f x =
    (\y -> x { _dbsgEC2SecurityGroups = y })
       <$> f (_dbsgEC2SecurityGroups x)
{-# INLINE dbsgEC2SecurityGroups #-}

-- | Contains a list of IPRange elements.
dbsgIPRanges
    :: Functor f
    => ([IPRange]
    -> f ([IPRange]))
    -> DBSecurityGroup
    -> f DBSecurityGroup
dbsgIPRanges f x =
    (\y -> x { _dbsgIPRanges = y })
       <$> f (_dbsgIPRanges x)
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
dbsgmDBSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroupMembership
    -> f DBSecurityGroupMembership
dbsgmDBSecurityGroupName f x =
    (\y -> x { _dbsgmDBSecurityGroupName = y })
       <$> f (_dbsgmDBSecurityGroupName x)
{-# INLINE dbsgmDBSecurityGroupName #-}

-- | The status of the DB security group.
dbsgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSecurityGroupMembership
    -> f DBSecurityGroupMembership
dbsgmStatus f x =
    (\y -> x { _dbsgmStatus = y })
       <$> f (_dbsgmStatus x)
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
dbsDBSnapshotIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsDBSnapshotIdentifier f x =
    (\y -> x { _dbsDBSnapshotIdentifier = y })
       <$> f (_dbsDBSnapshotIdentifier x)
{-# INLINE dbsDBSnapshotIdentifier #-}

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbsDBInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsDBInstanceIdentifier f x =
    (\y -> x { _dbsDBInstanceIdentifier = y })
       <$> f (_dbsDBInstanceIdentifier x)
{-# INLINE dbsDBInstanceIdentifier #-}

-- | Provides the time (UTC) when the snapshot was taken.
dbsSnapshotCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DBSnapshot
    -> f DBSnapshot
dbsSnapshotCreateTime f x =
    (\y -> x { _dbsSnapshotCreateTime = y })
       <$> f (_dbsSnapshotCreateTime x)
{-# INLINE dbsSnapshotCreateTime #-}

-- | Specifies the name of the database engine.
dbsEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsEngine f x =
    (\y -> x { _dbsEngine = y })
       <$> f (_dbsEngine x)
{-# INLINE dbsEngine #-}

-- | Specifies the allocated storage size in gigabytes (GB).
dbsAllocatedStorage
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBSnapshot
    -> f DBSnapshot
dbsAllocatedStorage f x =
    (\y -> x { _dbsAllocatedStorage = y })
       <$> f (_dbsAllocatedStorage x)
{-# INLINE dbsAllocatedStorage #-}

-- | Specifies the status of this DB snapshot.
dbsStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsStatus f x =
    (\y -> x { _dbsStatus = y })
       <$> f (_dbsStatus x)
{-# INLINE dbsStatus #-}

-- | Specifies the port that the database engine was listening on at the time of
-- the snapshot.
dbsPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBSnapshot
    -> f DBSnapshot
dbsPort f x =
    (\y -> x { _dbsPort = y })
       <$> f (_dbsPort x)
{-# INLINE dbsPort #-}

-- | Specifies the name of the Availability Zone the DB instance was located in
-- at the time of the DB snapshot.
dbsAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsAvailabilityZone f x =
    (\y -> x { _dbsAvailabilityZone = y })
       <$> f (_dbsAvailabilityZone x)
{-# INLINE dbsAvailabilityZone #-}

-- | Provides the Vpc Id associated with the DB snapshot.
dbsVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsVpcId f x =
    (\y -> x { _dbsVpcId = y })
       <$> f (_dbsVpcId x)
{-# INLINE dbsVpcId #-}

-- | Specifies the time (UTC) when the snapshot was taken.
dbsInstanceCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DBSnapshot
    -> f DBSnapshot
dbsInstanceCreateTime f x =
    (\y -> x { _dbsInstanceCreateTime = y })
       <$> f (_dbsInstanceCreateTime x)
{-# INLINE dbsInstanceCreateTime #-}

-- | Provides the master username for the DB snapshot.
dbsMasterUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsMasterUsername f x =
    (\y -> x { _dbsMasterUsername = y })
       <$> f (_dbsMasterUsername x)
{-# INLINE dbsMasterUsername #-}

-- | Specifies the version of the database engine.
dbsEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsEngineVersion f x =
    (\y -> x { _dbsEngineVersion = y })
       <$> f (_dbsEngineVersion x)
{-# INLINE dbsEngineVersion #-}

-- | License model information for the restored DB instance.
dbsLicenseModel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsLicenseModel f x =
    (\y -> x { _dbsLicenseModel = y })
       <$> f (_dbsLicenseModel x)
{-# INLINE dbsLicenseModel #-}

-- | Provides the type of the DB snapshot.
dbsSnapshotType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsSnapshotType f x =
    (\y -> x { _dbsSnapshotType = y })
       <$> f (_dbsSnapshotType x)
{-# INLINE dbsSnapshotType #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB
-- instance at the time of the snapshot.
dbsIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBSnapshot
    -> f DBSnapshot
dbsIops f x =
    (\y -> x { _dbsIops = y })
       <$> f (_dbsIops x)
{-# INLINE dbsIops #-}

-- | Provides the option group name for the DB snapshot.
dbsOptionGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsOptionGroupName f x =
    (\y -> x { _dbsOptionGroupName = y })
       <$> f (_dbsOptionGroupName x)
{-# INLINE dbsOptionGroupName #-}

-- | The percentage of the estimated data that has been transferred.
dbsPercentProgress
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DBSnapshot
    -> f DBSnapshot
dbsPercentProgress f x =
    (\y -> x { _dbsPercentProgress = y })
       <$> f (_dbsPercentProgress x)
{-# INLINE dbsPercentProgress #-}

-- | The region that the DB snapshot was created in or copied from.
dbsSourceRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSnapshot
    -> f DBSnapshot
dbsSourceRegion f x =
    (\y -> x { _dbsSourceRegion = y })
       <$> f (_dbsSourceRegion x)
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
dbshDBSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSubnetGroup
    -> f DBSubnetGroup
dbshDBSubnetGroupName f x =
    (\y -> x { _dbshDBSubnetGroupName = y })
       <$> f (_dbshDBSubnetGroupName x)
{-# INLINE dbshDBSubnetGroupName #-}

-- | Provides the description of the DB subnet group.
dbshDBSubnetGroupDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSubnetGroup
    -> f DBSubnetGroup
dbshDBSubnetGroupDescription f x =
    (\y -> x { _dbshDBSubnetGroupDescription = y })
       <$> f (_dbshDBSubnetGroupDescription x)
{-# INLINE dbshDBSubnetGroupDescription #-}

-- | Provides the VpcId of the DB subnet group.
dbshVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSubnetGroup
    -> f DBSubnetGroup
dbshVpcId f x =
    (\y -> x { _dbshVpcId = y })
       <$> f (_dbshVpcId x)
{-# INLINE dbshVpcId #-}

-- | Provides the status of the DB subnet group.
dbshSubnetGroupStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DBSubnetGroup
    -> f DBSubnetGroup
dbshSubnetGroupStatus f x =
    (\y -> x { _dbshSubnetGroupStatus = y })
       <$> f (_dbshSubnetGroupStatus x)
{-# INLINE dbshSubnetGroupStatus #-}

-- | Contains a list of Subnet elements.
dbshSubnets
    :: Functor f
    => ([Subnet]
    -> f ([Subnet]))
    -> DBSubnetGroup
    -> f DBSubnetGroup
dbshSubnets f x =
    (\y -> x { _dbshSubnets = y })
       <$> f (_dbshSubnets x)
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
ddblfdLogFileName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBLogFilesDetails
    -> f DescribeDBLogFilesDetails
ddblfdLogFileName f x =
    (\y -> x { _ddblfdLogFileName = y })
       <$> f (_ddblfdLogFileName x)
{-# INLINE ddblfdLogFileName #-}

-- | A POSIX timestamp when the last log entry was written.
ddblfdLastWritten
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBLogFilesDetails
    -> f DescribeDBLogFilesDetails
ddblfdLastWritten f x =
    (\y -> x { _ddblfdLastWritten = y })
       <$> f (_ddblfdLastWritten x)
{-# INLINE ddblfdLastWritten #-}

-- | The size, in bytes, of the log file for the specified DB instance.
ddblfdSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBLogFilesDetails
    -> f DescribeDBLogFilesDetails
ddblfdSize f x =
    (\y -> x { _ddblfdSize = y })
       <$> f (_ddblfdSize x)
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
ecsgStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgStatus f x =
    (\y -> x { _ecsgStatus = y })
       <$> f (_ecsgStatus x)
{-# INLINE ecsgStatus #-}

-- | Specifies the name of the EC2 security group.
ecsgEC2SecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupName f x =
    (\y -> x { _ecsgEC2SecurityGroupName = y })
       <$> f (_ecsgEC2SecurityGroupName x)
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | Specifies the id of the EC2 security group.
ecsgEC2SecurityGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupId f x =
    (\y -> x { _ecsgEC2SecurityGroupId = y })
       <$> f (_ecsgEC2SecurityGroupId x)
{-# INLINE ecsgEC2SecurityGroupId #-}

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupOwnerId f x =
    (\y -> x { _ecsgEC2SecurityGroupOwnerId = y })
       <$> f (_ecsgEC2SecurityGroupOwnerId x)
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
eAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Endpoint
    -> f Endpoint
eAddress f x =
    (\y -> x { _eAddress = y })
       <$> f (_eAddress x)
{-# INLINE eAddress #-}

-- | Specifies the port that the database engine is listening on.
ePort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Endpoint
    -> f Endpoint
ePort f x =
    (\y -> x { _ePort = y })
       <$> f (_ePort x)
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
edDBParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EngineDefaults
    -> f EngineDefaults
edDBParameterGroupFamily f x =
    (\y -> x { _edDBParameterGroupFamily = y })
       <$> f (_edDBParameterGroupFamily x)
{-# INLINE edDBParameterGroupFamily #-}

-- | An optional pagination token provided by a previous EngineDefaults request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords .
edMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EngineDefaults
    -> f EngineDefaults
edMarker f x =
    (\y -> x { _edMarker = y })
       <$> f (_edMarker x)
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> EngineDefaults
    -> f EngineDefaults
edParameters f x =
    (\y -> x { _edParameters = y })
       <$> f (_edParameters x)
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
eySourceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
eySourceIdentifier f x =
    (\y -> x { _eySourceIdentifier = y })
       <$> f (_eySourceIdentifier x)
{-# INLINE eySourceIdentifier #-}

-- | Specifies the source type for this event.
eySourceType
    :: Functor f
    => (Maybe SourceType
    -> f (Maybe SourceType))
    -> Event
    -> f Event
eySourceType f x =
    (\y -> x { _eySourceType = y })
       <$> f (_eySourceType x)
{-# INLINE eySourceType #-}

-- | Provides the text of this event.
eyMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
eyMessage f x =
    (\y -> x { _eyMessage = y })
       <$> f (_eyMessage x)
{-# INLINE eyMessage #-}

-- | Specifies the category for the event.
eyEventCategories
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> Event
    -> f Event
eyEventCategories f x =
    (\y -> x { _eyEventCategories = y })
       <$> f (_eyEventCategories x)
{-# INLINE eyEventCategories #-}

-- | Specifies the date and time of the event.
eyDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Event
    -> f Event
eyDate f x =
    (\y -> x { _eyDate = y })
       <$> f (_eyDate x)
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
ecqSourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventCategoriesMap
    -> f EventCategoriesMap
ecqSourceType f x =
    (\y -> x { _ecqSourceType = y })
       <$> f (_ecqSourceType x)
{-# INLINE ecqSourceType #-}

-- | The event categories for the specified source type.
ecqEventCategories
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventCategoriesMap
    -> f EventCategoriesMap
ecqEventCategories f x =
    (\y -> x { _ecqEventCategories = y })
       <$> f (_ecqEventCategories x)
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
esCustomerAwsId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esCustomerAwsId f x =
    (\y -> x { _esCustomerAwsId = y })
       <$> f (_esCustomerAwsId x)
{-# INLINE esCustomerAwsId #-}

-- | The RDS event notification subscription Id.
esCustSubscriptionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esCustSubscriptionId f x =
    (\y -> x { _esCustSubscriptionId = y })
       <$> f (_esCustSubscriptionId x)
{-# INLINE esCustSubscriptionId #-}

-- | The topic ARN of the RDS event notification subscription.
esSnsTopicArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSnsTopicArn f x =
    (\y -> x { _esSnsTopicArn = y })
       <$> f (_esSnsTopicArn x)
{-# INLINE esSnsTopicArn #-}

-- | The status of the RDS event notification subscription. Constraints: Can be
-- one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist The status "no-permission" indicates that
-- RDS no longer has permission to post to the SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esStatus f x =
    (\y -> x { _esStatus = y })
       <$> f (_esStatus x)
{-# INLINE esStatus #-}

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSubscriptionCreationTime f x =
    (\y -> x { _esSubscriptionCreationTime = y })
       <$> f (_esSubscriptionCreationTime x)
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type for the RDS event notification subscription.
esSourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventSubscription
    -> f EventSubscription
esSourceType f x =
    (\y -> x { _esSourceType = y })
       <$> f (_esSourceType x)
{-# INLINE esSourceType #-}

-- | A list of source Ids for the RDS event notification subscription.
esSourceIdsList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventSubscription
    -> f EventSubscription
esSourceIdsList f x =
    (\y -> x { _esSourceIdsList = y })
       <$> f (_esSourceIdsList x)
{-# INLINE esSourceIdsList #-}

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> EventSubscription
    -> f EventSubscription
esEventCategoriesList f x =
    (\y -> x { _esEventCategoriesList = y })
       <$> f (_esEventCategoriesList x)
{-# INLINE esEventCategoriesList #-}

-- | A Boolean value indicating if the subscription is enabled. True indicates
-- the subscription is enabled.
esEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> EventSubscription
    -> f EventSubscription
esEnabled f x =
    (\y -> x { _esEnabled = y })
       <$> f (_esEnabled x)
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
iprStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IPRange
    -> f IPRange
iprStatus f x =
    (\y -> x { _iprStatus = y })
       <$> f (_iprStatus x)
{-# INLINE iprStatus #-}

-- | Specifies the IP range.
iprCIDRIP
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> IPRange
    -> f IPRange
iprCIDRIP f x =
    (\y -> x { _iprCIDRIP = y })
       <$> f (_iprCIDRIP x)
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
onOptionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Option
    -> f Option
onOptionName f x =
    (\y -> x { _onOptionName = y })
       <$> f (_onOptionName x)
{-# INLINE onOptionName #-}

-- | The description of the option.
onOptionDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Option
    -> f Option
onOptionDescription f x =
    (\y -> x { _onOptionDescription = y })
       <$> f (_onOptionDescription x)
{-# INLINE onOptionDescription #-}

-- | Indicate if this option is persistent.
onPersistent
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Option
    -> f Option
onPersistent f x =
    (\y -> x { _onPersistent = y })
       <$> f (_onPersistent x)
{-# INLINE onPersistent #-}

-- | Indicate if this option is permanent.
onPermanent
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Option
    -> f Option
onPermanent f x =
    (\y -> x { _onPermanent = y })
       <$> f (_onPermanent x)
{-# INLINE onPermanent #-}

-- | If required, the port configured for this option to use.
onPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Option
    -> f Option
onPort f x =
    (\y -> x { _onPort = y })
       <$> f (_onPort x)
{-# INLINE onPort #-}

-- | The option settings for this option.
onOptionSettings
    :: Functor f
    => ([OptionSetting]
    -> f ([OptionSetting]))
    -> Option
    -> f Option
onOptionSettings f x =
    (\y -> x { _onOptionSettings = y })
       <$> f (_onOptionSettings x)
{-# INLINE onOptionSettings #-}

-- | If the option requires access to a port, then this DB security group allows
-- access to the port.
onDBSecurityGroupMemberships
    :: Functor f
    => ([DBSecurityGroupMembership]
    -> f ([DBSecurityGroupMembership]))
    -> Option
    -> f Option
onDBSecurityGroupMemberships f x =
    (\y -> x { _onDBSecurityGroupMemberships = y })
       <$> f (_onDBSecurityGroupMemberships x)
{-# INLINE onDBSecurityGroupMemberships #-}

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
onVpcSecurityGroupMemberships
    :: Functor f
    => ([VpcSecurityGroupMembership]
    -> f ([VpcSecurityGroupMembership]))
    -> Option
    -> f Option
onVpcSecurityGroupMemberships f x =
    (\y -> x { _onVpcSecurityGroupMemberships = y })
       <$> f (_onVpcSecurityGroupMemberships x)
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
ocOptionName
    :: Functor f
    => (Text
    -> f (Text))
    -> OptionConfiguration
    -> f OptionConfiguration
ocOptionName f x =
    (\y -> x { _ocOptionName = y })
       <$> f (_ocOptionName x)
{-# INLINE ocOptionName #-}

-- | The optional port for the option.
ocPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> OptionConfiguration
    -> f OptionConfiguration
ocPort f x =
    (\y -> x { _ocPort = y })
       <$> f (_ocPort x)
{-# INLINE ocPort #-}

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> OptionConfiguration
    -> f OptionConfiguration
ocDBSecurityGroupMemberships f x =
    (\y -> x { _ocDBSecurityGroupMemberships = y })
       <$> f (_ocDBSecurityGroupMemberships x)
{-# INLINE ocDBSecurityGroupMemberships #-}

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupMemberships
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> OptionConfiguration
    -> f OptionConfiguration
ocVpcSecurityGroupMemberships f x =
    (\y -> x { _ocVpcSecurityGroupMemberships = y })
       <$> f (_ocVpcSecurityGroupMemberships x)
{-# INLINE ocVpcSecurityGroupMemberships #-}

-- | The option settings to include in an option group.
ocOptionSettings
    :: Functor f
    => ([OptionSetting]
    -> f ([OptionSetting]))
    -> OptionConfiguration
    -> f OptionConfiguration
ocOptionSettings f x =
    (\y -> x { _ocOptionSettings = y })
       <$> f (_ocOptionSettings x)
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
ogOptionGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroup
    -> f OptionGroup
ogOptionGroupName f x =
    (\y -> x { _ogOptionGroupName = y })
       <$> f (_ogOptionGroupName x)
{-# INLINE ogOptionGroupName #-}

-- | Provides the description of the option group.
ogOptionGroupDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroup
    -> f OptionGroup
ogOptionGroupDescription f x =
    (\y -> x { _ogOptionGroupDescription = y })
       <$> f (_ogOptionGroupDescription x)
{-# INLINE ogOptionGroupDescription #-}

-- | Engine name that this option group can be applied to.
ogEngineName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroup
    -> f OptionGroup
ogEngineName f x =
    (\y -> x { _ogEngineName = y })
       <$> f (_ogEngineName x)
{-# INLINE ogEngineName #-}

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroup
    -> f OptionGroup
ogMajorEngineVersion f x =
    (\y -> x { _ogMajorEngineVersion = y })
       <$> f (_ogMajorEngineVersion x)
{-# INLINE ogMajorEngineVersion #-}

-- | Indicates what options are available in the option group.
ogOptions
    :: Functor f
    => ([Option]
    -> f ([Option]))
    -> OptionGroup
    -> f OptionGroup
ogOptions f x =
    (\y -> x { _ogOptions = y })
       <$> f (_ogOptions x)
{-# INLINE ogOptions #-}

-- | Indicates whether this option group can be applied to both VPC and non-VPC
-- instances. The value 'true' indicates the option group can be applied to
-- both VPC and non-VPC instances.
ogAllowsVpcAndNonVpcInstanceMemberships
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionGroup
    -> f OptionGroup
ogAllowsVpcAndNonVpcInstanceMemberships f x =
    (\y -> x { _ogAllowsVpcAndNonVpcInstanceMemberships = y })
       <$> f (_ogAllowsVpcAndNonVpcInstanceMemberships x)
{-# INLINE ogAllowsVpcAndNonVpcInstanceMemberships #-}

-- | If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
-- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is blank,
-- then this option group can be applied to both VPC and non-VPC instances. If
-- this field contains a value, then this option group can only be applied to
-- instances that are in the VPC indicated by this field.
ogVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroup
    -> f OptionGroup
ogVpcId f x =
    (\y -> x { _ogVpcId = y })
       <$> f (_ogVpcId x)
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
ogmOptionGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupMembership
    -> f OptionGroupMembership
ogmOptionGroupName f x =
    (\y -> x { _ogmOptionGroupName = y })
       <$> f (_ogmOptionGroupName x)
{-# INLINE ogmOptionGroupName #-}

-- | The status of the DB instance's option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupMembership
    -> f OptionGroupMembership
ogmStatus f x =
    (\y -> x { _ogmStatus = y })
       <$> f (_ogmStatus x)
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
ogqName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqName f x =
    (\y -> x { _ogqName = y })
       <$> f (_ogqName x)
{-# INLINE ogqName #-}

-- | The description of the option.
ogqDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqDescription f x =
    (\y -> x { _ogqDescription = y })
       <$> f (_ogqDescription x)
{-# INLINE ogqDescription #-}

-- | Engine name that this option can be applied to.
ogqEngineName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqEngineName f x =
    (\y -> x { _ogqEngineName = y })
       <$> f (_ogqEngineName x)
{-# INLINE ogqEngineName #-}

-- | Indicates the major engine version that the option is available for.
ogqMajorEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqMajorEngineVersion f x =
    (\y -> x { _ogqMajorEngineVersion = y })
       <$> f (_ogqMajorEngineVersion x)
{-# INLINE ogqMajorEngineVersion #-}

-- | The minimum required engine version for the option to be applied.
ogqMinimumRequiredMinorEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqMinimumRequiredMinorEngineVersion f x =
    (\y -> x { _ogqMinimumRequiredMinorEngineVersion = y })
       <$> f (_ogqMinimumRequiredMinorEngineVersion x)
{-# INLINE ogqMinimumRequiredMinorEngineVersion #-}

-- | Specifies whether the option requires a port.
ogqPortRequired
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqPortRequired f x =
    (\y -> x { _ogqPortRequired = y })
       <$> f (_ogqPortRequired x)
{-# INLINE ogqPortRequired #-}

-- | If the option requires a port, specifies the default port for the option.
ogqDefaultPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqDefaultPort f x =
    (\y -> x { _ogqDefaultPort = y })
       <$> f (_ogqDefaultPort x)
{-# INLINE ogqDefaultPort #-}

-- | List of all options that are prerequisites for this option.
ogqOptionsDependedOn
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqOptionsDependedOn f x =
    (\y -> x { _ogqOptionsDependedOn = y })
       <$> f (_ogqOptionsDependedOn x)
{-# INLINE ogqOptionsDependedOn #-}

-- | A persistent option cannot be removed from the option group once the option
-- group is used, but this option can be removed from the db instance while
-- modifying the related data and assigning another option group without this
-- option.
ogqPersistent
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqPersistent f x =
    (\y -> x { _ogqPersistent = y })
       <$> f (_ogqPersistent x)
{-# INLINE ogqPersistent #-}

-- | A permanent option cannot be removed from the option group once the option
-- group is used, and it cannot be removed from the db instance after
-- assigning an option group with this permanent option.
ogqPermanent
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqPermanent f x =
    (\y -> x { _ogqPermanent = y })
       <$> f (_ogqPermanent x)
{-# INLINE ogqPermanent #-}

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogqOptionGroupOptionSettings
    :: Functor f
    => ([OptionGroupOptionSetting]
    -> f ([OptionGroupOptionSetting]))
    -> OptionGroupOption
    -> f OptionGroupOption
ogqOptionGroupOptionSettings f x =
    (\y -> x { _ogqOptionGroupOptionSettings = y })
       <$> f (_ogqOptionGroupOptionSettings x)
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
ogosSettingName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosSettingName f x =
    (\y -> x { _ogosSettingName = y })
       <$> f (_ogosSettingName x)
{-# INLINE ogosSettingName #-}

-- | The description of the option group option.
ogosSettingDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosSettingDescription f x =
    (\y -> x { _ogosSettingDescription = y })
       <$> f (_ogosSettingDescription x)
{-# INLINE ogosSettingDescription #-}

-- | The default value for the option group option.
ogosDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosDefaultValue f x =
    (\y -> x { _ogosDefaultValue = y })
       <$> f (_ogosDefaultValue x)
{-# INLINE ogosDefaultValue #-}

-- | The DB engine specific parameter type for the option group option.
ogosApplyType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosApplyType f x =
    (\y -> x { _ogosApplyType = y })
       <$> f (_ogosApplyType x)
{-# INLINE ogosApplyType #-}

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosAllowedValues f x =
    (\y -> x { _ogosAllowedValues = y })
       <$> f (_ogosAllowedValues x)
{-# INLINE ogosAllowedValues #-}

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionGroupOptionSetting
    -> f OptionGroupOptionSetting
ogosIsModifiable f x =
    (\y -> x { _ogosIsModifiable = y })
       <$> f (_ogosIsModifiable x)
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
osName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osName f x =
    (\y -> x { _osName = y })
       <$> f (_osName x)
{-# INLINE osName #-}

-- | The current value of the option setting.
osValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osValue f x =
    (\y -> x { _osValue = y })
       <$> f (_osValue x)
{-# INLINE osValue #-}

-- | The default value of the option setting.
osDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osDefaultValue f x =
    (\y -> x { _osDefaultValue = y })
       <$> f (_osDefaultValue x)
{-# INLINE osDefaultValue #-}

-- | The description of the option setting.
osDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osDescription f x =
    (\y -> x { _osDescription = y })
       <$> f (_osDescription x)
{-# INLINE osDescription #-}

-- | The DB engine specific parameter type.
osApplyType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osApplyType f x =
    (\y -> x { _osApplyType = y })
       <$> f (_osApplyType x)
{-# INLINE osApplyType #-}

-- | The data type of the option setting.
osDataType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osDataType f x =
    (\y -> x { _osDataType = y })
       <$> f (_osDataType x)
{-# INLINE osDataType #-}

-- | The allowed values of the option setting.
osAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSetting
    -> f OptionSetting
osAllowedValues f x =
    (\y -> x { _osAllowedValues = y })
       <$> f (_osAllowedValues x)
{-# INLINE osAllowedValues #-}

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionSetting
    -> f OptionSetting
osIsModifiable f x =
    (\y -> x { _osIsModifiable = y })
       <$> f (_osIsModifiable x)
{-# INLINE osIsModifiable #-}

-- | Indicates if the option setting is part of a collection.
osIsCollection
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OptionSetting
    -> f OptionSetting
osIsCollection f x =
    (\y -> x { _osIsCollection = y })
       <$> f (_osIsCollection x)
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
odbioEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioEngine f x =
    (\y -> x { _odbioEngine = y })
       <$> f (_odbioEngine x)
{-# INLINE odbioEngine #-}

-- | The engine version of the orderable DB instance.
odbioEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioEngineVersion f x =
    (\y -> x { _odbioEngineVersion = y })
       <$> f (_odbioEngineVersion x)
{-# INLINE odbioEngineVersion #-}

-- | The DB instance Class for the orderable DB instance.
odbioDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioDBInstanceClass f x =
    (\y -> x { _odbioDBInstanceClass = y })
       <$> f (_odbioDBInstanceClass x)
{-# INLINE odbioDBInstanceClass #-}

-- | The license model for the orderable DB instance.
odbioLicenseModel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioLicenseModel f x =
    (\y -> x { _odbioLicenseModel = y })
       <$> f (_odbioLicenseModel x)
{-# INLINE odbioLicenseModel #-}

-- | A list of availability zones for the orderable DB instance.
odbioAvailabilityZones
    :: Functor f
    => ([AvailabilityZone]
    -> f ([AvailabilityZone]))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioAvailabilityZones f x =
    (\y -> x { _odbioAvailabilityZones = y })
       <$> f (_odbioAvailabilityZones x)
{-# INLINE odbioAvailabilityZones #-}

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odbioMultiAZCapable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioMultiAZCapable f x =
    (\y -> x { _odbioMultiAZCapable = y })
       <$> f (_odbioMultiAZCapable x)
{-# INLINE odbioMultiAZCapable #-}

-- | Indicates whether this orderable DB instance can have a read replica.
odbioReadReplicaCapable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioReadReplicaCapable f x =
    (\y -> x { _odbioReadReplicaCapable = y })
       <$> f (_odbioReadReplicaCapable x)
{-# INLINE odbioReadReplicaCapable #-}

-- | Indicates whether this is a VPC orderable DB instance.
odbioVpc
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> OrderableDBInstanceOption
    -> f OrderableDBInstanceOption
odbioVpc f x =
    (\y -> x { _odbioVpc = y })
       <$> f (_odbioVpc x)
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
prParameterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterName f x =
    (\y -> x { _prParameterName = y })
       <$> f (_prParameterName x)
{-# INLINE prParameterName #-}

-- | Specifies the value of the parameter.
prParameterValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterValue f x =
    (\y -> x { _prParameterValue = y })
       <$> f (_prParameterValue x)
{-# INLINE prParameterValue #-}

-- | Provides a description of the parameter.
prDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDescription f x =
    (\y -> x { _prDescription = y })
       <$> f (_prDescription x)
{-# INLINE prDescription #-}

-- | Indicates the source of the parameter value.
prSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prSource f x =
    (\y -> x { _prSource = y })
       <$> f (_prSource x)
{-# INLINE prSource #-}

-- | Specifies the engine specific parameters type.
prApplyType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prApplyType f x =
    (\y -> x { _prApplyType = y })
       <$> f (_prApplyType x)
{-# INLINE prApplyType #-}

-- | Specifies the valid data type for the parameter.
prDataType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDataType f x =
    (\y -> x { _prDataType = y })
       <$> f (_prDataType x)
{-# INLINE prDataType #-}

-- | Specifies the valid range of values for the parameter.
prAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prAllowedValues f x =
    (\y -> x { _prAllowedValues = y })
       <$> f (_prAllowedValues x)
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Parameter
    -> f Parameter
prIsModifiable f x =
    (\y -> x { _prIsModifiable = y })
       <$> f (_prIsModifiable x)
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prMinimumEngineVersion f x =
    (\y -> x { _prMinimumEngineVersion = y })
       <$> f (_prMinimumEngineVersion x)
{-# INLINE prMinimumEngineVersion #-}

-- | Indicates when to apply parameter updates.
prApplyMethod
    :: Functor f
    => (Maybe ApplyMethod
    -> f (Maybe ApplyMethod))
    -> Parameter
    -> f Parameter
prApplyMethod f x =
    (\y -> x { _prApplyMethod = y })
       <$> f (_prApplyMethod x)
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
pmvDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvDBInstanceClass f x =
    (\y -> x { _pmvDBInstanceClass = y })
       <$> f (_pmvDBInstanceClass x)
{-# INLINE pmvDBInstanceClass #-}

-- | Contains the new AllocatedStorage size for the DB instance that will be
-- applied or is in progress.
pmvAllocatedStorage
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvAllocatedStorage f x =
    (\y -> x { _pmvAllocatedStorage = y })
       <$> f (_pmvAllocatedStorage x)
{-# INLINE pmvAllocatedStorage #-}

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvMasterUserPassword f x =
    (\y -> x { _pmvMasterUserPassword = y })
       <$> f (_pmvMasterUserPassword x)
{-# INLINE pmvMasterUserPassword #-}

-- | Specifies the pending port for the DB instance.
pmvPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvPort f x =
    (\y -> x { _pmvPort = y })
       <$> f (_pmvPort x)
{-# INLINE pmvPort #-}

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvBackupRetentionPeriod f x =
    (\y -> x { _pmvBackupRetentionPeriod = y })
       <$> f (_pmvBackupRetentionPeriod x)
{-# INLINE pmvBackupRetentionPeriod #-}

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvMultiAZ f x =
    (\y -> x { _pmvMultiAZ = y })
       <$> f (_pmvMultiAZ x)
{-# INLINE pmvMultiAZ #-}

-- | Indicates the database engine version.
pmvEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvEngineVersion f x =
    (\y -> x { _pmvEngineVersion = y })
       <$> f (_pmvEngineVersion x)
{-# INLINE pmvEngineVersion #-}

-- | Specifies the new Provisioned IOPS value for the DB instance that will be
-- applied or is being applied.
pmvIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvIops f x =
    (\y -> x { _pmvIops = y })
       <$> f (_pmvIops x)
{-# INLINE pmvIops #-}

-- | Contains the new DBInstanceIdentifier for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvDBInstanceIdentifier f x =
    (\y -> x { _pmvDBInstanceIdentifier = y })
       <$> f (_pmvDBInstanceIdentifier x)
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
rcRecurringChargeAmount
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeAmount f x =
    (\y -> x { _rcRecurringChargeAmount = y })
       <$> f (_rcRecurringChargeAmount x)
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeFrequency f x =
    (\y -> x { _rcRecurringChargeFrequency = y })
       <$> f (_rcRecurringChargeFrequency x)
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
rdbiReservedDBInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiReservedDBInstanceId f x =
    (\y -> x { _rdbiReservedDBInstanceId = y })
       <$> f (_rdbiReservedDBInstanceId x)
{-# INLINE rdbiReservedDBInstanceId #-}

-- | The offering identifier.
rdbiReservedDBInstancesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiReservedDBInstancesOfferingId f x =
    (\y -> x { _rdbiReservedDBInstancesOfferingId = y })
       <$> f (_rdbiReservedDBInstancesOfferingId x)
{-# INLINE rdbiReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbiDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiDBInstanceClass f x =
    (\y -> x { _rdbiDBInstanceClass = y })
       <$> f (_rdbiDBInstanceClass x)
{-# INLINE rdbiDBInstanceClass #-}

-- | The time the reservation started.
rdbiStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiStartTime f x =
    (\y -> x { _rdbiStartTime = y })
       <$> f (_rdbiStartTime x)
{-# INLINE rdbiStartTime #-}

-- | The duration of the reservation in seconds.
rdbiDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiDuration f x =
    (\y -> x { _rdbiDuration = y })
       <$> f (_rdbiDuration x)
{-# INLINE rdbiDuration #-}

-- | The fixed price charged for this reserved DB instance.
rdbiFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiFixedPrice f x =
    (\y -> x { _rdbiFixedPrice = y })
       <$> f (_rdbiFixedPrice x)
{-# INLINE rdbiFixedPrice #-}

-- | The hourly price charged for this reserved DB instance.
rdbiUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiUsagePrice f x =
    (\y -> x { _rdbiUsagePrice = y })
       <$> f (_rdbiUsagePrice x)
{-# INLINE rdbiUsagePrice #-}

-- | The currency code for the reserved DB instance.
rdbiCurrencyCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiCurrencyCode f x =
    (\y -> x { _rdbiCurrencyCode = y })
       <$> f (_rdbiCurrencyCode x)
{-# INLINE rdbiCurrencyCode #-}

-- | The number of reserved DB instances.
rdbiDBInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiDBInstanceCount f x =
    (\y -> x { _rdbiDBInstanceCount = y })
       <$> f (_rdbiDBInstanceCount x)
{-# INLINE rdbiDBInstanceCount #-}

-- | The description of the reserved DB instance.
rdbiProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiProductDescription f x =
    (\y -> x { _rdbiProductDescription = y })
       <$> f (_rdbiProductDescription x)
{-# INLINE rdbiProductDescription #-}

-- | The offering type of this reserved DB instance.
rdbiOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiOfferingType f x =
    (\y -> x { _rdbiOfferingType = y })
       <$> f (_rdbiOfferingType x)
{-# INLINE rdbiOfferingType #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdbiMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiMultiAZ f x =
    (\y -> x { _rdbiMultiAZ = y })
       <$> f (_rdbiMultiAZ x)
{-# INLINE rdbiMultiAZ #-}

-- | The state of the reserved DB instance.
rdbiState
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiState f x =
    (\y -> x { _rdbiState = y })
       <$> f (_rdbiState x)
{-# INLINE rdbiState #-}

-- | The recurring price charged to run this reserved DB instance.
rdbiRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedDBInstance
    -> f ReservedDBInstance
rdbiRecurringCharges f x =
    (\y -> x { _rdbiRecurringCharges = y })
       <$> f (_rdbiRecurringCharges x)
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
rdbioReservedDBInstancesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioReservedDBInstancesOfferingId f x =
    (\y -> x { _rdbioReservedDBInstancesOfferingId = y })
       <$> f (_rdbioReservedDBInstancesOfferingId x)
{-# INLINE rdbioReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbioDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioDBInstanceClass f x =
    (\y -> x { _rdbioDBInstanceClass = y })
       <$> f (_rdbioDBInstanceClass x)
{-# INLINE rdbioDBInstanceClass #-}

-- | The duration of the offering in seconds.
rdbioDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioDuration f x =
    (\y -> x { _rdbioDuration = y })
       <$> f (_rdbioDuration x)
{-# INLINE rdbioDuration #-}

-- | The fixed price charged for this offering.
rdbioFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioFixedPrice f x =
    (\y -> x { _rdbioFixedPrice = y })
       <$> f (_rdbioFixedPrice x)
{-# INLINE rdbioFixedPrice #-}

-- | The hourly price charged for this offering.
rdbioUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioUsagePrice f x =
    (\y -> x { _rdbioUsagePrice = y })
       <$> f (_rdbioUsagePrice x)
{-# INLINE rdbioUsagePrice #-}

-- | The currency code for the reserved DB instance offering.
rdbioCurrencyCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioCurrencyCode f x =
    (\y -> x { _rdbioCurrencyCode = y })
       <$> f (_rdbioCurrencyCode x)
{-# INLINE rdbioCurrencyCode #-}

-- | The database engine used by the offering.
rdbioProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioProductDescription f x =
    (\y -> x { _rdbioProductDescription = y })
       <$> f (_rdbioProductDescription x)
{-# INLINE rdbioProductDescription #-}

-- | The offering type.
rdbioOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioOfferingType f x =
    (\y -> x { _rdbioOfferingType = y })
       <$> f (_rdbioOfferingType x)
{-# INLINE rdbioOfferingType #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
rdbioMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioMultiAZ f x =
    (\y -> x { _rdbioMultiAZ = y })
       <$> f (_rdbioMultiAZ x)
{-# INLINE rdbioMultiAZ #-}

-- | The recurring price charged to run this reserved DB instance.
rdbioRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedDBInstancesOffering
    -> f ReservedDBInstancesOffering
rdbioRecurringCharges f x =
    (\y -> x { _rdbioRecurringCharges = y })
       <$> f (_rdbioRecurringCharges x)
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
ssssstSubnetIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
ssssstSubnetIdentifier f x =
    (\y -> x { _ssssstSubnetIdentifier = y })
       <$> f (_ssssstSubnetIdentifier x)
{-# INLINE ssssstSubnetIdentifier #-}

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
ssssstSubnetAvailabilityZone
    :: Functor f
    => (Maybe AvailabilityZone
    -> f (Maybe AvailabilityZone))
    -> Subnet
    -> f Subnet
ssssstSubnetAvailabilityZone f x =
    (\y -> x { _ssssstSubnetAvailabilityZone = y })
       <$> f (_ssssstSubnetAvailabilityZone x)
{-# INLINE ssssstSubnetAvailabilityZone #-}

-- | Specifies the status of the subnet.
ssssstSubnetStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
ssssstSubnetStatus f x =
    (\y -> x { _ssssstSubnetStatus = y })
       <$> f (_ssssstSubnetStatus x)
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
tgKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
tgKey f x =
    (\y -> x { _tgKey = y })
       <$> f (_tgKey x)
{-# INLINE tgKey #-}

-- | A value is the optional value of the tag. The string value can be from 1 to
-- 256 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tgValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
tgValue f x =
    (\y -> x { _tgValue = y })
       <$> f (_tgValue x)
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
vsgmVpcSecurityGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcSecurityGroupMembership
    -> f VpcSecurityGroupMembership
vsgmVpcSecurityGroupId f x =
    (\y -> x { _vsgmVpcSecurityGroupId = y })
       <$> f (_vsgmVpcSecurityGroupId x)
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | The status of the VPC security group.
vsgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VpcSecurityGroupMembership
    -> f VpcSecurityGroupMembership
vsgmStatus f x =
    (\y -> x { _vsgmStatus = y })
       <$> f (_vsgmStatus x)
{-# INLINE vsgmStatus #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
