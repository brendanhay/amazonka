{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AvailabilityZone
    , mkAvailabilityZone
    , azName
    , azProvisionedIopsCapable

    -- * CharacterSet
    , CharacterSet
    , mkCharacterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- * DBEngineVersion
    , DBEngineVersion
    , dbevEngine
    , dbevEngineVersion
    , dbevDBParameterGroupFamily
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDefaultCharacterSet
    , dbevSupportedCharacterSets

    -- * DBInstance
    , DBInstance
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
    , DBInstanceStatusInfo
    , mkDBInstanceStatusInfo
    , dbisiStatusType
    , dbisiNormal
    , dbisiStatus
    , dbisiMessage

    -- * DBParameterGroup
    , DBParameterGroup
    , dbpgDBParameterGroupName
    , dbpgDBParameterGroupFamily
    , dbpgDescription

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus
    , mkDBParameterGroupStatus
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- * DBSecurityGroup
    , DBSecurityGroup
    , dbsgOwnerId
    , dbsgDBSecurityGroupName
    , dbsgDBSecurityGroupDescription
    , dbsgVpcId
    , dbsgEC2SecurityGroups
    , dbsgIPRanges

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , mkDBSecurityGroupMembership
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- * DBSnapshot
    , DBSnapshot
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
    , mkDBSubnetGroup
    , dbshDBSubnetGroupName
    , dbshDBSubnetGroupDescription
    , dbshVpcId
    , dbshSubnetGroupStatus
    , dbshSubnets

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , ddblfdLogFileName
    , ddblfdLastWritten
    , ddblfdSize

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , mkEC2SecurityGroup
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupOwnerId

    -- * Endpoint
    , Endpoint
    , mkEndpoint
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event
    , eySourceIdentifier
    , eySourceType
    , eyMessage
    , eyEventCategories
    , eyDate

    -- * EventCategoriesMap
    , EventCategoriesMap
    , ecqSourceType
    , ecqEventCategories

    -- * EventSubscription
    , EventSubscription
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
    , IPRange
    , mkIPRange
    , iprStatus
    , iprCIDRIP

    -- * Option
    , Option
    , mkOption
    , onOptionName
    , onOptionDescription
    , onPersistent
    , onPermanent
    , onPort
    , onOptionSettings
    , onDBSecurityGroupMemberships
    , onVpcSecurityGroupMemberships

    -- * OptionConfiguration
    , OptionConfiguration
    , mkOptionConfiguration
    , ocOptionName
    , ocPort
    , ocDBSecurityGroupMemberships
    , ocVpcSecurityGroupMemberships
    , ocOptionSettings

    -- * OptionGroup
    , OptionGroup
    , ogOptionGroupName
    , ogOptionGroupDescription
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptions
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogVpcId

    -- * OptionGroupMembership
    , OptionGroupMembership
    , mkOptionGroupMembership
    , ogmOptionGroupName
    , ogmStatus

    -- * OptionGroupOption
    , OptionGroupOption
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
    , OptionGroupOptionSetting
    , mkOptionGroupOptionSetting
    , ogosSettingName
    , ogosSettingDescription
    , ogosDefaultValue
    , ogosApplyType
    , ogosAllowedValues
    , ogosIsModifiable

    -- * OptionSetting
    , OptionSetting
    , mkOptionSetting
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
    , odbioEngine
    , odbioEngineVersion
    , odbioDBInstanceClass
    , odbioLicenseModel
    , odbioAvailabilityZones
    , odbioMultiAZCapable
    , odbioReadReplicaCapable
    , odbioVpc

    -- * Parameter
    , Parameter
    , mkParameter
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
    , PendingModifiedValues
    , mkPendingModifiedValues
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
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedDBInstance
    , ReservedDBInstance
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
    , ReservedDBInstancesOffering
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
    , Subnet
    , mkSubnet
    , ssssstSubnetIdentifier
    , ssssstSubnetAvailabilityZone
    , ssssstSubnetStatus

    -- * Tag
    , Tag
    , mkTag
    , tgKey
    , tgValue

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , mkVpcSecurityGroupMembership
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
azName = lens _azName (\s a -> s { _azName = a })
{-# INLINE azName #-}

-- | True indicates the availability zone is capable of provisioned IOPs.
azProvisionedIopsCapable :: Lens' AvailabilityZone (Maybe Bool)
azProvisionedIopsCapable = lens _azProvisionedIopsCapable (\s a -> s { _azProvisionedIopsCapable = a })
{-# INLINE azProvisionedIopsCapable #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
    { _azName = Nothing
    , _azProvisionedIopsCapable = Nothing
    }
{-# INLINE mkAvailabilityZone #-}

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
csCharacterSetName = lens _csCharacterSetName (\s a -> s { _csCharacterSetName = a })
{-# INLINE csCharacterSetName #-}

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription = lens _csCharacterSetDescription (\s a -> s { _csCharacterSetDescription = a })
{-# INLINE csCharacterSetDescription #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CharacterSet' data type to populate a request.
mkCharacterSet :: CharacterSet
mkCharacterSet = CharacterSet
    { _csCharacterSetName = Nothing
    , _csCharacterSetDescription = Nothing
    }
{-# INLINE mkCharacterSet #-}

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
dbevEngine = lens _dbevEngine (\s a -> s { _dbevEngine = a })
{-# INLINE dbevEngine #-}

-- | The version number of the database engine.
dbevEngineVersion :: Lens' DBEngineVersion (Maybe Text)
dbevEngineVersion = lens _dbevEngineVersion (\s a -> s { _dbevEngineVersion = a })
{-# INLINE dbevEngineVersion #-}

-- | The name of the DB parameter group family for the database engine.
dbevDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
dbevDBParameterGroupFamily = lens _dbevDBParameterGroupFamily (\s a -> s { _dbevDBParameterGroupFamily = a })
{-# INLINE dbevDBParameterGroupFamily #-}

-- | The description of the database engine.
dbevDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineDescription = lens _dbevDBEngineDescription (\s a -> s { _dbevDBEngineDescription = a })
{-# INLINE dbevDBEngineDescription #-}

-- | The description of the database engine version.
dbevDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineVersionDescription = lens _dbevDBEngineVersionDescription (\s a -> s { _dbevDBEngineVersionDescription = a })
{-# INLINE dbevDBEngineVersionDescription #-}

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
dbevDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
dbevDefaultCharacterSet = lens _dbevDefaultCharacterSet (\s a -> s { _dbevDefaultCharacterSet = a })
{-# INLINE dbevDefaultCharacterSet #-}

-- | A list of the character sets supported by this engine for the
-- CharacterSetName parameter of the CreateDBInstance API.
dbevSupportedCharacterSets :: Lens' DBEngineVersion ([CharacterSet])
dbevSupportedCharacterSets = lens _dbevSupportedCharacterSets (\s a -> s { _dbevSupportedCharacterSets = a })
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
dbiDBInstanceIdentifier = lens _dbiDBInstanceIdentifier (\s a -> s { _dbiDBInstanceIdentifier = a })
{-# INLINE dbiDBInstanceIdentifier #-}

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbiDBInstanceClass :: Lens' DBInstance (Maybe Text)
dbiDBInstanceClass = lens _dbiDBInstanceClass (\s a -> s { _dbiDBInstanceClass = a })
{-# INLINE dbiDBInstanceClass #-}

-- | Provides the name of the database engine to be used for this DB instance.
dbiEngine :: Lens' DBInstance (Maybe Text)
dbiEngine = lens _dbiEngine (\s a -> s { _dbiEngine = a })
{-# INLINE dbiEngine #-}

-- | Specifies the current state of this database.
dbiDBInstanceStatus :: Lens' DBInstance (Maybe Text)
dbiDBInstanceStatus = lens _dbiDBInstanceStatus (\s a -> s { _dbiDBInstanceStatus = a })
{-# INLINE dbiDBInstanceStatus #-}

-- | Contains the master username for the DB instance.
dbiMasterUsername :: Lens' DBInstance (Maybe Text)
dbiMasterUsername = lens _dbiMasterUsername (\s a -> s { _dbiMasterUsername = a })
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
dbiDBName = lens _dbiDBName (\s a -> s { _dbiDBName = a })
{-# INLINE dbiDBName #-}

-- | Specifies the connection endpoint.
dbiEndpoint :: Lens' DBInstance (Maybe Endpoint)
dbiEndpoint = lens _dbiEndpoint (\s a -> s { _dbiEndpoint = a })
{-# INLINE dbiEndpoint #-}

-- | Specifies the allocated storage size specified in gigabytes.
dbiAllocatedStorage :: Lens' DBInstance (Maybe Integer)
dbiAllocatedStorage = lens _dbiAllocatedStorage (\s a -> s { _dbiAllocatedStorage = a })
{-# INLINE dbiAllocatedStorage #-}

-- | Provides the date and time the DB instance was created.
dbiInstanceCreateTime :: Lens' DBInstance (Maybe ISO8601)
dbiInstanceCreateTime = lens _dbiInstanceCreateTime (\s a -> s { _dbiInstanceCreateTime = a })
{-# INLINE dbiInstanceCreateTime #-}

-- | Specifies the daily time range during which automated backups are created
-- if automated backups are enabled, as determined by the
-- BackupRetentionPeriod.
dbiPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredBackupWindow = lens _dbiPreferredBackupWindow (\s a -> s { _dbiPreferredBackupWindow = a })
{-# INLINE dbiPreferredBackupWindow #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
dbiBackupRetentionPeriod :: Lens' DBInstance (Maybe Integer)
dbiBackupRetentionPeriod = lens _dbiBackupRetentionPeriod (\s a -> s { _dbiBackupRetentionPeriod = a })
{-# INLINE dbiBackupRetentionPeriod #-}

-- | Provides List of DB security group elements containing only
-- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
dbiDBSecurityGroups :: Lens' DBInstance ([DBSecurityGroupMembership])
dbiDBSecurityGroups = lens _dbiDBSecurityGroups (\s a -> s { _dbiDBSecurityGroups = a })
{-# INLINE dbiDBSecurityGroups #-}

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroups :: Lens' DBInstance ([VpcSecurityGroupMembership])
dbiVpcSecurityGroups = lens _dbiVpcSecurityGroups (\s a -> s { _dbiVpcSecurityGroups = a })
{-# INLINE dbiVpcSecurityGroups #-}

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroups :: Lens' DBInstance ([DBParameterGroupStatus])
dbiDBParameterGroups = lens _dbiDBParameterGroups (\s a -> s { _dbiDBParameterGroups = a })
{-# INLINE dbiDBParameterGroups #-}

-- | Specifies the name of the Availability Zone the DB instance is located in.
dbiAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiAvailabilityZone = lens _dbiAvailabilityZone (\s a -> s { _dbiAvailabilityZone = a })
{-# INLINE dbiAvailabilityZone #-}

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
dbiDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
dbiDBSubnetGroup = lens _dbiDBSubnetGroup (\s a -> s { _dbiDBSubnetGroup = a })
{-# INLINE dbiDBSubnetGroup #-}

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
dbiPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredMaintenanceWindow = lens _dbiPreferredMaintenanceWindow (\s a -> s { _dbiPreferredMaintenanceWindow = a })
{-# INLINE dbiPreferredMaintenanceWindow #-}

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
dbiPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
dbiPendingModifiedValues = lens _dbiPendingModifiedValues (\s a -> s { _dbiPendingModifiedValues = a })
{-# INLINE dbiPendingModifiedValues #-}

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbiLatestRestorableTime :: Lens' DBInstance (Maybe ISO8601)
dbiLatestRestorableTime = lens _dbiLatestRestorableTime (\s a -> s { _dbiLatestRestorableTime = a })
{-# INLINE dbiLatestRestorableTime #-}

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbiMultiAZ :: Lens' DBInstance (Maybe Bool)
dbiMultiAZ = lens _dbiMultiAZ (\s a -> s { _dbiMultiAZ = a })
{-# INLINE dbiMultiAZ #-}

-- | Indicates the database engine version.
dbiEngineVersion :: Lens' DBInstance (Maybe Text)
dbiEngineVersion = lens _dbiEngineVersion (\s a -> s { _dbiEngineVersion = a })
{-# INLINE dbiEngineVersion #-}

-- | Indicates that minor version patches are applied automatically.
dbiAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
dbiAutoMinorVersionUpgrade = lens _dbiAutoMinorVersionUpgrade (\s a -> s { _dbiAutoMinorVersionUpgrade = a })
{-# INLINE dbiAutoMinorVersionUpgrade #-}

-- | Contains the identifier of the source DB instance if this DB instance is a
-- read replica.
dbiReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiReadReplicaSourceDBInstanceIdentifier = lens _dbiReadReplicaSourceDBInstanceIdentifier (\s a -> s { _dbiReadReplicaSourceDBInstanceIdentifier = a })
{-# INLINE dbiReadReplicaSourceDBInstanceIdentifier #-}

-- | Contains one or more identifiers of the read replicas associated with this
-- DB instance.
dbiReadReplicaDBInstanceIdentifiers :: Lens' DBInstance ([Text])
dbiReadReplicaDBInstanceIdentifiers = lens _dbiReadReplicaDBInstanceIdentifiers (\s a -> s { _dbiReadReplicaDBInstanceIdentifiers = a })
{-# INLINE dbiReadReplicaDBInstanceIdentifiers #-}

-- | License model information for this DB instance.
dbiLicenseModel :: Lens' DBInstance (Maybe Text)
dbiLicenseModel = lens _dbiLicenseModel (\s a -> s { _dbiLicenseModel = a })
{-# INLINE dbiLicenseModel #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops :: Lens' DBInstance (Maybe Integer)
dbiIops = lens _dbiIops (\s a -> s { _dbiIops = a })
{-# INLINE dbiIops #-}

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMemberships :: Lens' DBInstance ([OptionGroupMembership])
dbiOptionGroupMemberships = lens _dbiOptionGroupMemberships (\s a -> s { _dbiOptionGroupMemberships = a })
{-# INLINE dbiOptionGroupMemberships #-}

-- | If present, specifies the name of the character set that this instance is
-- associated with.
dbiCharacterSetName :: Lens' DBInstance (Maybe Text)
dbiCharacterSetName = lens _dbiCharacterSetName (\s a -> s { _dbiCharacterSetName = a })
{-# INLINE dbiCharacterSetName #-}

-- | If present, specifies the name of the secondary Availability Zone for a DB
-- instance with multi-AZ support.
dbiSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiSecondaryAvailabilityZone = lens _dbiSecondaryAvailabilityZone (\s a -> s { _dbiSecondaryAvailabilityZone = a })
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
dbiPubliclyAccessible = lens _dbiPubliclyAccessible (\s a -> s { _dbiPubliclyAccessible = a })
{-# INLINE dbiPubliclyAccessible #-}

-- | The status of a read replica. If the instance is not a read replica, this
-- will be blank.
dbiStatusInfos :: Lens' DBInstance ([DBInstanceStatusInfo])
dbiStatusInfos = lens _dbiStatusInfos (\s a -> s { _dbiStatusInfos = a })
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
dbisiStatusType = lens _dbisiStatusType (\s a -> s { _dbisiStatusType = a })
{-# INLINE dbisiStatusType #-}

-- | Boolean value that is true if the instance is operating normally, or false
-- if the instance is in an error state.
dbisiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
dbisiNormal = lens _dbisiNormal (\s a -> s { _dbisiNormal = a })
{-# INLINE dbisiNormal #-}

-- | Status of the DB instance. For a StatusType of read replica, the values can
-- be replicating, error, stopped, or terminated.
dbisiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatus = lens _dbisiStatus (\s a -> s { _dbisiStatus = a })
{-# INLINE dbisiStatus #-}

-- | Details of the error if there is an error for the instance. If the instance
-- is not in an error state, this value is blank.
dbisiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiMessage = lens _dbisiMessage (\s a -> s { _dbisiMessage = a })
{-# INLINE dbisiMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBInstanceStatusInfo' data type to populate a request.
mkDBInstanceStatusInfo :: DBInstanceStatusInfo
mkDBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType = Nothing
    , _dbisiNormal = Nothing
    , _dbisiStatus = Nothing
    , _dbisiMessage = Nothing
    }
{-# INLINE mkDBInstanceStatusInfo #-}

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
dbpgDBParameterGroupName = lens _dbpgDBParameterGroupName (\s a -> s { _dbpgDBParameterGroupName = a })
{-# INLINE dbpgDBParameterGroupName #-}

-- | Provides the name of the DB parameter group family that this DB parameter
-- group is compatible with.
dbpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupFamily = lens _dbpgDBParameterGroupFamily (\s a -> s { _dbpgDBParameterGroupFamily = a })
{-# INLINE dbpgDBParameterGroupFamily #-}

-- | Provides the customer-specified description for this DB parameter group.
dbpgDescription :: Lens' DBParameterGroup (Maybe Text)
dbpgDescription = lens _dbpgDescription (\s a -> s { _dbpgDescription = a })
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
dbpgsDBParameterGroupName = lens _dbpgsDBParameterGroupName (\s a -> s { _dbpgsDBParameterGroupName = a })
{-# INLINE dbpgsDBParameterGroupName #-}

-- | The status of parameter updates.
dbpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dbpgsParameterApplyStatus = lens _dbpgsParameterApplyStatus (\s a -> s { _dbpgsParameterApplyStatus = a })
{-# INLINE dbpgsParameterApplyStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBParameterGroupStatus' data type to populate a request.
mkDBParameterGroupStatus :: DBParameterGroupStatus
mkDBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName = Nothing
    , _dbpgsParameterApplyStatus = Nothing
    }
{-# INLINE mkDBParameterGroupStatus #-}

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
dbsgOwnerId = lens _dbsgOwnerId (\s a -> s { _dbsgOwnerId = a })
{-# INLINE dbsgOwnerId #-}

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName = lens _dbsgDBSecurityGroupName (\s a -> s { _dbsgDBSecurityGroupName = a })
{-# INLINE dbsgDBSecurityGroupName #-}

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription = lens _dbsgDBSecurityGroupDescription (\s a -> s { _dbsgDBSecurityGroupDescription = a })
{-# INLINE dbsgDBSecurityGroupDescription #-}

-- | Provides the VpcId of the DB security group.
dbsgVpcId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVpcId = lens _dbsgVpcId (\s a -> s { _dbsgVpcId = a })
{-# INLINE dbsgVpcId #-}

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup ([EC2SecurityGroup])
dbsgEC2SecurityGroups = lens _dbsgEC2SecurityGroups (\s a -> s { _dbsgEC2SecurityGroups = a })
{-# INLINE dbsgEC2SecurityGroups #-}

-- | Contains a list of IPRange elements.
dbsgIPRanges :: Lens' DBSecurityGroup ([IPRange])
dbsgIPRanges = lens _dbsgIPRanges (\s a -> s { _dbsgIPRanges = a })
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
dbsgmDBSecurityGroupName = lens _dbsgmDBSecurityGroupName (\s a -> s { _dbsgmDBSecurityGroupName = a })
{-# INLINE dbsgmDBSecurityGroupName #-}

-- | The status of the DB security group.
dbsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dbsgmStatus = lens _dbsgmStatus (\s a -> s { _dbsgmStatus = a })
{-# INLINE dbsgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSecurityGroupMembership' data type to populate a request.
mkDBSecurityGroupMembership :: DBSecurityGroupMembership
mkDBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName = Nothing
    , _dbsgmStatus = Nothing
    }
{-# INLINE mkDBSecurityGroupMembership #-}

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
dbsDBSnapshotIdentifier = lens _dbsDBSnapshotIdentifier (\s a -> s { _dbsDBSnapshotIdentifier = a })
{-# INLINE dbsDBSnapshotIdentifier #-}

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBInstanceIdentifier = lens _dbsDBInstanceIdentifier (\s a -> s { _dbsDBInstanceIdentifier = a })
{-# INLINE dbsDBInstanceIdentifier #-}

-- | Provides the time (UTC) when the snapshot was taken.
dbsSnapshotCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsSnapshotCreateTime = lens _dbsSnapshotCreateTime (\s a -> s { _dbsSnapshotCreateTime = a })
{-# INLINE dbsSnapshotCreateTime #-}

-- | Specifies the name of the database engine.
dbsEngine :: Lens' DBSnapshot (Maybe Text)
dbsEngine = lens _dbsEngine (\s a -> s { _dbsEngine = a })
{-# INLINE dbsEngine #-}

-- | Specifies the allocated storage size in gigabytes (GB).
dbsAllocatedStorage :: Lens' DBSnapshot (Maybe Integer)
dbsAllocatedStorage = lens _dbsAllocatedStorage (\s a -> s { _dbsAllocatedStorage = a })
{-# INLINE dbsAllocatedStorage #-}

-- | Specifies the status of this DB snapshot.
dbsStatus :: Lens' DBSnapshot (Maybe Text)
dbsStatus = lens _dbsStatus (\s a -> s { _dbsStatus = a })
{-# INLINE dbsStatus #-}

-- | Specifies the port that the database engine was listening on at the time of
-- the snapshot.
dbsPort :: Lens' DBSnapshot (Maybe Integer)
dbsPort = lens _dbsPort (\s a -> s { _dbsPort = a })
{-# INLINE dbsPort #-}

-- | Specifies the name of the Availability Zone the DB instance was located in
-- at the time of the DB snapshot.
dbsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dbsAvailabilityZone = lens _dbsAvailabilityZone (\s a -> s { _dbsAvailabilityZone = a })
{-# INLINE dbsAvailabilityZone #-}

-- | Provides the Vpc Id associated with the DB snapshot.
dbsVpcId :: Lens' DBSnapshot (Maybe Text)
dbsVpcId = lens _dbsVpcId (\s a -> s { _dbsVpcId = a })
{-# INLINE dbsVpcId #-}

-- | Specifies the time (UTC) when the snapshot was taken.
dbsInstanceCreateTime :: Lens' DBSnapshot (Maybe ISO8601)
dbsInstanceCreateTime = lens _dbsInstanceCreateTime (\s a -> s { _dbsInstanceCreateTime = a })
{-# INLINE dbsInstanceCreateTime #-}

-- | Provides the master username for the DB snapshot.
dbsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dbsMasterUsername = lens _dbsMasterUsername (\s a -> s { _dbsMasterUsername = a })
{-# INLINE dbsMasterUsername #-}

-- | Specifies the version of the database engine.
dbsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dbsEngineVersion = lens _dbsEngineVersion (\s a -> s { _dbsEngineVersion = a })
{-# INLINE dbsEngineVersion #-}

-- | License model information for the restored DB instance.
dbsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dbsLicenseModel = lens _dbsLicenseModel (\s a -> s { _dbsLicenseModel = a })
{-# INLINE dbsLicenseModel #-}

-- | Provides the type of the DB snapshot.
dbsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dbsSnapshotType = lens _dbsSnapshotType (\s a -> s { _dbsSnapshotType = a })
{-# INLINE dbsSnapshotType #-}

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB
-- instance at the time of the snapshot.
dbsIops :: Lens' DBSnapshot (Maybe Integer)
dbsIops = lens _dbsIops (\s a -> s { _dbsIops = a })
{-# INLINE dbsIops #-}

-- | Provides the option group name for the DB snapshot.
dbsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dbsOptionGroupName = lens _dbsOptionGroupName (\s a -> s { _dbsOptionGroupName = a })
{-# INLINE dbsOptionGroupName #-}

-- | The percentage of the estimated data that has been transferred.
dbsPercentProgress :: Lens' DBSnapshot (Maybe Integer)
dbsPercentProgress = lens _dbsPercentProgress (\s a -> s { _dbsPercentProgress = a })
{-# INLINE dbsPercentProgress #-}

-- | The region that the DB snapshot was created in or copied from.
dbsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dbsSourceRegion = lens _dbsSourceRegion (\s a -> s { _dbsSourceRegion = a })
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
dbshDBSubnetGroupName = lens _dbshDBSubnetGroupName (\s a -> s { _dbshDBSubnetGroupName = a })
{-# INLINE dbshDBSubnetGroupName #-}

-- | Provides the description of the DB subnet group.
dbshDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dbshDBSubnetGroupDescription = lens _dbshDBSubnetGroupDescription (\s a -> s { _dbshDBSubnetGroupDescription = a })
{-# INLINE dbshDBSubnetGroupDescription #-}

-- | Provides the VpcId of the DB subnet group.
dbshVpcId :: Lens' DBSubnetGroup (Maybe Text)
dbshVpcId = lens _dbshVpcId (\s a -> s { _dbshVpcId = a })
{-# INLINE dbshVpcId #-}

-- | Provides the status of the DB subnet group.
dbshSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dbshSubnetGroupStatus = lens _dbshSubnetGroupStatus (\s a -> s { _dbshSubnetGroupStatus = a })
{-# INLINE dbshSubnetGroupStatus #-}

-- | Contains a list of Subnet elements.
dbshSubnets :: Lens' DBSubnetGroup ([Subnet])
dbshSubnets = lens _dbshSubnets (\s a -> s { _dbshSubnets = a })
{-# INLINE dbshSubnets #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSubnetGroup' data type to populate a request.
mkDBSubnetGroup :: DBSubnetGroup
mkDBSubnetGroup = DBSubnetGroup
    { _dbshDBSubnetGroupName = Nothing
    , _dbshDBSubnetGroupDescription = Nothing
    , _dbshVpcId = Nothing
    , _dbshSubnetGroupStatus = Nothing
    , _dbshSubnets = mempty
    }
{-# INLINE mkDBSubnetGroup #-}

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
ddblfdLogFileName = lens _ddblfdLogFileName (\s a -> s { _ddblfdLogFileName = a })
{-# INLINE ddblfdLogFileName #-}

-- | A POSIX timestamp when the last log entry was written.
ddblfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdLastWritten = lens _ddblfdLastWritten (\s a -> s { _ddblfdLastWritten = a })
{-# INLINE ddblfdLastWritten #-}

-- | The size, in bytes, of the log file for the specified DB instance.
ddblfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdSize = lens _ddblfdSize (\s a -> s { _ddblfdSize = a })
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
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })
{-# INLINE ecsgStatus #-}

-- | Specifies the name of the EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName = lens _ecsgEC2SecurityGroupName (\s a -> s { _ecsgEC2SecurityGroupName = a })
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | Specifies the id of the EC2 security group.
ecsgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupId = lens _ecsgEC2SecurityGroupId (\s a -> s { _ecsgEC2SecurityGroupId = a })
{-# INLINE ecsgEC2SecurityGroupId #-}

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId = lens _ecsgEC2SecurityGroupOwnerId (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
mkEC2SecurityGroup :: EC2SecurityGroup
mkEC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupId = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE mkEC2SecurityGroup #-}

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
eAddress = lens _eAddress (\s a -> s { _eAddress = a })
{-# INLINE eAddress #-}

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })
{-# INLINE ePort #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type to populate a request.
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _eAddress = Nothing
    , _ePort = Nothing
    }
{-# INLINE mkEndpoint #-}

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
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\s a -> s { _edDBParameterGroupFamily = a })
{-# INLINE edDBParameterGroupFamily #-}

-- | An optional pagination token provided by a previous EngineDefaults request.
-- If this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s { _edMarker = a })
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults ([Parameter])
edParameters = lens _edParameters (\s a -> s { _edParameters = a })
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
eySourceIdentifier = lens _eySourceIdentifier (\s a -> s { _eySourceIdentifier = a })
{-# INLINE eySourceIdentifier #-}

-- | Specifies the source type for this event.
eySourceType :: Lens' Event (Maybe SourceType)
eySourceType = lens _eySourceType (\s a -> s { _eySourceType = a })
{-# INLINE eySourceType #-}

-- | Provides the text of this event.
eyMessage :: Lens' Event (Maybe Text)
eyMessage = lens _eyMessage (\s a -> s { _eyMessage = a })
{-# INLINE eyMessage #-}

-- | Specifies the category for the event.
eyEventCategories :: Lens' Event ([Text])
eyEventCategories = lens _eyEventCategories (\s a -> s { _eyEventCategories = a })
{-# INLINE eyEventCategories #-}

-- | Specifies the date and time of the event.
eyDate :: Lens' Event (Maybe ISO8601)
eyDate = lens _eyDate (\s a -> s { _eyDate = a })
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
ecqSourceType = lens _ecqSourceType (\s a -> s { _ecqSourceType = a })
{-# INLINE ecqSourceType #-}

-- | The event categories for the specified source type.
ecqEventCategories :: Lens' EventCategoriesMap ([Text])
ecqEventCategories = lens _ecqEventCategories (\s a -> s { _ecqEventCategories = a })
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
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })
{-# INLINE esCustomerAwsId #-}

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })
{-# INLINE esCustSubscriptionId #-}

-- | The topic ARN of the RDS event notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })
{-# INLINE esSnsTopicArn #-}

-- | The status of the RDS event notification subscription. Constraints: Can be
-- one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist The status "no-permission" indicates that
-- RDS no longer has permission to post to the SNS topic. The status
-- "topic-not-exist" indicates that the topic was deleted after the
-- subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\s a -> s { _esStatus = a })
{-# INLINE esStatus #-}

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\s a -> s { _esSubscriptionCreationTime = a })
{-# INLINE esSubscriptionCreationTime #-}

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })
{-# INLINE esSourceType #-}

-- | A list of source Ids for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription ([Text])
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a })
{-# INLINE esSourceIdsList #-}

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription ([Text])
esEventCategoriesList = lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })
{-# INLINE esEventCategoriesList #-}

-- | A Boolean value indicating if the subscription is enabled. True indicates
-- the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })
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
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })
{-# INLINE iprStatus #-}

-- | Specifies the IP range.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })
{-# INLINE iprCIDRIP #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IPRange' data type to populate a request.
mkIPRange :: IPRange
mkIPRange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    }
{-# INLINE mkIPRange #-}

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
onOptionName = lens _onOptionName (\s a -> s { _onOptionName = a })
{-# INLINE onOptionName #-}

-- | The description of the option.
onOptionDescription :: Lens' Option (Maybe Text)
onOptionDescription = lens _onOptionDescription (\s a -> s { _onOptionDescription = a })
{-# INLINE onOptionDescription #-}

-- | Indicate if this option is persistent.
onPersistent :: Lens' Option (Maybe Bool)
onPersistent = lens _onPersistent (\s a -> s { _onPersistent = a })
{-# INLINE onPersistent #-}

-- | Indicate if this option is permanent.
onPermanent :: Lens' Option (Maybe Bool)
onPermanent = lens _onPermanent (\s a -> s { _onPermanent = a })
{-# INLINE onPermanent #-}

-- | If required, the port configured for this option to use.
onPort :: Lens' Option (Maybe Integer)
onPort = lens _onPort (\s a -> s { _onPort = a })
{-# INLINE onPort #-}

-- | The option settings for this option.
onOptionSettings :: Lens' Option ([OptionSetting])
onOptionSettings = lens _onOptionSettings (\s a -> s { _onOptionSettings = a })
{-# INLINE onOptionSettings #-}

-- | If the option requires access to a port, then this DB security group allows
-- access to the port.
onDBSecurityGroupMemberships :: Lens' Option ([DBSecurityGroupMembership])
onDBSecurityGroupMemberships = lens _onDBSecurityGroupMemberships (\s a -> s { _onDBSecurityGroupMemberships = a })
{-# INLINE onDBSecurityGroupMemberships #-}

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
onVpcSecurityGroupMemberships :: Lens' Option ([VpcSecurityGroupMembership])
onVpcSecurityGroupMemberships = lens _onVpcSecurityGroupMemberships (\s a -> s { _onVpcSecurityGroupMemberships = a })
{-# INLINE onVpcSecurityGroupMemberships #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Option' data type to populate a request.
mkOption :: Option
mkOption = Option
    { _onOptionName = Nothing
    , _onOptionDescription = Nothing
    , _onPersistent = Nothing
    , _onPermanent = Nothing
    , _onPort = Nothing
    , _onOptionSettings = mempty
    , _onDBSecurityGroupMemberships = mempty
    , _onVpcSecurityGroupMemberships = mempty
    }
{-# INLINE mkOption #-}

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
ocOptionName = lens _ocOptionName (\s a -> s { _ocOptionName = a })
{-# INLINE ocOptionName #-}

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Integer)
ocPort = lens _ocPort (\s a -> s { _ocPort = a })
{-# INLINE ocPort #-}

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration ([Text])
ocDBSecurityGroupMemberships = lens _ocDBSecurityGroupMemberships (\s a -> s { _ocDBSecurityGroupMemberships = a })
{-# INLINE ocDBSecurityGroupMemberships #-}

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupMemberships :: Lens' OptionConfiguration ([Text])
ocVpcSecurityGroupMemberships = lens _ocVpcSecurityGroupMemberships (\s a -> s { _ocVpcSecurityGroupMemberships = a })
{-# INLINE ocVpcSecurityGroupMemberships #-}

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration ([OptionSetting])
ocOptionSettings = lens _ocOptionSettings (\s a -> s { _ocOptionSettings = a })
{-# INLINE ocOptionSettings #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionConfiguration' data type to populate a request.
mkOptionConfiguration :: Text -- ^ 'ocOptionName'
                      -> OptionConfiguration
mkOptionConfiguration p1 = OptionConfiguration
    { _ocOptionName = p1
    , _ocPort = Nothing
    , _ocDBSecurityGroupMemberships = mempty
    , _ocVpcSecurityGroupMemberships = mempty
    , _ocOptionSettings = mempty
    }
{-# INLINE mkOptionConfiguration #-}

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
ogOptionGroupName = lens _ogOptionGroupName (\s a -> s { _ogOptionGroupName = a })
{-# INLINE ogOptionGroupName #-}

-- | Provides the description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription = lens _ogOptionGroupDescription (\s a -> s { _ogOptionGroupDescription = a })
{-# INLINE ogOptionGroupDescription #-}

-- | Engine name that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\s a -> s { _ogEngineName = a })
{-# INLINE ogEngineName #-}

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion = lens _ogMajorEngineVersion (\s a -> s { _ogMajorEngineVersion = a })
{-# INLINE ogMajorEngineVersion #-}

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup ([Option])
ogOptions = lens _ogOptions (\s a -> s { _ogOptions = a })
{-# INLINE ogOptions #-}

-- | Indicates whether this option group can be applied to both VPC and non-VPC
-- instances. The value 'true' indicates the option group can be applied to
-- both VPC and non-VPC instances.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVpcAndNonVpcInstanceMemberships = lens _ogAllowsVpcAndNonVpcInstanceMemberships (\s a -> s { _ogAllowsVpcAndNonVpcInstanceMemberships = a })
{-# INLINE ogAllowsVpcAndNonVpcInstanceMemberships #-}

-- | If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
-- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is blank,
-- then this option group can be applied to both VPC and non-VPC instances. If
-- this field contains a value, then this option group can only be applied to
-- instances that are in the VPC indicated by this field.
ogVpcId :: Lens' OptionGroup (Maybe Text)
ogVpcId = lens _ogVpcId (\s a -> s { _ogVpcId = a })
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
ogmOptionGroupName = lens _ogmOptionGroupName (\s a -> s { _ogmOptionGroupName = a })
{-# INLINE ogmOptionGroupName #-}

-- | The status of the DB instance's option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\s a -> s { _ogmStatus = a })
{-# INLINE ogmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupMembership' data type to populate a request.
mkOptionGroupMembership :: OptionGroupMembership
mkOptionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName = Nothing
    , _ogmStatus = Nothing
    }
{-# INLINE mkOptionGroupMembership #-}

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
ogqName = lens _ogqName (\s a -> s { _ogqName = a })
{-# INLINE ogqName #-}

-- | The description of the option.
ogqDescription :: Lens' OptionGroupOption (Maybe Text)
ogqDescription = lens _ogqDescription (\s a -> s { _ogqDescription = a })
{-# INLINE ogqDescription #-}

-- | Engine name that this option can be applied to.
ogqEngineName :: Lens' OptionGroupOption (Maybe Text)
ogqEngineName = lens _ogqEngineName (\s a -> s { _ogqEngineName = a })
{-# INLINE ogqEngineName #-}

-- | Indicates the major engine version that the option is available for.
ogqMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogqMajorEngineVersion = lens _ogqMajorEngineVersion (\s a -> s { _ogqMajorEngineVersion = a })
{-# INLINE ogqMajorEngineVersion #-}

-- | The minimum required engine version for the option to be applied.
ogqMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogqMinimumRequiredMinorEngineVersion = lens _ogqMinimumRequiredMinorEngineVersion (\s a -> s { _ogqMinimumRequiredMinorEngineVersion = a })
{-# INLINE ogqMinimumRequiredMinorEngineVersion #-}

-- | Specifies whether the option requires a port.
ogqPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogqPortRequired = lens _ogqPortRequired (\s a -> s { _ogqPortRequired = a })
{-# INLINE ogqPortRequired #-}

-- | If the option requires a port, specifies the default port for the option.
ogqDefaultPort :: Lens' OptionGroupOption (Maybe Integer)
ogqDefaultPort = lens _ogqDefaultPort (\s a -> s { _ogqDefaultPort = a })
{-# INLINE ogqDefaultPort #-}

-- | List of all options that are prerequisites for this option.
ogqOptionsDependedOn :: Lens' OptionGroupOption ([Text])
ogqOptionsDependedOn = lens _ogqOptionsDependedOn (\s a -> s { _ogqOptionsDependedOn = a })
{-# INLINE ogqOptionsDependedOn #-}

-- | A persistent option cannot be removed from the option group once the option
-- group is used, but this option can be removed from the db instance while
-- modifying the related data and assigning another option group without this
-- option.
ogqPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogqPersistent = lens _ogqPersistent (\s a -> s { _ogqPersistent = a })
{-# INLINE ogqPersistent #-}

-- | A permanent option cannot be removed from the option group once the option
-- group is used, and it cannot be removed from the db instance after
-- assigning an option group with this permanent option.
ogqPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogqPermanent = lens _ogqPermanent (\s a -> s { _ogqPermanent = a })
{-# INLINE ogqPermanent #-}

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogqOptionGroupOptionSettings :: Lens' OptionGroupOption ([OptionGroupOptionSetting])
ogqOptionGroupOptionSettings = lens _ogqOptionGroupOptionSettings (\s a -> s { _ogqOptionGroupOptionSettings = a })
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
ogosSettingName = lens _ogosSettingName (\s a -> s { _ogosSettingName = a })
{-# INLINE ogosSettingName #-}

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription = lens _ogosSettingDescription (\s a -> s { _ogosSettingDescription = a })
{-# INLINE ogosSettingDescription #-}

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\s a -> s { _ogosDefaultValue = a })
{-# INLINE ogosDefaultValue #-}

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\s a -> s { _ogosApplyType = a })
{-# INLINE ogosApplyType #-}

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues = lens _ogosAllowedValues (\s a -> s { _ogosAllowedValues = a })
{-# INLINE ogosAllowedValues #-}

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\s a -> s { _ogosIsModifiable = a })
{-# INLINE ogosIsModifiable #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupOptionSetting' data type to populate a request.
mkOptionGroupOptionSetting :: OptionGroupOptionSetting
mkOptionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosSettingName = Nothing
    , _ogosSettingDescription = Nothing
    , _ogosDefaultValue = Nothing
    , _ogosApplyType = Nothing
    , _ogosAllowedValues = Nothing
    , _ogosIsModifiable = Nothing
    }
{-# INLINE mkOptionGroupOptionSetting #-}

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
osName = lens _osName (\s a -> s { _osName = a })
{-# INLINE osName #-}

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\s a -> s { _osValue = a })
{-# INLINE osValue #-}

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\s a -> s { _osDefaultValue = a })
{-# INLINE osDefaultValue #-}

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\s a -> s { _osDescription = a })
{-# INLINE osDescription #-}

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\s a -> s { _osApplyType = a })
{-# INLINE osApplyType #-}

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\s a -> s { _osDataType = a })
{-# INLINE osDataType #-}

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\s a -> s { _osAllowedValues = a })
{-# INLINE osAllowedValues #-}

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\s a -> s { _osIsModifiable = a })
{-# INLINE osIsModifiable #-}

-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection = lens _osIsCollection (\s a -> s { _osIsCollection = a })
{-# INLINE osIsCollection #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionSetting' data type to populate a request.
mkOptionSetting :: OptionSetting
mkOptionSetting = OptionSetting
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
{-# INLINE mkOptionSetting #-}

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
odbioEngine = lens _odbioEngine (\s a -> s { _odbioEngine = a })
{-# INLINE odbioEngine #-}

-- | The engine version of the orderable DB instance.
odbioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngineVersion = lens _odbioEngineVersion (\s a -> s { _odbioEngineVersion = a })
{-# INLINE odbioEngineVersion #-}

-- | The DB instance Class for the orderable DB instance.
odbioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioDBInstanceClass = lens _odbioDBInstanceClass (\s a -> s { _odbioDBInstanceClass = a })
{-# INLINE odbioDBInstanceClass #-}

-- | The license model for the orderable DB instance.
odbioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioLicenseModel = lens _odbioLicenseModel (\s a -> s { _odbioLicenseModel = a })
{-# INLINE odbioLicenseModel #-}

-- | A list of availability zones for the orderable DB instance.
odbioAvailabilityZones :: Lens' OrderableDBInstanceOption ([AvailabilityZone])
odbioAvailabilityZones = lens _odbioAvailabilityZones (\s a -> s { _odbioAvailabilityZones = a })
{-# INLINE odbioAvailabilityZones #-}

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odbioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioMultiAZCapable = lens _odbioMultiAZCapable (\s a -> s { _odbioMultiAZCapable = a })
{-# INLINE odbioMultiAZCapable #-}

-- | Indicates whether this orderable DB instance can have a read replica.
odbioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioReadReplicaCapable = lens _odbioReadReplicaCapable (\s a -> s { _odbioReadReplicaCapable = a })
{-# INLINE odbioReadReplicaCapable #-}

-- | Indicates whether this is a VPC orderable DB instance.
odbioVpc :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioVpc = lens _odbioVpc (\s a -> s { _odbioVpc = a })
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
prParameterName = lens _prParameterName (\s a -> s { _prParameterName = a })
{-# INLINE prParameterName #-}

-- | Specifies the value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue = lens _prParameterValue (\s a -> s { _prParameterValue = a })
{-# INLINE prParameterValue #-}

-- | Provides a description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription = lens _prDescription (\s a -> s { _prDescription = a })
{-# INLINE prDescription #-}

-- | Indicates the source of the parameter value.
prSource :: Lens' Parameter (Maybe Text)
prSource = lens _prSource (\s a -> s { _prSource = a })
{-# INLINE prSource #-}

-- | Specifies the engine specific parameters type.
prApplyType :: Lens' Parameter (Maybe Text)
prApplyType = lens _prApplyType (\s a -> s { _prApplyType = a })
{-# INLINE prApplyType #-}

-- | Specifies the valid data type for the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType = lens _prDataType (\s a -> s { _prDataType = a })
{-# INLINE prDataType #-}

-- | Specifies the valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues = lens _prAllowedValues (\s a -> s { _prAllowedValues = a })
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable = lens _prIsModifiable (\s a -> s { _prIsModifiable = a })
{-# INLINE prIsModifiable #-}

-- | The earliest engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion = lens _prMinimumEngineVersion (\s a -> s { _prMinimumEngineVersion = a })
{-# INLINE prMinimumEngineVersion #-}

-- | Indicates when to apply parameter updates.
prApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
prApplyMethod = lens _prApplyMethod (\s a -> s { _prApplyMethod = a })
{-# INLINE prApplyMethod #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
mkParameter :: Parameter
mkParameter = Parameter
    { _prParameterName = Nothing
    , _prParameterValue = Nothing
    , _prDescription = Nothing
    , _prSource = Nothing
    , _prApplyType = Nothing
    , _prDataType = Nothing
    , _prAllowedValues = Nothing
    , _prIsModifiable = Nothing
    , _prMinimumEngineVersion = Nothing
    , _prApplyMethod = Nothing
    }
{-# INLINE mkParameter #-}

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
pmvDBInstanceClass = lens _pmvDBInstanceClass (\s a -> s { _pmvDBInstanceClass = a })
{-# INLINE pmvDBInstanceClass #-}

-- | Contains the new AllocatedStorage size for the DB instance that will be
-- applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Integer)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\s a -> s { _pmvAllocatedStorage = a })
{-# INLINE pmvAllocatedStorage #-}

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })
{-# INLINE pmvMasterUserPassword #-}

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Integer)
pmvPort = lens _pmvPort (\s a -> s { _pmvPort = a })
{-# INLINE pmvPort #-}

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Integer)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\s a -> s { _pmvBackupRetentionPeriod = a })
{-# INLINE pmvBackupRetentionPeriod #-}

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\s a -> s { _pmvMultiAZ = a })
{-# INLINE pmvMultiAZ #-}

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })
{-# INLINE pmvEngineVersion #-}

-- | Specifies the new Provisioned IOPS value for the DB instance that will be
-- applied or is being applied.
pmvIops :: Lens' PendingModifiedValues (Maybe Integer)
pmvIops = lens _pmvIops (\s a -> s { _pmvIops = a })
{-# INLINE pmvIops #-}

-- | Contains the new DBInstanceIdentifier for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\s a -> s { _pmvDBInstanceIdentifier = a })
{-# INLINE pmvDBInstanceIdentifier #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
mkPendingModifiedValues :: PendingModifiedValues
mkPendingModifiedValues = PendingModifiedValues
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
{-# INLINE mkPendingModifiedValues #-}

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
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\s a -> s { _rcRecurringChargeFrequency = a })
{-# INLINE rcRecurringChargeFrequency #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }
{-# INLINE mkRecurringCharge #-}

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
rdbiReservedDBInstanceId = lens _rdbiReservedDBInstanceId (\s a -> s { _rdbiReservedDBInstanceId = a })
{-# INLINE rdbiReservedDBInstanceId #-}

-- | The offering identifier.
rdbiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdbiReservedDBInstancesOfferingId = lens _rdbiReservedDBInstancesOfferingId (\s a -> s { _rdbiReservedDBInstancesOfferingId = a })
{-# INLINE rdbiReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdbiDBInstanceClass = lens _rdbiDBInstanceClass (\s a -> s { _rdbiDBInstanceClass = a })
{-# INLINE rdbiDBInstanceClass #-}

-- | The time the reservation started.
rdbiStartTime :: Lens' ReservedDBInstance (Maybe ISO8601)
rdbiStartTime = lens _rdbiStartTime (\s a -> s { _rdbiStartTime = a })
{-# INLINE rdbiStartTime #-}

-- | The duration of the reservation in seconds.
rdbiDuration :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDuration = lens _rdbiDuration (\s a -> s { _rdbiDuration = a })
{-# INLINE rdbiDuration #-}

-- | The fixed price charged for this reserved DB instance.
rdbiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiFixedPrice = lens _rdbiFixedPrice (\s a -> s { _rdbiFixedPrice = a })
{-# INLINE rdbiFixedPrice #-}

-- | The hourly price charged for this reserved DB instance.
rdbiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiUsagePrice = lens _rdbiUsagePrice (\s a -> s { _rdbiUsagePrice = a })
{-# INLINE rdbiUsagePrice #-}

-- | The currency code for the reserved DB instance.
rdbiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdbiCurrencyCode = lens _rdbiCurrencyCode (\s a -> s { _rdbiCurrencyCode = a })
{-# INLINE rdbiCurrencyCode #-}

-- | The number of reserved DB instances.
rdbiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Integer)
rdbiDBInstanceCount = lens _rdbiDBInstanceCount (\s a -> s { _rdbiDBInstanceCount = a })
{-# INLINE rdbiDBInstanceCount #-}

-- | The description of the reserved DB instance.
rdbiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdbiProductDescription = lens _rdbiProductDescription (\s a -> s { _rdbiProductDescription = a })
{-# INLINE rdbiProductDescription #-}

-- | The offering type of this reserved DB instance.
rdbiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdbiOfferingType = lens _rdbiOfferingType (\s a -> s { _rdbiOfferingType = a })
{-# INLINE rdbiOfferingType #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdbiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdbiMultiAZ = lens _rdbiMultiAZ (\s a -> s { _rdbiMultiAZ = a })
{-# INLINE rdbiMultiAZ #-}

-- | The state of the reserved DB instance.
rdbiState :: Lens' ReservedDBInstance (Maybe Text)
rdbiState = lens _rdbiState (\s a -> s { _rdbiState = a })
{-# INLINE rdbiState #-}

-- | The recurring price charged to run this reserved DB instance.
rdbiRecurringCharges :: Lens' ReservedDBInstance ([RecurringCharge])
rdbiRecurringCharges = lens _rdbiRecurringCharges (\s a -> s { _rdbiRecurringCharges = a })
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
rdbioReservedDBInstancesOfferingId = lens _rdbioReservedDBInstancesOfferingId (\s a -> s { _rdbioReservedDBInstancesOfferingId = a })
{-# INLINE rdbioReservedDBInstancesOfferingId #-}

-- | The DB instance class for the reserved DB instance.
rdbioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioDBInstanceClass = lens _rdbioDBInstanceClass (\s a -> s { _rdbioDBInstanceClass = a })
{-# INLINE rdbioDBInstanceClass #-}

-- | The duration of the offering in seconds.
rdbioDuration :: Lens' ReservedDBInstancesOffering (Maybe Integer)
rdbioDuration = lens _rdbioDuration (\s a -> s { _rdbioDuration = a })
{-# INLINE rdbioDuration #-}

-- | The fixed price charged for this offering.
rdbioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioFixedPrice = lens _rdbioFixedPrice (\s a -> s { _rdbioFixedPrice = a })
{-# INLINE rdbioFixedPrice #-}

-- | The hourly price charged for this offering.
rdbioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioUsagePrice = lens _rdbioUsagePrice (\s a -> s { _rdbioUsagePrice = a })
{-# INLINE rdbioUsagePrice #-}

-- | The currency code for the reserved DB instance offering.
rdbioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioCurrencyCode = lens _rdbioCurrencyCode (\s a -> s { _rdbioCurrencyCode = a })
{-# INLINE rdbioCurrencyCode #-}

-- | The database engine used by the offering.
rdbioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioProductDescription = lens _rdbioProductDescription (\s a -> s { _rdbioProductDescription = a })
{-# INLINE rdbioProductDescription #-}

-- | The offering type.
rdbioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioOfferingType = lens _rdbioOfferingType (\s a -> s { _rdbioOfferingType = a })
{-# INLINE rdbioOfferingType #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
rdbioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdbioMultiAZ = lens _rdbioMultiAZ (\s a -> s { _rdbioMultiAZ = a })
{-# INLINE rdbioMultiAZ #-}

-- | The recurring price charged to run this reserved DB instance.
rdbioRecurringCharges :: Lens' ReservedDBInstancesOffering ([RecurringCharge])
rdbioRecurringCharges = lens _rdbioRecurringCharges (\s a -> s { _rdbioRecurringCharges = a })
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
ssssstSubnetIdentifier = lens _ssssstSubnetIdentifier (\s a -> s { _ssssstSubnetIdentifier = a })
{-# INLINE ssssstSubnetIdentifier #-}

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
ssssstSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
ssssstSubnetAvailabilityZone = lens _ssssstSubnetAvailabilityZone (\s a -> s { _ssssstSubnetAvailabilityZone = a })
{-# INLINE ssssstSubnetAvailabilityZone #-}

-- | Specifies the status of the subnet.
ssssstSubnetStatus :: Lens' Subnet (Maybe Text)
ssssstSubnetStatus = lens _ssssstSubnetStatus (\s a -> s { _ssssstSubnetStatus = a })
{-# INLINE ssssstSubnetStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
mkSubnet :: Subnet
mkSubnet = Subnet
    { _ssssstSubnetIdentifier = Nothing
    , _ssssstSubnetAvailabilityZone = Nothing
    , _ssssstSubnetStatus = Nothing
    }
{-# INLINE mkSubnet #-}

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
tgKey = lens _tgKey (\s a -> s { _tgKey = a })
{-# INLINE tgKey #-}

-- | A value is the optional value of the tag. The string value can be from 1 to
-- 256 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tgValue :: Lens' Tag (Maybe Text)
tgValue = lens _tgValue (\s a -> s { _tgValue = a })
{-# INLINE tgValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
    { _tgKey = Nothing
    , _tgValue = Nothing
    }
{-# INLINE mkTag #-}

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
vsgmVpcSecurityGroupId = lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })
{-# INLINE vsgmVpcSecurityGroupId #-}

-- | The status of the VPC security group.
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })
{-# INLINE vsgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcSecurityGroupMembership' data type to populate a request.
mkVpcSecurityGroupMembership :: VpcSecurityGroupMembership
mkVpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus = Nothing
    }
{-# INLINE mkVpcSecurityGroupMembership #-}

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericQuery def
