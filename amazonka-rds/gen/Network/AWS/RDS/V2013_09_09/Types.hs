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
    , mkDBEngineVersion
    , dbevEngine
    , dbevEngineVersion
    , dbevDBParameterGroupFamily
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDefaultCharacterSet
    , dbevSupportedCharacterSets

    -- * DBInstance
    , DBInstance
    , mkDBInstance
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
    , mkDBParameterGroup
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
    , mkDBSecurityGroup
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
    , mkDBSnapshot
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
    , dbsgrDBSubnetGroupName
    , dbsgrDBSubnetGroupDescription
    , dbsgrVpcId
    , dbsgrSubnetGroupStatus
    , dbsgrSubnets

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , mkDescribeDBLogFilesDetails
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
    , mkEngineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event
    , mkEvent
    , erSourceIdentifier
    , erSourceType
    , erMessage
    , erEventCategories
    , erDate

    -- * EventCategoriesMap
    , EventCategoriesMap
    , mkEventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- * EventSubscription
    , EventSubscription
    , mkEventSubscription
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
    , oOptionName
    , oOptionDescription
    , oPersistent
    , oPermanent
    , oPort
    , oOptionSettings
    , oDBSecurityGroupMemberships
    , oVpcSecurityGroupMemberships

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
    , mkOptionGroup
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
    , mkOptionGroupOption
    , ogoName
    , ogoDescription
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoMinimumRequiredMinorEngineVersion
    , ogoPortRequired
    , ogoDefaultPort
    , ogoOptionsDependedOn
    , ogoPersistent
    , ogoPermanent
    , ogoOptionGroupOptionSettings

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
    , mkOrderableDBInstanceOption
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
    , mkReservedDBInstance
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
    , mkReservedDBInstancesOffering
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
    , sSubnetIdentifier
    , sSubnetAvailabilityZone
    , sSubnetStatus

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue

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
    , _azProvisionedIopsCapable :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CharacterSet' data type to populate a request.
mkCharacterSet :: CharacterSet
mkCharacterSet = CharacterSet
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
    , _dbevSupportedCharacterSets :: [CharacterSet]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBEngineVersion' data type to populate a request.
mkDBEngineVersion :: DBEngineVersion
mkDBEngineVersion = DBEngineVersion
    { _dbevEngine = Nothing
    , _dbevEngineVersion = Nothing
    , _dbevDBParameterGroupFamily = Nothing
    , _dbevDBEngineDescription = Nothing
    , _dbevDBEngineVersionDescription = Nothing
    , _dbevDefaultCharacterSet = Nothing
    , _dbevSupportedCharacterSets = mempty
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
dbevSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
dbevSupportedCharacterSets =
    lens _dbevSupportedCharacterSets
         (\s a -> s { _dbevSupportedCharacterSets = a })

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
    , _dbiEndpoint :: Maybe Endpoint
    , _dbiAllocatedStorage :: Maybe Integer
    , _dbiInstanceCreateTime :: Maybe ISO8601
    , _dbiPreferredBackupWindow :: Maybe Text
    , _dbiBackupRetentionPeriod :: Maybe Integer
    , _dbiDBSecurityGroups :: [DBSecurityGroupMembership]
    , _dbiVpcSecurityGroups :: [VpcSecurityGroupMembership]
    , _dbiDBParameterGroups :: [DBParameterGroupStatus]
    , _dbiAvailabilityZone :: Maybe Text
    , _dbiDBSubnetGroup :: Maybe DBSubnetGroup
    , _dbiPreferredMaintenanceWindow :: Maybe Text
    , _dbiPendingModifiedValues :: Maybe PendingModifiedValues
    , _dbiLatestRestorableTime :: Maybe ISO8601
    , _dbiMultiAZ :: Maybe Bool
    , _dbiEngineVersion :: Maybe Text
    , _dbiAutoMinorVersionUpgrade :: Maybe Bool
    , _dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
    , _dbiReadReplicaDBInstanceIdentifiers :: [Text]
    , _dbiLicenseModel :: Maybe Text
    , _dbiIops :: Maybe Integer
    , _dbiOptionGroupMemberships :: [OptionGroupMembership]
    , _dbiCharacterSetName :: Maybe Text
    , _dbiSecondaryAvailabilityZone :: Maybe Text
    , _dbiPubliclyAccessible :: Maybe Bool
    , _dbiStatusInfos :: [DBInstanceStatusInfo]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBInstance' data type to populate a request.
mkDBInstance :: DBInstance
mkDBInstance = DBInstance
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
    , _dbiDBSecurityGroups = mempty
    , _dbiVpcSecurityGroups = mempty
    , _dbiDBParameterGroups = mempty
    , _dbiAvailabilityZone = Nothing
    , _dbiDBSubnetGroup = Nothing
    , _dbiPreferredMaintenanceWindow = Nothing
    , _dbiPendingModifiedValues = Nothing
    , _dbiLatestRestorableTime = Nothing
    , _dbiMultiAZ = Nothing
    , _dbiEngineVersion = Nothing
    , _dbiAutoMinorVersionUpgrade = Nothing
    , _dbiReadReplicaSourceDBInstanceIdentifier = Nothing
    , _dbiReadReplicaDBInstanceIdentifiers = mempty
    , _dbiLicenseModel = Nothing
    , _dbiIops = Nothing
    , _dbiOptionGroupMemberships = mempty
    , _dbiCharacterSetName = Nothing
    , _dbiSecondaryAvailabilityZone = Nothing
    , _dbiPubliclyAccessible = Nothing
    , _dbiStatusInfos = mempty
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
dbiEndpoint :: Lens' DBInstance (Maybe Endpoint)
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
dbiDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
dbiDBSecurityGroups =
    lens _dbiDBSecurityGroups (\s a -> s { _dbiDBSecurityGroups = a })

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroups :: Lens' DBInstance [VpcSecurityGroupMembership]
dbiVpcSecurityGroups =
    lens _dbiVpcSecurityGroups (\s a -> s { _dbiVpcSecurityGroups = a })

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
dbiDBParameterGroups =
    lens _dbiDBParameterGroups (\s a -> s { _dbiDBParameterGroups = a })

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
dbiReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
dbiReadReplicaDBInstanceIdentifiers =
    lens _dbiReadReplicaDBInstanceIdentifiers
         (\s a -> s { _dbiReadReplicaDBInstanceIdentifiers = a })

-- | License model information for this DB instance.
dbiLicenseModel :: Lens' DBInstance (Maybe Text)
dbiLicenseModel = lens _dbiLicenseModel (\s a -> s { _dbiLicenseModel = a })

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops :: Lens' DBInstance (Maybe Integer)
dbiIops = lens _dbiIops (\s a -> s { _dbiIops = a })

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
dbiOptionGroupMemberships =
    lens _dbiOptionGroupMemberships
         (\s a -> s { _dbiOptionGroupMemberships = a })

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
dbiStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
dbiStatusInfos = lens _dbiStatusInfos (\s a -> s { _dbiStatusInfos = a })

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstance"

-- | Provides a list of status information for a DB instance.
data DBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType :: Maybe Text
    , _dbisiNormal :: Maybe Bool
    , _dbisiStatus :: Maybe Text
    , _dbisiMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBInstanceStatusInfo' data type to populate a request.
mkDBInstanceStatusInfo :: DBInstanceStatusInfo
mkDBInstanceStatusInfo = DBInstanceStatusInfo
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBParameterGroup' data type to populate a request.
mkDBParameterGroup :: DBParameterGroup
mkDBParameterGroup = DBParameterGroup
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBParameterGroupStatus' data type to populate a request.
mkDBParameterGroupStatus :: DBParameterGroupStatus
mkDBParameterGroupStatus = DBParameterGroupStatus
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
    , _dbsgEC2SecurityGroups :: [EC2SecurityGroup]
    , _dbsgIPRanges :: [IPRange]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSecurityGroup' data type to populate a request.
mkDBSecurityGroup :: DBSecurityGroup
mkDBSecurityGroup = DBSecurityGroup
    { _dbsgOwnerId = Nothing
    , _dbsgDBSecurityGroupName = Nothing
    , _dbsgDBSecurityGroupDescription = Nothing
    , _dbsgVpcId = Nothing
    , _dbsgEC2SecurityGroups = mempty
    , _dbsgIPRanges = mempty
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
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroups =
    lens _dbsgEC2SecurityGroups (\s a -> s { _dbsgEC2SecurityGroups = a })

-- | Contains a list of IPRange elements.
dbsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dbsgIPRanges = lens _dbsgIPRanges (\s a -> s { _dbsgIPRanges = a })

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

-- | This data type is used as a response element in the following actions:
-- ModifyDBInstance RebootDBInstance RestoreDBInstanceFromDBSnapshot
-- RestoreDBInstanceToPointInTime.
data DBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName :: Maybe Text
    , _dbsgmStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSecurityGroupMembership' data type to populate a request.
mkDBSecurityGroupMembership :: DBSecurityGroupMembership
mkDBSecurityGroupMembership = DBSecurityGroupMembership
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSnapshot' data type to populate a request.
mkDBSnapshot :: DBSnapshot
mkDBSnapshot = DBSnapshot
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

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
data DBSubnetGroup = DBSubnetGroup
    { _dbsgrDBSubnetGroupName :: Maybe Text
    , _dbsgrDBSubnetGroupDescription :: Maybe Text
    , _dbsgrVpcId :: Maybe Text
    , _dbsgrSubnetGroupStatus :: Maybe Text
    , _dbsgrSubnets :: [Subnet]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DBSubnetGroup' data type to populate a request.
mkDBSubnetGroup :: DBSubnetGroup
mkDBSubnetGroup = DBSubnetGroup
    { _dbsgrDBSubnetGroupName = Nothing
    , _dbsgrDBSubnetGroupDescription = Nothing
    , _dbsgrVpcId = Nothing
    , _dbsgrSubnetGroupStatus = Nothing
    , _dbsgrSubnets = mempty
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
dbsgrSubnets :: Lens' DBSubnetGroup [Subnet]
dbsgrSubnets = lens _dbsgrSubnets (\s a -> s { _dbsgrSubnets = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DescribeDBLogFilesDetails' data type to populate a request.
mkDescribeDBLogFilesDetails :: DescribeDBLogFilesDetails
mkDescribeDBLogFilesDetails = DescribeDBLogFilesDetails
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
mkEC2SecurityGroup :: EC2SecurityGroup
mkEC2SecurityGroup = EC2SecurityGroup
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
data Endpoint = Endpoint
    { _eAddress :: Maybe Text
    , _ePort :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type to populate a request.
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _eAddress = Nothing
    , _ePort = Nothing
    }

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
data EngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily :: Maybe Text
    , _edMarker :: Maybe Text
    , _edParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EngineDefaults' data type to populate a request.
mkEngineDefaults :: EngineDefaults
mkEngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameters = mempty
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
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\s a -> s { _edParameters = a })

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | This data type is used as a response element in the DescribeEvents action.
data Event = Event
    { _erSourceIdentifier :: Maybe Text
    , _erSourceType :: Maybe SourceType
    , _erMessage :: Maybe Text
    , _erEventCategories :: [Text]
    , _erDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Event' data type to populate a request.
mkEvent :: Event
mkEvent = Event
    { _erSourceIdentifier = Nothing
    , _erSourceType = Nothing
    , _erMessage = Nothing
    , _erEventCategories = mempty
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
erEventCategories :: Lens' Event [Text]
erEventCategories =
    lens _erEventCategories (\s a -> s { _erEventCategories = a })

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
    , _ecmEventCategories :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventCategoriesMap' data type to populate a request.
mkEventCategoriesMap :: EventCategoriesMap
mkEventCategoriesMap = EventCategoriesMap
    { _ecmSourceType = Nothing
    , _ecmEventCategories = mempty
    }

-- | The source type that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s { _ecmSourceType = a })

-- | The event categories for the specified source type.
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories =
    lens _ecmEventCategories (\s a -> s { _ecmEventCategories = a })

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
    , _esSourceIdsList :: [Text]
    , _esEventCategoriesList :: [Text]
    , _esEnabled :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventSubscription' data type to populate a request.
mkEventSubscription :: EventSubscription
mkEventSubscription = EventSubscription
    { _esCustomerAwsId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSnsTopicArn = Nothing
    , _esStatus = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esSourceType = Nothing
    , _esSourceIdsList = mempty
    , _esEventCategoriesList = mempty
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
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a })

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList =
    lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IPRange' data type to populate a request.
mkIPRange :: IPRange
mkIPRange = IPRange
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
    , _oOptionSettings :: [OptionSetting]
    , _oDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
    , _oVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Option' data type to populate a request.
mkOption :: Option
mkOption = Option
    { _oOptionName = Nothing
    , _oOptionDescription = Nothing
    , _oPersistent = Nothing
    , _oPermanent = Nothing
    , _oPort = Nothing
    , _oOptionSettings = mempty
    , _oDBSecurityGroupMemberships = mempty
    , _oVpcSecurityGroupMemberships = mempty
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
oOptionSettings :: Lens' Option [OptionSetting]
oOptionSettings = lens _oOptionSettings (\s a -> s { _oOptionSettings = a })

-- | If the option requires access to a port, then this DB security group allows
-- access to the port.
oDBSecurityGroupMemberships :: Lens' Option [DBSecurityGroupMembership]
oDBSecurityGroupMemberships =
    lens _oDBSecurityGroupMemberships
         (\s a -> s { _oDBSecurityGroupMemberships = a })

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
oVpcSecurityGroupMemberships :: Lens' Option [VpcSecurityGroupMembership]
oVpcSecurityGroupMemberships =
    lens _oVpcSecurityGroupMemberships
         (\s a -> s { _oVpcSecurityGroupMemberships = a })

instance FromXML Option where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Option"

instance ToQuery Option where
    toQuery = genericQuery def

-- | A list of all available options.
data OptionConfiguration = OptionConfiguration
    { _ocOptionName :: Text
    , _ocPort :: Maybe Integer
    , _ocDBSecurityGroupMemberships :: [Text]
    , _ocVpcSecurityGroupMemberships :: [Text]
    , _ocOptionSettings :: [OptionSetting]
    } deriving (Show, Generic)

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

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\s a -> s { _ocOptionName = a })

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Integer)
ocPort = lens _ocPort (\s a -> s { _ocPort = a })

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships =
    lens _ocDBSecurityGroupMemberships
         (\s a -> s { _ocDBSecurityGroupMemberships = a })

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVpcSecurityGroupMemberships =
    lens _ocVpcSecurityGroupMemberships
         (\s a -> s { _ocVpcSecurityGroupMemberships = a })

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings =
    lens _ocOptionSettings (\s a -> s { _ocOptionSettings = a })

instance ToQuery OptionConfiguration where
    toQuery = genericQuery def

-- | 
data OptionGroup = OptionGroup
    { _ogOptionGroupName :: Maybe Text
    , _ogOptionGroupDescription :: Maybe Text
    , _ogEngineName :: Maybe Text
    , _ogMajorEngineVersion :: Maybe Text
    , _ogOptions :: [Option]
    , _ogAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
    , _ogVpcId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroup' data type to populate a request.
mkOptionGroup :: OptionGroup
mkOptionGroup = OptionGroup
    { _ogOptionGroupName = Nothing
    , _ogOptionGroupDescription = Nothing
    , _ogEngineName = Nothing
    , _ogMajorEngineVersion = Nothing
    , _ogOptions = mempty
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
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\s a -> s { _ogOptions = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupMembership' data type to populate a request.
mkOptionGroupMembership :: OptionGroupMembership
mkOptionGroupMembership = OptionGroupMembership
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
    , _ogoOptionsDependedOn :: [Text]
    , _ogoPersistent :: Maybe Bool
    , _ogoPermanent :: Maybe Bool
    , _ogoOptionGroupOptionSettings :: [OptionGroupOptionSetting]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionGroupOption' data type to populate a request.
mkOptionGroupOption :: OptionGroupOption
mkOptionGroupOption = OptionGroupOption
    { _ogoName = Nothing
    , _ogoDescription = Nothing
    , _ogoEngineName = Nothing
    , _ogoMajorEngineVersion = Nothing
    , _ogoMinimumRequiredMinorEngineVersion = Nothing
    , _ogoPortRequired = Nothing
    , _ogoDefaultPort = Nothing
    , _ogoOptionsDependedOn = mempty
    , _ogoPersistent = Nothing
    , _ogoPermanent = Nothing
    , _ogoOptionGroupOptionSettings = mempty
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
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn =
    lens _ogoOptionsDependedOn (\s a -> s { _ogoOptionsDependedOn = a })

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
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings =
    lens _ogoOptionGroupOptionSettings
         (\s a -> s { _ogoOptionGroupOptionSettings = a })

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
    } deriving (Show, Generic)

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
    } deriving (Show, Generic)

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
    , _odbioAvailabilityZones :: [AvailabilityZone]
    , _odbioMultiAZCapable :: Maybe Bool
    , _odbioReadReplicaCapable :: Maybe Bool
    , _odbioVpc :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OrderableDBInstanceOption' data type to populate a request.
mkOrderableDBInstanceOption :: OrderableDBInstanceOption
mkOrderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioEngine = Nothing
    , _odbioEngineVersion = Nothing
    , _odbioDBInstanceClass = Nothing
    , _odbioLicenseModel = Nothing
    , _odbioAvailabilityZones = mempty
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
odbioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odbioAvailabilityZones =
    lens _odbioAvailabilityZones (\s a -> s { _odbioAvailabilityZones = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type to populate a request.
mkParameter :: Parameter
mkParameter = Parameter
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
    } deriving (Show, Generic)

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
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
    , _rdbiRecurringCharges :: [RecurringCharge]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedDBInstance' data type to populate a request.
mkReservedDBInstance :: ReservedDBInstance
mkReservedDBInstance = ReservedDBInstance
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
    , _rdbiRecurringCharges = mempty
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
rdbiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdbiRecurringCharges =
    lens _rdbiRecurringCharges (\s a -> s { _rdbiRecurringCharges = a })

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
    , _rdbioRecurringCharges :: [RecurringCharge]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedDBInstancesOffering' data type to populate a request.
mkReservedDBInstancesOffering :: ReservedDBInstancesOffering
mkReservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioReservedDBInstancesOfferingId = Nothing
    , _rdbioDBInstanceClass = Nothing
    , _rdbioDuration = Nothing
    , _rdbioFixedPrice = Nothing
    , _rdbioUsagePrice = Nothing
    , _rdbioCurrencyCode = Nothing
    , _rdbioProductDescription = Nothing
    , _rdbioOfferingType = Nothing
    , _rdbioMultiAZ = Nothing
    , _rdbioRecurringCharges = mempty
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
rdbioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdbioRecurringCharges =
    lens _rdbioRecurringCharges (\s a -> s { _rdbioRecurringCharges = a })

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOffering"

-- | This data type is used as a response element in the DescribeDBSubnetGroups
-- action.
data Subnet = Subnet
    { _sSubnetIdentifier :: Maybe Text
    , _sSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _sSubnetStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
mkSubnet :: Subnet
mkSubnet = Subnet
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VpcSecurityGroupMembership' data type to populate a request.
mkVpcSecurityGroupMembership :: VpcSecurityGroupMembership
mkVpcSecurityGroupMembership = VpcSecurityGroupMembership
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
