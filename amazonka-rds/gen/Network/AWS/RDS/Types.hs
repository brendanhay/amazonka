{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.RDS.Types
    (
    -- * Service
      RDS
    -- ** Errors
    , RDSError (..)
    , _RDSHttp
    , _RDSSerializer
    , _RDSService
    -- ** XML
    , xmlOptions

    -- * OptionGroup
    , OptionGroup
    , optionGroup
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptionGroupDescription
    , ogOptionGroupName
    , ogOptions
    , ogVpcId

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus
    , dbparameterGroupStatus
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- * Event
    , Event
    , event
    , eDate
    , eEventCategories
    , eMessage
    , eSourceIdentifier
    , eSourceType

    -- * DBSecurityGroup
    , DBSecurityGroup
    , dbsecurityGroup
    , dbsgDBSecurityGroupDescription
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgIPRanges
    , dbsgOwnerId
    , dbsgVpcId

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * DBEngineVersion
    , DBEngineVersion
    , dbengineVersion
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDBParameterGroupFamily
    , dbevDefaultCharacterSet
    , dbevEngine
    , dbevEngineVersion
    , dbevSupportedCharacterSets

    -- * DBSnapshot
    , DBSnapshot
    , dbsnapshot
    , dbsAllocatedStorage
    , dbsAvailabilityZone
    , dbsDBInstanceIdentifier
    , dbsDBSnapshotIdentifier
    , dbsEngine
    , dbsEngineVersion
    , dbsInstanceCreateTime
    , dbsIops
    , dbsLicenseModel
    , dbsMasterUsername
    , dbsOptionGroupName
    , dbsPercentProgress
    , dbsPort
    , dbsSnapshotCreateTime
    , dbsSnapshotType
    , dbsSourceRegion
    , dbsStatus
    , dbsStorageType
    , dbsTdeCredentialArn
    , dbsVpcId

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dbsecurityGroupMembership
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus

    -- * SourceType
    , SourceType (..)

    -- * DBParameterGroup
    , DBParameterGroup
    , dbparameterGroup
    , dbpgDBParameterGroupFamily
    , dbpgDBParameterGroupName
    , dbpgDescription

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdbioCurrencyCode
    , rdbioDBInstanceClass
    , rdbioDuration
    , rdbioFixedPrice
    , rdbioMultiAZ
    , rdbioOfferingType
    , rdbioProductDescription
    , rdbioRecurringCharges
    , rdbioReservedDBInstancesOfferingId
    , rdbioUsagePrice

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetDescription
    , csCharacterSetName

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetStatus

    -- * ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdbiCurrencyCode
    , rdbiDBInstanceClass
    , rdbiDBInstanceCount
    , rdbiDuration
    , rdbiFixedPrice
    , rdbiMultiAZ
    , rdbiOfferingType
    , rdbiProductDescription
    , rdbiRecurringCharges
    , rdbiReservedDBInstanceId
    , rdbiReservedDBInstancesOfferingId
    , rdbiStartTime
    , rdbiState
    , rdbiUsagePrice

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * DBParameterGroupNameMessage
    , DBParameterGroupNameMessage
    , dbparameterGroupNameMessage
    , dbpgnmDBParameterGroupName

    -- * OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoDefaultPort
    , ogoDescription
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoMinimumRequiredMinorEngineVersion
    , ogoName
    , ogoOptionGroupOptionSettings
    , ogoOptionsDependedOn
    , ogoPermanent
    , ogoPersistent
    , ogoPortRequired

    -- * DBInstance
    , DBInstance
    , dbinstance
    , dbiAllocatedStorage
    , dbiAutoMinorVersionUpgrade
    , dbiAvailabilityZone
    , dbiBackupRetentionPeriod
    , dbiCharacterSetName
    , dbiDBInstanceClass
    , dbiDBInstanceIdentifier
    , dbiDBInstanceStatus
    , dbiDBName
    , dbiDBParameterGroups
    , dbiDBSecurityGroups
    , dbiDBSubnetGroup
    , dbiEndpoint
    , dbiEngine
    , dbiEngineVersion
    , dbiInstanceCreateTime
    , dbiIops
    , dbiLatestRestorableTime
    , dbiLicenseModel
    , dbiMasterUsername
    , dbiMultiAZ
    , dbiOptionGroupMemberships
    , dbiPendingModifiedValues
    , dbiPreferredBackupWindow
    , dbiPreferredMaintenanceWindow
    , dbiPubliclyAccessible
    , dbiReadReplicaDBInstanceIdentifiers
    , dbiReadReplicaSourceDBInstanceIdentifier
    , dbiSecondaryAvailabilityZone
    , dbiStatusInfos
    , dbiStorageType
    , dbiTdeCredentialArn
    , dbiVpcSecurityGroups

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esCustSubscriptionId
    , esCustomerAwsId
    , esEnabled
    , esEventCategoriesList
    , esSnsTopicArn
    , esSourceIdsList
    , esSourceType
    , esStatus
    , esSubscriptionCreationTime

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dbsubnetGroup
    , dbsg1DBSubnetGroupDescription
    , dbsg1DBSubnetGroupName
    , dbsg1SubnetGroupStatus
    , dbsg1Subnets
    , dbsg1VpcId

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbinstanceStatusInfo
    , dbisiMessage
    , dbisiNormal
    , dbisiStatus
    , dbisiStatusType

    -- * OptionSetting
    , OptionSetting
    , optionSetting
    , osAllowedValues
    , osApplyType
    , osDataType
    , osDefaultValue
    , osDescription
    , osIsCollection
    , osIsModifiable
    , osName
    , osValue

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddblfdLastWritten
    , ddblfdLogFileName
    , ddblfdSize

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odbioAvailabilityZones
    , odbioDBInstanceClass
    , odbioEngine
    , odbioEngineVersion
    , odbioLicenseModel
    , odbioMultiAZCapable
    , odbioReadReplicaCapable
    , odbioStorageType
    , odbioSupportsIops
    , odbioVpc

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocDBSecurityGroupMemberships
    , ocOptionName
    , ocOptionSettings
    , ocPort
    , ocVpcSecurityGroupMemberships

    -- * Option
    , Option
    , option
    , oDBSecurityGroupMemberships
    , oOptionDescription
    , oOptionName
    , oOptionSettings
    , oPermanent
    , oPersistent
    , oPort
    , oVpcSecurityGroupMemberships

    -- * IPRange
    , IPRange
    , iprange
    , iprCIDRIP
    , iprStatus

    -- * OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmOptionGroupName
    , ogmStatus

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmEventCategories
    , ecmSourceType

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvAllocatedStorage
    , pmvBackupRetentionPeriod
    , pmvDBInstanceClass
    , pmvDBInstanceIdentifier
    , pmvEngineVersion
    , pmvIops
    , pmvMasterUserPassword
    , pmvMultiAZ
    , pmvPort
    , pmvStorageType

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- * Parameter
    , Parameter
    , parameter
    , pAllowedValues
    , pApplyMethod
    , pApplyType
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosAllowedValues
    , ogosApplyType
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosSettingDescription
    , ogosSettingName
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-09-01@) of the Amazon Relational Database Service.
data RDS deriving (Typeable)

instance AWSService RDS where
    type Sg RDS = V4
    type Er RDS = RDSError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "rds"
        , _svcVersion  = "2014-09-01"
        , _svcTarget   = Nothing
        }

    handle = xmlError alwaysFail

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data OptionGroup = OptionGroup
    { _ogAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
    , _ogEngineName                            :: Maybe Text
    , _ogMajorEngineVersion                    :: Maybe Text
    , _ogOptionGroupDescription                :: Maybe Text
    , _ogOptionGroupName                       :: Maybe Text
    , _ogOptions                               :: [Option]
    , _ogVpcId                                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'OptionGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogAllowsVpcAndNonVpcInstanceMemberships' @::@ 'Maybe' 'Bool'
--
-- * 'ogEngineName' @::@ 'Maybe' 'Text'
--
-- * 'ogMajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ogOptionGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'ogOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ogOptions' @::@ ['Option']
--
-- * 'ogVpcId' @::@ 'Maybe' 'Text'
--
optionGroup :: OptionGroup
optionGroup = OptionGroup
    { _ogOptionGroupName                       = Nothing
    , _ogOptionGroupDescription                = Nothing
    , _ogEngineName                            = Nothing
    , _ogMajorEngineVersion                    = Nothing
    , _ogOptions                               = mempty
    , _ogAllowsVpcAndNonVpcInstanceMemberships = Nothing
    , _ogVpcId                                 = Nothing
    }

-- | Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value 'true' indicates the option group can be
-- applied to both VPC and non-VPC instances.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVpcAndNonVpcInstanceMemberships =
    lens _ogAllowsVpcAndNonVpcInstanceMemberships
        (\s a -> s { _ogAllowsVpcAndNonVpcInstanceMemberships = a })

-- | Engine name that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\s a -> s { _ogEngineName = a })

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion =
    lens _ogMajorEngineVersion (\s a -> s { _ogMajorEngineVersion = a })

-- | Provides the description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription =
    lens _ogOptionGroupDescription
        (\s a -> s { _ogOptionGroupDescription = a })

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName =
    lens _ogOptionGroupName (\s a -> s { _ogOptionGroupName = a })

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\s a -> s { _ogOptions = a })

-- | If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
-- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is
-- blank, then this option group can be applied to both VPC and non-VPC
-- instances. If this field contains a value, then this option group can
-- only be applied to instances that are in the VPC indicated by this field.
ogVpcId :: Lens' OptionGroup (Maybe Text)
ogVpcId = lens _ogVpcId (\s a -> s { _ogVpcId = a })

instance FromXML OptionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroup"

instance ToQuery OptionGroup

data DBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName :: Maybe Text
    , _dbpgsParameterApplyStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBParameterGroupStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpgsDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbpgsParameterApplyStatus' @::@ 'Maybe' 'Text'
--
dbparameterGroupStatus :: DBParameterGroupStatus
dbparameterGroupStatus = DBParameterGroupStatus
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
    fromXMLRoot    = fromRoot "DBParameterGroupStatus"

instance ToQuery DBParameterGroupStatus

data Event = Event
    { _eDate             :: Maybe RFC822
    , _eEventCategories  :: [Text]
    , _eMessage          :: Maybe Text
    , _eSourceIdentifier :: Maybe Text
    , _eSourceType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Event' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'eEventCategories' @::@ ['Text']
--
-- * 'eMessage' @::@ 'Maybe' 'Text'
--
-- * 'eSourceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'eSourceType' @::@ 'Maybe' 'Text'
--
event :: Event
event = Event
    { _eSourceIdentifier = Nothing
    , _eSourceType       = Nothing
    , _eMessage          = Nothing
    , _eEventCategories  = mempty
    , _eDate             = Nothing
    }

-- | Specifies the date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\s a -> s { _eDate = a })
    . mapping _Time

-- | Specifies the category for the event.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\s a -> s { _eEventCategories = a })

-- | Provides the text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s { _eMessage = a })

-- | Provides the identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier =
    lens _eSourceIdentifier (\s a -> s { _eSourceIdentifier = a })

-- | Specifies the source type for this event.
eSourceType :: Lens' Event (Maybe Text)
eSourceType = lens _eSourceType (\s a -> s { _eSourceType = a })

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

instance ToQuery Event

data DBSecurityGroup = DBSecurityGroup
    { _dbsgDBSecurityGroupDescription :: Maybe Text
    , _dbsgDBSecurityGroupName        :: Maybe Text
    , _dbsgEC2SecurityGroups          :: [EC2SecurityGroup]
    , _dbsgIPRanges                   :: [IPRange]
    , _dbsgOwnerId                    :: Maybe Text
    , _dbsgVpcId                      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DBSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsgDBSecurityGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'dbsgDBSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbsgEC2SecurityGroups' @::@ ['EC2SecurityGroup']
--
-- * 'dbsgIPRanges' @::@ ['IPRange']
--
-- * 'dbsgOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'dbsgVpcId' @::@ 'Maybe' 'Text'
--
dbsecurityGroup :: DBSecurityGroup
dbsecurityGroup = DBSecurityGroup
    { _dbsgOwnerId                    = Nothing
    , _dbsgDBSecurityGroupName        = Nothing
    , _dbsgDBSecurityGroupDescription = Nothing
    , _dbsgVpcId                      = Nothing
    , _dbsgEC2SecurityGroups          = mempty
    , _dbsgIPRanges                   = mempty
    }

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription =
    lens _dbsgDBSecurityGroupDescription
        (\s a -> s { _dbsgDBSecurityGroupDescription = a })

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName =
    lens _dbsgDBSecurityGroupName (\s a -> s { _dbsgDBSecurityGroupName = a })

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroups =
    lens _dbsgEC2SecurityGroups (\s a -> s { _dbsgEC2SecurityGroups = a })

-- | Contains a list of IPRange elements.
dbsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dbsgIPRanges = lens _dbsgIPRanges (\s a -> s { _dbsgIPRanges = a })

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId = lens _dbsgOwnerId (\s a -> s { _dbsgOwnerId = a })

-- | Provides the VpcId of the DB security group.
dbsgVpcId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVpcId = lens _dbsgVpcId (\s a -> s { _dbsgVpcId = a })

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

instance ToQuery DBSecurityGroup

data Tag = Tag
    { _tagKey   :: Maybe Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Maybe' 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Tag
tag = Tag
    { _tagKey   = Nothing
    , _tagValue = Nothing
    }

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and cannot be prefixed with "aws:" or
-- "rds:". The string may only contain only the set of Unicode letters,
-- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
-- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag

data DBEngineVersion = DBEngineVersion
    { _dbevDBEngineDescription        :: Maybe Text
    , _dbevDBEngineVersionDescription :: Maybe Text
    , _dbevDBParameterGroupFamily     :: Maybe Text
    , _dbevDefaultCharacterSet        :: Maybe CharacterSet
    , _dbevEngine                     :: Maybe Text
    , _dbevEngineVersion              :: Maybe Text
    , _dbevSupportedCharacterSets     :: [CharacterSet]
    } deriving (Eq, Show, Generic)

-- | 'DBEngineVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbevDBEngineDescription' @::@ 'Maybe' 'Text'
--
-- * 'dbevDBEngineVersionDescription' @::@ 'Maybe' 'Text'
--
-- * 'dbevDBParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dbevDefaultCharacterSet' @::@ 'Maybe' 'CharacterSet'
--
-- * 'dbevEngine' @::@ 'Maybe' 'Text'
--
-- * 'dbevEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dbevSupportedCharacterSets' @::@ ['CharacterSet']
--
dbengineVersion :: DBEngineVersion
dbengineVersion = DBEngineVersion
    { _dbevEngine                     = Nothing
    , _dbevEngineVersion              = Nothing
    , _dbevDBParameterGroupFamily     = Nothing
    , _dbevDBEngineDescription        = Nothing
    , _dbevDBEngineVersionDescription = Nothing
    , _dbevDefaultCharacterSet        = Nothing
    , _dbevSupportedCharacterSets     = mempty
    }

-- | The description of the database engine.
dbevDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineDescription =
    lens _dbevDBEngineDescription (\s a -> s { _dbevDBEngineDescription = a })

-- | The description of the database engine version.
dbevDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
dbevDBEngineVersionDescription =
    lens _dbevDBEngineVersionDescription
        (\s a -> s { _dbevDBEngineVersionDescription = a })

-- | The name of the DB parameter group family for the database engine.
dbevDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
dbevDBParameterGroupFamily =
    lens _dbevDBParameterGroupFamily
        (\s a -> s { _dbevDBParameterGroupFamily = a })

-- | The default character set for new instances of this engine version, if
-- the CharacterSetName parameter of the CreateDBInstance API is not
-- specified.
dbevDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
dbevDefaultCharacterSet =
    lens _dbevDefaultCharacterSet (\s a -> s { _dbevDefaultCharacterSet = a })

-- | The name of the database engine.
dbevEngine :: Lens' DBEngineVersion (Maybe Text)
dbevEngine = lens _dbevEngine (\s a -> s { _dbevEngine = a })

-- | The version number of the database engine.
dbevEngineVersion :: Lens' DBEngineVersion (Maybe Text)
dbevEngineVersion =
    lens _dbevEngineVersion (\s a -> s { _dbevEngineVersion = a })

-- | A list of the character sets supported by this engine for the
-- CharacterSetName parameter of the CreateDBInstance API.
dbevSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
dbevSupportedCharacterSets =
    lens _dbevSupportedCharacterSets
        (\s a -> s { _dbevSupportedCharacterSets = a })

instance FromXML DBEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBEngineVersion"

instance ToQuery DBEngineVersion

data DBSnapshot = DBSnapshot
    { _dbsAllocatedStorage     :: Maybe Int
    , _dbsAvailabilityZone     :: Maybe Text
    , _dbsDBInstanceIdentifier :: Maybe Text
    , _dbsDBSnapshotIdentifier :: Maybe Text
    , _dbsEngine               :: Maybe Text
    , _dbsEngineVersion        :: Maybe Text
    , _dbsInstanceCreateTime   :: Maybe RFC822
    , _dbsIops                 :: Maybe Int
    , _dbsLicenseModel         :: Maybe Text
    , _dbsMasterUsername       :: Maybe Text
    , _dbsOptionGroupName      :: Maybe Text
    , _dbsPercentProgress      :: Maybe Int
    , _dbsPort                 :: Maybe Int
    , _dbsSnapshotCreateTime   :: Maybe RFC822
    , _dbsSnapshotType         :: Maybe Text
    , _dbsSourceRegion         :: Maybe Text
    , _dbsStatus               :: Maybe Text
    , _dbsStorageType          :: Maybe Text
    , _dbsTdeCredentialArn     :: Maybe Text
    , _dbsVpcId                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsAllocatedStorage' @::@ 'Maybe' 'Int'
--
-- * 'dbsAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'dbsDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dbsDBSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dbsEngine' @::@ 'Maybe' 'Text'
--
-- * 'dbsEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dbsInstanceCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dbsIops' @::@ 'Maybe' 'Int'
--
-- * 'dbsLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'dbsMasterUsername' @::@ 'Maybe' 'Text'
--
-- * 'dbsOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbsPercentProgress' @::@ 'Maybe' 'Int'
--
-- * 'dbsPort' @::@ 'Maybe' 'Int'
--
-- * 'dbsSnapshotCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dbsSnapshotType' @::@ 'Maybe' 'Text'
--
-- * 'dbsSourceRegion' @::@ 'Maybe' 'Text'
--
-- * 'dbsStatus' @::@ 'Maybe' 'Text'
--
-- * 'dbsStorageType' @::@ 'Maybe' 'Text'
--
-- * 'dbsTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'dbsVpcId' @::@ 'Maybe' 'Text'
--
dbsnapshot :: DBSnapshot
dbsnapshot = DBSnapshot
    { _dbsDBSnapshotIdentifier = Nothing
    , _dbsDBInstanceIdentifier = Nothing
    , _dbsSnapshotCreateTime   = Nothing
    , _dbsEngine               = Nothing
    , _dbsAllocatedStorage     = Nothing
    , _dbsStatus               = Nothing
    , _dbsPort                 = Nothing
    , _dbsAvailabilityZone     = Nothing
    , _dbsVpcId                = Nothing
    , _dbsInstanceCreateTime   = Nothing
    , _dbsMasterUsername       = Nothing
    , _dbsEngineVersion        = Nothing
    , _dbsLicenseModel         = Nothing
    , _dbsSnapshotType         = Nothing
    , _dbsIops                 = Nothing
    , _dbsOptionGroupName      = Nothing
    , _dbsPercentProgress      = Nothing
    , _dbsSourceRegion         = Nothing
    , _dbsStorageType          = Nothing
    , _dbsTdeCredentialArn     = Nothing
    }

-- | Specifies the allocated storage size in gigabytes (GB).
dbsAllocatedStorage :: Lens' DBSnapshot (Maybe Int)
dbsAllocatedStorage =
    lens _dbsAllocatedStorage (\s a -> s { _dbsAllocatedStorage = a })

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dbsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dbsAvailabilityZone =
    lens _dbsAvailabilityZone (\s a -> s { _dbsAvailabilityZone = a })

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dbsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBInstanceIdentifier =
    lens _dbsDBInstanceIdentifier (\s a -> s { _dbsDBInstanceIdentifier = a })

-- | Specifies the identifier for the DB snapshot.
dbsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dbsDBSnapshotIdentifier =
    lens _dbsDBSnapshotIdentifier (\s a -> s { _dbsDBSnapshotIdentifier = a })

-- | Specifies the name of the database engine.
dbsEngine :: Lens' DBSnapshot (Maybe Text)
dbsEngine = lens _dbsEngine (\s a -> s { _dbsEngine = a })

-- | Specifies the version of the database engine.
dbsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dbsEngineVersion = lens _dbsEngineVersion (\s a -> s { _dbsEngineVersion = a })

-- | Specifies the time (UTC) when the snapshot was taken.
dbsInstanceCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dbsInstanceCreateTime =
    lens _dbsInstanceCreateTime (\s a -> s { _dbsInstanceCreateTime = a })
        . mapping _Time

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the
-- DB instance at the time of the snapshot.
dbsIops :: Lens' DBSnapshot (Maybe Int)
dbsIops = lens _dbsIops (\s a -> s { _dbsIops = a })

-- | License model information for the restored DB instance.
dbsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dbsLicenseModel = lens _dbsLicenseModel (\s a -> s { _dbsLicenseModel = a })

-- | Provides the master username for the DB snapshot.
dbsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dbsMasterUsername =
    lens _dbsMasterUsername (\s a -> s { _dbsMasterUsername = a })

-- | Provides the option group name for the DB snapshot.
dbsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dbsOptionGroupName =
    lens _dbsOptionGroupName (\s a -> s { _dbsOptionGroupName = a })

-- | The percentage of the estimated data that has been transferred.
dbsPercentProgress :: Lens' DBSnapshot (Maybe Int)
dbsPercentProgress =
    lens _dbsPercentProgress (\s a -> s { _dbsPercentProgress = a })

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dbsPort :: Lens' DBSnapshot (Maybe Int)
dbsPort = lens _dbsPort (\s a -> s { _dbsPort = a })

-- | Provides the time (UTC) when the snapshot was taken.
dbsSnapshotCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dbsSnapshotCreateTime =
    lens _dbsSnapshotCreateTime (\s a -> s { _dbsSnapshotCreateTime = a })
        . mapping _Time

-- | Provides the type of the DB snapshot.
dbsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dbsSnapshotType = lens _dbsSnapshotType (\s a -> s { _dbsSnapshotType = a })

-- | The region that the DB snapshot was created in or copied from.
dbsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dbsSourceRegion = lens _dbsSourceRegion (\s a -> s { _dbsSourceRegion = a })

-- | Specifies the status of this DB snapshot.
dbsStatus :: Lens' DBSnapshot (Maybe Text)
dbsStatus = lens _dbsStatus (\s a -> s { _dbsStatus = a })

-- | Specifies storage type associated with DB Snapshot.
dbsStorageType :: Lens' DBSnapshot (Maybe Text)
dbsStorageType = lens _dbsStorageType (\s a -> s { _dbsStorageType = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
dbsTdeCredentialArn :: Lens' DBSnapshot (Maybe Text)
dbsTdeCredentialArn =
    lens _dbsTdeCredentialArn (\s a -> s { _dbsTdeCredentialArn = a })

-- | Provides the Vpc Id associated with the DB snapshot.
dbsVpcId :: Lens' DBSnapshot (Maybe Text)
dbsVpcId = lens _dbsVpcId (\s a -> s { _dbsVpcId = a })

instance FromXML DBSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSnapshot"

instance ToQuery DBSnapshot

data DBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName :: Maybe Text
    , _dbsgmStatus              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBSecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsgmDBSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbsgmStatus' @::@ 'Maybe' 'Text'
--
dbsecurityGroupMembership :: DBSecurityGroupMembership
dbsecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmDBSecurityGroupName = Nothing
    , _dbsgmStatus              = Nothing
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
    fromXMLRoot    = fromRoot "DBSecurityGroupMembership"

instance ToQuery DBSecurityGroupMembership

data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgEC2SecurityGroupId      :: Maybe Text
    , _ecsgEC2SecurityGroupName    :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    , _ecsgStatus                  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EC2SecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecsgEC2SecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'ecsgEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ecsgEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'ecsgStatus' @::@ 'Maybe' 'Text'
--
ec2SecurityGroup :: EC2SecurityGroup
ec2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus                  = Nothing
    , _ecsgEC2SecurityGroupName    = Nothing
    , _ecsgEC2SecurityGroupId      = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }

-- | Specifies the id of the EC2 security group.
ecsgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupId =
    lens _ecsgEC2SecurityGroupId (\s a -> s { _ecsgEC2SecurityGroupId = a })

-- | Specifies the name of the EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName =
    lens _ecsgEC2SecurityGroupName
        (\s a -> s { _ecsgEC2SecurityGroupName = a })

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the EC2SecurityGroupName field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId =
    lens _ecsgEC2SecurityGroupOwnerId
        (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })

-- | Provides the status of the EC2 security group. Status can be
-- "authorizing", "authorized", "revoking", and "revoked".
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup

data SourceType
    = DbInstance       -- ^ db-instance
    | DbParameterGroup -- ^ db-parameter-group
    | DbSecurityGroup  -- ^ db-security-group
    | DbSnapshot       -- ^ db-snapshot
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "db-instance"        DbInstance
         <|> match "db-parameter-group" DbParameterGroup
         <|> match "db-security-group"  DbSecurityGroup
         <|> match "db-snapshot"        DbSnapshot

instance ToText SourceType where
    toText = \case
        DbInstance       -> "db-instance"
        DbParameterGroup -> "db-parameter-group"
        DbSecurityGroup  -> "db-security-group"
        DbSnapshot       -> "db-snapshot"

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType

data DBParameterGroup = DBParameterGroup
    { _dbpgDBParameterGroupFamily :: Maybe Text
    , _dbpgDBParameterGroupName   :: Maybe Text
    , _dbpgDescription            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpgDBParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dbpgDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbpgDescription' @::@ 'Maybe' 'Text'
--
dbparameterGroup :: DBParameterGroup
dbparameterGroup = DBParameterGroup
    { _dbpgDBParameterGroupName   = Nothing
    , _dbpgDBParameterGroupFamily = Nothing
    , _dbpgDescription            = Nothing
    }

-- | Provides the name of the DB parameter group family that this DB parameter
-- group is compatible with.
dbpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupFamily =
    lens _dbpgDBParameterGroupFamily
        (\s a -> s { _dbpgDBParameterGroupFamily = a })

-- | Provides the name of the DB parameter group.
dbpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dbpgDBParameterGroupName =
    lens _dbpgDBParameterGroupName
        (\s a -> s { _dbpgDBParameterGroupName = a })

-- | Provides the customer-specified description for this DB parameter group.
dbpgDescription :: Lens' DBParameterGroup (Maybe Text)
dbpgDescription = lens _dbpgDescription (\s a -> s { _dbpgDescription = a })

instance FromXML DBParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

instance ToQuery DBParameterGroup

data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioCurrencyCode                  :: Maybe Text
    , _rdbioDBInstanceClass               :: Maybe Text
    , _rdbioDuration                      :: Maybe Int
    , _rdbioFixedPrice                    :: Maybe Double
    , _rdbioMultiAZ                       :: Maybe Bool
    , _rdbioOfferingType                  :: Maybe Text
    , _rdbioProductDescription            :: Maybe Text
    , _rdbioRecurringCharges              :: [RecurringCharge]
    , _rdbioReservedDBInstancesOfferingId :: Maybe Text
    , _rdbioUsagePrice                    :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedDBInstancesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbioCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'rdbioDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbioDuration' @::@ 'Maybe' 'Int'
--
-- * 'rdbioFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rdbioMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbioOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rdbioProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'rdbioRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rdbioReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rdbioUsagePrice' @::@ 'Maybe' 'Double'
--
reservedDBInstancesOffering :: ReservedDBInstancesOffering
reservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioReservedDBInstancesOfferingId = Nothing
    , _rdbioDBInstanceClass               = Nothing
    , _rdbioDuration                      = Nothing
    , _rdbioFixedPrice                    = Nothing
    , _rdbioUsagePrice                    = Nothing
    , _rdbioCurrencyCode                  = Nothing
    , _rdbioProductDescription            = Nothing
    , _rdbioOfferingType                  = Nothing
    , _rdbioMultiAZ                       = Nothing
    , _rdbioRecurringCharges              = mempty
    }

-- | The currency code for the reserved DB instance offering.
rdbioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioCurrencyCode =
    lens _rdbioCurrencyCode (\s a -> s { _rdbioCurrencyCode = a })

-- | The DB instance class for the reserved DB instance.
rdbioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioDBInstanceClass =
    lens _rdbioDBInstanceClass (\s a -> s { _rdbioDBInstanceClass = a })

-- | The duration of the offering in seconds.
rdbioDuration :: Lens' ReservedDBInstancesOffering (Maybe Int)
rdbioDuration = lens _rdbioDuration (\s a -> s { _rdbioDuration = a })

-- | The fixed price charged for this offering.
rdbioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioFixedPrice = lens _rdbioFixedPrice (\s a -> s { _rdbioFixedPrice = a })

-- | Indicates if the offering applies to Multi-AZ deployments.
rdbioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdbioMultiAZ = lens _rdbioMultiAZ (\s a -> s { _rdbioMultiAZ = a })

-- | The offering type.
rdbioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioOfferingType =
    lens _rdbioOfferingType (\s a -> s { _rdbioOfferingType = a })

-- | The database engine used by the offering.
rdbioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioProductDescription =
    lens _rdbioProductDescription (\s a -> s { _rdbioProductDescription = a })

-- | The recurring price charged to run this reserved DB instance.
rdbioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdbioRecurringCharges =
    lens _rdbioRecurringCharges (\s a -> s { _rdbioRecurringCharges = a })

-- | The offering identifier.
rdbioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdbioReservedDBInstancesOfferingId =
    lens _rdbioReservedDBInstancesOfferingId
        (\s a -> s { _rdbioReservedDBInstancesOfferingId = a })

-- | The hourly price charged for this offering.
rdbioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdbioUsagePrice = lens _rdbioUsagePrice (\s a -> s { _rdbioUsagePrice = a })

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOffering"

instance ToQuery ReservedDBInstancesOffering

data ApplyMethod
    = Immediate     -- ^ immediate
    | PendingReboot -- ^ pending-reboot
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ApplyMethod

instance FromText ApplyMethod where
    parser = match "immediate"      Immediate
         <|> match "pending-reboot" PendingReboot

instance ToText ApplyMethod where
    toText = \case
        Immediate     -> "immediate"
        PendingReboot -> "pending-reboot"

instance FromXML ApplyMethod where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplyMethod"

instance ToQuery ApplyMethod

data CharacterSet = CharacterSet
    { _csCharacterSetDescription :: Maybe Text
    , _csCharacterSetName        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CharacterSet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csCharacterSetDescription' @::@ 'Maybe' 'Text'
--
-- * 'csCharacterSetName' @::@ 'Maybe' 'Text'
--
characterSet :: CharacterSet
characterSet = CharacterSet
    { _csCharacterSetName        = Nothing
    , _csCharacterSetDescription = Nothing
    }

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription =
    lens _csCharacterSetDescription
        (\s a -> s { _csCharacterSetDescription = a })

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName =
    lens _csCharacterSetName (\s a -> s { _csCharacterSetName = a })

instance FromXML CharacterSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CharacterSet"

instance ToQuery CharacterSet

data Subnet = Subnet
    { _sSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _sSubnetIdentifier       :: Maybe Text
    , _sSubnetStatus           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Subnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sSubnetAvailabilityZone' @::@ 'Maybe' 'AvailabilityZone'
--
-- * 'sSubnetIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'sSubnetStatus' @::@ 'Maybe' 'Text'
--
subnet :: Subnet
subnet = Subnet
    { _sSubnetIdentifier       = Nothing
    , _sSubnetAvailabilityZone = Nothing
    , _sSubnetStatus           = Nothing
    }

sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone =
    lens _sSubnetAvailabilityZone (\s a -> s { _sSubnetAvailabilityZone = a })

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier =
    lens _sSubnetIdentifier (\s a -> s { _sSubnetIdentifier = a })

-- | Specifies the status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\s a -> s { _sSubnetStatus = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet

data ReservedDBInstance = ReservedDBInstance
    { _rdbiCurrencyCode                  :: Maybe Text
    , _rdbiDBInstanceClass               :: Maybe Text
    , _rdbiDBInstanceCount               :: Maybe Int
    , _rdbiDuration                      :: Maybe Int
    , _rdbiFixedPrice                    :: Maybe Double
    , _rdbiMultiAZ                       :: Maybe Bool
    , _rdbiOfferingType                  :: Maybe Text
    , _rdbiProductDescription            :: Maybe Text
    , _rdbiRecurringCharges              :: [RecurringCharge]
    , _rdbiReservedDBInstanceId          :: Maybe Text
    , _rdbiReservedDBInstancesOfferingId :: Maybe Text
    , _rdbiStartTime                     :: Maybe RFC822
    , _rdbiState                         :: Maybe Text
    , _rdbiUsagePrice                    :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedDBInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbiCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'rdbiDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbiDBInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'rdbiDuration' @::@ 'Maybe' 'Int'
--
-- * 'rdbiFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rdbiMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbiOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rdbiProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'rdbiRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rdbiReservedDBInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'rdbiReservedDBInstancesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rdbiStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'rdbiState' @::@ 'Maybe' 'Text'
--
-- * 'rdbiUsagePrice' @::@ 'Maybe' 'Double'
--
reservedDBInstance :: ReservedDBInstance
reservedDBInstance = ReservedDBInstance
    { _rdbiReservedDBInstanceId          = Nothing
    , _rdbiReservedDBInstancesOfferingId = Nothing
    , _rdbiDBInstanceClass               = Nothing
    , _rdbiStartTime                     = Nothing
    , _rdbiDuration                      = Nothing
    , _rdbiFixedPrice                    = Nothing
    , _rdbiUsagePrice                    = Nothing
    , _rdbiCurrencyCode                  = Nothing
    , _rdbiDBInstanceCount               = Nothing
    , _rdbiProductDescription            = Nothing
    , _rdbiOfferingType                  = Nothing
    , _rdbiMultiAZ                       = Nothing
    , _rdbiState                         = Nothing
    , _rdbiRecurringCharges              = mempty
    }

-- | The currency code for the reserved DB instance.
rdbiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdbiCurrencyCode = lens _rdbiCurrencyCode (\s a -> s { _rdbiCurrencyCode = a })

-- | The DB instance class for the reserved DB instance.
rdbiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdbiDBInstanceClass =
    lens _rdbiDBInstanceClass (\s a -> s { _rdbiDBInstanceClass = a })

-- | The number of reserved DB instances.
rdbiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Int)
rdbiDBInstanceCount =
    lens _rdbiDBInstanceCount (\s a -> s { _rdbiDBInstanceCount = a })

-- | The duration of the reservation in seconds.
rdbiDuration :: Lens' ReservedDBInstance (Maybe Int)
rdbiDuration = lens _rdbiDuration (\s a -> s { _rdbiDuration = a })

-- | The fixed price charged for this reserved DB instance.
rdbiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiFixedPrice = lens _rdbiFixedPrice (\s a -> s { _rdbiFixedPrice = a })

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdbiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdbiMultiAZ = lens _rdbiMultiAZ (\s a -> s { _rdbiMultiAZ = a })

-- | The offering type of this reserved DB instance.
rdbiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdbiOfferingType = lens _rdbiOfferingType (\s a -> s { _rdbiOfferingType = a })

-- | The description of the reserved DB instance.
rdbiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdbiProductDescription =
    lens _rdbiProductDescription (\s a -> s { _rdbiProductDescription = a })

-- | The recurring price charged to run this reserved DB instance.
rdbiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdbiRecurringCharges =
    lens _rdbiRecurringCharges (\s a -> s { _rdbiRecurringCharges = a })

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

-- | The time the reservation started.
rdbiStartTime :: Lens' ReservedDBInstance (Maybe UTCTime)
rdbiStartTime = lens _rdbiStartTime (\s a -> s { _rdbiStartTime = a })
    . mapping _Time

-- | The state of the reserved DB instance.
rdbiState :: Lens' ReservedDBInstance (Maybe Text)
rdbiState = lens _rdbiState (\s a -> s { _rdbiState = a })

-- | The hourly price charged for this reserved DB instance.
rdbiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdbiUsagePrice = lens _rdbiUsagePrice (\s a -> s { _rdbiUsagePrice = a })

instance FromXML ReservedDBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstance"

instance ToQuery ReservedDBInstance

data EngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily :: Maybe Text
    , _edMarker                 :: Maybe Text
    , _edParameters             :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'EngineDefaults' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edDBParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'edMarker' @::@ 'Maybe' 'Text'
--
-- * 'edParameters' @::@ ['Parameter']
--
engineDefaults :: EngineDefaults
engineDefaults = EngineDefaults
    { _edDBParameterGroupFamily = Nothing
    , _edMarker                 = Nothing
    , _edParameters             = mempty
    }

-- | Specifies the name of the DB parameter group family which the engine
-- default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily =
    lens _edDBParameterGroupFamily
        (\s a -> s { _edDBParameterGroupFamily = a })

-- | An optional pagination token provided by a previous EngineDefaults
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s { _edMarker = a })

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\s a -> s { _edParameters = a })

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

instance ToQuery EngineDefaults

newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage
    { _dbpgnmDBParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBParameterGroupNameMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpgnmDBParameterGroupName' @::@ 'Maybe' 'Text'
--
dbparameterGroupNameMessage :: DBParameterGroupNameMessage
dbparameterGroupNameMessage = DBParameterGroupNameMessage
    { _dbpgnmDBParameterGroupName = Nothing
    }

-- | The name of the DB parameter group.
dbpgnmDBParameterGroupName :: Lens' DBParameterGroupNameMessage (Maybe Text)
dbpgnmDBParameterGroupName =
    lens _dbpgnmDBParameterGroupName
        (\s a -> s { _dbpgnmDBParameterGroupName = a })

instance FromXML DBParameterGroupNameMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroupNameMessage"

instance ToQuery DBParameterGroupNameMessage

data OptionGroupOption = OptionGroupOption
    { _ogoDefaultPort                       :: Maybe Int
    , _ogoDescription                       :: Maybe Text
    , _ogoEngineName                        :: Maybe Text
    , _ogoMajorEngineVersion                :: Maybe Text
    , _ogoMinimumRequiredMinorEngineVersion :: Maybe Text
    , _ogoName                              :: Maybe Text
    , _ogoOptionGroupOptionSettings         :: [OptionGroupOptionSetting]
    , _ogoOptionsDependedOn                 :: [Text]
    , _ogoPermanent                         :: Maybe Bool
    , _ogoPersistent                        :: Maybe Bool
    , _ogoPortRequired                      :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'OptionGroupOption' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogoDefaultPort' @::@ 'Maybe' 'Int'
--
-- * 'ogoDescription' @::@ 'Maybe' 'Text'
--
-- * 'ogoEngineName' @::@ 'Maybe' 'Text'
--
-- * 'ogoMajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ogoMinimumRequiredMinorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ogoName' @::@ 'Maybe' 'Text'
--
-- * 'ogoOptionGroupOptionSettings' @::@ ['OptionGroupOptionSetting']
--
-- * 'ogoOptionsDependedOn' @::@ ['Text']
--
-- * 'ogoPermanent' @::@ 'Maybe' 'Bool'
--
-- * 'ogoPersistent' @::@ 'Maybe' 'Bool'
--
-- * 'ogoPortRequired' @::@ 'Maybe' 'Bool'
--
optionGroupOption :: OptionGroupOption
optionGroupOption = OptionGroupOption
    { _ogoName                              = Nothing
    , _ogoDescription                       = Nothing
    , _ogoEngineName                        = Nothing
    , _ogoMajorEngineVersion                = Nothing
    , _ogoMinimumRequiredMinorEngineVersion = Nothing
    , _ogoPortRequired                      = Nothing
    , _ogoDefaultPort                       = Nothing
    , _ogoOptionsDependedOn                 = mempty
    , _ogoPersistent                        = Nothing
    , _ogoPermanent                         = Nothing
    , _ogoOptionGroupOptionSettings         = mempty
    }

-- | If the option requires a port, specifies the default port for the option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Int)
ogoDefaultPort = lens _ogoDefaultPort (\s a -> s { _ogoDefaultPort = a })

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

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\s a -> s { _ogoName = a })

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings =
    lens _ogoOptionGroupOptionSettings
        (\s a -> s { _ogoOptionGroupOptionSettings = a })

-- | List of all options that are prerequisites for this option.
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn =
    lens _ogoOptionsDependedOn (\s a -> s { _ogoOptionsDependedOn = a })

-- | A permanent option cannot be removed from the option group once the
-- option group is used, and it cannot be removed from the db instance after
-- assigning an option group with this permanent option.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\s a -> s { _ogoPermanent = a })

-- | A persistent option cannot be removed from the option group once the
-- option group is used, but this option can be removed from the db instance
-- while modifying the related data and assigning another option group
-- without this option.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\s a -> s { _ogoPersistent = a })

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\s a -> s { _ogoPortRequired = a })

instance FromXML OptionGroupOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOption"

instance ToQuery OptionGroupOption

data DBInstance = DBInstance
    { _dbiAllocatedStorage                      :: Maybe Int
    , _dbiAutoMinorVersionUpgrade               :: Maybe Bool
    , _dbiAvailabilityZone                      :: Maybe Text
    , _dbiBackupRetentionPeriod                 :: Maybe Int
    , _dbiCharacterSetName                      :: Maybe Text
    , _dbiDBInstanceClass                       :: Maybe Text
    , _dbiDBInstanceIdentifier                  :: Maybe Text
    , _dbiDBInstanceStatus                      :: Maybe Text
    , _dbiDBName                                :: Maybe Text
    , _dbiDBParameterGroups                     :: [DBParameterGroupStatus]
    , _dbiDBSecurityGroups                      :: [DBSecurityGroupMembership]
    , _dbiDBSubnetGroup                         :: Maybe DBSubnetGroup
    , _dbiEndpoint                              :: Maybe Endpoint
    , _dbiEngine                                :: Maybe Text
    , _dbiEngineVersion                         :: Maybe Text
    , _dbiInstanceCreateTime                    :: Maybe RFC822
    , _dbiIops                                  :: Maybe Int
    , _dbiLatestRestorableTime                  :: Maybe RFC822
    , _dbiLicenseModel                          :: Maybe Text
    , _dbiMasterUsername                        :: Maybe Text
    , _dbiMultiAZ                               :: Maybe Bool
    , _dbiOptionGroupMemberships                :: [OptionGroupMembership]
    , _dbiPendingModifiedValues                 :: Maybe PendingModifiedValues
    , _dbiPreferredBackupWindow                 :: Maybe Text
    , _dbiPreferredMaintenanceWindow            :: Maybe Text
    , _dbiPubliclyAccessible                    :: Maybe Bool
    , _dbiReadReplicaDBInstanceIdentifiers      :: [Text]
    , _dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
    , _dbiSecondaryAvailabilityZone             :: Maybe Text
    , _dbiStatusInfos                           :: [DBInstanceStatusInfo]
    , _dbiStorageType                           :: Maybe Text
    , _dbiTdeCredentialArn                      :: Maybe Text
    , _dbiVpcSecurityGroups                     :: [VpcSecurityGroupMembership]
    } deriving (Eq, Show, Generic)

-- | 'DBInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbiAllocatedStorage' @::@ 'Maybe' 'Int'
--
-- * 'dbiAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'dbiAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'dbiBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'dbiCharacterSetName' @::@ 'Maybe' 'Text'
--
-- * 'dbiDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'dbiDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dbiDBInstanceStatus' @::@ 'Maybe' 'Text'
--
-- * 'dbiDBName' @::@ 'Maybe' 'Text'
--
-- * 'dbiDBParameterGroups' @::@ ['DBParameterGroupStatus']
--
-- * 'dbiDBSecurityGroups' @::@ ['DBSecurityGroupMembership']
--
-- * 'dbiDBSubnetGroup' @::@ 'Maybe' 'DBSubnetGroup'
--
-- * 'dbiEndpoint' @::@ 'Maybe' 'Endpoint'
--
-- * 'dbiEngine' @::@ 'Maybe' 'Text'
--
-- * 'dbiEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dbiInstanceCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dbiIops' @::@ 'Maybe' 'Int'
--
-- * 'dbiLatestRestorableTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dbiLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'dbiMasterUsername' @::@ 'Maybe' 'Text'
--
-- * 'dbiMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'dbiOptionGroupMemberships' @::@ ['OptionGroupMembership']
--
-- * 'dbiPendingModifiedValues' @::@ 'Maybe' 'PendingModifiedValues'
--
-- * 'dbiPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
-- * 'dbiPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'dbiPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'dbiReadReplicaDBInstanceIdentifiers' @::@ ['Text']
--
-- * 'dbiReadReplicaSourceDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dbiSecondaryAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'dbiStatusInfos' @::@ ['DBInstanceStatusInfo']
--
-- * 'dbiStorageType' @::@ 'Maybe' 'Text'
--
-- * 'dbiTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'dbiVpcSecurityGroups' @::@ ['VpcSecurityGroupMembership']
--
dbinstance :: DBInstance
dbinstance = DBInstance
    { _dbiDBInstanceIdentifier                  = Nothing
    , _dbiDBInstanceClass                       = Nothing
    , _dbiEngine                                = Nothing
    , _dbiDBInstanceStatus                      = Nothing
    , _dbiMasterUsername                        = Nothing
    , _dbiDBName                                = Nothing
    , _dbiEndpoint                              = Nothing
    , _dbiAllocatedStorage                      = Nothing
    , _dbiInstanceCreateTime                    = Nothing
    , _dbiPreferredBackupWindow                 = Nothing
    , _dbiBackupRetentionPeriod                 = Nothing
    , _dbiDBSecurityGroups                      = mempty
    , _dbiVpcSecurityGroups                     = mempty
    , _dbiDBParameterGroups                     = mempty
    , _dbiAvailabilityZone                      = Nothing
    , _dbiDBSubnetGroup                         = Nothing
    , _dbiPreferredMaintenanceWindow            = Nothing
    , _dbiPendingModifiedValues                 = Nothing
    , _dbiLatestRestorableTime                  = Nothing
    , _dbiMultiAZ                               = Nothing
    , _dbiEngineVersion                         = Nothing
    , _dbiAutoMinorVersionUpgrade               = Nothing
    , _dbiReadReplicaSourceDBInstanceIdentifier = Nothing
    , _dbiReadReplicaDBInstanceIdentifiers      = mempty
    , _dbiLicenseModel                          = Nothing
    , _dbiIops                                  = Nothing
    , _dbiOptionGroupMemberships                = mempty
    , _dbiCharacterSetName                      = Nothing
    , _dbiSecondaryAvailabilityZone             = Nothing
    , _dbiPubliclyAccessible                    = Nothing
    , _dbiStatusInfos                           = mempty
    , _dbiStorageType                           = Nothing
    , _dbiTdeCredentialArn                      = Nothing
    }

-- | Specifies the allocated storage size specified in gigabytes.
dbiAllocatedStorage :: Lens' DBInstance (Maybe Int)
dbiAllocatedStorage =
    lens _dbiAllocatedStorage (\s a -> s { _dbiAllocatedStorage = a })

-- | Indicates that minor version patches are applied automatically.
dbiAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
dbiAutoMinorVersionUpgrade =
    lens _dbiAutoMinorVersionUpgrade
        (\s a -> s { _dbiAutoMinorVersionUpgrade = a })

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
dbiAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiAvailabilityZone =
    lens _dbiAvailabilityZone (\s a -> s { _dbiAvailabilityZone = a })

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbiBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
dbiBackupRetentionPeriod =
    lens _dbiBackupRetentionPeriod
        (\s a -> s { _dbiBackupRetentionPeriod = a })

-- | If present, specifies the name of the character set that this instance is
-- associated with.
dbiCharacterSetName :: Lens' DBInstance (Maybe Text)
dbiCharacterSetName =
    lens _dbiCharacterSetName (\s a -> s { _dbiCharacterSetName = a })

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbiDBInstanceClass :: Lens' DBInstance (Maybe Text)
dbiDBInstanceClass =
    lens _dbiDBInstanceClass (\s a -> s { _dbiDBInstanceClass = a })

-- | Contains a user-supplied database identifier. This is the unique key that
-- identifies a DB instance.
dbiDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiDBInstanceIdentifier =
    lens _dbiDBInstanceIdentifier (\s a -> s { _dbiDBInstanceIdentifier = a })

-- | Specifies the current state of this database.
dbiDBInstanceStatus :: Lens' DBInstance (Maybe Text)
dbiDBInstanceStatus =
    lens _dbiDBInstanceStatus (\s a -> s { _dbiDBInstanceStatus = a })

-- | The meaning of this parameter differs according to the database engine
-- you use. For example, this value returns only MySQL information when
-- returning values from CreateDBInstanceReadReplica since read replicas are
-- only supported for MySQL. MySQL Contains the name of the initial database
-- of this instance that was provided at create time, if one was specified
-- when the DB instance was created. This same name is returned for the life
-- of the DB instance. Type: String Oracle Contains the Oracle System ID
-- (SID) of the created DB instance. Not shown when the returned parameters
-- do not apply to an Oracle DB instance.
dbiDBName :: Lens' DBInstance (Maybe Text)
dbiDBName = lens _dbiDBName (\s a -> s { _dbiDBName = a })

-- | Provides the list of DB parameter groups applied to this DB instance.
dbiDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
dbiDBParameterGroups =
    lens _dbiDBParameterGroups (\s a -> s { _dbiDBParameterGroups = a })

-- | Provides List of DB security group elements containing only
-- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
dbiDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
dbiDBSecurityGroups =
    lens _dbiDBSecurityGroups (\s a -> s { _dbiDBSecurityGroups = a })

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
dbiDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
dbiDBSubnetGroup = lens _dbiDBSubnetGroup (\s a -> s { _dbiDBSubnetGroup = a })

-- | Specifies the connection endpoint.
dbiEndpoint :: Lens' DBInstance (Maybe Endpoint)
dbiEndpoint = lens _dbiEndpoint (\s a -> s { _dbiEndpoint = a })

-- | Provides the name of the database engine to be used for this DB instance.
dbiEngine :: Lens' DBInstance (Maybe Text)
dbiEngine = lens _dbiEngine (\s a -> s { _dbiEngine = a })

-- | Indicates the database engine version.
dbiEngineVersion :: Lens' DBInstance (Maybe Text)
dbiEngineVersion = lens _dbiEngineVersion (\s a -> s { _dbiEngineVersion = a })

-- | Provides the date and time the DB instance was created.
dbiInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
dbiInstanceCreateTime =
    lens _dbiInstanceCreateTime (\s a -> s { _dbiInstanceCreateTime = a })
        . mapping _Time

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
dbiIops :: Lens' DBInstance (Maybe Int)
dbiIops = lens _dbiIops (\s a -> s { _dbiIops = a })

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbiLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
dbiLatestRestorableTime =
    lens _dbiLatestRestorableTime (\s a -> s { _dbiLatestRestorableTime = a })
        . mapping _Time

-- | License model information for this DB instance.
dbiLicenseModel :: Lens' DBInstance (Maybe Text)
dbiLicenseModel = lens _dbiLicenseModel (\s a -> s { _dbiLicenseModel = a })

-- | Contains the master username for the DB instance.
dbiMasterUsername :: Lens' DBInstance (Maybe Text)
dbiMasterUsername =
    lens _dbiMasterUsername (\s a -> s { _dbiMasterUsername = a })

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbiMultiAZ :: Lens' DBInstance (Maybe Bool)
dbiMultiAZ = lens _dbiMultiAZ (\s a -> s { _dbiMultiAZ = a })

-- | Provides the list of option group memberships for this DB instance.
dbiOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
dbiOptionGroupMemberships =
    lens _dbiOptionGroupMemberships
        (\s a -> s { _dbiOptionGroupMemberships = a })

-- | Specifies that changes to the DB instance are pending. This element is
-- only included when changes are pending. Specific changes are identified
-- by subelements.
dbiPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
dbiPendingModifiedValues =
    lens _dbiPendingModifiedValues
        (\s a -> s { _dbiPendingModifiedValues = a })

-- | Specifies the daily time range during which automated backups are created
-- if automated backups are enabled, as determined by the
-- BackupRetentionPeriod.
dbiPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredBackupWindow =
    lens _dbiPreferredBackupWindow
        (\s a -> s { _dbiPreferredBackupWindow = a })

-- | Specifies the weekly time range (in UTC) during which system maintenance
-- can occur.
dbiPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
dbiPreferredMaintenanceWindow =
    lens _dbiPreferredMaintenanceWindow
        (\s a -> s { _dbiPreferredMaintenanceWindow = a })

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS
-- name, which resolves to a public IP address. A value of false specifies
-- an internal instance with a DNS name that resolves to a private IP
-- address. Default: The default behavior varies depending on whether a VPC
-- has been requested or not. The following list shows the default behavior
-- in each case. Default VPC:true VPC:false If no DB subnet group has been
-- specified as part of the request and the PubliclyAccessible value has not
-- been set, the DB instance will be publicly accessible. If a specific DB
-- subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- private.
dbiPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
dbiPubliclyAccessible =
    lens _dbiPubliclyAccessible (\s a -> s { _dbiPubliclyAccessible = a })

-- | Contains one or more identifiers of the read replicas associated with
-- this DB instance.
dbiReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
dbiReadReplicaDBInstanceIdentifiers =
    lens _dbiReadReplicaDBInstanceIdentifiers
        (\s a -> s { _dbiReadReplicaDBInstanceIdentifiers = a })

-- | Contains the identifier of the source DB instance if this DB instance is
-- a read replica.
dbiReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
dbiReadReplicaSourceDBInstanceIdentifier =
    lens _dbiReadReplicaSourceDBInstanceIdentifier
        (\s a -> s { _dbiReadReplicaSourceDBInstanceIdentifier = a })

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
dbiSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
dbiSecondaryAvailabilityZone =
    lens _dbiSecondaryAvailabilityZone
        (\s a -> s { _dbiSecondaryAvailabilityZone = a })

-- | The status of a read replica. If the instance is not a read replica, this
-- will be blank.
dbiStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
dbiStatusInfos = lens _dbiStatusInfos (\s a -> s { _dbiStatusInfos = a })

-- | Specifies storage type associated with DB Instance.
dbiStorageType :: Lens' DBInstance (Maybe Text)
dbiStorageType = lens _dbiStorageType (\s a -> s { _dbiStorageType = a })

-- | The ARN from the Key Store with which the instance is associated for TDE
-- encryption.
dbiTdeCredentialArn :: Lens' DBInstance (Maybe Text)
dbiTdeCredentialArn =
    lens _dbiTdeCredentialArn (\s a -> s { _dbiTdeCredentialArn = a })

-- | Provides List of VPC security group elements that the DB instance belongs
-- to.
dbiVpcSecurityGroups :: Lens' DBInstance [VpcSecurityGroupMembership]
dbiVpcSecurityGroups =
    lens _dbiVpcSecurityGroups (\s a -> s { _dbiVpcSecurityGroups = a })

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstance"

instance ToQuery DBInstance

newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AvailabilityZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azName' @::@ 'Maybe' 'Text'
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone

data EventSubscription = EventSubscription
    { _esCustSubscriptionId       :: Maybe Text
    , _esCustomerAwsId            :: Maybe Text
    , _esEnabled                  :: Maybe Bool
    , _esEventCategoriesList      :: [Text]
    , _esSnsTopicArn              :: Maybe Text
    , _esSourceIdsList            :: [Text]
    , _esSourceType               :: Maybe Text
    , _esStatus                   :: Maybe Text
    , _esSubscriptionCreationTime :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EventSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esCustSubscriptionId' @::@ 'Maybe' 'Text'
--
-- * 'esCustomerAwsId' @::@ 'Maybe' 'Text'
--
-- * 'esEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'esEventCategoriesList' @::@ ['Text']
--
-- * 'esSnsTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'esSourceIdsList' @::@ ['Text']
--
-- * 'esSourceType' @::@ 'Maybe' 'Text'
--
-- * 'esStatus' @::@ 'Maybe' 'Text'
--
-- * 'esSubscriptionCreationTime' @::@ 'Maybe' 'Text'
--
eventSubscription :: EventSubscription
eventSubscription = EventSubscription
    { _esCustomerAwsId            = Nothing
    , _esCustSubscriptionId       = Nothing
    , _esSnsTopicArn              = Nothing
    , _esStatus                   = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esSourceType               = Nothing
    , _esSourceIdsList            = mempty
    , _esEventCategoriesList      = mempty
    , _esEnabled                  = Nothing
    }

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId =
    lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })

-- | The AWS customer account associated with the RDS event notification
-- subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })

-- | A Boolean value indicating if the subscription is enabled. True indicates
-- the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList =
    lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })

-- | The topic ARN of the RDS event notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })

-- | A list of source IDs for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a })

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })

-- | The status of the RDS event notification subscription. Constraints: Can
-- be one of the following: creating | modifying | deleting | active |
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

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

instance ToQuery EventSubscription

data DBSubnetGroup = DBSubnetGroup
    { _dbsg1DBSubnetGroupDescription :: Maybe Text
    , _dbsg1DBSubnetGroupName        :: Maybe Text
    , _dbsg1SubnetGroupStatus        :: Maybe Text
    , _dbsg1Subnets                  :: [Subnet]
    , _dbsg1VpcId                    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DBSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsg1DBSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'dbsg1DBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dbsg1SubnetGroupStatus' @::@ 'Maybe' 'Text'
--
-- * 'dbsg1Subnets' @::@ ['Subnet']
--
-- * 'dbsg1VpcId' @::@ 'Maybe' 'Text'
--
dbsubnetGroup :: DBSubnetGroup
dbsubnetGroup = DBSubnetGroup
    { _dbsg1DBSubnetGroupName        = Nothing
    , _dbsg1DBSubnetGroupDescription = Nothing
    , _dbsg1VpcId                    = Nothing
    , _dbsg1SubnetGroupStatus        = Nothing
    , _dbsg1Subnets                  = mempty
    }

-- | Provides the description of the DB subnet group.
dbsg1DBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dbsg1DBSubnetGroupDescription =
    lens _dbsg1DBSubnetGroupDescription
        (\s a -> s { _dbsg1DBSubnetGroupDescription = a })

-- | Specifies the name of the DB subnet group.
dbsg1DBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dbsg1DBSubnetGroupName =
    lens _dbsg1DBSubnetGroupName (\s a -> s { _dbsg1DBSubnetGroupName = a })

-- | Provides the status of the DB subnet group.
dbsg1SubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dbsg1SubnetGroupStatus =
    lens _dbsg1SubnetGroupStatus (\s a -> s { _dbsg1SubnetGroupStatus = a })

-- | Contains a list of Subnet elements.
dbsg1Subnets :: Lens' DBSubnetGroup [Subnet]
dbsg1Subnets = lens _dbsg1Subnets (\s a -> s { _dbsg1Subnets = a })

-- | Provides the VpcId of the DB subnet group.
dbsg1VpcId :: Lens' DBSubnetGroup (Maybe Text)
dbsg1VpcId = lens _dbsg1VpcId (\s a -> s { _dbsg1VpcId = a })

instance FromXML DBSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSubnetGroup"

instance ToQuery DBSubnetGroup

data DBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiMessage    :: Maybe Text
    , _dbisiNormal     :: Maybe Bool
    , _dbisiStatus     :: Maybe Text
    , _dbisiStatusType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DBInstanceStatusInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbisiMessage' @::@ 'Maybe' 'Text'
--
-- * 'dbisiNormal' @::@ 'Maybe' 'Bool'
--
-- * 'dbisiStatus' @::@ 'Maybe' 'Text'
--
-- * 'dbisiStatusType' @::@ 'Maybe' 'Text'
--
dbinstanceStatusInfo :: DBInstanceStatusInfo
dbinstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatusType = Nothing
    , _dbisiNormal     = Nothing
    , _dbisiStatus     = Nothing
    , _dbisiMessage    = Nothing
    }

-- | Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
dbisiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiMessage = lens _dbisiMessage (\s a -> s { _dbisiMessage = a })

-- | Boolean value that is true if the instance is operating normally, or
-- false if the instance is in an error state.
dbisiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
dbisiNormal = lens _dbisiNormal (\s a -> s { _dbisiNormal = a })

-- | Status of the DB instance. For a StatusType of read replica, the values
-- can be replicating, error, stopped, or terminated.
dbisiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatus = lens _dbisiStatus (\s a -> s { _dbisiStatus = a })

-- | This value is currently "read replication.".
dbisiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
dbisiStatusType = lens _dbisiStatusType (\s a -> s { _dbisiStatusType = a })

instance FromXML DBInstanceStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstanceStatusInfo"

instance ToQuery DBInstanceStatusInfo

data OptionSetting = OptionSetting
    { _osAllowedValues :: Maybe Text
    , _osApplyType     :: Maybe Text
    , _osDataType      :: Maybe Text
    , _osDefaultValue  :: Maybe Text
    , _osDescription   :: Maybe Text
    , _osIsCollection  :: Maybe Bool
    , _osIsModifiable  :: Maybe Bool
    , _osName          :: Maybe Text
    , _osValue         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'OptionSetting' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'osApplyType' @::@ 'Maybe' 'Text'
--
-- * 'osDataType' @::@ 'Maybe' 'Text'
--
-- * 'osDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'osDescription' @::@ 'Maybe' 'Text'
--
-- * 'osIsCollection' @::@ 'Maybe' 'Bool'
--
-- * 'osIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'osName' @::@ 'Maybe' 'Text'
--
-- * 'osValue' @::@ 'Maybe' 'Text'
--
optionSetting :: OptionSetting
optionSetting = OptionSetting
    { _osName          = Nothing
    , _osValue         = Nothing
    , _osDefaultValue  = Nothing
    , _osDescription   = Nothing
    , _osApplyType     = Nothing
    , _osDataType      = Nothing
    , _osAllowedValues = Nothing
    , _osIsModifiable  = Nothing
    , _osIsCollection  = Nothing
    }

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\s a -> s { _osAllowedValues = a })

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\s a -> s { _osApplyType = a })

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\s a -> s { _osDataType = a })

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\s a -> s { _osDefaultValue = a })

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\s a -> s { _osDescription = a })

-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection = lens _osIsCollection (\s a -> s { _osIsCollection = a })

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\s a -> s { _osIsModifiable = a })

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName = lens _osName (\s a -> s { _osName = a })

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\s a -> s { _osValue = a })

instance FromXML OptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionSetting"

instance ToQuery OptionSetting

data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLastWritten :: Maybe Integer
    , _ddblfdLogFileName :: Maybe Text
    , _ddblfdSize        :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeDBLogFilesDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddblfdLastWritten' @::@ 'Maybe' 'Integer'
--
-- * 'ddblfdLogFileName' @::@ 'Maybe' 'Text'
--
-- * 'ddblfdSize' @::@ 'Maybe' 'Integer'
--
describeDBLogFilesDetails :: DescribeDBLogFilesDetails
describeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLogFileName = Nothing
    , _ddblfdLastWritten = Nothing
    , _ddblfdSize        = Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
ddblfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdLastWritten =
    lens _ddblfdLastWritten (\s a -> s { _ddblfdLastWritten = a })

-- | The name of the log file for the specified DB instance.
ddblfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddblfdLogFileName =
    lens _ddblfdLogFileName (\s a -> s { _ddblfdLogFileName = a })

-- | The size, in bytes, of the log file for the specified DB instance.
ddblfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddblfdSize = lens _ddblfdSize (\s a -> s { _ddblfdSize = a })

instance FromXML DescribeDBLogFilesDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDBLogFilesDetails"

instance ToQuery DescribeDBLogFilesDetails

data OrderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioAvailabilityZones  :: [AvailabilityZone]
    , _odbioDBInstanceClass    :: Maybe Text
    , _odbioEngine             :: Maybe Text
    , _odbioEngineVersion      :: Maybe Text
    , _odbioLicenseModel       :: Maybe Text
    , _odbioMultiAZCapable     :: Maybe Bool
    , _odbioReadReplicaCapable :: Maybe Bool
    , _odbioStorageType        :: Maybe Text
    , _odbioSupportsIops       :: Maybe Bool
    , _odbioVpc                :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'OrderableDBInstanceOption' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'odbioAvailabilityZones' @::@ ['AvailabilityZone']
--
-- * 'odbioDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'odbioEngine' @::@ 'Maybe' 'Text'
--
-- * 'odbioEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'odbioLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'odbioMultiAZCapable' @::@ 'Maybe' 'Bool'
--
-- * 'odbioReadReplicaCapable' @::@ 'Maybe' 'Bool'
--
-- * 'odbioStorageType' @::@ 'Maybe' 'Text'
--
-- * 'odbioSupportsIops' @::@ 'Maybe' 'Bool'
--
-- * 'odbioVpc' @::@ 'Maybe' 'Bool'
--
orderableDBInstanceOption :: OrderableDBInstanceOption
orderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioEngine             = Nothing
    , _odbioEngineVersion      = Nothing
    , _odbioDBInstanceClass    = Nothing
    , _odbioLicenseModel       = Nothing
    , _odbioAvailabilityZones  = mempty
    , _odbioMultiAZCapable     = Nothing
    , _odbioReadReplicaCapable = Nothing
    , _odbioVpc                = Nothing
    , _odbioStorageType        = Nothing
    , _odbioSupportsIops       = Nothing
    }

-- | A list of availability zones for the orderable DB instance.
odbioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odbioAvailabilityZones =
    lens _odbioAvailabilityZones (\s a -> s { _odbioAvailabilityZones = a })

-- | The DB instance Class for the orderable DB instance.
odbioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioDBInstanceClass =
    lens _odbioDBInstanceClass (\s a -> s { _odbioDBInstanceClass = a })

-- | The engine type of the orderable DB instance.
odbioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngine = lens _odbioEngine (\s a -> s { _odbioEngine = a })

-- | The engine version of the orderable DB instance.
odbioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioEngineVersion =
    lens _odbioEngineVersion (\s a -> s { _odbioEngineVersion = a })

-- | The license model for the orderable DB instance.
odbioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioLicenseModel =
    lens _odbioLicenseModel (\s a -> s { _odbioLicenseModel = a })

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odbioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioMultiAZCapable =
    lens _odbioMultiAZCapable (\s a -> s { _odbioMultiAZCapable = a })

-- | Indicates whether this orderable DB instance can have a read replica.
odbioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioReadReplicaCapable =
    lens _odbioReadReplicaCapable (\s a -> s { _odbioReadReplicaCapable = a })

-- | The storage type for this orderable DB instance.
odbioStorageType :: Lens' OrderableDBInstanceOption (Maybe Text)
odbioStorageType = lens _odbioStorageType (\s a -> s { _odbioStorageType = a })

-- | Indicates whether this orderable DB instance supports provisioned IOPS.
odbioSupportsIops :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioSupportsIops =
    lens _odbioSupportsIops (\s a -> s { _odbioSupportsIops = a })

-- | Indicates whether this is a VPC orderable DB instance.
odbioVpc :: Lens' OrderableDBInstanceOption (Maybe Bool)
odbioVpc = lens _odbioVpc (\s a -> s { _odbioVpc = a })

instance FromXML OrderableDBInstanceOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableDBInstanceOption"

instance ToQuery OrderableDBInstanceOption

data Filter = Filter
    { _fName   :: Text
    , _fValues :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'Filter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fName' @::@ 'Text'
--
-- * 'fValues' @::@ ['Text']
--
filter' :: Text -- ^ 'fName'
        -> Filter
filter' p1 = Filter
    { _fName   = p1
    , _fValues = mempty
    }

-- | This parameter is not currently supported.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | This parameter is not currently supported.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a })

instance FromXML Filter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Filter"

instance ToQuery Filter

data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RecurringCharge' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRecurringChargeAmount' @::@ 'Maybe' 'Double'
--
-- * 'rcRecurringChargeFrequency' @::@ 'Maybe' 'Text'
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
        (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge

data Endpoint = Endpoint
    { _eAddress :: Maybe Text
    , _ePort    :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'Endpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eAddress' @::@ 'Maybe' 'Text'
--
-- * 'ePort' @::@ 'Maybe' 'Int'
--
endpoint :: Endpoint
endpoint = Endpoint
    { _eAddress = Nothing
    , _ePort    = Nothing
    }

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint

data OptionConfiguration = OptionConfiguration
    { _ocDBSecurityGroupMemberships  :: [Text]
    , _ocOptionName                  :: Text
    , _ocOptionSettings              :: [OptionSetting]
    , _ocPort                        :: Maybe Int
    , _ocVpcSecurityGroupMemberships :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'OptionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ocDBSecurityGroupMemberships' @::@ ['Text']
--
-- * 'ocOptionName' @::@ 'Text'
--
-- * 'ocOptionSettings' @::@ ['OptionSetting']
--
-- * 'ocPort' @::@ 'Maybe' 'Int'
--
-- * 'ocVpcSecurityGroupMemberships' @::@ ['Text']
--
optionConfiguration :: Text -- ^ 'ocOptionName'
                    -> OptionConfiguration
optionConfiguration p1 = OptionConfiguration
    { _ocOptionName                  = p1
    , _ocPort                        = Nothing
    , _ocDBSecurityGroupMemberships  = mempty
    , _ocVpcSecurityGroupMemberships = mempty
    , _ocOptionSettings              = mempty
    }

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships =
    lens _ocDBSecurityGroupMemberships
        (\s a -> s { _ocDBSecurityGroupMemberships = a })

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\s a -> s { _ocOptionName = a })

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings = lens _ocOptionSettings (\s a -> s { _ocOptionSettings = a })

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Int)
ocPort = lens _ocPort (\s a -> s { _ocPort = a })

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVpcSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVpcSecurityGroupMemberships =
    lens _ocVpcSecurityGroupMemberships
        (\s a -> s { _ocVpcSecurityGroupMemberships = a })

instance FromXML OptionConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionConfiguration"

instance ToQuery OptionConfiguration

data Option = Option
    { _oDBSecurityGroupMemberships  :: [DBSecurityGroupMembership]
    , _oOptionDescription           :: Maybe Text
    , _oOptionName                  :: Maybe Text
    , _oOptionSettings              :: [OptionSetting]
    , _oPermanent                   :: Maybe Bool
    , _oPersistent                  :: Maybe Bool
    , _oPort                        :: Maybe Int
    , _oVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
    } deriving (Eq, Show, Generic)

-- | 'Option' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oDBSecurityGroupMemberships' @::@ ['DBSecurityGroupMembership']
--
-- * 'oOptionDescription' @::@ 'Maybe' 'Text'
--
-- * 'oOptionName' @::@ 'Maybe' 'Text'
--
-- * 'oOptionSettings' @::@ ['OptionSetting']
--
-- * 'oPermanent' @::@ 'Maybe' 'Bool'
--
-- * 'oPersistent' @::@ 'Maybe' 'Bool'
--
-- * 'oPort' @::@ 'Maybe' 'Int'
--
-- * 'oVpcSecurityGroupMemberships' @::@ ['VpcSecurityGroupMembership']
--
option :: Option
option = Option
    { _oOptionName                  = Nothing
    , _oOptionDescription           = Nothing
    , _oPersistent                  = Nothing
    , _oPermanent                   = Nothing
    , _oPort                        = Nothing
    , _oOptionSettings              = mempty
    , _oDBSecurityGroupMemberships  = mempty
    , _oVpcSecurityGroupMemberships = mempty
    }

-- | If the option requires access to a port, then this DB security group
-- allows access to the port.
oDBSecurityGroupMemberships :: Lens' Option [DBSecurityGroupMembership]
oDBSecurityGroupMemberships =
    lens _oDBSecurityGroupMemberships
        (\s a -> s { _oDBSecurityGroupMemberships = a })

-- | The description of the option.
oOptionDescription :: Lens' Option (Maybe Text)
oOptionDescription =
    lens _oOptionDescription (\s a -> s { _oOptionDescription = a })

-- | The name of the option.
oOptionName :: Lens' Option (Maybe Text)
oOptionName = lens _oOptionName (\s a -> s { _oOptionName = a })

-- | The option settings for this option.
oOptionSettings :: Lens' Option [OptionSetting]
oOptionSettings = lens _oOptionSettings (\s a -> s { _oOptionSettings = a })

-- | Indicate if this option is permanent.
oPermanent :: Lens' Option (Maybe Bool)
oPermanent = lens _oPermanent (\s a -> s { _oPermanent = a })

-- | Indicate if this option is persistent.
oPersistent :: Lens' Option (Maybe Bool)
oPersistent = lens _oPersistent (\s a -> s { _oPersistent = a })

-- | If required, the port configured for this option to use.
oPort :: Lens' Option (Maybe Int)
oPort = lens _oPort (\s a -> s { _oPort = a })

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
oVpcSecurityGroupMemberships :: Lens' Option [VpcSecurityGroupMembership]
oVpcSecurityGroupMemberships =
    lens _oVpcSecurityGroupMemberships
        (\s a -> s { _oVpcSecurityGroupMemberships = a })

instance FromXML Option where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Option"

instance ToQuery Option

data IPRange = IPRange
    { _iprCIDRIP :: Maybe Text
    , _iprStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'IPRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iprCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'iprStatus' @::@ 'Maybe' 'Text'
--
iprange :: IPRange
iprange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    }

-- | Specifies the IP range.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })

-- | Specifies the status of the IP range. Status can be "authorizing",
-- "authorized", "revoking", and "revoked".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange

data OptionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName :: Maybe Text
    , _ogmStatus          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'OptionGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogmOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ogmStatus' @::@ 'Maybe' 'Text'
--
optionGroupMembership :: OptionGroupMembership
optionGroupMembership = OptionGroupMembership
    { _ogmOptionGroupName = Nothing
    , _ogmStatus          = Nothing
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

instance ToQuery OptionGroupMembership

data EventCategoriesMap = EventCategoriesMap
    { _ecmEventCategories :: [Text]
    , _ecmSourceType      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EventCategoriesMap' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmEventCategories' @::@ ['Text']
--
-- * 'ecmSourceType' @::@ 'Maybe' 'Text'
--
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap = EventCategoriesMap
    { _ecmSourceType      = Nothing
    , _ecmEventCategories = mempty
    }

-- | The event categories for the specified source type.
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories =
    lens _ecmEventCategories (\s a -> s { _ecmEventCategories = a })

-- | The source type that the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s { _ecmSourceType = a })

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

instance ToQuery EventCategoriesMap

data PendingModifiedValues = PendingModifiedValues
    { _pmvAllocatedStorage      :: Maybe Int
    , _pmvBackupRetentionPeriod :: Maybe Int
    , _pmvDBInstanceClass       :: Maybe Text
    , _pmvDBInstanceIdentifier  :: Maybe Text
    , _pmvEngineVersion         :: Maybe Text
    , _pmvIops                  :: Maybe Int
    , _pmvMasterUserPassword    :: Maybe Text
    , _pmvMultiAZ               :: Maybe Bool
    , _pmvPort                  :: Maybe Int
    , _pmvStorageType           :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PendingModifiedValues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmvAllocatedStorage' @::@ 'Maybe' 'Int'
--
-- * 'pmvBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'pmvDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'pmvDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'pmvEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'pmvIops' @::@ 'Maybe' 'Int'
--
-- * 'pmvMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'pmvMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'pmvPort' @::@ 'Maybe' 'Int'
--
-- * 'pmvStorageType' @::@ 'Maybe' 'Text'
--
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues
    { _pmvDBInstanceClass       = Nothing
    , _pmvAllocatedStorage      = Nothing
    , _pmvMasterUserPassword    = Nothing
    , _pmvPort                  = Nothing
    , _pmvBackupRetentionPeriod = Nothing
    , _pmvMultiAZ               = Nothing
    , _pmvEngineVersion         = Nothing
    , _pmvIops                  = Nothing
    , _pmvDBInstanceIdentifier  = Nothing
    , _pmvStorageType           = Nothing
    }

-- | Contains the new AllocatedStorage size for the DB instance that will be
-- applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage =
    lens _pmvAllocatedStorage (\s a -> s { _pmvAllocatedStorage = a })

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod =
    lens _pmvBackupRetentionPeriod
        (\s a -> s { _pmvBackupRetentionPeriod = a })

-- | Contains the new DBInstanceClass for the DB instance that will be applied
-- or is in progress.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass =
    lens _pmvDBInstanceClass (\s a -> s { _pmvDBInstanceClass = a })

-- | Contains the new DBInstanceIdentifier for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier =
    lens _pmvDBInstanceIdentifier (\s a -> s { _pmvDBInstanceIdentifier = a })

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })

-- | Specifies the new Provisioned IOPS value for the DB instance that will be
-- applied or is being applied.
pmvIops :: Lens' PendingModifiedValues (Maybe Int)
pmvIops = lens _pmvIops (\s a -> s { _pmvIops = a })

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword =
    lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\s a -> s { _pmvMultiAZ = a })

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\s a -> s { _pmvPort = a })

-- | Specifies storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\s a -> s { _pmvStorageType = a })

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues

data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmStatus             :: Maybe Text
    , _vsgmVpcSecurityGroupId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VpcSecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsgmStatus' @::@ 'Maybe' 'Text'
--
-- * 'vsgmVpcSecurityGroupId' @::@ 'Maybe' 'Text'
--
vpcSecurityGroupMembership :: VpcSecurityGroupMembership
vpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus             = Nothing
    }

-- | The status of the VPC security group.
vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })

-- | The name of the VPC security group.
vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId =
    lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership

data Parameter = Parameter
    { _pAllowedValues        :: Maybe Text
    , _pApplyMethod          :: Maybe Text
    , _pApplyType            :: Maybe Text
    , _pDataType             :: Maybe Text
    , _pDescription          :: Maybe Text
    , _pIsModifiable         :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    , _pParameterName        :: Maybe Text
    , _pParameterValue       :: Maybe Text
    , _pSource               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Parameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'pApplyMethod' @::@ 'Maybe' 'Text'
--
-- * 'pApplyType' @::@ 'Maybe' 'Text'
--
-- * 'pDataType' @::@ 'Maybe' 'Text'
--
-- * 'pDescription' @::@ 'Maybe' 'Text'
--
-- * 'pIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'pMinimumEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'pParameterName' @::@ 'Maybe' 'Text'
--
-- * 'pParameterValue' @::@ 'Maybe' 'Text'
--
-- * 'pSource' @::@ 'Maybe' 'Text'
--
parameter :: Parameter
parameter = Parameter
    { _pParameterName        = Nothing
    , _pParameterValue       = Nothing
    , _pDescription          = Nothing
    , _pSource               = Nothing
    , _pApplyType            = Nothing
    , _pDataType             = Nothing
    , _pAllowedValues        = Nothing
    , _pIsModifiable         = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pApplyMethod          = Nothing
    }

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe Text)
pApplyMethod = lens _pApplyMethod (\s a -> s { _pApplyMethod = a })

-- | Specifies the engine specific parameters type.
pApplyType :: Lens' Parameter (Maybe Text)
pApplyType = lens _pApplyType (\s a -> s { _pApplyType = a })

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | Indicates whether (true) or not (false) the parameter can be modified.
-- Some parameters have security or operational implications that prevent
-- them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter

data OptionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosAllowedValues      :: Maybe Text
    , _ogosApplyType          :: Maybe Text
    , _ogosDefaultValue       :: Maybe Text
    , _ogosIsModifiable       :: Maybe Bool
    , _ogosSettingDescription :: Maybe Text
    , _ogosSettingName        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'OptionGroupOptionSetting' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogosAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'ogosApplyType' @::@ 'Maybe' 'Text'
--
-- * 'ogosDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'ogosIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'ogosSettingDescription' @::@ 'Maybe' 'Text'
--
-- * 'ogosSettingName' @::@ 'Maybe' 'Text'
--
optionGroupOptionSetting :: OptionGroupOptionSetting
optionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosSettingName        = Nothing
    , _ogosSettingDescription = Nothing
    , _ogosDefaultValue       = Nothing
    , _ogosApplyType          = Nothing
    , _ogosAllowedValues      = Nothing
    , _ogosIsModifiable       = Nothing
    }

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues =
    lens _ogosAllowedValues (\s a -> s { _ogosAllowedValues = a })

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\s a -> s { _ogosApplyType = a })

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\s a -> s { _ogosDefaultValue = a })

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\s a -> s { _ogosIsModifiable = a })

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription =
    lens _ogosSettingDescription (\s a -> s { _ogosSettingDescription = a })

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\s a -> s { _ogosSettingName = a })

instance FromXML OptionGroupOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOptionSetting"

instance ToQuery OptionGroupOptionSetting
