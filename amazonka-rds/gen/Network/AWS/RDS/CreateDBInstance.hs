{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB instance.
module Network.AWS.RDS.CreateDBInstance
    (
    -- * Request
      CreateDBInstanceMessage
    -- ** Request constructor
    , createDBInstanceMessage
    -- ** Request lenses
    , cdbimAllocatedStorage
    , cdbimAutoMinorVersionUpgrade
    , cdbimAvailabilityZone
    , cdbimBackupRetentionPeriod
    , cdbimCharacterSetName
    , cdbimDBInstanceClass
    , cdbimDBInstanceIdentifier
    , cdbimDBName
    , cdbimDBParameterGroupName
    , cdbimDBSecurityGroups
    , cdbimDBSubnetGroupName
    , cdbimEngine
    , cdbimEngineVersion
    , cdbimIops
    , cdbimLicenseModel
    , cdbimMasterUserPassword
    , cdbimMasterUsername
    , cdbimMultiAZ
    , cdbimOptionGroupName
    , cdbimPort
    , cdbimPreferredBackupWindow
    , cdbimPreferredMaintenanceWindow
    , cdbimPubliclyAccessible
    , cdbimStorageType
    , cdbimTags
    , cdbimTdeCredentialArn
    , cdbimTdeCredentialPassword
    , cdbimVpcSecurityGroupIds

    -- * Response
    , CreateDBInstanceResult
    -- ** Response constructor
    , createDBInstanceResult
    -- ** Response lenses
    , cdbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBInstanceMessage = CreateDBInstanceMessage
    { _cdbimAllocatedStorage           :: Int
    , _cdbimAutoMinorVersionUpgrade    :: Maybe Bool
    , _cdbimAvailabilityZone           :: Maybe Text
    , _cdbimBackupRetentionPeriod      :: Maybe Int
    , _cdbimCharacterSetName           :: Maybe Text
    , _cdbimDBInstanceClass            :: Text
    , _cdbimDBInstanceIdentifier       :: Text
    , _cdbimDBName                     :: Maybe Text
    , _cdbimDBParameterGroupName       :: Maybe Text
    , _cdbimDBSecurityGroups           :: [Text]
    , _cdbimDBSubnetGroupName          :: Maybe Text
    , _cdbimEngine                     :: Text
    , _cdbimEngineVersion              :: Maybe Text
    , _cdbimIops                       :: Maybe Int
    , _cdbimLicenseModel               :: Maybe Text
    , _cdbimMasterUserPassword         :: Text
    , _cdbimMasterUsername             :: Text
    , _cdbimMultiAZ                    :: Maybe Bool
    , _cdbimOptionGroupName            :: Maybe Text
    , _cdbimPort                       :: Maybe Int
    , _cdbimPreferredBackupWindow      :: Maybe Text
    , _cdbimPreferredMaintenanceWindow :: Maybe Text
    , _cdbimPubliclyAccessible         :: Maybe Bool
    , _cdbimStorageType                :: Maybe Text
    , _cdbimTags                       :: [Tag]
    , _cdbimTdeCredentialArn           :: Maybe Text
    , _cdbimTdeCredentialPassword      :: Maybe Text
    , _cdbimVpcSecurityGroupIds        :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbimAllocatedStorage' @::@ 'Int'
--
-- * 'cdbimAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cdbimAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cdbimBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'cdbimCharacterSetName' @::@ 'Maybe' 'Text'
--
-- * 'cdbimDBInstanceClass' @::@ 'Text'
--
-- * 'cdbimDBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbimDBName' @::@ 'Maybe' 'Text'
--
-- * 'cdbimDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbimDBSecurityGroups' @::@ ['Text']
--
-- * 'cdbimDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbimEngine' @::@ 'Text'
--
-- * 'cdbimEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'cdbimIops' @::@ 'Maybe' 'Int'
--
-- * 'cdbimLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'cdbimMasterUserPassword' @::@ 'Text'
--
-- * 'cdbimMasterUsername' @::@ 'Text'
--
-- * 'cdbimMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'cdbimOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbimPort' @::@ 'Maybe' 'Int'
--
-- * 'cdbimPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
-- * 'cdbimPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'cdbimPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'cdbimStorageType' @::@ 'Maybe' 'Text'
--
-- * 'cdbimTags' @::@ ['Tag']
--
-- * 'cdbimTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'cdbimTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
-- * 'cdbimVpcSecurityGroupIds' @::@ ['Text']
--
createDBInstanceMessage :: Text -- ^ 'cdbimDBInstanceIdentifier'
                        -> Int -- ^ 'cdbimAllocatedStorage'
                        -> Text -- ^ 'cdbimDBInstanceClass'
                        -> Text -- ^ 'cdbimEngine'
                        -> Text -- ^ 'cdbimMasterUsername'
                        -> Text -- ^ 'cdbimMasterUserPassword'
                        -> CreateDBInstanceMessage
createDBInstanceMessage p1 p2 p3 p4 p5 p6 = CreateDBInstanceMessage
    { _cdbimDBInstanceIdentifier       = p1
    , _cdbimAllocatedStorage           = p2
    , _cdbimDBInstanceClass            = p3
    , _cdbimEngine                     = p4
    , _cdbimMasterUsername             = p5
    , _cdbimMasterUserPassword         = p6
    , _cdbimDBName                     = Nothing
    , _cdbimDBSecurityGroups           = mempty
    , _cdbimVpcSecurityGroupIds        = mempty
    , _cdbimAvailabilityZone           = Nothing
    , _cdbimDBSubnetGroupName          = Nothing
    , _cdbimPreferredMaintenanceWindow = Nothing
    , _cdbimDBParameterGroupName       = Nothing
    , _cdbimBackupRetentionPeriod      = Nothing
    , _cdbimPreferredBackupWindow      = Nothing
    , _cdbimPort                       = Nothing
    , _cdbimMultiAZ                    = Nothing
    , _cdbimEngineVersion              = Nothing
    , _cdbimAutoMinorVersionUpgrade    = Nothing
    , _cdbimLicenseModel               = Nothing
    , _cdbimIops                       = Nothing
    , _cdbimOptionGroupName            = Nothing
    , _cdbimCharacterSetName           = Nothing
    , _cdbimPubliclyAccessible         = Nothing
    , _cdbimTags                       = mempty
    , _cdbimStorageType                = Nothing
    , _cdbimTdeCredentialArn           = Nothing
    , _cdbimTdeCredentialPassword      = Nothing
    }

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance. Type: Integer MySQL Constraints: Must be an integer
-- from 5 to 3072. PostgreSQL Constraints: Must be an integer from 5 to
-- 3072. Oracle Constraints: Must be an integer from 10 to 3072. SQL Server
-- Constraints: Must be an integer from 200 to 1024 (Standard Edition and
-- Enterprise Edition) or from 30 to 1024 (Express Edition and Web Edition).
cdbimAllocatedStorage :: Lens' CreateDBInstanceMessage Int
cdbimAllocatedStorage =
    lens _cdbimAllocatedStorage (\s a -> s { _cdbimAllocatedStorage = a })

-- | Indicates that minor engine upgrades will be applied automatically to the
-- DB instance during the maintenance window. Default: true.
cdbimAutoMinorVersionUpgrade :: Lens' CreateDBInstanceMessage (Maybe Bool)
cdbimAutoMinorVersionUpgrade =
    lens _cdbimAutoMinorVersionUpgrade
        (\s a -> s { _cdbimAutoMinorVersionUpgrade = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d Constraint: The AvailabilityZone parameter
-- cannot be specified if the MultiAZ parameter is set to true. The
-- specified Availability Zone must be in the same region as the current
-- endpoint.
cdbimAvailabilityZone :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimAvailabilityZone =
    lens _cdbimAvailabilityZone (\s a -> s { _cdbimAvailabilityZone = a })

-- | The number of days for which automated backups are retained. Setting this
-- parameter to a positive number enables backups. Setting this parameter to
-- 0 disables automated backups. Default: 1 Constraints: Must be a value
-- from 0 to 35 Cannot be set to 0 if the DB instance is a source to read
-- replicas.
cdbimBackupRetentionPeriod :: Lens' CreateDBInstanceMessage (Maybe Int)
cdbimBackupRetentionPeriod =
    lens _cdbimBackupRetentionPeriod
        (\s a -> s { _cdbimBackupRetentionPeriod = a })

-- | For supported engines, indicates that the DB instance should be
-- associated with the specified CharacterSet.
cdbimCharacterSetName :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimCharacterSetName =
    lens _cdbimCharacterSetName (\s a -> s { _cdbimCharacterSetName = a })

-- | The compute and memory capacity of the DB instance. Valid Values:
-- db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge |
-- db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large
-- | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |
-- db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small
-- | db.t2.medium.
cdbimDBInstanceClass :: Lens' CreateDBInstanceMessage Text
cdbimDBInstanceClass =
    lens _cdbimDBInstanceClass (\s a -> s { _cdbimDBInstanceClass = a })

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 63 alphanumeric characters or
-- hyphens (1 to 15 for SQL Server). First character must be a letter.
-- Cannot end with a hyphen or contain two consecutive hyphens. Example:
-- mydbinstance.
cdbimDBInstanceIdentifier :: Lens' CreateDBInstanceMessage Text
cdbimDBInstanceIdentifier =
    lens _cdbimDBInstanceIdentifier
        (\s a -> s { _cdbimDBInstanceIdentifier = a })

-- | The meaning of this parameter differs according to the database engine
-- you use. Type: String MySQL The name of the database to create when the
-- DB instance is created. If this parameter is not specified, no database
-- is created in the DB instance. Constraints: Must contain 1 to 64
-- alphanumeric characters Cannot be a word reserved by the specified
-- database engine PostgreSQL The name of the database to create when the DB
-- instance is created. If this parameter is not specified, no database is
-- created in the DB instance. Constraints: Must contain 1 to 63
-- alphanumeric characters Must begin with a letter or an underscore.
-- Subsequent characters can be letters, underscores, or digits (0-9).
-- Cannot be a word reserved by the specified database engine Oracle The
-- Oracle System ID (SID) of the created DB instance. Default: ORCL
-- Constraints: Cannot be longer than 8 characters SQL Server Not
-- applicable. Must be null.
cdbimDBName :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimDBName = lens _cdbimDBName (\s a -> s { _cdbimDBName = a })

-- | The name of the DB parameter group to associate with this DB instance. If
-- this argument is omitted, the default DBParameterGroup for the specified
-- engine will be used. Constraints: Must be 1 to 255 alphanumeric
-- characters First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens.
cdbimDBParameterGroupName :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimDBParameterGroupName =
    lens _cdbimDBParameterGroupName
        (\s a -> s { _cdbimDBParameterGroupName = a })

-- | A list of DB security groups to associate with this DB instance. Default:
-- The default DB security group for the database engine.
cdbimDBSecurityGroups :: Lens' CreateDBInstanceMessage [Text]
cdbimDBSecurityGroups =
    lens _cdbimDBSecurityGroups (\s a -> s { _cdbimDBSecurityGroups = a })

-- | A DB subnet group to associate with this DB instance. If there is no DB
-- subnet group, then it is a non-VPC DB instance.
cdbimDBSubnetGroupName :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimDBSubnetGroupName =
    lens _cdbimDBSubnetGroupName (\s a -> s { _cdbimDBSubnetGroupName = a })

-- | The name of the database engine to be used for this instance. Valid
-- Values: MySQL | oracle-se1 | oracle-se | oracle-ee | sqlserver-ee |
-- sqlserver-se | sqlserver-ex | sqlserver-web | postgres.
cdbimEngine :: Lens' CreateDBInstanceMessage Text
cdbimEngine = lens _cdbimEngine (\s a -> s { _cdbimEngine = a })

-- | The version number of the database engine to use. MySQL Example: 5.1.42
-- Type: String PostgreSQL Example: 9.3 Type: String Oracle Example:
-- 11.2.0.2.v2 Type: String SQL Server Example: 10.50.2789.0.v1.
cdbimEngineVersion :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimEngineVersion =
    lens _cdbimEngineVersion (\s a -> s { _cdbimEngineVersion = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: To use PIOPS, this
-- value must be an integer greater than 1000.
cdbimIops :: Lens' CreateDBInstanceMessage (Maybe Int)
cdbimIops = lens _cdbimIops (\s a -> s { _cdbimIops = a })

-- | License model information for this DB instance. Valid values:
-- license-included | bring-your-own-license | general-public-license.
cdbimLicenseModel :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimLicenseModel =
    lens _cdbimLicenseModel (\s a -> s { _cdbimLicenseModel = a })

-- | The password for the master database user. Can be any printable ASCII
-- character except "/", """, or "@". Type: String MySQL Constraints: Must
-- contain from 8 to 41 characters. Oracle Constraints: Must contain from 8
-- to 30 characters. SQL Server Constraints: Must contain from 8 to 128
-- characters.
cdbimMasterUserPassword :: Lens' CreateDBInstanceMessage Text
cdbimMasterUserPassword =
    lens _cdbimMasterUserPassword (\s a -> s { _cdbimMasterUserPassword = a })

-- | The name of master user for the client DB instance. MySQL Constraints:
-- Must be 1 to 16 alphanumeric characters. First character must be a
-- letter. Cannot be a reserved word for the chosen database engine. Type:
-- String Oracle Constraints: Must be 1 to 30 alphanumeric characters. First
-- character must be a letter. Cannot be a reserved word for the chosen
-- database engine. SQL Server Constraints: Must be 1 to 128 alphanumeric
-- characters. First character must be a letter. Cannot be a reserved word
-- for the chosen database engine.
cdbimMasterUsername :: Lens' CreateDBInstanceMessage Text
cdbimMasterUsername =
    lens _cdbimMasterUsername (\s a -> s { _cdbimMasterUsername = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set the
-- AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdbimMultiAZ :: Lens' CreateDBInstanceMessage (Maybe Bool)
cdbimMultiAZ = lens _cdbimMultiAZ (\s a -> s { _cdbimMultiAZ = a })

-- | Indicates that the DB instance should be associated with the specified
-- option group. Permanent options, such as the TDE option for Oracle
-- Advanced Security TDE, cannot be removed from an option group, and that
-- option group cannot be removed from a DB instance once it is associated
-- with a DB instance.
cdbimOptionGroupName :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimOptionGroupName =
    lens _cdbimOptionGroupName (\s a -> s { _cdbimOptionGroupName = a })

-- | The port number on which the database accepts connections. MySQL Default:
-- 3306 Valid Values: 1150-65535 Type: Integer PostgreSQL Default: 5432
-- Valid Values: 1150-65535 Type: Integer Oracle Default: 1521 Valid Values:
-- 1150-65535 SQL Server Default: 1433 Valid Values: 1150-65535 except for
-- 1434, 3389, 47001, 49152, and 49152 through 49156.
cdbimPort :: Lens' CreateDBInstanceMessage (Maybe Int)
cdbimPort = lens _cdbimPort (\s a -> s { _cdbimPort = a })

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the BackupRetentionPeriod parameter.
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. See the Amazon RDS User Guide for the time blocks for
-- each region from which the default backup windows are assigned.
-- Constraints: Must be in the format hh24:mi-hh24:mi. Times should be
-- Universal Time Coordinated (UTC). Must not conflict with the preferred
-- maintenance window. Must be at least 30 minutes.
cdbimPreferredBackupWindow :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimPreferredBackupWindow =
    lens _cdbimPreferredBackupWindow
        (\s a -> s { _cdbimPreferredBackupWindow = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window selected at
-- random from an 8-hour block of time per region, occurring on a random day
-- of the week. To see the time blocks available, see Adjusting the
-- Preferred Maintenance Window in the Amazon RDS User Guide. Valid Days:
-- Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdbimPreferredMaintenanceWindow :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimPreferredMaintenanceWindow =
    lens _cdbimPreferredMaintenanceWindow
        (\s a -> s { _cdbimPreferredMaintenanceWindow = a })

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
cdbimPubliclyAccessible :: Lens' CreateDBInstanceMessage (Maybe Bool)
cdbimPubliclyAccessible =
    lens _cdbimPubliclyAccessible (\s a -> s { _cdbimPubliclyAccessible = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
cdbimStorageType :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimStorageType = lens _cdbimStorageType (\s a -> s { _cdbimStorageType = a })

cdbimTags :: Lens' CreateDBInstanceMessage [Tag]
cdbimTags = lens _cdbimTags (\s a -> s { _cdbimTags = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
cdbimTdeCredentialArn :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimTdeCredentialArn =
    lens _cdbimTdeCredentialArn (\s a -> s { _cdbimTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
cdbimTdeCredentialPassword :: Lens' CreateDBInstanceMessage (Maybe Text)
cdbimTdeCredentialPassword =
    lens _cdbimTdeCredentialPassword
        (\s a -> s { _cdbimTdeCredentialPassword = a })

-- | A list of EC2 VPC security groups to associate with this DB instance.
-- Default: The default EC2 VPC security group for the DB subnet group's
-- VPC.
cdbimVpcSecurityGroupIds :: Lens' CreateDBInstanceMessage [Text]
cdbimVpcSecurityGroupIds =
    lens _cdbimVpcSecurityGroupIds
        (\s a -> s { _cdbimVpcSecurityGroupIds = a })

instance ToPath CreateDBInstanceMessage where
    toPath = const "/"

instance ToQuery CreateDBInstanceMessage

newtype CreateDBInstanceResult = CreateDBInstanceResult
    { _cdbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
createDBInstanceResult :: CreateDBInstanceResult
createDBInstanceResult = CreateDBInstanceResult
    { _cdbirDBInstance = Nothing
    }

cdbirDBInstance :: Lens' CreateDBInstanceResult (Maybe DBInstance)
cdbirDBInstance = lens _cdbirDBInstance (\s a -> s { _cdbirDBInstance = a })

instance AWSRequest CreateDBInstanceMessage where
    type Sv CreateDBInstanceMessage = RDS
    type Rs CreateDBInstanceMessage = CreateDBInstanceResult

    request  = post "CreateDBInstance"
    response = const . xmlResponse $ \h x -> CreateDBInstanceResult
newtype
