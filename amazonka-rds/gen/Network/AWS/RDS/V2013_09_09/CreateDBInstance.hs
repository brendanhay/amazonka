{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB instance. https://rds.amazonaws.com/
-- ?Action=CreateDBInstance &DBInstanceIdentifier=SimCoProd01 &Engine=mysql
-- &MasterUserPassword=Password01 &AllocatedStorage=10 &MasterUsername=master
-- &Version=2013-05-15 &DBInstanceClass=db.m1.large
-- &DBSubnetGroupName=dbSubnetgroup01 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T05%3A54%3A53.578Z
-- &AWSAccessKeyId= &Signature= mysql **** 1 false general-public-license
-- 990524496922 Complete description subnet_grp1 Active subnet-7c5b4115
-- us-east-1c Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57
-- us-east-1d creating 5.1.50 simcoprod01 in-sync default.mysql5.1 active
-- default 00:00-00:30 true sat:07:30-sat:08:00 10 db.m1.large master
-- 2e5d4270-8501-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.V2013_09_09.CreateDBInstance
    (
    -- * Request
      CreateDBInstance
    -- ** Request constructor
    , createDBInstance
    -- ** Request lenses
    , cdbimAllocatedStorage
    , cdbimDBInstanceIdentifier
    , cdbimDBInstanceClass
    , cdbimEngine
    , cdbimMasterUsername
    , cdbimMasterUserPassword
    , cdbimMultiAZ
    , cdbimAutoMinorVersionUpgrade
    , cdbimPubliclyAccessible
    , cdbimDBSecurityGroups
    , cdbimBackupRetentionPeriod
    , cdbimPort
    , cdbimIops
    , cdbimDBName
    , cdbimAvailabilityZone
    , cdbimDBSubnetGroupName
    , cdbimPreferredMaintenanceWindow
    , cdbimDBParameterGroupName
    , cdbimPreferredBackupWindow
    , cdbimEngineVersion
    , cdbimLicenseModel
    , cdbimOptionGroupName
    , cdbimCharacterSetName
    , cdbimTags
    , cdbimVpcSecurityGroupIds

    -- * Response
    , CreateDBInstanceResponse
    -- ** Response lenses
    , dbiwDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDBInstance' request.
createDBInstance :: Integer -- ^ 'cdbimAllocatedStorage'
                 -> Text -- ^ 'cdbimDBInstanceIdentifier'
                 -> Text -- ^ 'cdbimDBInstanceClass'
                 -> Text -- ^ 'cdbimEngine'
                 -> Text -- ^ 'cdbimMasterUsername'
                 -> Text -- ^ 'cdbimMasterUserPassword'
                 -> CreateDBInstance
createDBInstance p1 p2 p3 p4 p5 p6 = CreateDBInstance
    { _cdbimAllocatedStorage = p1
    , _cdbimDBInstanceIdentifier = p2
    , _cdbimDBInstanceClass = p3
    , _cdbimEngine = p4
    , _cdbimMasterUsername = p5
    , _cdbimMasterUserPassword = p6
    , _cdbimMultiAZ = Nothing
    , _cdbimAutoMinorVersionUpgrade = Nothing
    , _cdbimPubliclyAccessible = Nothing
    , _cdbimDBSecurityGroups = mempty
    , _cdbimBackupRetentionPeriod = Nothing
    , _cdbimPort = Nothing
    , _cdbimIops = Nothing
    , _cdbimDBName = Nothing
    , _cdbimAvailabilityZone = Nothing
    , _cdbimDBSubnetGroupName = Nothing
    , _cdbimPreferredMaintenanceWindow = Nothing
    , _cdbimDBParameterGroupName = Nothing
    , _cdbimPreferredBackupWindow = Nothing
    , _cdbimEngineVersion = Nothing
    , _cdbimLicenseModel = Nothing
    , _cdbimOptionGroupName = Nothing
    , _cdbimCharacterSetName = Nothing
    , _cdbimTags = mempty
    , _cdbimVpcSecurityGroupIds = mempty
    }

data CreateDBInstance = CreateDBInstance
    { _cdbimAllocatedStorage :: Integer
      -- ^ The amount of storage (in gigabytes) to be initially allocated
      -- for the database instance. MySQL Constraints: Must be an integer
      -- from 5 to 1024. Type: Integer Oracle Constraints: Must be an
      -- integer from 10 to 1024. SQL Server Constraints: Must be an
      -- integer from 200 to 1024 (Standard Edition and Enterprise
      -- Edition) or from 30 to 1024 (Express Edition and Web Edition).
    , _cdbimDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier. This parameter is stored as a
      -- lowercase string. Constraints: Must contain from 1 to 63
      -- alphanumeric characters or hyphens (1 to 15 for SQL Server).
      -- First character must be a letter. Cannot end with a hyphen or
      -- contain two consecutive hyphens. Example: mydbinstance.
    , _cdbimDBInstanceClass :: Text
      -- ^ The compute and memory capacity of the DB instance. Valid Values:
      -- db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
      -- db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge.
    , _cdbimEngine :: Text
      -- ^ The name of the database engine to be used for this instance.
      -- Valid Values: MySQL | oracle-se1 | oracle-se | oracle-ee |
      -- sqlserver-ee | sqlserver-se | sqlserver-ex | sqlserver-web.
    , _cdbimMasterUsername :: Text
      -- ^ The name of master user for the client DB instance. MySQL
      -- Constraints: Must be 1 to 16 alphanumeric characters. First
      -- character must be a letter. Cannot be a reserved word for the
      -- chosen database engine. Type: String Oracle Constraints: Must be
      -- 1 to 30 alphanumeric characters. First character must be a
      -- letter. Cannot be a reserved word for the chosen database engine.
      -- SQL Server Constraints: Must be 1 to 128 alphanumeric characters.
      -- First character must be a letter. Cannot be a reserved word for
      -- the chosen database engine.
    , _cdbimMasterUserPassword :: Text
      -- ^ The password for the master database user. Can be any printable
      -- ASCII character except "/", """, or "@". Type: String MySQL
      -- Constraints: Must contain from 8 to 41 characters. Oracle
      -- Constraints: Must contain from 8 to 30 characters. SQL Server
      -- Constraints: Must contain from 8 to 128 characters.
    , _cdbimMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment. You cannot
      -- set the AvailabilityZone parameter if the MultiAZ parameter is
      -- set to true.
    , _cdbimAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor engine upgrades will be applied
      -- automatically to the DB instance during the maintenance window.
      -- Default: true.
    , _cdbimPubliclyAccessible :: Maybe Bool
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
    , _cdbimDBSecurityGroups :: [Text]
      -- ^ A list of DB security groups to associate with this DB instance.
      -- Default: The default DB security group for the database engine.
    , _cdbimBackupRetentionPeriod :: Maybe Integer
      -- ^ The number of days for which automated backups are retained.
      -- Setting this parameter to a positive number enables backups.
      -- Setting this parameter to 0 disables automated backups. Default:
      -- 1 Constraints: Must be a value from 0 to 35 Cannot be set to 0 if
      -- the DB instance is a source to read replicas.
    , _cdbimPort :: Maybe Integer
      -- ^ The port number on which the database accepts connections. MySQL
      -- Default: 3306 Valid Values: 1150-65535 Type: Integer Oracle
      -- Default: 1521 Valid Values: 1150-65535 SQL Server Default: 1433
      -- Valid Values: 1150-65535 except for 1434 and 3389.
    , _cdbimIops :: Maybe Integer
      -- ^ The amount of Provisioned IOPS (input/output operations per
      -- second) to be initially allocated for the DB instance.
      -- Constraints: Must be an integer greater than 1000.
    , _cdbimDBName :: Maybe Text
      -- ^ The meaning of this parameter differs according to the database
      -- engine you use. MySQL The name of the database to create when the
      -- DB instance is created. If this parameter is not specified, no
      -- database is created in the DB instance. Constraints: Must contain
      -- 1 to 64 alphanumeric characters Cannot be a word reserved by the
      -- specified database engine Type: String Oracle The Oracle System
      -- ID (SID) of the created DB instance. Default: ORCL Constraints:
      -- Cannot be longer than 8 characters SQL Server Not applicable.
      -- Must be null.
    , _cdbimAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be
      -- created in. Default: A random, system-chosen Availability Zone in
      -- the endpoint's region. Example: us-east-1d Constraint: The
      -- AvailabilityZone parameter cannot be specified if the MultiAZ
      -- parameter is set to true. The specified Availability Zone must be
      -- in the same region as the current endpoint.
    , _cdbimDBSubnetGroupName :: Maybe Text
      -- ^ A DB subnet group to associate with this DB instance. If there is
      -- no DB subnet group, then it is a non-VPC DB instance.
    , _cdbimPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur. Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute
      -- window selected at random from an 8-hour block of time per
      -- region, occurring on a random day of the week. To see the time
      -- blocks available, see Adjusting the Preferred Maintenance Window
      -- in the Amazon RDS User Guide. Valid Days: Mon, Tue, Wed, Thu,
      -- Fri, Sat, Sun Constraints: Minimum 30-minute window.
    , _cdbimDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group to associate with this DB
      -- instance. If this argument is omitted, the default
      -- DBParameterGroup for the specified engine will be used.
      -- Constraints: Must be 1 to 255 alphanumeric characters First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens.
    , _cdbimPreferredBackupWindow :: Maybe Text
      -- ^ The daily time range during which automated backups are created
      -- if automated backups are enabled, using the BackupRetentionPeriod
      -- parameter. Default: A 30-minute window selected at random from an
      -- 8-hour block of time per region. See the Amazon RDS User Guide
      -- for the time blocks for each region from which the default backup
      -- windows are assigned. Constraints: Must be in the format
      -- hh24:mi-hh24:mi. Times should be Universal Time Coordinated
      -- (UTC). Must not conflict with the preferred maintenance window.
      -- Must be at least 30 minutes.
    , _cdbimEngineVersion :: Maybe Text
      -- ^ The version number of the database engine to use. MySQL Example:
      -- 5.1.42 Type: String Oracle Example: 11.2.0.2.v2 Type: String SQL
      -- Server Example: 10.50.2789.0.v1.
    , _cdbimLicenseModel :: Maybe Text
      -- ^ License model information for this DB instance. Valid values:
      -- license-included | bring-your-own-license |
      -- general-public-license.
    , _cdbimOptionGroupName :: Maybe Text
      -- ^ Indicates that the DB instance should be associated with the
      -- specified option group. cannot be removed from an option group
      -- while DB instances are associated with the option group. -->
      -- Permanent options, such as the TDE option for Oracle Advanced
      -- Security TDE, cannot be removed from an option group, and that
      -- option group cannot be removed from a DB instance once it is
      -- associated with a DB instance.
    , _cdbimCharacterSetName :: Maybe Text
      -- ^ For supported engines, indicates that the DB instance should be
      -- associated with the specified CharacterSet.
    , _cdbimTags :: [Tag]
      -- ^ A list of tags.
    , _cdbimVpcSecurityGroupIds :: [Text]
      -- ^ A list of EC2 VPC security groups to associate with this DB
      -- instance. Default: The default EC2 VPC security group for the DB
      -- subnet group's VPC.
    } deriving (Show, Generic)

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance. MySQL Constraints: Must be an integer from 5 to 1024.
-- Type: Integer Oracle Constraints: Must be an integer from 10 to 1024. SQL
-- Server Constraints: Must be an integer from 200 to 1024 (Standard Edition
-- and Enterprise Edition) or from 30 to 1024 (Express Edition and Web
-- Edition).
cdbimAllocatedStorage
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimAllocatedStorage f x =
    (\y -> x { _cdbimAllocatedStorage = y })
       <$> f (_cdbimAllocatedStorage x)
{-# INLINE cdbimAllocatedStorage #-}

-- | The DB instance identifier. This parameter is stored as a lowercase string.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or hyphens
-- (1 to 15 for SQL Server). First character must be a letter. Cannot end with
-- a hyphen or contain two consecutive hyphens. Example: mydbinstance.
cdbimDBInstanceIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBInstanceIdentifier f x =
    (\y -> x { _cdbimDBInstanceIdentifier = y })
       <$> f (_cdbimDBInstanceIdentifier x)
{-# INLINE cdbimDBInstanceIdentifier #-}

-- | The compute and memory capacity of the DB instance. Valid Values:
-- db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge |
-- db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge.
cdbimDBInstanceClass
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBInstanceClass f x =
    (\y -> x { _cdbimDBInstanceClass = y })
       <$> f (_cdbimDBInstanceClass x)
{-# INLINE cdbimDBInstanceClass #-}

-- | The name of the database engine to be used for this instance. Valid Values:
-- MySQL | oracle-se1 | oracle-se | oracle-ee | sqlserver-ee | sqlserver-se |
-- sqlserver-ex | sqlserver-web.
cdbimEngine
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimEngine f x =
    (\y -> x { _cdbimEngine = y })
       <$> f (_cdbimEngine x)
{-# INLINE cdbimEngine #-}

-- | The name of master user for the client DB instance. MySQL Constraints: Must
-- be 1 to 16 alphanumeric characters. First character must be a letter.
-- Cannot be a reserved word for the chosen database engine. Type: String
-- Oracle Constraints: Must be 1 to 30 alphanumeric characters. First
-- character must be a letter. Cannot be a reserved word for the chosen
-- database engine. SQL Server Constraints: Must be 1 to 128 alphanumeric
-- characters. First character must be a letter. Cannot be a reserved word for
-- the chosen database engine.
cdbimMasterUsername
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimMasterUsername f x =
    (\y -> x { _cdbimMasterUsername = y })
       <$> f (_cdbimMasterUsername x)
{-# INLINE cdbimMasterUsername #-}

-- | The password for the master database user. Can be any printable ASCII
-- character except "/", """, or "@". Type: String MySQL Constraints: Must
-- contain from 8 to 41 characters. Oracle Constraints: Must contain from 8 to
-- 30 characters. SQL Server Constraints: Must contain from 8 to 128
-- characters.
cdbimMasterUserPassword
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimMasterUserPassword f x =
    (\y -> x { _cdbimMasterUserPassword = y })
       <$> f (_cdbimMasterUserPassword x)
{-# INLINE cdbimMasterUserPassword #-}

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set the
-- AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdbimMultiAZ
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimMultiAZ f x =
    (\y -> x { _cdbimMultiAZ = y })
       <$> f (_cdbimMultiAZ x)
{-# INLINE cdbimMultiAZ #-}

-- | Indicates that minor engine upgrades will be applied automatically to the
-- DB instance during the maintenance window. Default: true.
cdbimAutoMinorVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimAutoMinorVersionUpgrade f x =
    (\y -> x { _cdbimAutoMinorVersionUpgrade = y })
       <$> f (_cdbimAutoMinorVersionUpgrade x)
{-# INLINE cdbimAutoMinorVersionUpgrade #-}

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
cdbimPubliclyAccessible
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimPubliclyAccessible f x =
    (\y -> x { _cdbimPubliclyAccessible = y })
       <$> f (_cdbimPubliclyAccessible x)
{-# INLINE cdbimPubliclyAccessible #-}

-- | A list of DB security groups to associate with this DB instance. Default:
-- The default DB security group for the database engine.
cdbimDBSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBSecurityGroups f x =
    (\y -> x { _cdbimDBSecurityGroups = y })
       <$> f (_cdbimDBSecurityGroups x)
{-# INLINE cdbimDBSecurityGroups #-}

-- | The number of days for which automated backups are retained. Setting this
-- parameter to a positive number enables backups. Setting this parameter to 0
-- disables automated backups. Default: 1 Constraints: Must be a value from 0
-- to 35 Cannot be set to 0 if the DB instance is a source to read replicas.
cdbimBackupRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimBackupRetentionPeriod f x =
    (\y -> x { _cdbimBackupRetentionPeriod = y })
       <$> f (_cdbimBackupRetentionPeriod x)
{-# INLINE cdbimBackupRetentionPeriod #-}

-- | The port number on which the database accepts connections. MySQL Default:
-- 3306 Valid Values: 1150-65535 Type: Integer Oracle Default: 1521 Valid
-- Values: 1150-65535 SQL Server Default: 1433 Valid Values: 1150-65535 except
-- for 1434 and 3389.
cdbimPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimPort f x =
    (\y -> x { _cdbimPort = y })
       <$> f (_cdbimPort x)
{-# INLINE cdbimPort #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000.
cdbimIops
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimIops f x =
    (\y -> x { _cdbimIops = y })
       <$> f (_cdbimIops x)
{-# INLINE cdbimIops #-}

-- | The meaning of this parameter differs according to the database engine you
-- use. MySQL The name of the database to create when the DB instance is
-- created. If this parameter is not specified, no database is created in the
-- DB instance. Constraints: Must contain 1 to 64 alphanumeric characters
-- Cannot be a word reserved by the specified database engine Type: String
-- Oracle The Oracle System ID (SID) of the created DB instance. Default: ORCL
-- Constraints: Cannot be longer than 8 characters SQL Server Not applicable.
-- Must be null.
cdbimDBName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBName f x =
    (\y -> x { _cdbimDBName = y })
       <$> f (_cdbimDBName x)
{-# INLINE cdbimDBName #-}

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d Constraint: The AvailabilityZone parameter
-- cannot be specified if the MultiAZ parameter is set to true. The specified
-- Availability Zone must be in the same region as the current endpoint.
cdbimAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimAvailabilityZone f x =
    (\y -> x { _cdbimAvailabilityZone = y })
       <$> f (_cdbimAvailabilityZone x)
{-# INLINE cdbimAvailabilityZone #-}

-- | A DB subnet group to associate with this DB instance. If there is no DB
-- subnet group, then it is a non-VPC DB instance.
cdbimDBSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBSubnetGroupName f x =
    (\y -> x { _cdbimDBSubnetGroupName = y })
       <$> f (_cdbimDBSubnetGroupName x)
{-# INLINE cdbimDBSubnetGroupName #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window selected at
-- random from an 8-hour block of time per region, occurring on a random day
-- of the week. To see the time blocks available, see Adjusting the Preferred
-- Maintenance Window in the Amazon RDS User Guide. Valid Days: Mon, Tue, Wed,
-- Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdbimPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimPreferredMaintenanceWindow f x =
    (\y -> x { _cdbimPreferredMaintenanceWindow = y })
       <$> f (_cdbimPreferredMaintenanceWindow x)
{-# INLINE cdbimPreferredMaintenanceWindow #-}

-- | The name of the DB parameter group to associate with this DB instance. If
-- this argument is omitted, the default DBParameterGroup for the specified
-- engine will be used. Constraints: Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
cdbimDBParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimDBParameterGroupName f x =
    (\y -> x { _cdbimDBParameterGroupName = y })
       <$> f (_cdbimDBParameterGroupName x)
{-# INLINE cdbimDBParameterGroupName #-}

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the BackupRetentionPeriod parameter.
-- Default: A 30-minute window selected at random from an 8-hour block of time
-- per region. See the Amazon RDS User Guide for the time blocks for each
-- region from which the default backup windows are assigned. Constraints:
-- Must be in the format hh24:mi-hh24:mi. Times should be Universal Time
-- Coordinated (UTC). Must not conflict with the preferred maintenance window.
-- Must be at least 30 minutes.
cdbimPreferredBackupWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimPreferredBackupWindow f x =
    (\y -> x { _cdbimPreferredBackupWindow = y })
       <$> f (_cdbimPreferredBackupWindow x)
{-# INLINE cdbimPreferredBackupWindow #-}

-- | The version number of the database engine to use. MySQL Example: 5.1.42
-- Type: String Oracle Example: 11.2.0.2.v2 Type: String SQL Server Example:
-- 10.50.2789.0.v1.
cdbimEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimEngineVersion f x =
    (\y -> x { _cdbimEngineVersion = y })
       <$> f (_cdbimEngineVersion x)
{-# INLINE cdbimEngineVersion #-}

-- | License model information for this DB instance. Valid values:
-- license-included | bring-your-own-license | general-public-license.
cdbimLicenseModel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimLicenseModel f x =
    (\y -> x { _cdbimLicenseModel = y })
       <$> f (_cdbimLicenseModel x)
{-# INLINE cdbimLicenseModel #-}

-- | Indicates that the DB instance should be associated with the specified
-- option group. cannot be removed from an option group while DB instances are
-- associated with the option group. --> Permanent options, such as the TDE
-- option for Oracle Advanced Security TDE, cannot be removed from an option
-- group, and that option group cannot be removed from a DB instance once it
-- is associated with a DB instance.
cdbimOptionGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimOptionGroupName f x =
    (\y -> x { _cdbimOptionGroupName = y })
       <$> f (_cdbimOptionGroupName x)
{-# INLINE cdbimOptionGroupName #-}

-- | For supported engines, indicates that the DB instance should be associated
-- with the specified CharacterSet.
cdbimCharacterSetName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimCharacterSetName f x =
    (\y -> x { _cdbimCharacterSetName = y })
       <$> f (_cdbimCharacterSetName x)
{-# INLINE cdbimCharacterSetName #-}

-- | A list of tags.
cdbimTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimTags f x =
    (\y -> x { _cdbimTags = y })
       <$> f (_cdbimTags x)
{-# INLINE cdbimTags #-}

-- | A list of EC2 VPC security groups to associate with this DB instance.
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
cdbimVpcSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateDBInstance
    -> f CreateDBInstance
cdbimVpcSecurityGroupIds f x =
    (\y -> x { _cdbimVpcSecurityGroupIds = y })
       <$> f (_cdbimVpcSecurityGroupIds x)
{-# INLINE cdbimVpcSecurityGroupIds #-}

instance ToQuery CreateDBInstance where
    toQuery = genericQuery def

data CreateDBInstanceResponse = CreateDBInstanceResponse
    { _dbiwDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
dbiwDBInstance
    :: Functor f
    => (Maybe DBInstance
    -> f (Maybe DBInstance))
    -> CreateDBInstanceResponse
    -> f CreateDBInstanceResponse
dbiwDBInstance f x =
    (\y -> x { _dbiwDBInstance = y })
       <$> f (_dbiwDBInstance x)
{-# INLINE dbiwDBInstance #-}

instance FromXML CreateDBInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBInstance where
    type Sv CreateDBInstance = RDS
    type Rs CreateDBInstance = CreateDBInstanceResponse

    request = post "CreateDBInstance"
    response _ = xmlResponse
