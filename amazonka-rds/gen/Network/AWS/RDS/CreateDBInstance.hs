{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      CreateDBInstance
    -- ** Request constructor
    , mkCreateDBInstance
    -- ** Request lenses
    , cdbiDBName
    , cdbiDBInstanceIdentifier
    , cdbiAllocatedStorage
    , cdbiDBInstanceClass
    , cdbiEngine
    , cdbiMasterUsername
    , cdbiMasterUserPassword
    , cdbiDBSecurityGroups
    , cdbiVpcSecurityGroupIds
    , cdbiAvailabilityZone
    , cdbiDBSubnetGroupName
    , cdbiPreferredMaintenanceWindow
    , cdbiDBParameterGroupName
    , cdbiBackupRetentionPeriod
    , cdbiPreferredBackupWindow
    , cdbiPort
    , cdbiMultiAZ
    , cdbiEngineVersion
    , cdbiAutoMinorVersionUpgrade
    , cdbiLicenseModel
    , cdbiIops
    , cdbiOptionGroupName
    , cdbiCharacterSetName
    , cdbiPubliclyAccessible
    , cdbiTags

    -- * Response
    , CreateDBInstanceResponse
    -- ** Response constructor
    , mkCreateDBInstanceResponse
    -- ** Response lenses
    , cdbirDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateDBInstance = CreateDBInstance
    { _cdbiDBName :: !(Maybe Text)
    , _cdbiDBInstanceIdentifier :: !Text
    , _cdbiAllocatedStorage :: !Integer
    , _cdbiDBInstanceClass :: !Text
    , _cdbiEngine :: !Text
    , _cdbiMasterUsername :: !Text
    , _cdbiMasterUserPassword :: !Text
    , _cdbiDBSecurityGroups :: [Text]
    , _cdbiVpcSecurityGroupIds :: [Text]
    , _cdbiAvailabilityZone :: !(Maybe Text)
    , _cdbiDBSubnetGroupName :: !(Maybe Text)
    , _cdbiPreferredMaintenanceWindow :: !(Maybe Text)
    , _cdbiDBParameterGroupName :: !(Maybe Text)
    , _cdbiBackupRetentionPeriod :: !(Maybe Integer)
    , _cdbiPreferredBackupWindow :: !(Maybe Text)
    , _cdbiPort :: !(Maybe Integer)
    , _cdbiMultiAZ :: !(Maybe Bool)
    , _cdbiEngineVersion :: !(Maybe Text)
    , _cdbiAutoMinorVersionUpgrade :: !(Maybe Bool)
    , _cdbiLicenseModel :: !(Maybe Text)
    , _cdbiIops :: !(Maybe Integer)
    , _cdbiOptionGroupName :: !(Maybe Text)
    , _cdbiCharacterSetName :: !(Maybe Text)
    , _cdbiPubliclyAccessible :: !(Maybe Bool)
    , _cdbiTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBName ::@ @Maybe Text@
--
-- * @DBInstanceIdentifier ::@ @Text@
--
-- * @AllocatedStorage ::@ @Integer@
--
-- * @DBInstanceClass ::@ @Text@
--
-- * @Engine ::@ @Text@
--
-- * @MasterUsername ::@ @Text@
--
-- * @MasterUserPassword ::@ @Text@
--
-- * @DBSecurityGroups ::@ @[Text]@
--
-- * @VpcSecurityGroupIds ::@ @[Text]@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @DBSubnetGroupName ::@ @Maybe Text@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @DBParameterGroupName ::@ @Maybe Text@
--
-- * @BackupRetentionPeriod ::@ @Maybe Integer@
--
-- * @PreferredBackupWindow ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @LicenseModel ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @CharacterSetName ::@ @Maybe Text@
--
-- * @PubliclyAccessible ::@ @Maybe Bool@
--
-- * @Tags ::@ @[Tag]@
--
mkCreateDBInstance :: Text -- ^ 'cdbiDBInstanceIdentifier'
                   -> Integer -- ^ 'cdbiAllocatedStorage'
                   -> Text -- ^ 'cdbiDBInstanceClass'
                   -> Text -- ^ 'cdbiEngine'
                   -> Text -- ^ 'cdbiMasterUsername'
                   -> Text -- ^ 'cdbiMasterUserPassword'
                   -> CreateDBInstance
mkCreateDBInstance p2 p3 p4 p5 p6 p7 = CreateDBInstance
    { _cdbiDBName = Nothing
    , _cdbiDBInstanceIdentifier = p2
    , _cdbiAllocatedStorage = p3
    , _cdbiDBInstanceClass = p4
    , _cdbiEngine = p5
    , _cdbiMasterUsername = p6
    , _cdbiMasterUserPassword = p7
    , _cdbiDBSecurityGroups = mempty
    , _cdbiVpcSecurityGroupIds = mempty
    , _cdbiAvailabilityZone = Nothing
    , _cdbiDBSubnetGroupName = Nothing
    , _cdbiPreferredMaintenanceWindow = Nothing
    , _cdbiDBParameterGroupName = Nothing
    , _cdbiBackupRetentionPeriod = Nothing
    , _cdbiPreferredBackupWindow = Nothing
    , _cdbiPort = Nothing
    , _cdbiMultiAZ = Nothing
    , _cdbiEngineVersion = Nothing
    , _cdbiAutoMinorVersionUpgrade = Nothing
    , _cdbiLicenseModel = Nothing
    , _cdbiIops = Nothing
    , _cdbiOptionGroupName = Nothing
    , _cdbiCharacterSetName = Nothing
    , _cdbiPubliclyAccessible = Nothing
    , _cdbiTags = mempty
    }

-- | The meaning of this parameter differs according to the database engine you
-- use. MySQL The name of the database to create when the DB instance is
-- created. If this parameter is not specified, no database is created in the
-- DB instance. Constraints: Must contain 1 to 64 alphanumeric characters
-- Cannot be a word reserved by the specified database engine Type: String
-- Oracle The Oracle System ID (SID) of the created DB instance. Default: ORCL
-- Constraints: Cannot be longer than 8 characters SQL Server Not applicable.
-- Must be null.
cdbiDBName :: Lens' CreateDBInstance (Maybe Text)
cdbiDBName = lens _cdbiDBName (\s a -> s { _cdbiDBName = a })

-- | The DB instance identifier. This parameter is stored as a lowercase string.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or hyphens
-- (1 to 15 for SQL Server). First character must be a letter. Cannot end with
-- a hyphen or contain two consecutive hyphens. Example: mydbinstance.
cdbiDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdbiDBInstanceIdentifier =
    lens _cdbiDBInstanceIdentifier
         (\s a -> s { _cdbiDBInstanceIdentifier = a })

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance. MySQL Constraints: Must be an integer from 5 to 1024.
-- Type: Integer Oracle Constraints: Must be an integer from 10 to 1024. SQL
-- Server Constraints: Must be an integer from 200 to 1024 (Standard Edition
-- and Enterprise Edition) or from 30 to 1024 (Express Edition and Web
-- Edition).
cdbiAllocatedStorage :: Lens' CreateDBInstance Integer
cdbiAllocatedStorage =
    lens _cdbiAllocatedStorage (\s a -> s { _cdbiAllocatedStorage = a })

-- | The compute and memory capacity of the DB instance. Valid Values:
-- db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge |
-- db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge.
cdbiDBInstanceClass :: Lens' CreateDBInstance Text
cdbiDBInstanceClass =
    lens _cdbiDBInstanceClass (\s a -> s { _cdbiDBInstanceClass = a })

-- | The name of the database engine to be used for this instance. Valid Values:
-- MySQL | oracle-se1 | oracle-se | oracle-ee | sqlserver-ee | sqlserver-se |
-- sqlserver-ex | sqlserver-web.
cdbiEngine :: Lens' CreateDBInstance Text
cdbiEngine = lens _cdbiEngine (\s a -> s { _cdbiEngine = a })

-- | The name of master user for the client DB instance. MySQL Constraints: Must
-- be 1 to 16 alphanumeric characters. First character must be a letter.
-- Cannot be a reserved word for the chosen database engine. Type: String
-- Oracle Constraints: Must be 1 to 30 alphanumeric characters. First
-- character must be a letter. Cannot be a reserved word for the chosen
-- database engine. SQL Server Constraints: Must be 1 to 128 alphanumeric
-- characters. First character must be a letter. Cannot be a reserved word for
-- the chosen database engine.
cdbiMasterUsername :: Lens' CreateDBInstance Text
cdbiMasterUsername =
    lens _cdbiMasterUsername (\s a -> s { _cdbiMasterUsername = a })

-- | The password for the master database user. Can be any printable ASCII
-- character except "/", """, or "@". Type: String MySQL Constraints: Must
-- contain from 8 to 41 characters. Oracle Constraints: Must contain from 8 to
-- 30 characters. SQL Server Constraints: Must contain from 8 to 128
-- characters.
cdbiMasterUserPassword :: Lens' CreateDBInstance Text
cdbiMasterUserPassword =
    lens _cdbiMasterUserPassword (\s a -> s { _cdbiMasterUserPassword = a })

-- | A list of DB security groups to associate with this DB instance. Default:
-- The default DB security group for the database engine.
cdbiDBSecurityGroups :: Lens' CreateDBInstance [Text]
cdbiDBSecurityGroups =
    lens _cdbiDBSecurityGroups (\s a -> s { _cdbiDBSecurityGroups = a })

-- | A list of EC2 VPC security groups to associate with this DB instance.
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
cdbiVpcSecurityGroupIds :: Lens' CreateDBInstance [Text]
cdbiVpcSecurityGroupIds =
    lens _cdbiVpcSecurityGroupIds
         (\s a -> s { _cdbiVpcSecurityGroupIds = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d Constraint: The AvailabilityZone parameter
-- cannot be specified if the MultiAZ parameter is set to true. The specified
-- Availability Zone must be in the same region as the current endpoint.
cdbiAvailabilityZone :: Lens' CreateDBInstance (Maybe Text)
cdbiAvailabilityZone =
    lens _cdbiAvailabilityZone (\s a -> s { _cdbiAvailabilityZone = a })

-- | A DB subnet group to associate with this DB instance. If there is no DB
-- subnet group, then it is a non-VPC DB instance.
cdbiDBSubnetGroupName :: Lens' CreateDBInstance (Maybe Text)
cdbiDBSubnetGroupName =
    lens _cdbiDBSubnetGroupName (\s a -> s { _cdbiDBSubnetGroupName = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window selected at
-- random from an 8-hour block of time per region, occurring on a random day
-- of the week. To see the time blocks available, see Adjusting the Preferred
-- Maintenance Window in the Amazon RDS User Guide. Valid Days: Mon, Tue, Wed,
-- Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdbiPreferredMaintenanceWindow :: Lens' CreateDBInstance (Maybe Text)
cdbiPreferredMaintenanceWindow =
    lens _cdbiPreferredMaintenanceWindow
         (\s a -> s { _cdbiPreferredMaintenanceWindow = a })

-- | The name of the DB parameter group to associate with this DB instance. If
-- this argument is omitted, the default DBParameterGroup for the specified
-- engine will be used. Constraints: Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
cdbiDBParameterGroupName :: Lens' CreateDBInstance (Maybe Text)
cdbiDBParameterGroupName =
    lens _cdbiDBParameterGroupName
         (\s a -> s { _cdbiDBParameterGroupName = a })

-- | The number of days for which automated backups are retained. Setting this
-- parameter to a positive number enables backups. Setting this parameter to 0
-- disables automated backups. Default: 1 Constraints: Must be a value from 0
-- to 35 Cannot be set to 0 if the DB instance is a source to read replicas.
cdbiBackupRetentionPeriod :: Lens' CreateDBInstance (Maybe Integer)
cdbiBackupRetentionPeriod =
    lens _cdbiBackupRetentionPeriod
         (\s a -> s { _cdbiBackupRetentionPeriod = a })

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the BackupRetentionPeriod parameter.
-- Default: A 30-minute window selected at random from an 8-hour block of time
-- per region. See the Amazon RDS User Guide for the time blocks for each
-- region from which the default backup windows are assigned. Constraints:
-- Must be in the format hh24:mi-hh24:mi. Times should be Universal Time
-- Coordinated (UTC). Must not conflict with the preferred maintenance window.
-- Must be at least 30 minutes.
cdbiPreferredBackupWindow :: Lens' CreateDBInstance (Maybe Text)
cdbiPreferredBackupWindow =
    lens _cdbiPreferredBackupWindow
         (\s a -> s { _cdbiPreferredBackupWindow = a })

-- | The port number on which the database accepts connections. MySQL Default:
-- 3306 Valid Values: 1150-65535 Type: Integer Oracle Default: 1521 Valid
-- Values: 1150-65535 SQL Server Default: 1433 Valid Values: 1150-65535 except
-- for 1434 and 3389.
cdbiPort :: Lens' CreateDBInstance (Maybe Integer)
cdbiPort = lens _cdbiPort (\s a -> s { _cdbiPort = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set the
-- AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdbiMultiAZ :: Lens' CreateDBInstance (Maybe Bool)
cdbiMultiAZ = lens _cdbiMultiAZ (\s a -> s { _cdbiMultiAZ = a })

-- | The version number of the database engine to use. MySQL Example: 5.1.42
-- Type: String Oracle Example: 11.2.0.2.v2 Type: String SQL Server Example:
-- 10.50.2789.0.v1.
cdbiEngineVersion :: Lens' CreateDBInstance (Maybe Text)
cdbiEngineVersion =
    lens _cdbiEngineVersion (\s a -> s { _cdbiEngineVersion = a })

-- | Indicates that minor engine upgrades will be applied automatically to the
-- DB instance during the maintenance window. Default: true.
cdbiAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdbiAutoMinorVersionUpgrade =
    lens _cdbiAutoMinorVersionUpgrade
         (\s a -> s { _cdbiAutoMinorVersionUpgrade = a })

-- | License model information for this DB instance. Valid values:
-- license-included | bring-your-own-license | general-public-license.
cdbiLicenseModel :: Lens' CreateDBInstance (Maybe Text)
cdbiLicenseModel =
    lens _cdbiLicenseModel (\s a -> s { _cdbiLicenseModel = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000.
cdbiIops :: Lens' CreateDBInstance (Maybe Integer)
cdbiIops = lens _cdbiIops (\s a -> s { _cdbiIops = a })

-- | Indicates that the DB instance should be associated with the specified
-- option group. cannot be removed from an option group while DB instances are
-- associated with the option group. --> Permanent options, such as the TDE
-- option for Oracle Advanced Security TDE, cannot be removed from an option
-- group, and that option group cannot be removed from a DB instance once it
-- is associated with a DB instance.
cdbiOptionGroupName :: Lens' CreateDBInstance (Maybe Text)
cdbiOptionGroupName =
    lens _cdbiOptionGroupName (\s a -> s { _cdbiOptionGroupName = a })

-- | For supported engines, indicates that the DB instance should be associated
-- with the specified CharacterSet.
cdbiCharacterSetName :: Lens' CreateDBInstance (Maybe Text)
cdbiCharacterSetName =
    lens _cdbiCharacterSetName (\s a -> s { _cdbiCharacterSetName = a })

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
cdbiPubliclyAccessible :: Lens' CreateDBInstance (Maybe Bool)
cdbiPubliclyAccessible =
    lens _cdbiPubliclyAccessible (\s a -> s { _cdbiPubliclyAccessible = a })

-- | A list of tags.
cdbiTags :: Lens' CreateDBInstance [Tag]
cdbiTags = lens _cdbiTags (\s a -> s { _cdbiTags = a })

instance ToQuery CreateDBInstance where
    toQuery = genericQuery def

newtype CreateDBInstanceResponse = CreateDBInstanceResponse
    { _cdbirDBInstance :: Maybe DBInstance
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstance ::@ @Maybe DBInstance@
--
mkCreateDBInstanceResponse :: CreateDBInstanceResponse
mkCreateDBInstanceResponse = CreateDBInstanceResponse
    { _cdbirDBInstance = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
cdbirDBInstance :: Lens' CreateDBInstanceResponse (Maybe DBInstance)
cdbirDBInstance = lens _cdbirDBInstance (\s a -> s { _cdbirDBInstance = a })

instance FromXML CreateDBInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBInstance where
    type Sv CreateDBInstance = RDS
    type Rs CreateDBInstance = CreateDBInstanceResponse

    request = post "CreateDBInstance"
    response _ = xmlResponse
