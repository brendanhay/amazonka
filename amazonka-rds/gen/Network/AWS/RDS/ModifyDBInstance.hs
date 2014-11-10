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

-- Module      : Network.AWS.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modify settings for a DB instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new values
-- in the request.
module Network.AWS.RDS.ModifyDBInstance
    (
    -- * Request
      ModifyDBInstanceMessage
    -- ** Request constructor
    , modifyDBInstance
    -- ** Request lenses
    , mdbimAllocatedStorage
    , mdbimAllowMajorVersionUpgrade
    , mdbimApplyImmediately
    , mdbimAutoMinorVersionUpgrade
    , mdbimBackupRetentionPeriod
    , mdbimDBInstanceClass
    , mdbimDBInstanceIdentifier
    , mdbimDBParameterGroupName
    , mdbimDBSecurityGroups
    , mdbimEngineVersion
    , mdbimIops
    , mdbimMasterUserPassword
    , mdbimMultiAZ
    , mdbimNewDBInstanceIdentifier
    , mdbimOptionGroupName
    , mdbimPreferredBackupWindow
    , mdbimPreferredMaintenanceWindow
    , mdbimStorageType
    , mdbimTdeCredentialArn
    , mdbimTdeCredentialPassword
    , mdbimVpcSecurityGroupIds

    -- * Response
    , ModifyDBInstanceResult
    -- ** Response constructor
    , modifyDBInstanceResponse
    -- ** Response lenses
    , mdbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data ModifyDBInstanceMessage = ModifyDBInstanceMessage
    { _mdbimAllocatedStorage           :: Maybe Int
    , _mdbimAllowMajorVersionUpgrade   :: Maybe Bool
    , _mdbimApplyImmediately           :: Maybe Bool
    , _mdbimAutoMinorVersionUpgrade    :: Maybe Bool
    , _mdbimBackupRetentionPeriod      :: Maybe Int
    , _mdbimDBInstanceClass            :: Maybe Text
    , _mdbimDBInstanceIdentifier       :: Text
    , _mdbimDBParameterGroupName       :: Maybe Text
    , _mdbimDBSecurityGroups           :: [Text]
    , _mdbimEngineVersion              :: Maybe Text
    , _mdbimIops                       :: Maybe Int
    , _mdbimMasterUserPassword         :: Maybe Text
    , _mdbimMultiAZ                    :: Maybe Bool
    , _mdbimNewDBInstanceIdentifier    :: Maybe Text
    , _mdbimOptionGroupName            :: Maybe Text
    , _mdbimPreferredBackupWindow      :: Maybe Text
    , _mdbimPreferredMaintenanceWindow :: Maybe Text
    , _mdbimStorageType                :: Maybe Text
    , _mdbimTdeCredentialArn           :: Maybe Text
    , _mdbimTdeCredentialPassword      :: Maybe Text
    , _mdbimVpcSecurityGroupIds        :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyDBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbimAllocatedStorage' @::@ 'Maybe' 'Int'
--
-- * 'mdbimAllowMajorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mdbimApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mdbimAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mdbimBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'mdbimDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'mdbimDBInstanceIdentifier' @::@ 'Text'
--
-- * 'mdbimDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mdbimDBSecurityGroups' @::@ ['Text']
--
-- * 'mdbimEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mdbimIops' @::@ 'Maybe' 'Int'
--
-- * 'mdbimMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'mdbimMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'mdbimNewDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mdbimOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mdbimPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
-- * 'mdbimPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mdbimStorageType' @::@ 'Maybe' 'Text'
--
-- * 'mdbimTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'mdbimTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
-- * 'mdbimVpcSecurityGroupIds' @::@ ['Text']
--
modifyDBInstance :: Text -- ^ 'mdbimDBInstanceIdentifier'
                 -> ModifyDBInstanceMessage
modifyDBInstance p1 = ModifyDBInstanceMessage
    { _mdbimDBInstanceIdentifier       = p1
    , _mdbimAllocatedStorage           = Nothing
    , _mdbimDBInstanceClass            = Nothing
    , _mdbimDBSecurityGroups           = mempty
    , _mdbimVpcSecurityGroupIds        = mempty
    , _mdbimApplyImmediately           = Nothing
    , _mdbimMasterUserPassword         = Nothing
    , _mdbimDBParameterGroupName       = Nothing
    , _mdbimBackupRetentionPeriod      = Nothing
    , _mdbimPreferredBackupWindow      = Nothing
    , _mdbimPreferredMaintenanceWindow = Nothing
    , _mdbimMultiAZ                    = Nothing
    , _mdbimEngineVersion              = Nothing
    , _mdbimAllowMajorVersionUpgrade   = Nothing
    , _mdbimAutoMinorVersionUpgrade    = Nothing
    , _mdbimIops                       = Nothing
    , _mdbimOptionGroupName            = Nothing
    , _mdbimNewDBInstanceIdentifier    = Nothing
    , _mdbimStorageType                = Nothing
    , _mdbimTdeCredentialArn           = Nothing
    , _mdbimTdeCredentialPassword      = Nothing
    }

-- | The new storage capacity of the RDS instance. Changing this setting does
-- not result in an outage and the change is applied during the next
-- maintenance window unless ApplyImmediately is set to true for this
-- request. MySQL Default: Uses existing setting Valid Values: 5-3072
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value
-- are rounded up so that they are 10% greater than the current value. Type:
-- Integer PostgreSQL Default: Uses existing setting Valid Values: 5-3072
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value
-- are rounded up so that they are 10% greater than the current value. Type:
-- Integer Oracle Default: Uses existing setting Valid Values: 10-3072
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value
-- are rounded up so that they are 10% greater than the current value. SQL
-- Server Cannot be modified. If you choose to migrate your DB instance from
-- using standard storage to using Provisioned IOPS, or from using
-- Provisioned IOPS to using standard storage, the process can take time.
-- The duration of the migration depends on several factors such as database
-- load, storage size, storage type (standard or Provisioned IOPS), amount
-- of IOPS provisioned (if any), and the number of prior scale storage
-- operations. Typical migration times are under 24 hours, but the process
-- can take up to several days in some cases. During the migration, the DB
-- instance will be available for use, but may experience performance
-- degradation. While the migration takes place, nightly backups for the
-- instance will be suspended. No other Amazon RDS operations can take place
-- for the instance, including modifying the instance, rebooting the
-- instance, deleting the instance, creating a read replica for the
-- instance, and creating a DB snapshot of the instance.
mdbimAllocatedStorage :: Lens' ModifyDBInstanceMessage (Maybe Int)
mdbimAllocatedStorage =
    lens _mdbimAllocatedStorage (\s a -> s { _mdbimAllocatedStorage = a })

-- | Indicates that major version upgrades are allowed. Changing this
-- parameter does not result in an outage and the change is asynchronously
-- applied as soon as possible. Constraints: This parameter must be set to
-- true when specifying a value for the EngineVersion parameter that is a
-- different major version than the DB instance's current version.
mdbimAllowMajorVersionUpgrade :: Lens' ModifyDBInstanceMessage (Maybe Bool)
mdbimAllowMajorVersionUpgrade =
    lens _mdbimAllowMajorVersionUpgrade
        (\s a -> s { _mdbimAllowMajorVersionUpgrade = a })

-- | Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the PreferredMaintenanceWindow setting for the DB instance. If this
-- parameter is set to false, changes to the DB instance are applied during
-- the next maintenance window. Some parameter changes can cause an outage
-- and will be applied on the next call to RebootDBInstance, or the next
-- failure reboot. Review the table of parameters in Modifying a DB Instance
-- and Using the Apply Immediately Parameter to see the impact that setting
-- ApplyImmediately to true or false has for each modified parameter and to
-- determine when the changes will be applied. Default: false.
mdbimApplyImmediately :: Lens' ModifyDBInstanceMessage (Maybe Bool)
mdbimApplyImmediately =
    lens _mdbimApplyImmediately (\s a -> s { _mdbimApplyImmediately = a })

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window. Changing this parameter
-- does not result in an outage except in the following case and the change
-- is asynchronously applied as soon as possible. An outage will result if
-- this parameter is set to true during the maintenance window, and a newer
-- minor version is available, and RDS has enabled auto patching for that
-- engine version.
mdbimAutoMinorVersionUpgrade :: Lens' ModifyDBInstanceMessage (Maybe Bool)
mdbimAutoMinorVersionUpgrade =
    lens _mdbimAutoMinorVersionUpgrade
        (\s a -> s { _mdbimAutoMinorVersionUpgrade = a })

-- | The number of days to retain automated backups. Setting this parameter to
-- a positive number enables backups. Setting this parameter to 0 disables
-- automated backups. Changing this parameter can result in an outage if you
-- change from 0 to a non-zero value or from a non-zero value to 0. These
-- changes are applied during the next maintenance window unless the
-- ApplyImmediately parameter is set to true for this request. If you change
-- the parameter from one non-zero value to another non-zero value, the
-- change is asynchronously applied as soon as possible. Default: Uses
-- existing setting Constraints: Must be a value from 0 to 35 Can be
-- specified for a read replica only if the source is running MySQL 5.6
-- Cannot be set to 0 if the DB instance is a source to read replicas.
mdbimBackupRetentionPeriod :: Lens' ModifyDBInstanceMessage (Maybe Int)
mdbimBackupRetentionPeriod =
    lens _mdbimBackupRetentionPeriod
        (\s a -> s { _mdbimBackupRetentionPeriod = a })

-- | The new compute and memory capacity of the DB instance. To determine the
-- instance classes that are available for a particular DB engine, use the
-- DescribeOrderableDBInstanceOptions action. Passing a value for this
-- setting causes an outage during the change and is applied during the next
-- maintenance window, unless ApplyImmediately is specified as true for this
-- request. Default: Uses existing setting Valid Values: db.t1.micro |
-- db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |
-- db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge
-- | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge |
-- db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium.
mdbimDBInstanceClass :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimDBInstanceClass =
    lens _mdbimDBInstanceClass (\s a -> s { _mdbimDBInstanceClass = a })

-- | The DB instance identifier. This value is stored as a lowercase string.
-- Constraints: Must be the identifier for an existing DB instance Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
mdbimDBInstanceIdentifier :: Lens' ModifyDBInstanceMessage Text
mdbimDBInstanceIdentifier =
    lens _mdbimDBInstanceIdentifier
        (\s a -> s { _mdbimDBInstanceIdentifier = a })

-- | The name of the DB parameter group to apply to the DB instance. Changing
-- this setting does not result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. The db instance
-- will NOT be rebooted automatically and the parameter changes will NOT be
-- applied during the next maintenance window. Default: Uses existing
-- setting Constraints: The DB parameter group must be in the same DB
-- parameter group family as this DB instance.
mdbimDBParameterGroupName :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimDBParameterGroupName =
    lens _mdbimDBParameterGroupName
        (\s a -> s { _mdbimDBParameterGroupName = a })

-- | A list of DB security groups to authorize on this DB instance. Changing
-- this setting does not result in an outage and the change is
-- asynchronously applied as soon as possible. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens.
mdbimDBSecurityGroups :: Lens' ModifyDBInstanceMessage [Text]
mdbimDBSecurityGroups =
    lens _mdbimDBSecurityGroups (\s a -> s { _mdbimDBSecurityGroups = a })

-- | The version number of the database engine to upgrade to. Changing this
-- parameter results in an outage and the change is applied during the next
-- maintenance window unless the ApplyImmediately parameter is set to true
-- for this request. For major version upgrades, if a non-default DB
-- parameter group is currently in use, a new DB parameter group in the DB
-- parameter group family for the new engine version must be specified. The
-- new DB parameter group can be the default for that DB parameter group
-- family. Example: 5.1.42.
mdbimEngineVersion :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimEngineVersion =
    lens _mdbimEngineVersion (\s a -> s { _mdbimEngineVersion = a })

-- | The new Provisioned IOPS (I/O operations per second) value for the RDS
-- instance. Changing this setting does not result in an outage and the
-- change is applied during the next maintenance window unless the
-- ApplyImmediately parameter is set to true for this request. Default: Uses
-- existing setting Constraints: Value supplied must be at least 10% greater
-- than the current value. Values that are not at least 10% greater than the
-- existing value are rounded up so that they are 10% greater than the
-- current value. If you are migrating from Provisioned IOPS to standard
-- storage, set this value to 0. The DB instance will require a reboot for
-- the change in storage type to take effect. SQL Server Setting the IOPS
-- value for the SQL Server database engine is not supported. Type: Integer
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration depends
-- on several factors such as database load, storage size, storage type
-- (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and
-- the number of prior scale storage operations. Typical migration times are
-- under 24 hours, but the process can take up to several days in some
-- cases. During the migration, the DB instance will be available for use,
-- but may experience performance degradation. While the migration takes
-- place, nightly backups for the instance will be suspended. No other
-- Amazon RDS operations can take place for the instance, including
-- modifying the instance, rebooting the instance, deleting the instance,
-- creating a read replica for the instance, and creating a DB snapshot of
-- the instance.
mdbimIops :: Lens' ModifyDBInstanceMessage (Maybe Int)
mdbimIops = lens _mdbimIops (\s a -> s { _mdbimIops = a })

-- | The new password for the DB instance master user. Can be any printable
-- ASCII character except "/", """, or "@". Changing this parameter does not
-- result in an outage and the change is asynchronously applied as soon as
-- possible. Between the time of the request and the completion of the
-- request, the MasterUserPassword element exists in the
-- PendingModifiedValues element of the operation response. Default: Uses
-- existing setting Constraints: Must be 8 to 41 alphanumeric characters
-- (MySQL), 8 to 30 alphanumeric characters (Oracle), or 8 to 128
-- alphanumeric characters (SQL Server).
mdbimMasterUserPassword :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimMasterUserPassword =
    lens _mdbimMasterUserPassword (\s a -> s { _mdbimMasterUserPassword = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Changing this
-- parameter does not result in an outage and the change is applied during
-- the next maintenance window unless the ApplyImmediately parameter is set
-- to true for this request. Constraints: Cannot be specified if the DB
-- instance is a read replica.
mdbimMultiAZ :: Lens' ModifyDBInstanceMessage (Maybe Bool)
mdbimMultiAZ = lens _mdbimMultiAZ (\s a -> s { _mdbimMultiAZ = a })

-- | The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- will occur immediately if you set Apply Immediately to true, or will
-- occur during the next maintenance window if Apply Immediately to false.
-- This value is stored as a lowercase string. Constraints: Must contain
-- from 1 to 63 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens.
mdbimNewDBInstanceIdentifier :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimNewDBInstanceIdentifier =
    lens _mdbimNewDBInstanceIdentifier
        (\s a -> s { _mdbimNewDBInstanceIdentifier = a })

-- | Indicates that the DB instance should be associated with the specified
-- option group. Changing this parameter does not result in an outage except
-- in the following case and the change is applied during the next
-- maintenance window unless the ApplyImmediately parameter is set to true
-- for this request. If the parameter change results in an option group that
-- enables OEM, this change can cause a brief (sub-second) period during
-- which new connections are rejected but existing connections are not
-- interrupted. Permanent options, such as the TDE option for Oracle
-- Advanced Security TDE, cannot be removed from an option group, and that
-- option group cannot be removed from a DB instance once it is associated
-- with a DB instance.
mdbimOptionGroupName :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimOptionGroupName =
    lens _mdbimOptionGroupName (\s a -> s { _mdbimOptionGroupName = a })

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- BackupRetentionPeriod. Changing this parameter does not result in an
-- outage and the change is asynchronously applied as soon as possible.
-- Constraints: Must be in the format hh24:mi-hh24:mi Times should be
-- Universal Time Coordinated (UTC) Must not conflict with the preferred
-- maintenance window Must be at least 30 minutes.
mdbimPreferredBackupWindow :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimPreferredBackupWindow =
    lens _mdbimPreferredBackupWindow
        (\s a -> s { _mdbimPreferredBackupWindow = a })

-- | The weekly time range (in UTC) during which system maintenance can occur,
-- which may result in an outage. Changing this parameter does not result in
-- an outage, except in the following situation, and the change is
-- asynchronously applied as soon as possible. If there are pending actions
-- that cause a reboot, and the maintenance window is changed to include the
-- current time, then changing this parameter will cause a reboot of the DB
-- instance. If moving this window to the current time, there must be at
-- least 30 minutes between the current time and end of the window to ensure
-- pending changes are applied. Default: Uses existing setting Format:
-- ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue | Wed | Thu | Fri | Sat |
-- Sun Constraints: Must be at least 30 minutes.
mdbimPreferredMaintenanceWindow :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimPreferredMaintenanceWindow =
    lens _mdbimPreferredMaintenanceWindow
        (\s a -> s { _mdbimPreferredMaintenanceWindow = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
mdbimStorageType :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimStorageType = lens _mdbimStorageType (\s a -> s { _mdbimStorageType = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
mdbimTdeCredentialArn :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimTdeCredentialArn =
    lens _mdbimTdeCredentialArn (\s a -> s { _mdbimTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
mdbimTdeCredentialPassword :: Lens' ModifyDBInstanceMessage (Maybe Text)
mdbimTdeCredentialPassword =
    lens _mdbimTdeCredentialPassword
        (\s a -> s { _mdbimTdeCredentialPassword = a })

-- | A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible. Constraints: Must
-- be 1 to 255 alphanumeric characters First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
mdbimVpcSecurityGroupIds :: Lens' ModifyDBInstanceMessage [Text]
mdbimVpcSecurityGroupIds =
    lens _mdbimVpcSecurityGroupIds
        (\s a -> s { _mdbimVpcSecurityGroupIds = a })

instance ToPath ModifyDBInstanceMessage where
    toPath = const "/"

instance ToQuery ModifyDBInstanceMessage

newtype ModifyDBInstanceResult = ModifyDBInstanceResult
    { _mdbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'ModifyDBInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
modifyDBInstanceResponse :: ModifyDBInstanceResult
modifyDBInstanceResponse = ModifyDBInstanceResult
    { _mdbirDBInstance = Nothing
    }

mdbirDBInstance :: Lens' ModifyDBInstanceResult (Maybe DBInstance)
mdbirDBInstance = lens _mdbirDBInstance (\s a -> s { _mdbirDBInstance = a })

instance AWSRequest ModifyDBInstanceMessage where
    type Sv ModifyDBInstanceMessage = RDS
    type Rs ModifyDBInstanceMessage = ModifyDBInstanceResult

    request  = post "ModifyDBInstance"
    response = xmlResponse $ \h x -> ModifyDBInstanceResult
        <$> x %| "DBInstance"
