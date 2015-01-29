{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modify settings for a DB instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new values in
-- the request.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBInstance.html>
module Network.AWS.RDS.ModifyDBInstance
    (
    -- * Request
      ModifyDBInstance
    -- ** Request constructor
    , modifyDBInstance
    -- ** Request lenses
    , mdbiAllocatedStorage
    , mdbiAllowMajorVersionUpgrade
    , mdbiApplyImmediately
    , mdbiAutoMinorVersionUpgrade
    , mdbiBackupRetentionPeriod
    , mdbiDBInstanceClass
    , mdbiDBInstanceIdentifier
    , mdbiDBParameterGroupName
    , mdbiDBSecurityGroups
    , mdbiEngineVersion
    , mdbiIops
    , mdbiMasterUserPassword
    , mdbiMultiAZ
    , mdbiNewDBInstanceIdentifier
    , mdbiOptionGroupName
    , mdbiPreferredBackupWindow
    , mdbiPreferredMaintenanceWindow
    , mdbiStorageType
    , mdbiTdeCredentialArn
    , mdbiTdeCredentialPassword
    , mdbiVpcSecurityGroupIds

    -- * Response
    , ModifyDBInstanceResponse
    -- ** Response constructor
    , modifyDBInstanceResponse
    -- ** Response lenses
    , mdbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ModifyDBInstance = ModifyDBInstance
    { _mdbiAllocatedStorage           :: Maybe Int
    , _mdbiAllowMajorVersionUpgrade   :: Maybe Bool
    , _mdbiApplyImmediately           :: Maybe Bool
    , _mdbiAutoMinorVersionUpgrade    :: Maybe Bool
    , _mdbiBackupRetentionPeriod      :: Maybe Int
    , _mdbiDBInstanceClass            :: Maybe Text
    , _mdbiDBInstanceIdentifier       :: Text
    , _mdbiDBParameterGroupName       :: Maybe Text
    , _mdbiDBSecurityGroups           :: List "member" Text
    , _mdbiEngineVersion              :: Maybe Text
    , _mdbiIops                       :: Maybe Int
    , _mdbiMasterUserPassword         :: Maybe Text
    , _mdbiMultiAZ                    :: Maybe Bool
    , _mdbiNewDBInstanceIdentifier    :: Maybe Text
    , _mdbiOptionGroupName            :: Maybe Text
    , _mdbiPreferredBackupWindow      :: Maybe Text
    , _mdbiPreferredMaintenanceWindow :: Maybe Text
    , _mdbiStorageType                :: Maybe Text
    , _mdbiTdeCredentialArn           :: Maybe Text
    , _mdbiTdeCredentialPassword      :: Maybe Text
    , _mdbiVpcSecurityGroupIds        :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyDBInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbiAllocatedStorage' @::@ 'Maybe' 'Int'
--
-- * 'mdbiAllowMajorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mdbiApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mdbiAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'mdbiBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'mdbiDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'mdbiDBInstanceIdentifier' @::@ 'Text'
--
-- * 'mdbiDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mdbiDBSecurityGroups' @::@ ['Text']
--
-- * 'mdbiEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'mdbiIops' @::@ 'Maybe' 'Int'
--
-- * 'mdbiMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'mdbiMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'mdbiNewDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'mdbiOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mdbiPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
-- * 'mdbiPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'mdbiStorageType' @::@ 'Maybe' 'Text'
--
-- * 'mdbiTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'mdbiTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
-- * 'mdbiVpcSecurityGroupIds' @::@ ['Text']
--
modifyDBInstance :: Text -- ^ 'mdbiDBInstanceIdentifier'
                 -> ModifyDBInstance
modifyDBInstance p1 = ModifyDBInstance
    { _mdbiDBInstanceIdentifier       = p1
    , _mdbiAllocatedStorage           = Nothing
    , _mdbiDBInstanceClass            = Nothing
    , _mdbiDBSecurityGroups           = mempty
    , _mdbiVpcSecurityGroupIds        = mempty
    , _mdbiApplyImmediately           = Nothing
    , _mdbiMasterUserPassword         = Nothing
    , _mdbiDBParameterGroupName       = Nothing
    , _mdbiBackupRetentionPeriod      = Nothing
    , _mdbiPreferredBackupWindow      = Nothing
    , _mdbiPreferredMaintenanceWindow = Nothing
    , _mdbiMultiAZ                    = Nothing
    , _mdbiEngineVersion              = Nothing
    , _mdbiAllowMajorVersionUpgrade   = Nothing
    , _mdbiAutoMinorVersionUpgrade    = Nothing
    , _mdbiIops                       = Nothing
    , _mdbiOptionGroupName            = Nothing
    , _mdbiNewDBInstanceIdentifier    = Nothing
    , _mdbiStorageType                = Nothing
    , _mdbiTdeCredentialArn           = Nothing
    , _mdbiTdeCredentialPassword      = Nothing
    }

-- | The new storage capacity of the RDS instance. Changing this setting does not
-- result in an outage and the change is applied during the next maintenance
-- window unless 'ApplyImmediately' is set to 'true' for this request.
--
-- MySQL
--
-- Default: Uses existing setting
--
-- Valid Values: 5-3072
--
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value are
-- rounded up so that they are 10% greater than the current value.
--
-- Type: Integer
--
-- PostgreSQL
--
-- Default: Uses existing setting
--
-- Valid Values: 5-3072
--
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value are
-- rounded up so that they are 10% greater than the current value.
--
-- Type: Integer
--
-- Oracle
--
-- Default: Uses existing setting
--
-- Valid Values: 10-3072
--
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value are
-- rounded up so that they are 10% greater than the current value.
--
-- SQL Server
--
-- Cannot be modified.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration depends on
-- several factors such as database load, storage size, storage type (standard
-- or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of
-- prior scale storage operations. Typical migration times are under 24 hours,
-- but the process can take up to several days in some cases. During the
-- migration, the DB instance will be available for use, but may experience
-- performance degradation. While the migration takes place, nightly backups for
-- the instance will be suspended. No other Amazon RDS operations can take place
-- for the instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a Read Replica for the instance, and creating
-- a DB snapshot of the instance.
mdbiAllocatedStorage :: Lens' ModifyDBInstance (Maybe Int)
mdbiAllocatedStorage =
    lens _mdbiAllocatedStorage (\s a -> s { _mdbiAllocatedStorage = a })

-- | Indicates that major version upgrades are allowed. Changing this parameter
-- does not result in an outage and the change is asynchronously applied as soon
-- as possible.
--
-- Constraints: This parameter must be set to true when specifying a value for
-- the EngineVersion parameter that is a different major version than the DB
-- instance's current version.
mdbiAllowMajorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdbiAllowMajorVersionUpgrade =
    lens _mdbiAllowMajorVersionUpgrade
        (\s a -> s { _mdbiAllowMajorVersionUpgrade = a })

-- | Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless of
-- the 'PreferredMaintenanceWindow' setting for the DB instance.
--
-- If this parameter is set to 'false', changes to the DB instance are applied
-- during the next maintenance window. Some parameter changes can cause an
-- outage and will be applied on the next call to 'RebootDBInstance', or the next
-- failure reboot. Review the table of parameters in <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.html#Overview.DBInstance.Modifying Modifying a DB Instance andUsing the Apply Immediately Parameter> to see the impact that setting 'ApplyImmediately' to 'true' or 'false' has for each modified parameter and to determine when the
-- changes will be applied.
--
-- Default: 'false'
mdbiApplyImmediately :: Lens' ModifyDBInstance (Maybe Bool)
mdbiApplyImmediately =
    lens _mdbiApplyImmediately (\s a -> s { _mdbiApplyImmediately = a })

-- | Indicates that minor version upgrades will be applied automatically to the
-- DB instance during the maintenance window. Changing this parameter does not
-- result in an outage except in the following case and the change is
-- asynchronously applied as soon as possible. An outage will result if this
-- parameter is set to 'true' during the maintenance window, and a newer minor
-- version is available, and RDS has enabled auto patching for that engine
-- version.
mdbiAutoMinorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdbiAutoMinorVersionUpgrade =
    lens _mdbiAutoMinorVersionUpgrade
        (\s a -> s { _mdbiAutoMinorVersionUpgrade = a })

-- | The number of days to retain automated backups. Setting this parameter to a
-- positive number enables backups. Setting this parameter to 0 disables
-- automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to a
-- non-zero value or from a non-zero value to 0. These changes are applied
-- during the next maintenance window unless the 'ApplyImmediately' parameter is
-- set to 'true' for this request. If you change the parameter from one non-zero
-- value to another non-zero value, the change is asynchronously applied as soon
-- as possible.
--
-- Default: Uses existing setting
--
-- Constraints:
--
-- Must be a value from 0 to 35 Can be specified for a MySQL Read Replica only
-- if the source is running MySQL 5.6 Can be specified for a PostgreSQL Read
-- Replica only if the source is running PostgreSQL 9.3.5 Cannot be set to 0 if
-- the DB instance is a source to Read Replicas
mdbiBackupRetentionPeriod :: Lens' ModifyDBInstance (Maybe Int)
mdbiBackupRetentionPeriod =
    lens _mdbiBackupRetentionPeriod
        (\s a -> s { _mdbiBackupRetentionPeriod = a })

-- | The new compute and memory capacity of the DB instance. To determine the
-- instance classes that are available for a particular DB engine, use the 'DescribeOrderableDBInstanceOptions' action.
--
-- Passing a value for this setting causes an outage during the change and is
-- applied during the next maintenance window, unless 'ApplyImmediately' is
-- specified as 'true' for this request.
--
-- Default: Uses existing setting
--
-- Valid Values: 'db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |db.m1.xlarge | db.m2.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium |db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small |db.t2.medium'
mdbiDBInstanceClass :: Lens' ModifyDBInstance (Maybe Text)
mdbiDBInstanceClass =
    lens _mdbiDBInstanceClass (\s a -> s { _mdbiDBInstanceClass = a })

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- Must be the identifier for an existing DB instance Must contain from 1 to
-- 63 alphanumeric characters or hyphens First character must be a letter Cannot
-- end with a hyphen or contain two consecutive hyphens
mdbiDBInstanceIdentifier :: Lens' ModifyDBInstance Text
mdbiDBInstanceIdentifier =
    lens _mdbiDBInstanceIdentifier
        (\s a -> s { _mdbiDBInstanceIdentifier = a })

-- | The name of the DB parameter group to apply to the DB instance. Changing
-- this setting does not result in an outage. The parameter group name itself is
-- changed immediately, but the actual parameter changes are not applied until
-- you reboot the instance without failover. The db instance will NOT be
-- rebooted automatically and the parameter changes will NOT be applied during
-- the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter group
-- family as this DB instance.
mdbiDBParameterGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdbiDBParameterGroupName =
    lens _mdbiDBParameterGroupName
        (\s a -> s { _mdbiDBParameterGroupName = a })

-- | A list of DB security groups to authorize on this DB instance. Changing this
-- setting does not result in an outage and the change is asynchronously applied
-- as soon as possible.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
--
mdbiDBSecurityGroups :: Lens' ModifyDBInstance [Text]
mdbiDBSecurityGroups =
    lens _mdbiDBSecurityGroups (\s a -> s { _mdbiDBSecurityGroups = a })
        . _List

-- | The version number of the database engine to upgrade to. Changing this
-- parameter results in an outage and the change is applied during the next
-- maintenance window unless the 'ApplyImmediately' parameter is set to 'true' for
-- this request.
--
-- For major version upgrades, if a non-default DB parameter group is
-- currently in use, a new DB parameter group in the DB parameter group family
-- for the new engine version must be specified. The new DB parameter group can
-- be the default for that DB parameter group family.
--
-- For a list of valid engine versions, see 'CreateDBInstance'.
mdbiEngineVersion :: Lens' ModifyDBInstance (Maybe Text)
mdbiEngineVersion =
    lens _mdbiEngineVersion (\s a -> s { _mdbiEngineVersion = a })

-- | The new Provisioned IOPS (I/O operations per second) value for the RDS
-- instance. Changing this setting does not result in an outage and the change
-- is applied during the next maintenance window unless the 'ApplyImmediately'
-- parameter is set to 'true' for this request.
--
-- Default: Uses existing setting
--
-- Constraints: Value supplied must be at least 10% greater than the current
-- value. Values that are not at least 10% greater than the existing value are
-- rounded up so that they are 10% greater than the current value. If you are
-- migrating from Provisioned IOPS to standard storage, set this value to 0. The
-- DB instance will require a reboot for the change in storage type to take
-- effect.
--
-- SQL Server
--
-- Setting the IOPS value for the SQL Server database engine is not supported.
--
-- Type: Integer
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration depends on
-- several factors such as database load, storage size, storage type (standard
-- or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of
-- prior scale storage operations. Typical migration times are under 24 hours,
-- but the process can take up to several days in some cases. During the
-- migration, the DB instance will be available for use, but may experience
-- performance degradation. While the migration takes place, nightly backups for
-- the instance will be suspended. No other Amazon RDS operations can take place
-- for the instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a Read Replica for the instance, and creating
-- a DB snapshot of the instance.
mdbiIops :: Lens' ModifyDBInstance (Maybe Int)
mdbiIops = lens _mdbiIops (\s a -> s { _mdbiIops = a })

-- | The new password for the DB instance master user. Can be any printable ASCII
-- character except "/", """, or "@".
--
-- Changing this parameter does not result in an outage and the change is
-- asynchronously applied as soon as possible. Between the time of the request
-- and the completion of the request, the 'MasterUserPassword' element exists in
-- the 'PendingModifiedValues' element of the operation response.
--
-- Default: Uses existing setting
--
-- Constraints: Must be 8 to 41 alphanumeric characters (MySQL), 8 to 30
-- alphanumeric characters (Oracle), or 8 to 128 alphanumeric characters (SQL
-- Server).
--
-- Amazon RDS API actions never return the password, so this action provides a
-- way to regain access to a master instance user if the password is lost. This
-- includes restoring privileges that may have been accidentally revoked.
mdbiMasterUserPassword :: Lens' ModifyDBInstance (Maybe Text)
mdbiMasterUserPassword =
    lens _mdbiMasterUserPassword (\s a -> s { _mdbiMasterUserPassword = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Changing this
-- parameter does not result in an outage and the change is applied during the
-- next maintenance window unless the 'ApplyImmediately' parameter is set to 'true'
-- for this request.
--
-- Constraints: Cannot be specified if the DB instance is a Read Replica.
mdbiMultiAZ :: Lens' ModifyDBInstance (Maybe Bool)
mdbiMultiAZ = lens _mdbiMultiAZ (\s a -> s { _mdbiMultiAZ = a })

-- | The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot will
-- occur immediately if you set 'Apply Immediately' to true, or will occur during
-- the next maintenance window if 'Apply Immediately' to false. This value is
-- stored as a lowercase string.
--
-- Constraints:
--
-- Must contain from 1 to 63 alphanumeric characters or hyphens First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens
mdbiNewDBInstanceIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdbiNewDBInstanceIdentifier =
    lens _mdbiNewDBInstanceIdentifier
        (\s a -> s { _mdbiNewDBInstanceIdentifier = a })

-- | Indicates that the DB instance should be associated with the specified
-- option group. Changing this parameter does not result in an outage except in
-- the following case and the change is applied during the next maintenance
-- window unless the 'ApplyImmediately' parameter is set to 'true' for this request.
-- If the parameter change results in an option group that enables OEM, this
-- change can cause a brief (sub-second) period during which new connections are
-- rejected but existing connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE,
-- cannot be removed from an option group, and that option group cannot be
-- removed from a DB instance once it is associated with a DB instance
mdbiOptionGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdbiOptionGroupName =
    lens _mdbiOptionGroupName (\s a -> s { _mdbiOptionGroupName = a })

-- | The daily time range during which automated backups are created if automated
-- backups are enabled, as determined by the 'BackupRetentionPeriod'. Changing
-- this parameter does not result in an outage and the change is asynchronously
-- applied as soon as possible.
--
-- Constraints:
--
-- Must be in the format hh24:mi-hh24:mi Times should be Universal Time
-- Coordinated (UTC) Must not conflict with the preferred maintenance window Must be at least 30 minutes
--
mdbiPreferredBackupWindow :: Lens' ModifyDBInstance (Maybe Text)
mdbiPreferredBackupWindow =
    lens _mdbiPreferredBackupWindow
        (\s a -> s { _mdbiPreferredBackupWindow = a })

-- | The weekly time range (in UTC) during which system maintenance can occur,
-- which may result in an outage. Changing this parameter does not result in an
-- outage, except in the following situation, and the change is asynchronously
-- applied as soon as possible. If there are pending actions that cause a
-- reboot, and the maintenance window is changed to include the current time,
-- then changing this parameter will cause a reboot of the DB instance. If
-- moving this window to the current time, there must be at least 30 minutes
-- between the current time and end of the window to ensure pending changes are
-- applied.
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
mdbiPreferredMaintenanceWindow :: Lens' ModifyDBInstance (Maybe Text)
mdbiPreferredMaintenanceWindow =
    lens _mdbiPreferredMaintenanceWindow
        (\s a -> s { _mdbiPreferredMaintenanceWindow = a })

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: 'standard | gp2 | io1'
--
-- If you specify 'io1', you must also include a value for the 'Iops' parameter.
--
-- Default: 'io1' if the 'Iops' parameter is specified; otherwise 'standard'
mdbiStorageType :: Lens' ModifyDBInstance (Maybe Text)
mdbiStorageType = lens _mdbiStorageType (\s a -> s { _mdbiStorageType = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
mdbiTdeCredentialArn :: Lens' ModifyDBInstance (Maybe Text)
mdbiTdeCredentialArn =
    lens _mdbiTdeCredentialArn (\s a -> s { _mdbiTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
mdbiTdeCredentialPassword :: Lens' ModifyDBInstance (Maybe Text)
mdbiTdeCredentialPassword =
    lens _mdbiTdeCredentialPassword
        (\s a -> s { _mdbiTdeCredentialPassword = a })

-- | A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
--
mdbiVpcSecurityGroupIds :: Lens' ModifyDBInstance [Text]
mdbiVpcSecurityGroupIds =
    lens _mdbiVpcSecurityGroupIds (\s a -> s { _mdbiVpcSecurityGroupIds = a })
        . _List

newtype ModifyDBInstanceResponse = ModifyDBInstanceResponse
    { _mdbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Read, Show)

-- | 'ModifyDBInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
modifyDBInstanceResponse :: ModifyDBInstanceResponse
modifyDBInstanceResponse = ModifyDBInstanceResponse
    { _mdbirDBInstance = Nothing
    }

mdbirDBInstance :: Lens' ModifyDBInstanceResponse (Maybe DBInstance)
mdbirDBInstance = lens _mdbirDBInstance (\s a -> s { _mdbirDBInstance = a })

instance ToPath ModifyDBInstance where
    toPath = const "/"

instance ToQuery ModifyDBInstance where
    toQuery ModifyDBInstance{..} = mconcat
        [ "AllocatedStorage"           =? _mdbiAllocatedStorage
        , "AllowMajorVersionUpgrade"   =? _mdbiAllowMajorVersionUpgrade
        , "ApplyImmediately"           =? _mdbiApplyImmediately
        , "AutoMinorVersionUpgrade"    =? _mdbiAutoMinorVersionUpgrade
        , "BackupRetentionPeriod"      =? _mdbiBackupRetentionPeriod
        , "DBInstanceClass"            =? _mdbiDBInstanceClass
        , "DBInstanceIdentifier"       =? _mdbiDBInstanceIdentifier
        , "DBParameterGroupName"       =? _mdbiDBParameterGroupName
        , "DBSecurityGroups"           =? _mdbiDBSecurityGroups
        , "EngineVersion"              =? _mdbiEngineVersion
        , "Iops"                       =? _mdbiIops
        , "MasterUserPassword"         =? _mdbiMasterUserPassword
        , "MultiAZ"                    =? _mdbiMultiAZ
        , "NewDBInstanceIdentifier"    =? _mdbiNewDBInstanceIdentifier
        , "OptionGroupName"            =? _mdbiOptionGroupName
        , "PreferredBackupWindow"      =? _mdbiPreferredBackupWindow
        , "PreferredMaintenanceWindow" =? _mdbiPreferredMaintenanceWindow
        , "StorageType"                =? _mdbiStorageType
        , "TdeCredentialArn"           =? _mdbiTdeCredentialArn
        , "TdeCredentialPassword"      =? _mdbiTdeCredentialPassword
        , "VpcSecurityGroupIds"        =? _mdbiVpcSecurityGroupIds
        ]

instance ToHeaders ModifyDBInstance

instance AWSRequest ModifyDBInstance where
    type Sv ModifyDBInstance = RDS
    type Rs ModifyDBInstance = ModifyDBInstanceResponse

    request  = post "ModifyDBInstance"
    response = xmlResponse

instance FromXML ModifyDBInstanceResponse where
    parseXML = withElement "ModifyDBInstanceResult" $ \x -> ModifyDBInstanceResponse
        <$> x .@? "DBInstance"
