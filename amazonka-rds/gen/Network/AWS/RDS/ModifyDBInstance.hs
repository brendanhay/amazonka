{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
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
-- configuration parameters by specifying these parameters and the new
-- values in the request.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBInstance.html>
module Network.AWS.RDS.ModifyDBInstance
    (
    -- * Request
      ModifyDBInstance
    -- ** Request constructor
    , modifyDBInstance
    -- ** Request lenses
    , mdiDBSecurityGroups
    , mdiEngineVersion
    , mdiAutoMinorVersionUpgrade
    , mdiMasterUserPassword
    , mdiIOPS
    , mdiAllowMajorVersionUpgrade
    , mdiNewDBInstanceIdentifier
    , mdiTDECredentialPassword
    , mdiDBInstanceClass
    , mdiPreferredMaintenanceWindow
    , mdiCACertificateIdentifier
    , mdiPreferredBackupWindow
    , mdiBackupRetentionPeriod
    , mdiDBParameterGroupName
    , mdiVPCSecurityGroupIds
    , mdiMultiAZ
    , mdiAllocatedStorage
    , mdiApplyImmediately
    , mdiTDECredentialARN
    , mdiOptionGroupName
    , mdiStorageType
    , mdiDBInstanceIdentifier

    -- * Response
    , ModifyDBInstanceResponse
    -- ** Response constructor
    , modifyDBInstanceResponse
    -- ** Response lenses
    , mdirDBInstance
    , mdirStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdiDBSecurityGroups'
--
-- * 'mdiEngineVersion'
--
-- * 'mdiAutoMinorVersionUpgrade'
--
-- * 'mdiMasterUserPassword'
--
-- * 'mdiIOPS'
--
-- * 'mdiAllowMajorVersionUpgrade'
--
-- * 'mdiNewDBInstanceIdentifier'
--
-- * 'mdiTDECredentialPassword'
--
-- * 'mdiDBInstanceClass'
--
-- * 'mdiPreferredMaintenanceWindow'
--
-- * 'mdiCACertificateIdentifier'
--
-- * 'mdiPreferredBackupWindow'
--
-- * 'mdiBackupRetentionPeriod'
--
-- * 'mdiDBParameterGroupName'
--
-- * 'mdiVPCSecurityGroupIds'
--
-- * 'mdiMultiAZ'
--
-- * 'mdiAllocatedStorage'
--
-- * 'mdiApplyImmediately'
--
-- * 'mdiTDECredentialARN'
--
-- * 'mdiOptionGroupName'
--
-- * 'mdiStorageType'
--
-- * 'mdiDBInstanceIdentifier'
data ModifyDBInstance = ModifyDBInstance'
    { _mdiDBSecurityGroups           :: !(Maybe [Text])
    , _mdiEngineVersion              :: !(Maybe Text)
    , _mdiAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _mdiMasterUserPassword         :: !(Maybe Text)
    , _mdiIOPS                       :: !(Maybe Int)
    , _mdiAllowMajorVersionUpgrade   :: !(Maybe Bool)
    , _mdiNewDBInstanceIdentifier    :: !(Maybe Text)
    , _mdiTDECredentialPassword      :: !(Maybe Text)
    , _mdiDBInstanceClass            :: !(Maybe Text)
    , _mdiPreferredMaintenanceWindow :: !(Maybe Text)
    , _mdiCACertificateIdentifier    :: !(Maybe Text)
    , _mdiPreferredBackupWindow      :: !(Maybe Text)
    , _mdiBackupRetentionPeriod      :: !(Maybe Int)
    , _mdiDBParameterGroupName       :: !(Maybe Text)
    , _mdiVPCSecurityGroupIds        :: !(Maybe [Text])
    , _mdiMultiAZ                    :: !(Maybe Bool)
    , _mdiAllocatedStorage           :: !(Maybe Int)
    , _mdiApplyImmediately           :: !(Maybe Bool)
    , _mdiTDECredentialARN           :: !(Maybe Text)
    , _mdiOptionGroupName            :: !(Maybe Text)
    , _mdiStorageType                :: !(Maybe Text)
    , _mdiDBInstanceIdentifier       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyDBInstance' smart constructor.
modifyDBInstance :: Text -> ModifyDBInstance
modifyDBInstance pDBInstanceIdentifier =
    ModifyDBInstance'
    { _mdiDBSecurityGroups = Nothing
    , _mdiEngineVersion = Nothing
    , _mdiAutoMinorVersionUpgrade = Nothing
    , _mdiMasterUserPassword = Nothing
    , _mdiIOPS = Nothing
    , _mdiAllowMajorVersionUpgrade = Nothing
    , _mdiNewDBInstanceIdentifier = Nothing
    , _mdiTDECredentialPassword = Nothing
    , _mdiDBInstanceClass = Nothing
    , _mdiPreferredMaintenanceWindow = Nothing
    , _mdiCACertificateIdentifier = Nothing
    , _mdiPreferredBackupWindow = Nothing
    , _mdiBackupRetentionPeriod = Nothing
    , _mdiDBParameterGroupName = Nothing
    , _mdiVPCSecurityGroupIds = Nothing
    , _mdiMultiAZ = Nothing
    , _mdiAllocatedStorage = Nothing
    , _mdiApplyImmediately = Nothing
    , _mdiTDECredentialARN = Nothing
    , _mdiOptionGroupName = Nothing
    , _mdiStorageType = Nothing
    , _mdiDBInstanceIdentifier = pDBInstanceIdentifier
    }

-- | A list of DB security groups to authorize on this DB instance. Changing
-- this setting does not result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mdiDBSecurityGroups :: Lens' ModifyDBInstance [Text]
mdiDBSecurityGroups = lens _mdiDBSecurityGroups (\ s a -> s{_mdiDBSecurityGroups = a}) . _Default;

-- | The version number of the database engine to upgrade to. Changing this
-- parameter results in an outage and the change is applied during the next
-- maintenance window unless the @ApplyImmediately@ parameter is set to
-- @true@ for this request.
--
-- For major version upgrades, if a non-default DB parameter group is
-- currently in use, a new DB parameter group in the DB parameter group
-- family for the new engine version must be specified. The new DB
-- parameter group can be the default for that DB parameter group family.
--
-- For a list of valid engine versions, see CreateDBInstance.
mdiEngineVersion :: Lens' ModifyDBInstance (Maybe Text)
mdiEngineVersion = lens _mdiEngineVersion (\ s a -> s{_mdiEngineVersion = a});

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window. Changing this parameter
-- does not result in an outage except in the following case and the change
-- is asynchronously applied as soon as possible. An outage will result if
-- this parameter is set to @true@ during the maintenance window, and a
-- newer minor version is available, and RDS has enabled auto patching for
-- that engine version.
mdiAutoMinorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdiAutoMinorVersionUpgrade = lens _mdiAutoMinorVersionUpgrade (\ s a -> s{_mdiAutoMinorVersionUpgrade = a});

-- | The new password for the DB instance master user. Can be any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Changing this parameter does not result in an outage and the change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- Default: Uses existing setting
--
-- Constraints: Must be 8 to 41 alphanumeric characters (MySQL), 8 to 30
-- alphanumeric characters (Oracle), or 8 to 128 alphanumeric characters
-- (SQL Server).
--
-- Amazon RDS API actions never return the password, so this action
-- provides a way to regain access to a primary instance user if the
-- password is lost. This includes restoring privileges that may have been
-- accidentally revoked.
mdiMasterUserPassword :: Lens' ModifyDBInstance (Maybe Text)
mdiMasterUserPassword = lens _mdiMasterUserPassword (\ s a -> s{_mdiMasterUserPassword = a});

-- | The new Provisioned IOPS (I\/O operations per second) value for the RDS
-- instance. Changing this setting does not result in an outage and the
-- change is applied during the next maintenance window unless the
-- @ApplyImmediately@ parameter is set to @true@ for this request.
--
-- Default: Uses existing setting
--
-- Constraints: Value supplied must be at least 10% greater than the
-- current value. Values that are not at least 10% greater than the
-- existing value are rounded up so that they are 10% greater than the
-- current value. If you are migrating from Provisioned IOPS to standard
-- storage, set this value to 0. The DB instance will require a reboot for
-- the change in storage type to take effect.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine is not
-- supported.
--
-- Type: Integer
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance will
-- be available for use, but may experience performance degradation. While
-- the migration takes place, nightly backups for the instance will be
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a Read Replica for the instance, and
-- creating a DB snapshot of the instance.
mdiIOPS :: Lens' ModifyDBInstance (Maybe Int)
mdiIOPS = lens _mdiIOPS (\ s a -> s{_mdiIOPS = a});

-- | Indicates that major version upgrades are allowed. Changing this
-- parameter does not result in an outage and the change is asynchronously
-- applied as soon as possible.
--
-- Constraints: This parameter must be set to true when specifying a value
-- for the EngineVersion parameter that is a different major version than
-- the DB instance\'s current version.
mdiAllowMajorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdiAllowMajorVersionUpgrade = lens _mdiAllowMajorVersionUpgrade (\ s a -> s{_mdiAllowMajorVersionUpgrade = a});

-- | The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- will occur immediately if you set @Apply Immediately@ to true, or will
-- occur during the next maintenance window if @Apply Immediately@ to
-- false. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mdiNewDBInstanceIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdiNewDBInstanceIdentifier = lens _mdiNewDBInstanceIdentifier (\ s a -> s{_mdiNewDBInstanceIdentifier = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
mdiTDECredentialPassword :: Lens' ModifyDBInstance (Maybe Text)
mdiTDECredentialPassword = lens _mdiTDECredentialPassword (\ s a -> s{_mdiTDECredentialPassword = a});

-- | The new compute and memory capacity of the DB instance. To determine the
-- instance classes that are available for a particular DB engine, use the
-- DescribeOrderableDBInstanceOptions action.
--
-- Passing a value for this setting causes an outage during the change and
-- is applied during the next maintenance window, unless @ApplyImmediately@
-- is specified as @true@ for this request.
--
-- Default: Uses existing setting
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
mdiDBInstanceClass :: Lens' ModifyDBInstance (Maybe Text)
mdiDBInstanceClass = lens _mdiDBInstanceClass (\ s a -> s{_mdiDBInstanceClass = a});

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, which may result in an outage. Changing this parameter does not
-- result in an outage, except in the following situation, and the change
-- is asynchronously applied as soon as possible. If there are pending
-- actions that cause a reboot, and the maintenance window is changed to
-- include the current time, then changing this parameter will cause a
-- reboot of the DB instance. If moving this window to the current time,
-- there must be at least 30 minutes between the current time and end of
-- the window to ensure pending changes are applied.
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
mdiPreferredMaintenanceWindow :: Lens' ModifyDBInstance (Maybe Text)
mdiPreferredMaintenanceWindow = lens _mdiPreferredMaintenanceWindow (\ s a -> s{_mdiPreferredMaintenanceWindow = a});

-- | Indicates the certificate which needs to be associated with the
-- instance.
mdiCACertificateIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdiCACertificateIdentifier = lens _mdiCACertificateIdentifier (\ s a -> s{_mdiCACertificateIdentifier = a});

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@. Changing this parameter does not result in an
-- outage and the change is asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   Must be in the format hh24:mi-hh24:mi
-- -   Times should be Universal Time Coordinated (UTC)
-- -   Must not conflict with the preferred maintenance window
-- -   Must be at least 30 minutes
mdiPreferredBackupWindow :: Lens' ModifyDBInstance (Maybe Text)
mdiPreferredBackupWindow = lens _mdiPreferredBackupWindow (\ s a -> s{_mdiPreferredBackupWindow = a});

-- | The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to
-- a non-zero value or from a non-zero value to 0. These changes are
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is set to @true@ for this request. If you change the parameter
-- from one non-zero value to another non-zero value, the change is
-- asynchronously applied as soon as possible.
--
-- Default: Uses existing setting
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
-- -   Can be specified for a MySQL Read Replica only if the source is
--     running MySQL 5.6
-- -   Can be specified for a PostgreSQL Read Replica only if the source is
--     running PostgreSQL 9.3.5
-- -   Cannot be set to 0 if the DB instance is a source to Read Replicas
mdiBackupRetentionPeriod :: Lens' ModifyDBInstance (Maybe Int)
mdiBackupRetentionPeriod = lens _mdiBackupRetentionPeriod (\ s a -> s{_mdiBackupRetentionPeriod = a});

-- | The name of the DB parameter group to apply to the DB instance. Changing
-- this setting does not result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. The db instance
-- will NOT be rebooted automatically and the parameter changes will NOT be
-- applied during the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter
-- group family as this DB instance.
mdiDBParameterGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdiDBParameterGroupName = lens _mdiDBParameterGroupName (\ s a -> s{_mdiDBParameterGroupName = a});

-- | A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mdiVPCSecurityGroupIds :: Lens' ModifyDBInstance [Text]
mdiVPCSecurityGroupIds = lens _mdiVPCSecurityGroupIds (\ s a -> s{_mdiVPCSecurityGroupIds = a}) . _Default;

-- | Specifies if the DB instance is a Multi-AZ deployment. Changing this
-- parameter does not result in an outage and the change is applied during
-- the next maintenance window unless the @ApplyImmediately@ parameter is
-- set to @true@ for this request.
--
-- Constraints: Cannot be specified if the DB instance is a Read Replica.
mdiMultiAZ :: Lens' ModifyDBInstance (Maybe Bool)
mdiMultiAZ = lens _mdiMultiAZ (\ s a -> s{_mdiMultiAZ = a});

-- | The new storage capacity of the RDS instance. Changing this setting does
-- not result in an outage and the change is applied during the next
-- maintenance window unless @ApplyImmediately@ is set to @true@ for this
-- request.
--
-- __MySQL__
--
-- Default: Uses existing setting
--
-- Valid Values: 5-3072
--
-- Constraints: Value supplied must be at least 10% greater than the
-- current value. Values that are not at least 10% greater than the
-- existing value are rounded up so that they are 10% greater than the
-- current value.
--
-- Type: Integer
--
-- __PostgreSQL__
--
-- Default: Uses existing setting
--
-- Valid Values: 5-3072
--
-- Constraints: Value supplied must be at least 10% greater than the
-- current value. Values that are not at least 10% greater than the
-- existing value are rounded up so that they are 10% greater than the
-- current value.
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: Uses existing setting
--
-- Valid Values: 10-3072
--
-- Constraints: Value supplied must be at least 10% greater than the
-- current value. Values that are not at least 10% greater than the
-- existing value are rounded up so that they are 10% greater than the
-- current value.
--
-- __SQL Server__
--
-- Cannot be modified.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance will
-- be available for use, but may experience performance degradation. While
-- the migration takes place, nightly backups for the instance will be
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a Read Replica for the instance, and
-- creating a DB snapshot of the instance.
mdiAllocatedStorage :: Lens' ModifyDBInstance (Maybe Int)
mdiAllocatedStorage = lens _mdiAllocatedStorage (\ s a -> s{_mdiAllocatedStorage = a});

-- | Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the @PreferredMaintenanceWindow@ setting for the DB instance.
--
-- If this parameter is set to @false@, changes to the DB instance are
-- applied during the next maintenance window. Some parameter changes can
-- cause an outage and will be applied on the next call to
-- RebootDBInstance, or the next failure reboot. Review the table of
-- parameters in
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.html#Overview.DBInstance.Modifying Modifying a DB Instance and Using the Apply Immediately Parameter>
-- to see the impact that setting @ApplyImmediately@ to @true@ or @false@
-- has for each modified parameter and to determine when the changes will
-- be applied.
--
-- Default: @false@
mdiApplyImmediately :: Lens' ModifyDBInstance (Maybe Bool)
mdiApplyImmediately = lens _mdiApplyImmediately (\ s a -> s{_mdiApplyImmediately = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
mdiTDECredentialARN :: Lens' ModifyDBInstance (Maybe Text)
mdiTDECredentialARN = lens _mdiTDECredentialARN (\ s a -> s{_mdiTDECredentialARN = a});

-- | Indicates that the DB instance should be associated with the specified
-- option group. Changing this parameter does not result in an outage
-- except in the following case and the change is applied during the next
-- maintenance window unless the @ApplyImmediately@ parameter is set to
-- @true@ for this request. If the parameter change results in an option
-- group that enables OEM, this change can cause a brief (sub-second)
-- period during which new connections are rejected but existing
-- connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
mdiOptionGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdiOptionGroupName = lens _mdiOptionGroupName (\ s a -> s{_mdiOptionGroupName = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
mdiStorageType :: Lens' ModifyDBInstance (Maybe Text)
mdiStorageType = lens _mdiStorageType (\ s a -> s{_mdiStorageType = a});

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must be the identifier for an existing DB instance
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mdiDBInstanceIdentifier :: Lens' ModifyDBInstance Text
mdiDBInstanceIdentifier = lens _mdiDBInstanceIdentifier (\ s a -> s{_mdiDBInstanceIdentifier = a});

instance AWSRequest ModifyDBInstance where
        type Sv ModifyDBInstance = RDS
        type Rs ModifyDBInstance = ModifyDBInstanceResponse
        request = post
        response
          = receiveXMLWrapper "ModifyDBInstanceResult"
              (\ s h x ->
                 ModifyDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders ModifyDBInstance where
        toHeaders = const mempty

instance ToPath ModifyDBInstance where
        toPath = const "/"

instance ToQuery ModifyDBInstance where
        toQuery ModifyDBInstance'{..}
          = mconcat
              ["Action" =: ("ModifyDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSecurityGroups" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _mdiDBSecurityGroups),
               "EngineVersion" =: _mdiEngineVersion,
               "AutoMinorVersionUpgrade" =:
                 _mdiAutoMinorVersionUpgrade,
               "MasterUserPassword" =: _mdiMasterUserPassword,
               "Iops" =: _mdiIOPS,
               "AllowMajorVersionUpgrade" =:
                 _mdiAllowMajorVersionUpgrade,
               "NewDBInstanceIdentifier" =:
                 _mdiNewDBInstanceIdentifier,
               "TdeCredentialPassword" =: _mdiTDECredentialPassword,
               "DBInstanceClass" =: _mdiDBInstanceClass,
               "PreferredMaintenanceWindow" =:
                 _mdiPreferredMaintenanceWindow,
               "CACertificateIdentifier" =:
                 _mdiCACertificateIdentifier,
               "PreferredBackupWindow" =: _mdiPreferredBackupWindow,
               "BackupRetentionPeriod" =: _mdiBackupRetentionPeriod,
               "DBParameterGroupName" =: _mdiDBParameterGroupName,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _mdiVPCSecurityGroupIds),
               "MultiAZ" =: _mdiMultiAZ,
               "AllocatedStorage" =: _mdiAllocatedStorage,
               "ApplyImmediately" =: _mdiApplyImmediately,
               "TdeCredentialArn" =: _mdiTDECredentialARN,
               "OptionGroupName" =: _mdiOptionGroupName,
               "StorageType" =: _mdiStorageType,
               "DBInstanceIdentifier" =: _mdiDBInstanceIdentifier]

-- | /See:/ 'modifyDBInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdirDBInstance'
--
-- * 'mdirStatus'
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
    { _mdirDBInstance :: !(Maybe DBInstance)
    , _mdirStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyDBInstanceResponse' smart constructor.
modifyDBInstanceResponse :: Int -> ModifyDBInstanceResponse
modifyDBInstanceResponse pStatus =
    ModifyDBInstanceResponse'
    { _mdirDBInstance = Nothing
    , _mdirStatus = pStatus
    }

-- | FIXME: Undocumented member.
mdirDBInstance :: Lens' ModifyDBInstanceResponse (Maybe DBInstance)
mdirDBInstance = lens _mdirDBInstance (\ s a -> s{_mdirDBInstance = a});

-- | FIXME: Undocumented member.
mdirStatus :: Lens' ModifyDBInstanceResponse Int
mdirStatus = lens _mdirStatus (\ s a -> s{_mdirStatus = a});
