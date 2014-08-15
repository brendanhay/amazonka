{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ModifyDBInstance
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
-- in the request. https://rds.amazonaws.com/ ?Action=ModifyDBInstance
-- &DBInstanceIdentifier=simcoprod01 &AllocatedStorage=50 &Version=2013-05-15
-- &ApplyImmediately=false &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T08%3A02%3A09.574Z &AWSAccessKeyId= &Signature=
-- 2011-05-23T08:00:00Z mysql 50 1 false general-public-license available
-- 5.1.50 3306 simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com
-- simcoprod01 in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large
-- master f61a020f-8512-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.ModifyDBInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyDBInstance' request.
modifyDBInstance :: Text -- ^ '_mdbimDBInstanceIdentifier'
                 -> ModifyDBInstance
modifyDBInstance p1 = ModifyDBInstance
    { _mdbimDBInstanceIdentifier = p1
    , _mdbimAllowMajorVersionUpgrade = Nothing
    , _mdbimApplyImmediately = Nothing
    , _mdbimAutoMinorVersionUpgrade = Nothing
    , _mdbimMultiAZ = Nothing
    , _mdbimDBSecurityGroups = mempty
    , _mdbimIops = Nothing
    , _mdbimBackupRetentionPeriod = Nothing
    , _mdbimAllocatedStorage = Nothing
    , _mdbimEngineVersion = Nothing
    , _mdbimMasterUserPassword = Nothing
    , _mdbimNewDBInstanceIdentifier = Nothing
    , _mdbimDBInstanceClass = Nothing
    , _mdbimPreferredMaintenanceWindow = Nothing
    , _mdbimDBParameterGroupName = Nothing
    , _mdbimPreferredBackupWindow = Nothing
    , _mdbimOptionGroupName = Nothing
    , _mdbimVpcSecurityGroupIds = mempty
    }

data ModifyDBInstance = ModifyDBInstance
    { _mdbimDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier. This value is stored as a lowercase
      -- string. Constraints: Must be the identifier for an existing DB
      -- instance Must contain from 1 to 63 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _mdbimAllowMajorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that major version upgrades are allowed. Changing this
      -- parameter does not result in an outage and the change is
      -- asynchronously applied as soon as possible. Constraints: This
      -- parameter must be set to true when specifying a value for the
      -- EngineVersion parameter that is a different major version than
      -- the DB instance's current version.
    , _mdbimApplyImmediately :: Maybe Bool
      -- ^ Specifies whether or not the modifications in this request and
      -- any pending modifications are asynchronously applied as soon as
      -- possible, regardless of the PreferredMaintenanceWindow setting
      -- for the DB instance. If this parameter is passed as false,
      -- changes to the DB instance are applied on the next call to
      -- RebootDBInstance, the next maintenance reboot, or the next
      -- failure reboot, whichever occurs first. See each parameter to
      -- determine when a change is applied. Default: false.
    , _mdbimAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version upgrades will be applied
      -- automatically to the DB instance during the maintenance window.
      -- Changing this parameter does not result in an outage except in
      -- the following case and the change is asynchronously applied as
      -- soon as possible. An outage will result if this parameter is set
      -- to true during the maintenance window, and a newer minor version
      -- is available, and RDS has enabled auto patching for that engine
      -- version.
    , _mdbimMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment. Changing
      -- this parameter does not result in an outage and the change is
      -- applied during the next maintenance window unless the
      -- ApplyImmediately parameter is set to true for this request.
      -- Constraints: Cannot be specified if the DB instance is a read
      -- replica.
    , _mdbimDBSecurityGroups :: [Text]
      -- ^ A list of DB security groups to authorize on this DB instance.
      -- Changing this parameter does not result in an outage and the
      -- change is asynchronously applied as soon as possible.
      -- Constraints: Must be 1 to 255 alphanumeric characters First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens.
    , _mdbimIops :: Maybe Integer
      -- ^ The new Provisioned IOPS (I/O operations per second) value for
      -- the RDS instance. Changing this parameter does not result in an
      -- outage and the change is applied during the next maintenance
      -- window unless the ApplyImmediately parameter is set to true for
      -- this request. Default: Uses existing setting Constraints: Value
      -- supplied must be at least 10% greater than the current value.
      -- Values that are not at least 10% greater than the existing value
      -- are rounded up so that they are 10% greater than the current
      -- value. Type: Integer If you choose to migrate your DB instance
      -- from using standard storage to using Provisioned IOPS, or from
      -- using Provisioned IOPS to using standard storage, the process can
      -- take time. The duration of the migration depends on several
      -- factors such as database load, storage size, storage type
      -- (standard or Provisioned IOPS), amount of IOPS provisioned (if
      -- any), and the number of prior scale storage operations. Typical
      -- migration times are under 24 hours, but the process can take up
      -- to several days in some cases. During the migration, the DB
      -- instance will be available for use, but may experience
      -- performance degradation. While the migration takes place, nightly
      -- backups for the instance will be suspended. No other Amazon RDS
      -- operations can take place for the instance, including modifying
      -- the instance, rebooting the instance, deleting the instance,
      -- creating a read replica for the instance, and creating a DB
      -- snapshot of the instance.
    , _mdbimBackupRetentionPeriod :: Maybe Integer
      -- ^ The number of days to retain automated backups. Setting this
      -- parameter to a positive number enables backups. Setting this
      -- parameter to 0 disables automated backups. Changing this
      -- parameter can result in an outage if you change from 0 to a
      -- non-zero value or from a non-zero value to 0. These changes are
      -- applied during the next maintenance window unless the
      -- ApplyImmediately parameter is set to true for this request. If
      -- you change the parameter from one non-zero value to another
      -- non-zero value, the change is asynchronously applied as soon as
      -- possible. Default: Uses existing setting Constraints: Must be a
      -- value from 0 to 35 Can be specified for a read replica only if
      -- the source is running MySQL 5.6 Cannot be set to 0 if the DB
      -- instance is a source to read replicas.
    , _mdbimAllocatedStorage :: Maybe Integer
      -- ^ The new storage capacity of the RDS instance. Changing this
      -- parameter does not result in an outage and the change is applied
      -- during the next maintenance window unless the ApplyImmediately
      -- parameter is set to true for this request. MySQL Default: Uses
      -- existing setting Valid Values: 5-1024 Constraints: Value supplied
      -- must be at least 10% greater than the current value. Values that
      -- are not at least 10% greater than the existing value are rounded
      -- up so that they are 10% greater than the current value. Type:
      -- Integer Oracle Default: Uses existing setting Valid Values:
      -- 10-1024 Constraints: Value supplied must be at least 10% greater
      -- than the current value. Values that are not at least 10% greater
      -- than the existing value are rounded up so that they are 10%
      -- greater than the current value. SQL Server Cannot be modified. If
      -- you choose to migrate your DB instance from using standard
      -- storage to using Provisioned IOPS, or from using Provisioned IOPS
      -- to using standard storage, the process can take time. The
      -- duration of the migration depends on several factors such as
      -- database load, storage size, storage type (standard or
      -- Provisioned IOPS), amount of IOPS provisioned (if any), and the
      -- number of prior scale storage operations. Typical migration times
      -- are under 24 hours, but the process can take up to several days
      -- in some cases. During the migration, the DB instance will be
      -- available for use, but may experience performance degradation.
      -- While the migration takes place, nightly backups for the instance
      -- will be suspended. No other Amazon RDS operations can take place
      -- for the instance, including modifying the instance, rebooting the
      -- instance, deleting the instance, creating a read replica for the
      -- instance, and creating a DB snapshot of the instance.
    , _mdbimEngineVersion :: Maybe Text
      -- ^ The version number of the database engine to upgrade to. Changing
      -- this parameter results in an outage and the change is applied
      -- during the next maintenance window unless the ApplyImmediately
      -- parameter is set to true for this request. For major version
      -- upgrades, if a non-default DB parameter group is currently in
      -- use, a new DB parameter group in the DB parameter group family
      -- for the new engine version must be specified. The new DB
      -- parameter group can be the default for that DB parameter group
      -- family. Example: 5.1.42.
    , _mdbimMasterUserPassword :: Maybe Text
      -- ^ The new password for the DB instance master user. Can be any
      -- printable ASCII character except "/", """, or "@". Changing this
      -- parameter does not result in an outage and the change is
      -- asynchronously applied as soon as possible. Between the time of
      -- the request and the completion of the request, the
      -- MasterUserPassword element exists in the PendingModifiedValues
      -- element of the operation response. Default: Uses existing setting
      -- Constraints: Must be 8 to 41 alphanumeric characters (MySQL), 8
      -- to 30 alphanumeric characters (Oracle), or 8 to 128 alphanumeric
      -- characters (SQL Server). Amazon RDS API actions never return the
      -- password, so this action provides a way to regain access to a
      -- master instance user if the password is lost.
    , _mdbimNewDBInstanceIdentifier :: Maybe Text
      -- ^ The new DB instance identifier for the DB instance when renaming
      -- a DB Instance. This value is stored as a lowercase string.
      -- Constraints: Must contain from 1 to 63 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _mdbimDBInstanceClass :: Maybe Text
      -- ^ The new compute and memory capacity of the DB instance. To
      -- determine the instance classes that are available for a
      -- particular DB engine, use the DescribeOrderableDBInstanceOptions
      -- action. Passing a value for this parameter causes an outage
      -- during the change and is applied during the next maintenance
      -- window, unless the ApplyImmediately parameter is specified as
      -- true for this request. Default: Uses existing setting Valid
      -- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
      -- db.m1.xlarge | db.m2.xlarge | db.m2.2xlarge | db.m2.4xlarge.
    , _mdbimPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur, which may result in an outage. Changing this parameter
      -- does not result in an outage, except in the following situation,
      -- and the change is asynchronously applied as soon as possible. If
      -- there are pending actions that cause a reboot, and the
      -- maintenance window is changed to include the current time, then
      -- changing this parameter will cause a reboot of the DB instance.
      -- If moving this window to the current time, there must be at least
      -- 30 minutes between the current time and end of the window to
      -- ensure pending changes are applied. Default: Uses existing
      -- setting Format: ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue |
      -- Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30
      -- minutes.
    , _mdbimDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group to apply to this DB instance.
      -- Changing this parameter does not result in an outage and the
      -- change is applied during the next maintenance window unless the
      -- ApplyImmediately parameter is set to true for this request.
      -- Default: Uses existing setting Constraints: The DB parameter
      -- group must be in the same DB parameter group family as this DB
      -- instance.
    , _mdbimPreferredBackupWindow :: Maybe Text
      -- ^ The daily time range during which automated backups are created
      -- if automated backups are enabled, as determined by the
      -- BackupRetentionPeriod. Changing this parameter does not result in
      -- an outage and the change is asynchronously applied as soon as
      -- possible. Constraints: Must be in the format hh24:mi-hh24:mi
      -- Times should be Universal Time Coordinated (UTC) Must not
      -- conflict with the preferred maintenance window Must be at least
      -- 30 minutes.
    , _mdbimOptionGroupName :: Maybe Text
      -- ^ Indicates that the DB instance should be associated with the
      -- specified option group. Changing this parameter does not result
      -- in an outage except in the following case and the change is
      -- applied during the next maintenance window unless the
      -- ApplyImmediately parameter is set to true for this request. If
      -- the parameter change results in an option group that enables OEM,
      -- this change can cause a brief (sub-second) period during which
      -- new connections are rejected but existing connections are not
      -- interrupted. cannot be removed from an option group while DB
      -- instances are associated with the option group. --> Permanent
      -- options, such as the TDE option for Oracle Advanced Security TDE,
      -- cannot be removed from an option group, and that option group
      -- cannot be removed from a DB instance once it is associated with a
      -- DB instance.
    , _mdbimVpcSecurityGroupIds :: [Text]
      -- ^ A list of EC2 VPC security groups to authorize on this DB
      -- instance. This change is asynchronously applied as soon as
      -- possible. Constraints: Must be 1 to 255 alphanumeric characters
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    } deriving (Show, Generic)

makeLenses ''ModifyDBInstance

instance ToQuery ModifyDBInstance where
    toQuery = genericQuery def

data ModifyDBInstanceResponse = ModifyDBInstanceResponse
    { _dbixDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

makeLenses ''ModifyDBInstanceResponse

instance FromXML ModifyDBInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyDBInstance where
    type Sv ModifyDBInstance = RDS
    type Rs ModifyDBInstance = ModifyDBInstanceResponse

    request = post "ModifyDBInstance"
    response _ = xmlResponse
