{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
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

-- | Creates a new DB instance from a DB snapshot. The target database is
-- created from the source database restore point with the same
-- configuration as the original source database, except that the new RDS
-- instance is created with the default security group.
--
-- If your intent is to replace your original DB instance with the new,
-- restored DB instance, then rename your original DB instance before you
-- call the RestoreDBInstanceFromDBSnapshot action. RDS does not allow two
-- DB instances with the same name. Once you have renamed your original DB
-- instance with a different identifier, then you can pass the original
-- name of the DB instance as the DBInstanceIdentifier in the call to the
-- RestoreDBInstanceFromDBSnapshot action. The result is that you will
-- replace the original DB instance with the DB instance created from the
-- snapshot.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RestoreDBInstanceFromDBSnapshot.html>
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
    (
    -- * Request
      RestoreDBInstanceFromDBSnapshot
    -- ** Request constructor
    , restoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdifdsAutoMinorVersionUpgrade
    , rdifdsPubliclyAccessible
    , rdifdsDBSubnetGroupName
    , rdifdsIOPS
    , rdifdsEngine
    , rdifdsTDECredentialPassword
    , rdifdsDBInstanceClass
    , rdifdsLicenseModel
    , rdifdsAvailabilityZone
    , rdifdsMultiAZ
    , rdifdsTDECredentialARN
    , rdifdsOptionGroupName
    , rdifdsDBName
    , rdifdsTags
    , rdifdsPort
    , rdifdsStorageType
    , rdifdsDBInstanceIdentifier
    , rdifdsDBSnapshotIdentifier

    -- * Response
    , RestoreDBInstanceFromDBSnapshotResponse
    -- ** Response constructor
    , restoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , rdifdsrDBInstance
    , rdifdsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'restoreDBInstanceFromDBSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdifdsAutoMinorVersionUpgrade'
--
-- * 'rdifdsPubliclyAccessible'
--
-- * 'rdifdsDBSubnetGroupName'
--
-- * 'rdifdsIOPS'
--
-- * 'rdifdsEngine'
--
-- * 'rdifdsTDECredentialPassword'
--
-- * 'rdifdsDBInstanceClass'
--
-- * 'rdifdsLicenseModel'
--
-- * 'rdifdsAvailabilityZone'
--
-- * 'rdifdsMultiAZ'
--
-- * 'rdifdsTDECredentialARN'
--
-- * 'rdifdsOptionGroupName'
--
-- * 'rdifdsDBName'
--
-- * 'rdifdsTags'
--
-- * 'rdifdsPort'
--
-- * 'rdifdsStorageType'
--
-- * 'rdifdsDBInstanceIdentifier'
--
-- * 'rdifdsDBSnapshotIdentifier'
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
    { _rdifdsAutoMinorVersionUpgrade :: !(Maybe Bool)
    , _rdifdsPubliclyAccessible      :: !(Maybe Bool)
    , _rdifdsDBSubnetGroupName       :: !(Maybe Text)
    , _rdifdsIOPS                    :: !(Maybe Int)
    , _rdifdsEngine                  :: !(Maybe Text)
    , _rdifdsTDECredentialPassword   :: !(Maybe Text)
    , _rdifdsDBInstanceClass         :: !(Maybe Text)
    , _rdifdsLicenseModel            :: !(Maybe Text)
    , _rdifdsAvailabilityZone        :: !(Maybe Text)
    , _rdifdsMultiAZ                 :: !(Maybe Bool)
    , _rdifdsTDECredentialARN        :: !(Maybe Text)
    , _rdifdsOptionGroupName         :: !(Maybe Text)
    , _rdifdsDBName                  :: !(Maybe Text)
    , _rdifdsTags                    :: !(Maybe [Tag])
    , _rdifdsPort                    :: !(Maybe Int)
    , _rdifdsStorageType             :: !(Maybe Text)
    , _rdifdsDBInstanceIdentifier    :: !Text
    , _rdifdsDBSnapshotIdentifier    :: !Text
    } deriving (Eq,Read,Show)

-- | 'RestoreDBInstanceFromDBSnapshot' smart constructor.
restoreDBInstanceFromDBSnapshot :: Text -> Text -> RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot pDBInstanceIdentifier pDBSnapshotIdentifier =
    RestoreDBInstanceFromDBSnapshot'
    { _rdifdsAutoMinorVersionUpgrade = Nothing
    , _rdifdsPubliclyAccessible = Nothing
    , _rdifdsDBSubnetGroupName = Nothing
    , _rdifdsIOPS = Nothing
    , _rdifdsEngine = Nothing
    , _rdifdsTDECredentialPassword = Nothing
    , _rdifdsDBInstanceClass = Nothing
    , _rdifdsLicenseModel = Nothing
    , _rdifdsAvailabilityZone = Nothing
    , _rdifdsMultiAZ = Nothing
    , _rdifdsTDECredentialARN = Nothing
    , _rdifdsOptionGroupName = Nothing
    , _rdifdsDBName = Nothing
    , _rdifdsTags = Nothing
    , _rdifdsPort = Nothing
    , _rdifdsStorageType = Nothing
    , _rdifdsDBInstanceIdentifier = pDBInstanceIdentifier
    , _rdifdsDBSnapshotIdentifier = pDBSnapshotIdentifier
    }

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rdifdsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsAutoMinorVersionUpgrade = lens _rdifdsAutoMinorVersionUpgrade (\ s a -> s{_rdifdsAutoMinorVersionUpgrade = a});

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS
-- name, which resolves to a public IP address. A value of false specifies
-- an internal instance with a DNS name that resolves to a private IP
-- address.
--
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case.
--
-- -   __Default VPC:__ true
-- -   __VPC:__ false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
rdifdsPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsPubliclyAccessible = lens _rdifdsPubliclyAccessible (\ s a -> s{_rdifdsPubliclyAccessible = a});

-- | The DB subnet group name to use for the new instance.
rdifdsDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBSubnetGroupName = lens _rdifdsDBSubnetGroupName (\ s a -> s{_rdifdsDBSubnetGroupName = a});

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I\/O operations per second. If this parameter is not specified, the
-- IOPS value will be taken from the backup. If this parameter is set to 0,
-- the new instance will be converted to a non-PIOPS instance, which will
-- take additional time, though your DB instance will be available for
-- connections before the conversion starts.
--
-- Constraints: Must be an integer greater than 1000.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine is not
-- supported.
rdifdsIOPS :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsIOPS = lens _rdifdsIOPS (\ s a -> s{_rdifdsIOPS = a});

-- | The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid Values: @MySQL@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ |
-- @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ |
-- @postgres@
rdifdsEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsEngine = lens _rdifdsEngine (\ s a -> s{_rdifdsEngine = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rdifdsTDECredentialPassword :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialPassword = lens _rdifdsTDECredentialPassword (\ s a -> s{_rdifdsTDECredentialPassword = a});

-- | The compute and memory capacity of the Amazon RDS DB instance.
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
rdifdsDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBInstanceClass = lens _rdifdsDBInstanceClass (\ s a -> s{_rdifdsDBInstanceClass = a});

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
rdifdsLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsLicenseModel = lens _rdifdsLicenseModel (\ s a -> s{_rdifdsLicenseModel = a});

-- | The EC2 Availability Zone that the database instance will be created in.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to @true@.
--
-- Example: @us-east-1a@
rdifdsAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsAvailabilityZone = lens _rdifdsAvailabilityZone (\ s a -> s{_rdifdsAvailabilityZone = a});

-- | Specifies if the DB instance is a Multi-AZ deployment.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to @true@.
rdifdsMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsMultiAZ = lens _rdifdsMultiAZ (\ s a -> s{_rdifdsMultiAZ = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rdifdsTDECredentialARN :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialARN = lens _rdifdsTDECredentialARN (\ s a -> s{_rdifdsTDECredentialARN = a});

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
rdifdsOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsOptionGroupName = lens _rdifdsOptionGroupName (\ s a -> s{_rdifdsOptionGroupName = a});

-- | The database name for the restored DB instance.
--
-- This parameter doesn\'t apply to the MySQL engine.
rdifdsDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBName = lens _rdifdsDBName (\ s a -> s{_rdifdsDBName = a});

-- | FIXME: Undocumented member.
rdifdsTags :: Lens' RestoreDBInstanceFromDBSnapshot [Tag]
rdifdsTags = lens _rdifdsTags (\ s a -> s{_rdifdsTags = a}) . _Default;

-- | The port number on which the database accepts connections.
--
-- Default: The same port as the original DB instance
--
-- Constraints: Value must be @1150-65535@
rdifdsPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsPort = lens _rdifdsPort (\ s a -> s{_rdifdsPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
rdifdsStorageType :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsStorageType = lens _rdifdsStorageType (\ s a -> s{_rdifdsStorageType = a});

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
rdifdsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBInstanceIdentifier = lens _rdifdsDBInstanceIdentifier (\ s a -> s{_rdifdsDBInstanceIdentifier = a});

-- | The identifier for the DB snapshot to restore from.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rdifdsDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBSnapshotIdentifier = lens _rdifdsDBSnapshotIdentifier (\ s a -> s{_rdifdsDBSnapshotIdentifier = a});

instance AWSRequest RestoreDBInstanceFromDBSnapshot
         where
        type Sv RestoreDBInstanceFromDBSnapshot = RDS
        type Rs RestoreDBInstanceFromDBSnapshot =
             RestoreDBInstanceFromDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper
              "RestoreDBInstanceFromDBSnapshotResult"
              (\ s h x ->
                 RestoreDBInstanceFromDBSnapshotResponse' <$>
                   (x .@? "DBInstance") <*> (pure s))

instance ToHeaders RestoreDBInstanceFromDBSnapshot
         where
        toHeaders = const mempty

instance ToPath RestoreDBInstanceFromDBSnapshot where
        toPath = const "/"

instance ToQuery RestoreDBInstanceFromDBSnapshot
         where
        toQuery RestoreDBInstanceFromDBSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBInstanceFromDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "AutoMinorVersionUpgrade" =:
                 _rdifdsAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _rdifdsPubliclyAccessible,
               "DBSubnetGroupName" =: _rdifdsDBSubnetGroupName,
               "Iops" =: _rdifdsIOPS, "Engine" =: _rdifdsEngine,
               "TdeCredentialPassword" =:
                 _rdifdsTDECredentialPassword,
               "DBInstanceClass" =: _rdifdsDBInstanceClass,
               "LicenseModel" =: _rdifdsLicenseModel,
               "AvailabilityZone" =: _rdifdsAvailabilityZone,
               "MultiAZ" =: _rdifdsMultiAZ,
               "TdeCredentialArn" =: _rdifdsTDECredentialARN,
               "OptionGroupName" =: _rdifdsOptionGroupName,
               "DBName" =: _rdifdsDBName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rdifdsTags),
               "Port" =: _rdifdsPort,
               "StorageType" =: _rdifdsStorageType,
               "DBInstanceIdentifier" =:
                 _rdifdsDBInstanceIdentifier,
               "DBSnapshotIdentifier" =:
                 _rdifdsDBSnapshotIdentifier]

-- | /See:/ 'restoreDBInstanceFromDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdifdsrDBInstance'
--
-- * 'rdifdsrStatus'
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
    { _rdifdsrDBInstance :: !(Maybe DBInstance)
    , _rdifdsrStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'RestoreDBInstanceFromDBSnapshotResponse' smart constructor.
restoreDBInstanceFromDBSnapshotResponse :: Status -> RestoreDBInstanceFromDBSnapshotResponse
restoreDBInstanceFromDBSnapshotResponse pStatus =
    RestoreDBInstanceFromDBSnapshotResponse'
    { _rdifdsrDBInstance = Nothing
    , _rdifdsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rdifdsrDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
rdifdsrDBInstance = lens _rdifdsrDBInstance (\ s a -> s{_rdifdsrDBInstance = a});

-- | FIXME: Undocumented member.
rdifdsrStatus :: Lens' RestoreDBInstanceFromDBSnapshotResponse Status
rdifdsrStatus = lens _rdifdsrStatus (\ s a -> s{_rdifdsrStatus = a});
