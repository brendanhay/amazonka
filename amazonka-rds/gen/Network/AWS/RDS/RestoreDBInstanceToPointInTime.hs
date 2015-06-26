{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
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

-- | Restores a DB instance to an arbitrary point-in-time. Users can restore
-- to any point in time before the LatestRestorableTime for up to
-- BackupRetentionPeriod days. The target database is created from the
-- source database with the same configuration as the original database
-- except that the DB instance is created with the default DB security
-- group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RestoreDBInstanceToPointInTime.html>
module Network.AWS.RDS.RestoreDBInstanceToPointInTime
    (
    -- * Request
      RestoreDBInstanceToPointInTime
    -- ** Request constructor
    , restoreDBInstanceToPointInTime
    -- ** Request lenses
    , rditpitUseLatestRestorableTime
    , rditpitAutoMinorVersionUpgrade
    , rditpitPubliclyAccessible
    , rditpitDBSubnetGroupName
    , rditpitRestoreTime
    , rditpitIOPS
    , rditpitEngine
    , rditpitTDECredentialPassword
    , rditpitDBInstanceClass
    , rditpitLicenseModel
    , rditpitAvailabilityZone
    , rditpitMultiAZ
    , rditpitTDECredentialARN
    , rditpitOptionGroupName
    , rditpitDBName
    , rditpitTags
    , rditpitPort
    , rditpitStorageType
    , rditpitSourceDBInstanceIdentifier
    , rditpitTargetDBInstanceIdentifier

    -- * Response
    , RestoreDBInstanceToPointInTimeResponse
    -- ** Response constructor
    , restoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , rditpitrDBInstance
    , rditpitrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'restoreDBInstanceToPointInTime' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rditpitUseLatestRestorableTime'
--
-- * 'rditpitAutoMinorVersionUpgrade'
--
-- * 'rditpitPubliclyAccessible'
--
-- * 'rditpitDBSubnetGroupName'
--
-- * 'rditpitRestoreTime'
--
-- * 'rditpitIOPS'
--
-- * 'rditpitEngine'
--
-- * 'rditpitTDECredentialPassword'
--
-- * 'rditpitDBInstanceClass'
--
-- * 'rditpitLicenseModel'
--
-- * 'rditpitAvailabilityZone'
--
-- * 'rditpitMultiAZ'
--
-- * 'rditpitTDECredentialARN'
--
-- * 'rditpitOptionGroupName'
--
-- * 'rditpitDBName'
--
-- * 'rditpitTags'
--
-- * 'rditpitPort'
--
-- * 'rditpitStorageType'
--
-- * 'rditpitSourceDBInstanceIdentifier'
--
-- * 'rditpitTargetDBInstanceIdentifier'
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'{_rditpitUseLatestRestorableTime :: Maybe Bool, _rditpitAutoMinorVersionUpgrade :: Maybe Bool, _rditpitPubliclyAccessible :: Maybe Bool, _rditpitDBSubnetGroupName :: Maybe Text, _rditpitRestoreTime :: Maybe ISO8601, _rditpitIOPS :: Maybe Int, _rditpitEngine :: Maybe Text, _rditpitTDECredentialPassword :: Maybe Text, _rditpitDBInstanceClass :: Maybe Text, _rditpitLicenseModel :: Maybe Text, _rditpitAvailabilityZone :: Maybe Text, _rditpitMultiAZ :: Maybe Bool, _rditpitTDECredentialARN :: Maybe Text, _rditpitOptionGroupName :: Maybe Text, _rditpitDBName :: Maybe Text, _rditpitTags :: Maybe [Tag], _rditpitPort :: Maybe Int, _rditpitStorageType :: Maybe Text, _rditpitSourceDBInstanceIdentifier :: Text, _rditpitTargetDBInstanceIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'RestoreDBInstanceToPointInTime' smart constructor.
restoreDBInstanceToPointInTime :: Text -> Text -> RestoreDBInstanceToPointInTime
restoreDBInstanceToPointInTime pSourceDBInstanceIdentifier pTargetDBInstanceIdentifier = RestoreDBInstanceToPointInTime'{_rditpitUseLatestRestorableTime = Nothing, _rditpitAutoMinorVersionUpgrade = Nothing, _rditpitPubliclyAccessible = Nothing, _rditpitDBSubnetGroupName = Nothing, _rditpitRestoreTime = Nothing, _rditpitIOPS = Nothing, _rditpitEngine = Nothing, _rditpitTDECredentialPassword = Nothing, _rditpitDBInstanceClass = Nothing, _rditpitLicenseModel = Nothing, _rditpitAvailabilityZone = Nothing, _rditpitMultiAZ = Nothing, _rditpitTDECredentialARN = Nothing, _rditpitOptionGroupName = Nothing, _rditpitDBName = Nothing, _rditpitTags = Nothing, _rditpitPort = Nothing, _rditpitStorageType = Nothing, _rditpitSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier, _rditpitTargetDBInstanceIdentifier = pTargetDBInstanceIdentifier};

-- | Specifies whether (@true@) or not (@false@) the DB instance is restored
-- from the latest backup time.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if RestoreTime parameter is provided.
rditpitUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitUseLatestRestorableTime = lens _rditpitUseLatestRestorableTime (\ s a -> s{_rditpitUseLatestRestorableTime = a});

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rditpitAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitAutoMinorVersionUpgrade = lens _rditpitAutoMinorVersionUpgrade (\ s a -> s{_rditpitAutoMinorVersionUpgrade = a});

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
-- -   __Default VPC:__true
-- -   __VPC:__false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
rditpitPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitPubliclyAccessible = lens _rditpitPubliclyAccessible (\ s a -> s{_rditpitPubliclyAccessible = a});

-- | The DB subnet group name to use for the new instance.
rditpitDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBSubnetGroupName = lens _rditpitDBSubnetGroupName (\ s a -> s{_rditpitDBSubnetGroupName = a});

-- | The date and time to restore from.
--
-- Valid Values: Value must be a UTC time
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
-- -   Cannot be specified if UseLatestRestorableTime parameter is true
--
-- Example: @2009-09-07T23:45:00Z@
rditpitRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe UTCTime)
rditpitRestoreTime = lens _rditpitRestoreTime (\ s a -> s{_rditpitRestoreTime = a}) . mapping _Time;

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine is not
-- supported.
rditpitIOPS :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitIOPS = lens _rditpitIOPS (\ s a -> s{_rditpitIOPS = a});

-- | The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid Values: @MySQL@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ |
-- @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ |
-- @postgres@
rditpitEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitEngine = lens _rditpitEngine (\ s a -> s{_rditpitEngine = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rditpitTDECredentialPassword :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitTDECredentialPassword = lens _rditpitTDECredentialPassword (\ s a -> s{_rditpitTDECredentialPassword = a});

-- | The compute and memory capacity of the Amazon RDS DB instance.
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
--
-- Default: The same DBInstanceClass as the original DB instance.
rditpitDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBInstanceClass = lens _rditpitDBInstanceClass (\ s a -> s{_rditpitDBInstanceClass = a});

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
rditpitLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitLicenseModel = lens _rditpitLicenseModel (\ s a -> s{_rditpitLicenseModel = a});

-- | The EC2 Availability Zone that the database instance will be created in.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to true.
--
-- Example: @us-east-1a@
rditpitAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitAvailabilityZone = lens _rditpitAvailabilityZone (\ s a -> s{_rditpitAvailabilityZone = a});

-- | Specifies if the DB instance is a Multi-AZ deployment.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to @true@.
rditpitMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitMultiAZ = lens _rditpitMultiAZ (\ s a -> s{_rditpitMultiAZ = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rditpitTDECredentialARN :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitTDECredentialARN = lens _rditpitTDECredentialARN (\ s a -> s{_rditpitTDECredentialARN = a});

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
rditpitOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitOptionGroupName = lens _rditpitOptionGroupName (\ s a -> s{_rditpitOptionGroupName = a});

-- | The database name for the restored DB instance.
--
-- This parameter is not used for the MySQL engine.
rditpitDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBName = lens _rditpitDBName (\ s a -> s{_rditpitDBName = a});

-- | FIXME: Undocumented member.
rditpitTags :: Lens' RestoreDBInstanceToPointInTime [Tag]
rditpitTags = lens _rditpitTags (\ s a -> s{_rditpitTags = a}) . _Default;

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
rditpitPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitPort = lens _rditpitPort (\ s a -> s{_rditpitPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
rditpitStorageType :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitStorageType = lens _rditpitStorageType (\ s a -> s{_rditpitStorageType = a});

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must be the identifier of an existing database instance
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rditpitSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitSourceDBInstanceIdentifier = lens _rditpitSourceDBInstanceIdentifier (\ s a -> s{_rditpitSourceDBInstanceIdentifier = a});

-- | The name of the new database instance to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rditpitTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitTargetDBInstanceIdentifier = lens _rditpitTargetDBInstanceIdentifier (\ s a -> s{_rditpitTargetDBInstanceIdentifier = a});

instance AWSRequest RestoreDBInstanceToPointInTime
         where
        type Sv RestoreDBInstanceToPointInTime = RDS
        type Rs RestoreDBInstanceToPointInTime =
             RestoreDBInstanceToPointInTimeResponse
        request = post
        response
          = receiveXMLWrapper
              "RestoreDBInstanceToPointInTimeResult"
              (\ s h x ->
                 RestoreDBInstanceToPointInTimeResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders RestoreDBInstanceToPointInTime
         where
        toHeaders = const mempty

instance ToPath RestoreDBInstanceToPointInTime where
        toPath = const "/"

instance ToQuery RestoreDBInstanceToPointInTime where
        toQuery RestoreDBInstanceToPointInTime'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBInstanceToPointInTime" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "UseLatestRestorableTime" =:
                 _rditpitUseLatestRestorableTime,
               "AutoMinorVersionUpgrade" =:
                 _rditpitAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _rditpitPubliclyAccessible,
               "DBSubnetGroupName" =: _rditpitDBSubnetGroupName,
               "RestoreTime" =: _rditpitRestoreTime,
               "Iops" =: _rditpitIOPS, "Engine" =: _rditpitEngine,
               "TdeCredentialPassword" =:
                 _rditpitTDECredentialPassword,
               "DBInstanceClass" =: _rditpitDBInstanceClass,
               "LicenseModel" =: _rditpitLicenseModel,
               "AvailabilityZone" =: _rditpitAvailabilityZone,
               "MultiAZ" =: _rditpitMultiAZ,
               "TdeCredentialArn" =: _rditpitTDECredentialARN,
               "OptionGroupName" =: _rditpitOptionGroupName,
               "DBName" =: _rditpitDBName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rditpitTags),
               "Port" =: _rditpitPort,
               "StorageType" =: _rditpitStorageType,
               "SourceDBInstanceIdentifier" =:
                 _rditpitSourceDBInstanceIdentifier,
               "TargetDBInstanceIdentifier" =:
                 _rditpitTargetDBInstanceIdentifier]

-- | /See:/ 'restoreDBInstanceToPointInTimeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rditpitrDBInstance'
--
-- * 'rditpitrStatusCode'
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'{_rditpitrDBInstance :: Maybe DBInstance, _rditpitrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RestoreDBInstanceToPointInTimeResponse' smart constructor.
restoreDBInstanceToPointInTimeResponse :: Int -> RestoreDBInstanceToPointInTimeResponse
restoreDBInstanceToPointInTimeResponse pStatusCode = RestoreDBInstanceToPointInTimeResponse'{_rditpitrDBInstance = Nothing, _rditpitrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
rditpitrDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
rditpitrDBInstance = lens _rditpitrDBInstance (\ s a -> s{_rditpitrDBInstance = a});

-- | FIXME: Undocumented member.
rditpitrStatusCode :: Lens' RestoreDBInstanceToPointInTimeResponse Int
rditpitrStatusCode = lens _rditpitrStatusCode (\ s a -> s{_rditpitrStatusCode = a});
