{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB instance to an arbitrary point-in-time. Users can restore
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
    , rditpitrqUseLatestRestorableTime
    , rditpitrqAutoMinorVersionUpgrade
    , rditpitrqPubliclyAccessible
    , rditpitrqDBSubnetGroupName
    , rditpitrqRestoreTime
    , rditpitrqIOPS
    , rditpitrqEngine
    , rditpitrqTDECredentialPassword
    , rditpitrqDBInstanceClass
    , rditpitrqLicenseModel
    , rditpitrqAvailabilityZone
    , rditpitrqMultiAZ
    , rditpitrqTDECredentialARN
    , rditpitrqOptionGroupName
    , rditpitrqDBName
    , rditpitrqTags
    , rditpitrqPort
    , rditpitrqStorageType
    , rditpitrqSourceDBInstanceIdentifier
    , rditpitrqTargetDBInstanceIdentifier

    -- * Response
    , RestoreDBInstanceToPointInTimeResponse
    -- ** Response constructor
    , restoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , rditpitrsDBInstance
    , rditpitrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'restoreDBInstanceToPointInTime' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rditpitrqUseLatestRestorableTime'
--
-- * 'rditpitrqAutoMinorVersionUpgrade'
--
-- * 'rditpitrqPubliclyAccessible'
--
-- * 'rditpitrqDBSubnetGroupName'
--
-- * 'rditpitrqRestoreTime'
--
-- * 'rditpitrqIOPS'
--
-- * 'rditpitrqEngine'
--
-- * 'rditpitrqTDECredentialPassword'
--
-- * 'rditpitrqDBInstanceClass'
--
-- * 'rditpitrqLicenseModel'
--
-- * 'rditpitrqAvailabilityZone'
--
-- * 'rditpitrqMultiAZ'
--
-- * 'rditpitrqTDECredentialARN'
--
-- * 'rditpitrqOptionGroupName'
--
-- * 'rditpitrqDBName'
--
-- * 'rditpitrqTags'
--
-- * 'rditpitrqPort'
--
-- * 'rditpitrqStorageType'
--
-- * 'rditpitrqSourceDBInstanceIdentifier'
--
-- * 'rditpitrqTargetDBInstanceIdentifier'
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
    { _rditpitrqUseLatestRestorableTime    :: !(Maybe Bool)
    , _rditpitrqAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _rditpitrqPubliclyAccessible         :: !(Maybe Bool)
    , _rditpitrqDBSubnetGroupName          :: !(Maybe Text)
    , _rditpitrqRestoreTime                :: !(Maybe ISO8601)
    , _rditpitrqIOPS                       :: !(Maybe Int)
    , _rditpitrqEngine                     :: !(Maybe Text)
    , _rditpitrqTDECredentialPassword      :: !(Maybe Text)
    , _rditpitrqDBInstanceClass            :: !(Maybe Text)
    , _rditpitrqLicenseModel               :: !(Maybe Text)
    , _rditpitrqAvailabilityZone           :: !(Maybe Text)
    , _rditpitrqMultiAZ                    :: !(Maybe Bool)
    , _rditpitrqTDECredentialARN           :: !(Maybe Text)
    , _rditpitrqOptionGroupName            :: !(Maybe Text)
    , _rditpitrqDBName                     :: !(Maybe Text)
    , _rditpitrqTags                       :: !(Maybe [Tag])
    , _rditpitrqPort                       :: !(Maybe Int)
    , _rditpitrqStorageType                :: !(Maybe Text)
    , _rditpitrqSourceDBInstanceIdentifier :: !Text
    , _rditpitrqTargetDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreDBInstanceToPointInTime' smart constructor.
restoreDBInstanceToPointInTime :: Text -> Text -> RestoreDBInstanceToPointInTime
restoreDBInstanceToPointInTime pSourceDBInstanceIdentifier pTargetDBInstanceIdentifier =
    RestoreDBInstanceToPointInTime'
    { _rditpitrqUseLatestRestorableTime = Nothing
    , _rditpitrqAutoMinorVersionUpgrade = Nothing
    , _rditpitrqPubliclyAccessible = Nothing
    , _rditpitrqDBSubnetGroupName = Nothing
    , _rditpitrqRestoreTime = Nothing
    , _rditpitrqIOPS = Nothing
    , _rditpitrqEngine = Nothing
    , _rditpitrqTDECredentialPassword = Nothing
    , _rditpitrqDBInstanceClass = Nothing
    , _rditpitrqLicenseModel = Nothing
    , _rditpitrqAvailabilityZone = Nothing
    , _rditpitrqMultiAZ = Nothing
    , _rditpitrqTDECredentialARN = Nothing
    , _rditpitrqOptionGroupName = Nothing
    , _rditpitrqDBName = Nothing
    , _rditpitrqTags = Nothing
    , _rditpitrqPort = Nothing
    , _rditpitrqStorageType = Nothing
    , _rditpitrqSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier
    , _rditpitrqTargetDBInstanceIdentifier = pTargetDBInstanceIdentifier
    }

-- | Specifies whether (@true@) or not (@false@) the DB instance is restored
-- from the latest backup time.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if RestoreTime parameter is provided.
rditpitrqUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitrqUseLatestRestorableTime = lens _rditpitrqUseLatestRestorableTime (\ s a -> s{_rditpitrqUseLatestRestorableTime = a});

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rditpitrqAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitrqAutoMinorVersionUpgrade = lens _rditpitrqAutoMinorVersionUpgrade (\ s a -> s{_rditpitrqAutoMinorVersionUpgrade = a});

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
rditpitrqPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitrqPubliclyAccessible = lens _rditpitrqPubliclyAccessible (\ s a -> s{_rditpitrqPubliclyAccessible = a});

-- | The DB subnet group name to use for the new instance.
rditpitrqDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqDBSubnetGroupName = lens _rditpitrqDBSubnetGroupName (\ s a -> s{_rditpitrqDBSubnetGroupName = a});

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
rditpitrqRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe UTCTime)
rditpitrqRestoreTime = lens _rditpitrqRestoreTime (\ s a -> s{_rditpitrqRestoreTime = a}) . mapping _Time;

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: Must be an integer greater than 1000.
--
-- __SQL Server__
--
-- Setting the IOPS value for the SQL Server database engine is not
-- supported.
rditpitrqIOPS :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitrqIOPS = lens _rditpitrqIOPS (\ s a -> s{_rditpitrqIOPS = a});

-- | The database engine to use for the new instance.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
--
-- Valid Values: @MySQL@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ |
-- @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ |
-- @postgres@
rditpitrqEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqEngine = lens _rditpitrqEngine (\ s a -> s{_rditpitrqEngine = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rditpitrqTDECredentialPassword :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqTDECredentialPassword = lens _rditpitrqTDECredentialPassword (\ s a -> s{_rditpitrqTDECredentialPassword = a});

-- | The compute and memory capacity of the Amazon RDS DB instance.
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
--
-- Default: The same DBInstanceClass as the original DB instance.
rditpitrqDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqDBInstanceClass = lens _rditpitrqDBInstanceClass (\ s a -> s{_rditpitrqDBInstanceClass = a});

-- | License model information for the restored DB instance.
--
-- Default: Same as source.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
rditpitrqLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqLicenseModel = lens _rditpitrqLicenseModel (\ s a -> s{_rditpitrqLicenseModel = a});

-- | The EC2 Availability Zone that the database instance will be created in.
--
-- Default: A random, system-chosen Availability Zone.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to true.
--
-- Example: @us-east-1a@
rditpitrqAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqAvailabilityZone = lens _rditpitrqAvailabilityZone (\ s a -> s{_rditpitrqAvailabilityZone = a});

-- | Specifies if the DB instance is a Multi-AZ deployment.
--
-- Constraint: You cannot specify the AvailabilityZone parameter if the
-- MultiAZ parameter is set to @true@.
rditpitrqMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitrqMultiAZ = lens _rditpitrqMultiAZ (\ s a -> s{_rditpitrqMultiAZ = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rditpitrqTDECredentialARN :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqTDECredentialARN = lens _rditpitrqTDECredentialARN (\ s a -> s{_rditpitrqTDECredentialARN = a});

-- | The name of the option group to be used for the restored DB instance.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
rditpitrqOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqOptionGroupName = lens _rditpitrqOptionGroupName (\ s a -> s{_rditpitrqOptionGroupName = a});

-- | The database name for the restored DB instance.
--
-- This parameter is not used for the MySQL engine.
rditpitrqDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqDBName = lens _rditpitrqDBName (\ s a -> s{_rditpitrqDBName = a});

-- | FIXME: Undocumented member.
rditpitrqTags :: Lens' RestoreDBInstanceToPointInTime [Tag]
rditpitrqTags = lens _rditpitrqTags (\ s a -> s{_rditpitrqTags = a}) . _Default;

-- | The port number on which the database accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB instance.
rditpitrqPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitrqPort = lens _rditpitrqPort (\ s a -> s{_rditpitrqPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
rditpitrqStorageType :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitrqStorageType = lens _rditpitrqStorageType (\ s a -> s{_rditpitrqStorageType = a});

-- | The identifier of the source DB instance from which to restore.
--
-- Constraints:
--
-- -   Must be the identifier of an existing database instance
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rditpitrqSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitrqSourceDBInstanceIdentifier = lens _rditpitrqSourceDBInstanceIdentifier (\ s a -> s{_rditpitrqSourceDBInstanceIdentifier = a});

-- | The name of the new database instance to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rditpitrqTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitrqTargetDBInstanceIdentifier = lens _rditpitrqTargetDBInstanceIdentifier (\ s a -> s{_rditpitrqTargetDBInstanceIdentifier = a});

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
                 _rditpitrqUseLatestRestorableTime,
               "AutoMinorVersionUpgrade" =:
                 _rditpitrqAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _rditpitrqPubliclyAccessible,
               "DBSubnetGroupName" =: _rditpitrqDBSubnetGroupName,
               "RestoreTime" =: _rditpitrqRestoreTime,
               "Iops" =: _rditpitrqIOPS,
               "Engine" =: _rditpitrqEngine,
               "TdeCredentialPassword" =:
                 _rditpitrqTDECredentialPassword,
               "DBInstanceClass" =: _rditpitrqDBInstanceClass,
               "LicenseModel" =: _rditpitrqLicenseModel,
               "AvailabilityZone" =: _rditpitrqAvailabilityZone,
               "MultiAZ" =: _rditpitrqMultiAZ,
               "TdeCredentialArn" =: _rditpitrqTDECredentialARN,
               "OptionGroupName" =: _rditpitrqOptionGroupName,
               "DBName" =: _rditpitrqDBName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rditpitrqTags),
               "Port" =: _rditpitrqPort,
               "StorageType" =: _rditpitrqStorageType,
               "SourceDBInstanceIdentifier" =:
                 _rditpitrqSourceDBInstanceIdentifier,
               "TargetDBInstanceIdentifier" =:
                 _rditpitrqTargetDBInstanceIdentifier]

-- | /See:/ 'restoreDBInstanceToPointInTimeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rditpitrsDBInstance'
--
-- * 'rditpitrsStatus'
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
    { _rditpitrsDBInstance :: !(Maybe DBInstance)
    , _rditpitrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreDBInstanceToPointInTimeResponse' smart constructor.
restoreDBInstanceToPointInTimeResponse :: Int -> RestoreDBInstanceToPointInTimeResponse
restoreDBInstanceToPointInTimeResponse pStatus =
    RestoreDBInstanceToPointInTimeResponse'
    { _rditpitrsDBInstance = Nothing
    , _rditpitrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
rditpitrsDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
rditpitrsDBInstance = lens _rditpitrsDBInstance (\ s a -> s{_rditpitrsDBInstance = a});

-- | FIXME: Undocumented member.
rditpitrsStatus :: Lens' RestoreDBInstanceToPointInTimeResponse Int
rditpitrsStatus = lens _rditpitrsStatus (\ s a -> s{_rditpitrsStatus = a});
