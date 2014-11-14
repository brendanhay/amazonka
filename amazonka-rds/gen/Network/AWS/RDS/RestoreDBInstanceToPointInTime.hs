{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores a DB instance to an arbitrary point-in-time. Users can restore to
-- any point in time before the latestRestorableTime for up to
-- backupRetentionPeriod days. The target database is created from the source
-- database with the same configuration as the original database except that
-- the DB instance is created with the default DB security group.
module Network.AWS.RDS.RestoreDBInstanceToPointInTime
    (
    -- * Request
      RestoreDBInstanceToPointInTime
    -- ** Request constructor
    , restoreDBInstanceToPointInTime
    -- ** Request lenses
    , rdbitpitAutoMinorVersionUpgrade
    , rdbitpitAvailabilityZone
    , rdbitpitDBInstanceClass
    , rdbitpitDBName
    , rdbitpitDBSubnetGroupName
    , rdbitpitEngine
    , rdbitpitIops
    , rdbitpitLicenseModel
    , rdbitpitMultiAZ
    , rdbitpitOptionGroupName
    , rdbitpitPort
    , rdbitpitPubliclyAccessible
    , rdbitpitRestoreTime
    , rdbitpitSourceDBInstanceIdentifier
    , rdbitpitStorageType
    , rdbitpitTags
    , rdbitpitTargetDBInstanceIdentifier
    , rdbitpitTdeCredentialArn
    , rdbitpitTdeCredentialPassword
    , rdbitpitUseLatestRestorableTime

    -- * Response
    , RestoreDBInstanceToPointInTimeResponse
    -- ** Response constructor
    , restoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , rdbitpitrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime
    { _rdbitpitAutoMinorVersionUpgrade    :: Maybe Bool
    , _rdbitpitAvailabilityZone           :: Maybe Text
    , _rdbitpitDBInstanceClass            :: Maybe Text
    , _rdbitpitDBName                     :: Maybe Text
    , _rdbitpitDBSubnetGroupName          :: Maybe Text
    , _rdbitpitEngine                     :: Maybe Text
    , _rdbitpitIops                       :: Maybe Int
    , _rdbitpitLicenseModel               :: Maybe Text
    , _rdbitpitMultiAZ                    :: Maybe Bool
    , _rdbitpitOptionGroupName            :: Maybe Text
    , _rdbitpitPort                       :: Maybe Int
    , _rdbitpitPubliclyAccessible         :: Maybe Bool
    , _rdbitpitRestoreTime                :: Maybe RFC822
    , _rdbitpitSourceDBInstanceIdentifier :: Text
    , _rdbitpitStorageType                :: Maybe Text
    , _rdbitpitTags                       :: [Tag]
    , _rdbitpitTargetDBInstanceIdentifier :: Text
    , _rdbitpitTdeCredentialArn           :: Maybe Text
    , _rdbitpitTdeCredentialPassword      :: Maybe Text
    , _rdbitpitUseLatestRestorableTime    :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceToPointInTime' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbitpitAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitDBName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitEngine' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitIops' @::@ 'Maybe' 'Int'
--
-- * 'rdbitpitLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitPort' @::@ 'Maybe' 'Int'
--
-- * 'rdbitpitPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitRestoreTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'rdbitpitSourceDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbitpitStorageType' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitTags' @::@ ['Tag']
--
-- * 'rdbitpitTargetDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbitpitTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitUseLatestRestorableTime' @::@ 'Maybe' 'Bool'
--
restoreDBInstanceToPointInTime :: Text -- ^ 'rdbitpitSourceDBInstanceIdentifier'
                               -> Text -- ^ 'rdbitpitTargetDBInstanceIdentifier'
                               -> RestoreDBInstanceToPointInTime
restoreDBInstanceToPointInTime p1 p2 = RestoreDBInstanceToPointInTime
    { _rdbitpitSourceDBInstanceIdentifier = p1
    , _rdbitpitTargetDBInstanceIdentifier = p2
    , _rdbitpitRestoreTime                = Nothing
    , _rdbitpitUseLatestRestorableTime    = Nothing
    , _rdbitpitDBInstanceClass            = Nothing
    , _rdbitpitPort                       = Nothing
    , _rdbitpitAvailabilityZone           = Nothing
    , _rdbitpitDBSubnetGroupName          = Nothing
    , _rdbitpitMultiAZ                    = Nothing
    , _rdbitpitPubliclyAccessible         = Nothing
    , _rdbitpitAutoMinorVersionUpgrade    = Nothing
    , _rdbitpitLicenseModel               = Nothing
    , _rdbitpitDBName                     = Nothing
    , _rdbitpitEngine                     = Nothing
    , _rdbitpitIops                       = Nothing
    , _rdbitpitOptionGroupName            = Nothing
    , _rdbitpitTags                       = mempty
    , _rdbitpitStorageType                = Nothing
    , _rdbitpitTdeCredentialArn           = Nothing
    , _rdbitpitTdeCredentialPassword      = Nothing
    }

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rdbitpitAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitAutoMinorVersionUpgrade =
    lens _rdbitpitAutoMinorVersionUpgrade
        (\s a -> s { _rdbitpitAutoMinorVersionUpgrade = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true. Example: us-east-1a.
rdbitpitAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitAvailabilityZone =
    lens _rdbitpitAvailabilityZone
        (\s a -> s { _rdbitpitAvailabilityZone = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large
-- | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |
-- db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small
-- | db.t2.medium Default: The same DBInstanceClass as the original DB
-- instance.
rdbitpitDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBInstanceClass =
    lens _rdbitpitDBInstanceClass (\s a -> s { _rdbitpitDBInstanceClass = a })

-- | The database name for the restored DB instance.
rdbitpitDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBName = lens _rdbitpitDBName (\s a -> s { _rdbitpitDBName = a })

-- | The DB subnet group name to use for the new instance.
rdbitpitDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBSubnetGroupName =
    lens _rdbitpitDBSubnetGroupName
        (\s a -> s { _rdbitpitDBSubnetGroupName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbitpitEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitEngine = lens _rdbitpitEngine (\s a -> s { _rdbitpitEngine = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000. SQL Server Setting the IOPS value for the SQL Server
-- database engine is not supported.
rdbitpitIops :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rdbitpitIops = lens _rdbitpitIops (\s a -> s { _rdbitpitIops = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbitpitLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitLicenseModel =
    lens _rdbitpitLicenseModel (\s a -> s { _rdbitpitLicenseModel = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbitpitMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitMultiAZ = lens _rdbitpitMultiAZ (\s a -> s { _rdbitpitMultiAZ = a })

-- | The name of the option group to be used for the restored DB instance.
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group cannot
-- be removed from a DB instance once it is associated with a DB instance.
rdbitpitOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitOptionGroupName =
    lens _rdbitpitOptionGroupName (\s a -> s { _rdbitpitOptionGroupName = a })

-- | The port number on which the database accepts connections. Constraints:
-- Value must be 1150-65535 Default: The same port as the original DB
-- instance.
rdbitpitPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rdbitpitPort = lens _rdbitpitPort (\s a -> s { _rdbitpitPort = a })

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
rdbitpitPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitPubliclyAccessible =
    lens _rdbitpitPubliclyAccessible
        (\s a -> s { _rdbitpitPubliclyAccessible = a })

-- | The date and time to restore from. Valid Values: Value must be a UTC time
-- Constraints: Must be before the latest restorable time for the DB
-- instance Cannot be specified if UseLatestRestorableTime parameter is true
-- Example: 2009-09-07T23:45:00Z.
rdbitpitRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe UTCTime)
rdbitpitRestoreTime =
    lens _rdbitpitRestoreTime (\s a -> s { _rdbitpitRestoreTime = a })
        . mapping _Time

-- | The identifier of the source DB instance from which to restore.
-- Constraints: Must be the identifier of an existing database instance Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rdbitpitSourceDBInstanceIdentifier =
    lens _rdbitpitSourceDBInstanceIdentifier
        (\s a -> s { _rdbitpitSourceDBInstanceIdentifier = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
rdbitpitStorageType :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitStorageType =
    lens _rdbitpitStorageType (\s a -> s { _rdbitpitStorageType = a })

rdbitpitTags :: Lens' RestoreDBInstanceToPointInTime [Tag]
rdbitpitTags = lens _rdbitpitTags (\s a -> s { _rdbitpitTags = a })

-- | The name of the new database instance to be created. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rdbitpitTargetDBInstanceIdentifier =
    lens _rdbitpitTargetDBInstanceIdentifier
        (\s a -> s { _rdbitpitTargetDBInstanceIdentifier = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rdbitpitTdeCredentialArn :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitTdeCredentialArn =
    lens _rdbitpitTdeCredentialArn
        (\s a -> s { _rdbitpitTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rdbitpitTdeCredentialPassword :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitTdeCredentialPassword =
    lens _rdbitpitTdeCredentialPassword
        (\s a -> s { _rdbitpitTdeCredentialPassword = a })

-- | Specifies whether (true) or not (false) the DB instance is restored from
-- the latest backup time. Default: false Constraints: Cannot be specified
-- if RestoreTime parameter is provided.
rdbitpitUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitUseLatestRestorableTime =
    lens _rdbitpitUseLatestRestorableTime
        (\s a -> s { _rdbitpitUseLatestRestorableTime = a })

instance ToQuery RestoreDBInstanceToPointInTime

instance ToPath RestoreDBInstanceToPointInTime where
    toPath = const "/"

newtype RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { _rdbitpitrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceToPointInTimeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbitpitrDBInstance' @::@ 'Maybe' 'DBInstance'
--
restoreDBInstanceToPointInTimeResponse :: RestoreDBInstanceToPointInTimeResponse
restoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { _rdbitpitrDBInstance = Nothing
    }

rdbitpitrDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
rdbitpitrDBInstance =
    lens _rdbitpitrDBInstance (\s a -> s { _rdbitpitrDBInstance = a })

instance AWSRequest RestoreDBInstanceToPointInTime where
    type Sv RestoreDBInstanceToPointInTime = RDS
    type Rs RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTimeResponse

    request  = post "RestoreDBInstanceToPointInTime"
    response = xmlResponse $ \h x -> RestoreDBInstanceToPointInTimeResponse
        <$> x %| "DBInstance"
