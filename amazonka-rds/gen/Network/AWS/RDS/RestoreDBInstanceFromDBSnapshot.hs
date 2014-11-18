{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB instance from a DB snapshot. The target database is
-- created from the source database restore point with the same configuration
-- as the original source database, except that the new RDS instance is
-- created with the default security group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RestoreDBInstanceFromDBSnapshot.html>
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
    (
    -- * Request
      RestoreDBInstanceFromDBSnapshot
    -- ** Request constructor
    , restoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdbifdbsAutoMinorVersionUpgrade
    , rdbifdbsAvailabilityZone
    , rdbifdbsDBInstanceClass
    , rdbifdbsDBInstanceIdentifier
    , rdbifdbsDBName
    , rdbifdbsDBSnapshotIdentifier
    , rdbifdbsDBSubnetGroupName
    , rdbifdbsEngine
    , rdbifdbsIops
    , rdbifdbsLicenseModel
    , rdbifdbsMultiAZ
    , rdbifdbsOptionGroupName
    , rdbifdbsPort
    , rdbifdbsPubliclyAccessible
    , rdbifdbsStorageType
    , rdbifdbsTags
    , rdbifdbsTdeCredentialArn
    , rdbifdbsTdeCredentialPassword

    -- * Response
    , RestoreDBInstanceFromDBSnapshotResponse
    -- ** Response constructor
    , restoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , rdbifdbsrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsAutoMinorVersionUpgrade :: Maybe Bool
    , _rdbifdbsAvailabilityZone        :: Maybe Text
    , _rdbifdbsDBInstanceClass         :: Maybe Text
    , _rdbifdbsDBInstanceIdentifier    :: Text
    , _rdbifdbsDBName                  :: Maybe Text
    , _rdbifdbsDBSnapshotIdentifier    :: Text
    , _rdbifdbsDBSubnetGroupName       :: Maybe Text
    , _rdbifdbsEngine                  :: Maybe Text
    , _rdbifdbsIops                    :: Maybe Int
    , _rdbifdbsLicenseModel            :: Maybe Text
    , _rdbifdbsMultiAZ                 :: Maybe Bool
    , _rdbifdbsOptionGroupName         :: Maybe Text
    , _rdbifdbsPort                    :: Maybe Int
    , _rdbifdbsPubliclyAccessible      :: Maybe Bool
    , _rdbifdbsStorageType             :: Maybe Text
    , _rdbifdbsTags                    :: [Tag]
    , _rdbifdbsTdeCredentialArn        :: Maybe Text
    , _rdbifdbsTdeCredentialPassword   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceFromDBSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbifdbsAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbifdbsDBName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsDBSnapshotIdentifier' @::@ 'Text'
--
-- * 'rdbifdbsDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsEngine' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsIops' @::@ 'Maybe' 'Int'
--
-- * 'rdbifdbsLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsPort' @::@ 'Maybe' 'Int'
--
-- * 'rdbifdbsPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsStorageType' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsTags' @::@ ['Tag']
--
-- * 'rdbifdbsTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
restoreDBInstanceFromDBSnapshot :: Text -- ^ 'rdbifdbsDBInstanceIdentifier'
                                -> Text -- ^ 'rdbifdbsDBSnapshotIdentifier'
                                -> RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot p1 p2 = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsDBInstanceIdentifier    = p1
    , _rdbifdbsDBSnapshotIdentifier    = p2
    , _rdbifdbsDBInstanceClass         = Nothing
    , _rdbifdbsPort                    = Nothing
    , _rdbifdbsAvailabilityZone        = Nothing
    , _rdbifdbsDBSubnetGroupName       = Nothing
    , _rdbifdbsMultiAZ                 = Nothing
    , _rdbifdbsPubliclyAccessible      = Nothing
    , _rdbifdbsAutoMinorVersionUpgrade = Nothing
    , _rdbifdbsLicenseModel            = Nothing
    , _rdbifdbsDBName                  = Nothing
    , _rdbifdbsEngine                  = Nothing
    , _rdbifdbsIops                    = Nothing
    , _rdbifdbsOptionGroupName         = Nothing
    , _rdbifdbsTags                    = mempty
    , _rdbifdbsStorageType             = Nothing
    , _rdbifdbsTdeCredentialArn        = Nothing
    , _rdbifdbsTdeCredentialPassword   = Nothing
    }

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rdbifdbsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsAutoMinorVersionUpgrade =
    lens _rdbifdbsAutoMinorVersionUpgrade
        (\s a -> s { _rdbifdbsAutoMinorVersionUpgrade = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true. Example: us-east-1a.
rdbifdbsAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsAvailabilityZone =
    lens _rdbifdbsAvailabilityZone
        (\s a -> s { _rdbifdbsAvailabilityZone = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large
-- | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |
-- db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small
-- | db.t2.medium.
rdbifdbsDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBInstanceClass =
    lens _rdbifdbsDBInstanceClass (\s a -> s { _rdbifdbsDBInstanceClass = a })

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn't case sensitive. Constraints: Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
rdbifdbsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdbifdbsDBInstanceIdentifier =
    lens _rdbifdbsDBInstanceIdentifier
        (\s a -> s { _rdbifdbsDBInstanceIdentifier = a })

-- | The database name for the restored DB instance.
rdbifdbsDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBName = lens _rdbifdbsDBName (\s a -> s { _rdbifdbsDBName = a })

-- | The identifier for the DB snapshot to restore from. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbifdbsDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdbifdbsDBSnapshotIdentifier =
    lens _rdbifdbsDBSnapshotIdentifier
        (\s a -> s { _rdbifdbsDBSnapshotIdentifier = a })

-- | The DB subnet group name to use for the new instance.
rdbifdbsDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBSubnetGroupName =
    lens _rdbifdbsDBSubnetGroupName
        (\s a -> s { _rdbifdbsDBSubnetGroupName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbifdbsEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsEngine = lens _rdbifdbsEngine (\s a -> s { _rdbifdbsEngine = a })

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I/O operations per second. If this parameter is not specified, the
-- IOPS value will be taken from the backup. If this parameter is set to 0,
-- the new instance will be converted to a non-PIOPS instance, which will
-- take additional time, though your DB instance will be available for
-- connections before the conversion starts. Constraints: Must be an integer
-- greater than 1000. SQL Server Setting the IOPS value for the SQL Server
-- database engine is not supported.
rdbifdbsIops :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdbifdbsIops = lens _rdbifdbsIops (\s a -> s { _rdbifdbsIops = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbifdbsLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsLicenseModel =
    lens _rdbifdbsLicenseModel (\s a -> s { _rdbifdbsLicenseModel = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbifdbsMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsMultiAZ = lens _rdbifdbsMultiAZ (\s a -> s { _rdbifdbsMultiAZ = a })

-- | The name of the option group to be used for the restored DB instance.
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group cannot
-- be removed from a DB instance once it is associated with a DB instance.
rdbifdbsOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsOptionGroupName =
    lens _rdbifdbsOptionGroupName (\s a -> s { _rdbifdbsOptionGroupName = a })

-- | The port number on which the database accepts connections. Default: The
-- same port as the original DB instance Constraints: Value must be
-- 1150-65535.
rdbifdbsPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdbifdbsPort = lens _rdbifdbsPort (\s a -> s { _rdbifdbsPort = a })

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
rdbifdbsPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsPubliclyAccessible =
    lens _rdbifdbsPubliclyAccessible
        (\s a -> s { _rdbifdbsPubliclyAccessible = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
rdbifdbsStorageType :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsStorageType =
    lens _rdbifdbsStorageType (\s a -> s { _rdbifdbsStorageType = a })

rdbifdbsTags :: Lens' RestoreDBInstanceFromDBSnapshot [Tag]
rdbifdbsTags = lens _rdbifdbsTags (\s a -> s { _rdbifdbsTags = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rdbifdbsTdeCredentialArn :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsTdeCredentialArn =
    lens _rdbifdbsTdeCredentialArn
        (\s a -> s { _rdbifdbsTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rdbifdbsTdeCredentialPassword :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsTdeCredentialPassword =
    lens _rdbifdbsTdeCredentialPassword
        (\s a -> s { _rdbifdbsTdeCredentialPassword = a })

newtype RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _rdbifdbsrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceFromDBSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbifdbsrDBInstance' @::@ 'Maybe' 'DBInstance'
--
restoreDBInstanceFromDBSnapshotResponse :: RestoreDBInstanceFromDBSnapshotResponse
restoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _rdbifdbsrDBInstance = Nothing
    }

rdbifdbsrDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
rdbifdbsrDBInstance =
    lens _rdbifdbsrDBInstance (\s a -> s { _rdbifdbsrDBInstance = a })

instance ToPath RestoreDBInstanceFromDBSnapshot where
    toPath = const "/"

instance ToQuery RestoreDBInstanceFromDBSnapshot

instance ToHeaders RestoreDBInstanceFromDBSnapshot

instance AWSRequest RestoreDBInstanceFromDBSnapshot where
    type Sv RestoreDBInstanceFromDBSnapshot = RDS
    type Rs RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshotResponse

    request  = post "RestoreDBInstanceFromDBSnapshot"
    response = xmlResponse

instance FromXML RestoreDBInstanceFromDBSnapshotResponse where
    parseXML c = RestoreDBInstanceFromDBSnapshotResponse
        <$> c .:? "DBInstance"
