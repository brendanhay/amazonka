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
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
    (
    -- * Request
      RestoreDBInstanceFromDBSnapshotMessage
    -- ** Request constructor
    , restoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdbifdbsmAutoMinorVersionUpgrade
    , rdbifdbsmAvailabilityZone
    , rdbifdbsmDBInstanceClass
    , rdbifdbsmDBInstanceIdentifier
    , rdbifdbsmDBName
    , rdbifdbsmDBSnapshotIdentifier
    , rdbifdbsmDBSubnetGroupName
    , rdbifdbsmEngine
    , rdbifdbsmIops
    , rdbifdbsmLicenseModel
    , rdbifdbsmMultiAZ
    , rdbifdbsmOptionGroupName
    , rdbifdbsmPort
    , rdbifdbsmPubliclyAccessible
    , rdbifdbsmStorageType
    , rdbifdbsmTags
    , rdbifdbsmTdeCredentialArn
    , rdbifdbsmTdeCredentialPassword

    -- * Response
    , RestoreDBInstanceFromDBSnapshotResult
    -- ** Response constructor
    , restoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , rdbifdbsrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RestoreDBInstanceFromDBSnapshotMessage = RestoreDBInstanceFromDBSnapshotMessage
    { _rdbifdbsmAutoMinorVersionUpgrade :: Maybe Bool
    , _rdbifdbsmAvailabilityZone        :: Maybe Text
    , _rdbifdbsmDBInstanceClass         :: Maybe Text
    , _rdbifdbsmDBInstanceIdentifier    :: Text
    , _rdbifdbsmDBName                  :: Maybe Text
    , _rdbifdbsmDBSnapshotIdentifier    :: Text
    , _rdbifdbsmDBSubnetGroupName       :: Maybe Text
    , _rdbifdbsmEngine                  :: Maybe Text
    , _rdbifdbsmIops                    :: Maybe Int
    , _rdbifdbsmLicenseModel            :: Maybe Text
    , _rdbifdbsmMultiAZ                 :: Maybe Bool
    , _rdbifdbsmOptionGroupName         :: Maybe Text
    , _rdbifdbsmPort                    :: Maybe Int
    , _rdbifdbsmPubliclyAccessible      :: Maybe Bool
    , _rdbifdbsmStorageType             :: Maybe Text
    , _rdbifdbsmTags                    :: [Tag]
    , _rdbifdbsmTdeCredentialArn        :: Maybe Text
    , _rdbifdbsmTdeCredentialPassword   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceFromDBSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbifdbsmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsmAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbifdbsmDBName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmDBSnapshotIdentifier' @::@ 'Text'
--
-- * 'rdbifdbsmDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmEngine' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmIops' @::@ 'Maybe' 'Int'
--
-- * 'rdbifdbsmLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsmOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmPort' @::@ 'Maybe' 'Int'
--
-- * 'rdbifdbsmPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rdbifdbsmStorageType' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmTags' @::@ ['Tag']
--
-- * 'rdbifdbsmTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'rdbifdbsmTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
restoreDBInstanceFromDBSnapshot :: Text -- ^ 'rdbifdbsmDBInstanceIdentifier'
                                -> Text -- ^ 'rdbifdbsmDBSnapshotIdentifier'
                                -> RestoreDBInstanceFromDBSnapshotMessage
restoreDBInstanceFromDBSnapshot p1 p2 = RestoreDBInstanceFromDBSnapshotMessage
    { _rdbifdbsmDBInstanceIdentifier    = p1
    , _rdbifdbsmDBSnapshotIdentifier    = p2
    , _rdbifdbsmDBInstanceClass         = Nothing
    , _rdbifdbsmPort                    = Nothing
    , _rdbifdbsmAvailabilityZone        = Nothing
    , _rdbifdbsmDBSubnetGroupName       = Nothing
    , _rdbifdbsmMultiAZ                 = Nothing
    , _rdbifdbsmPubliclyAccessible      = Nothing
    , _rdbifdbsmAutoMinorVersionUpgrade = Nothing
    , _rdbifdbsmLicenseModel            = Nothing
    , _rdbifdbsmDBName                  = Nothing
    , _rdbifdbsmEngine                  = Nothing
    , _rdbifdbsmIops                    = Nothing
    , _rdbifdbsmOptionGroupName         = Nothing
    , _rdbifdbsmTags                    = mempty
    , _rdbifdbsmStorageType             = Nothing
    , _rdbifdbsmTdeCredentialArn        = Nothing
    , _rdbifdbsmTdeCredentialPassword   = Nothing
    }

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rdbifdbsmAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Bool)
rdbifdbsmAutoMinorVersionUpgrade =
    lens _rdbifdbsmAutoMinorVersionUpgrade
        (\s a -> s { _rdbifdbsmAutoMinorVersionUpgrade = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true. Example: us-east-1a.
rdbifdbsmAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmAvailabilityZone =
    lens _rdbifdbsmAvailabilityZone
        (\s a -> s { _rdbifdbsmAvailabilityZone = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large
-- | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |
-- db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small
-- | db.t2.medium.
rdbifdbsmDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmDBInstanceClass =
    lens _rdbifdbsmDBInstanceClass
        (\s a -> s { _rdbifdbsmDBInstanceClass = a })

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn't case sensitive. Constraints: Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
rdbifdbsmDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshotMessage Text
rdbifdbsmDBInstanceIdentifier =
    lens _rdbifdbsmDBInstanceIdentifier
        (\s a -> s { _rdbifdbsmDBInstanceIdentifier = a })

-- | The database name for the restored DB instance.
rdbifdbsmDBName :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmDBName = lens _rdbifdbsmDBName (\s a -> s { _rdbifdbsmDBName = a })

-- | The identifier for the DB snapshot to restore from. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbifdbsmDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshotMessage Text
rdbifdbsmDBSnapshotIdentifier =
    lens _rdbifdbsmDBSnapshotIdentifier
        (\s a -> s { _rdbifdbsmDBSnapshotIdentifier = a })

-- | The DB subnet group name to use for the new instance.
rdbifdbsmDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmDBSubnetGroupName =
    lens _rdbifdbsmDBSubnetGroupName
        (\s a -> s { _rdbifdbsmDBSubnetGroupName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbifdbsmEngine :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmEngine = lens _rdbifdbsmEngine (\s a -> s { _rdbifdbsmEngine = a })

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed
-- in I/O operations per second. If this parameter is not specified, the
-- IOPS value will be taken from the backup. If this parameter is set to 0,
-- the new instance will be converted to a non-PIOPS instance, which will
-- take additional time, though your DB instance will be available for
-- connections before the conversion starts. Constraints: Must be an integer
-- greater than 1000. SQL Server Setting the IOPS value for the SQL Server
-- database engine is not supported.
rdbifdbsmIops :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Int)
rdbifdbsmIops = lens _rdbifdbsmIops (\s a -> s { _rdbifdbsmIops = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbifdbsmLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmLicenseModel =
    lens _rdbifdbsmLicenseModel (\s a -> s { _rdbifdbsmLicenseModel = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbifdbsmMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Bool)
rdbifdbsmMultiAZ = lens _rdbifdbsmMultiAZ (\s a -> s { _rdbifdbsmMultiAZ = a })

-- | The name of the option group to be used for the restored DB instance.
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group cannot
-- be removed from a DB instance once it is associated with a DB instance.
rdbifdbsmOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmOptionGroupName =
    lens _rdbifdbsmOptionGroupName
        (\s a -> s { _rdbifdbsmOptionGroupName = a })

-- | The port number on which the database accepts connections. Default: The
-- same port as the original DB instance Constraints: Value must be
-- 1150-65535.
rdbifdbsmPort :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Int)
rdbifdbsmPort = lens _rdbifdbsmPort (\s a -> s { _rdbifdbsmPort = a })

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
rdbifdbsmPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Bool)
rdbifdbsmPubliclyAccessible =
    lens _rdbifdbsmPubliclyAccessible
        (\s a -> s { _rdbifdbsmPubliclyAccessible = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
rdbifdbsmStorageType :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmStorageType =
    lens _rdbifdbsmStorageType (\s a -> s { _rdbifdbsmStorageType = a })

rdbifdbsmTags :: Lens' RestoreDBInstanceFromDBSnapshotMessage [Tag]
rdbifdbsmTags = lens _rdbifdbsmTags (\s a -> s { _rdbifdbsmTags = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rdbifdbsmTdeCredentialArn :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmTdeCredentialArn =
    lens _rdbifdbsmTdeCredentialArn
        (\s a -> s { _rdbifdbsmTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rdbifdbsmTdeCredentialPassword :: Lens' RestoreDBInstanceFromDBSnapshotMessage (Maybe Text)
rdbifdbsmTdeCredentialPassword =
    lens _rdbifdbsmTdeCredentialPassword
        (\s a -> s { _rdbifdbsmTdeCredentialPassword = a })

instance ToQuery RestoreDBInstanceFromDBSnapshotMessage

instance ToPath RestoreDBInstanceFromDBSnapshotMessage where
    toPath = const "/"

newtype RestoreDBInstanceFromDBSnapshotResult = RestoreDBInstanceFromDBSnapshotResult
    { _rdbifdbsrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceFromDBSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbifdbsrDBInstance' @::@ 'Maybe' 'DBInstance'
--
restoreDBInstanceFromDBSnapshotResponse :: RestoreDBInstanceFromDBSnapshotResult
restoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResult
    { _rdbifdbsrDBInstance = Nothing
    }

rdbifdbsrDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResult (Maybe DBInstance)
rdbifdbsrDBInstance =
    lens _rdbifdbsrDBInstance (\s a -> s { _rdbifdbsrDBInstance = a })

instance FromXML RestoreDBInstanceFromDBSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreDBInstanceFromDBSnapshotResult"

instance AWSRequest RestoreDBInstanceFromDBSnapshotMessage where
    type Sv RestoreDBInstanceFromDBSnapshotMessage = RDS
    type Rs RestoreDBInstanceFromDBSnapshotMessage = RestoreDBInstanceFromDBSnapshotResult

    request  = post "RestoreDBInstanceFromDBSnapshot"
    response = xmlResponse $ \h x -> RestoreDBInstanceFromDBSnapshotResult
        <$> x %| "DBInstance"
