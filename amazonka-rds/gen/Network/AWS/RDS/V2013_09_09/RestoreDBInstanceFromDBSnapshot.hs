{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot
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
-- created with the default security group. https://rds.amazon.com/
-- ?Action=RestoreDBInstanceFromDBSnapshot &DBSnapshotIdentifier=mydbsnapshot
-- &DBInstanceIdentifier=myrestoreddbinstance &Version=2013-05-15
-- &Timestamp=2011-05-23T06%3A47%3A11.071Z &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId= &Signature= mysql 1 false
-- general-public-license creating 5.1.50 myrestoreddbinstance in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00 10
-- db.m1.large master 7ca622e8-8508-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot
    (
    -- * Request
      RestoreDBInstanceFromDBSnapshot
    -- ** Request constructor
    , restoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdbifdbsmDBInstanceIdentifier
    , rdbifdbsmDBSnapshotIdentifier
    , rdbifdbsmMultiAZ
    , rdbifdbsmPubliclyAccessible
    , rdbifdbsmAutoMinorVersionUpgrade
    , rdbifdbsmPort
    , rdbifdbsmIops
    , rdbifdbsmDBInstanceClass
    , rdbifdbsmAvailabilityZone
    , rdbifdbsmDBSubnetGroupName
    , rdbifdbsmLicenseModel
    , rdbifdbsmDBName
    , rdbifdbsmEngine
    , rdbifdbsmOptionGroupName
    , rdbifdbsmTags

    -- * Response
    , RestoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , dbidtDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RestoreDBInstanceFromDBSnapshot' request.
restoreDBInstanceFromDBSnapshot :: Text -- ^ 'rdbifdbsmDBInstanceIdentifier'
                                -> Text -- ^ 'rdbifdbsmDBSnapshotIdentifier'
                                -> RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot p1 p2 = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsmDBInstanceIdentifier = p1
    , _rdbifdbsmDBSnapshotIdentifier = p2
    , _rdbifdbsmMultiAZ = Nothing
    , _rdbifdbsmPubliclyAccessible = Nothing
    , _rdbifdbsmAutoMinorVersionUpgrade = Nothing
    , _rdbifdbsmPort = Nothing
    , _rdbifdbsmIops = Nothing
    , _rdbifdbsmDBInstanceClass = Nothing
    , _rdbifdbsmAvailabilityZone = Nothing
    , _rdbifdbsmDBSubnetGroupName = Nothing
    , _rdbifdbsmLicenseModel = Nothing
    , _rdbifdbsmDBName = Nothing
    , _rdbifdbsmEngine = Nothing
    , _rdbifdbsmOptionGroupName = Nothing
    , _rdbifdbsmTags = mempty
    }
{-# INLINE restoreDBInstanceFromDBSnapshot #-}

data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsmDBInstanceIdentifier :: Text
      -- ^ The identifier for the DB snapshot to restore from. Constraints:
      -- Must contain from 1 to 63 alphanumeric characters or hyphens
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    , _rdbifdbsmDBSnapshotIdentifier :: Text
      -- ^ Name of the DB instance to create from the DB snapshot. This
      -- parameter isn't case sensitive. Constraints: Must contain from 1
      -- to 255 alphanumeric characters or hyphens First character must be
      -- a letter Cannot end with a hyphen or contain two consecutive
      -- hyphens Example: my-snapshot-id.
    , _rdbifdbsmMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true.
    , _rdbifdbsmPubliclyAccessible :: Maybe Bool
      -- ^ Specifies the accessibility options for the DB instance. A value
      -- of true specifies an Internet-facing instance with a publicly
      -- resolvable DNS name, which resolves to a public IP address. A
      -- value of false specifies an internal instance with a DNS name
      -- that resolves to a private IP address. Default: The default
      -- behavior varies depending on whether a VPC has been requested or
      -- not. The following list shows the default behavior in each case.
      -- Default VPC:true VPC:false If no DB subnet group has been
      -- specified as part of the request and the PubliclyAccessible value
      -- has not been set, the DB instance will be publicly accessible. If
      -- a specific DB subnet group has been specified as part of the
      -- request and the PubliclyAccessible value has not been set, the DB
      -- instance will be private.
    , _rdbifdbsmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version upgrades will be applied
      -- automatically to the DB instance during the maintenance window.
    , _rdbifdbsmPort :: Maybe Integer
      -- ^ The port number on which the database accepts connections.
      -- Default: The same port as the original DB instance Constraints:
      -- Value must be 1150-65535.
    , _rdbifdbsmIops :: Maybe Integer
      -- ^ Specifies the amount of provisioned IOPS for the DB instance,
      -- expressed in I/O operations per second. If this parameter is not
      -- specified, the IOPS value will be taken from the backup. If this
      -- parameter is set to 0, the new instance will be converted to a
      -- non-PIOPS instance, which will take additional time, though your
      -- DB instance will be available for connections before the
      -- conversion starts. Constraints: Must be an integer greater than
      -- 1000.
    , _rdbifdbsmDBInstanceClass :: Maybe Text
      -- ^ The compute and memory capacity of the Amazon RDS DB instance.
      -- Valid Values: db.t1.micro | db.m1.small | db.m1.medium |
      -- db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge.
    , _rdbifdbsmAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be
      -- created in. Default: A random, system-chosen Availability Zone.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true. Example: us-east-1a.
    , _rdbifdbsmDBSubnetGroupName :: Maybe Text
      -- ^ The DB subnet group name to use for the new instance.
    , _rdbifdbsmLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance. Default:
      -- Same as source. Valid values: license-included |
      -- bring-your-own-license | general-public-license.
    , _rdbifdbsmDBName :: Maybe Text
      -- ^ The database name for the restored DB instance. This parameter
      -- doesn't apply to the MySQL engine.
    , _rdbifdbsmEngine :: Maybe Text
      -- ^ The database engine to use for the new instance. Default: The
      -- same as source Constraint: Must be compatible with the engine of
      -- the source Example: oracle-ee.
    , _rdbifdbsmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to be used for the restored DB
      -- instance. cannot be removed from an option group while DB
      -- instances are associated with the option group. --> Permanent
      -- options, such as the TDE option for Oracle Advanced Security TDE,
      -- cannot be removed from an option group, and that option group
      -- cannot be removed from a DB instance once it is associated with a
      -- DB instance.
    , _rdbifdbsmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

-- | The identifier for the DB snapshot to restore from. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbifdbsmDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot (Text)
rdbifdbsmDBInstanceIdentifier f x =
    f (_rdbifdbsmDBInstanceIdentifier x)
        <&> \y -> x { _rdbifdbsmDBInstanceIdentifier = y }
{-# INLINE rdbifdbsmDBInstanceIdentifier #-}

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn't case sensitive. Constraints: Must contain from 1 to 255 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens Example: my-snapshot-id.
rdbifdbsmDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot (Text)
rdbifdbsmDBSnapshotIdentifier f x =
    f (_rdbifdbsmDBSnapshotIdentifier x)
        <&> \y -> x { _rdbifdbsmDBSnapshotIdentifier = y }
{-# INLINE rdbifdbsmDBSnapshotIdentifier #-}

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbifdbsmMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsmMultiAZ f x =
    f (_rdbifdbsmMultiAZ x)
        <&> \y -> x { _rdbifdbsmMultiAZ = y }
{-# INLINE rdbifdbsmMultiAZ #-}

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS name,
-- which resolves to a public IP address. A value of false specifies an
-- internal instance with a DNS name that resolves to a private IP address.
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case. Default VPC:true VPC:false If no DB subnet group has been specified
-- as part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be publicly accessible. If a specific DB subnet group
-- has been specified as part of the request and the PubliclyAccessible value
-- has not been set, the DB instance will be private.
rdbifdbsmPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsmPubliclyAccessible f x =
    f (_rdbifdbsmPubliclyAccessible x)
        <&> \y -> x { _rdbifdbsmPubliclyAccessible = y }
{-# INLINE rdbifdbsmPubliclyAccessible #-}

-- | Indicates that minor version upgrades will be applied automatically to the
-- DB instance during the maintenance window.
rdbifdbsmAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsmAutoMinorVersionUpgrade f x =
    f (_rdbifdbsmAutoMinorVersionUpgrade x)
        <&> \y -> x { _rdbifdbsmAutoMinorVersionUpgrade = y }
{-# INLINE rdbifdbsmAutoMinorVersionUpgrade #-}

-- | The port number on which the database accepts connections. Default: The
-- same port as the original DB instance Constraints: Value must be
-- 1150-65535.
rdbifdbsmPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Integer)
rdbifdbsmPort f x =
    f (_rdbifdbsmPort x)
        <&> \y -> x { _rdbifdbsmPort = y }
{-# INLINE rdbifdbsmPort #-}

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in
-- I/O operations per second. If this parameter is not specified, the IOPS
-- value will be taken from the backup. If this parameter is set to 0, the new
-- instance will be converted to a non-PIOPS instance, which will take
-- additional time, though your DB instance will be available for connections
-- before the conversion starts. Constraints: Must be an integer greater than
-- 1000.
rdbifdbsmIops :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Integer)
rdbifdbsmIops f x =
    f (_rdbifdbsmIops x)
        <&> \y -> x { _rdbifdbsmIops = y }
{-# INLINE rdbifdbsmIops #-}

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge.
rdbifdbsmDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmDBInstanceClass f x =
    f (_rdbifdbsmDBInstanceClass x)
        <&> \y -> x { _rdbifdbsmDBInstanceClass = y }
{-# INLINE rdbifdbsmDBInstanceClass #-}

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You cannot
-- specify the AvailabilityZone parameter if the MultiAZ parameter is set to
-- true. Example: us-east-1a.
rdbifdbsmAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmAvailabilityZone f x =
    f (_rdbifdbsmAvailabilityZone x)
        <&> \y -> x { _rdbifdbsmAvailabilityZone = y }
{-# INLINE rdbifdbsmAvailabilityZone #-}

-- | The DB subnet group name to use for the new instance.
rdbifdbsmDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmDBSubnetGroupName f x =
    f (_rdbifdbsmDBSubnetGroupName x)
        <&> \y -> x { _rdbifdbsmDBSubnetGroupName = y }
{-# INLINE rdbifdbsmDBSubnetGroupName #-}

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbifdbsmLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmLicenseModel f x =
    f (_rdbifdbsmLicenseModel x)
        <&> \y -> x { _rdbifdbsmLicenseModel = y }
{-# INLINE rdbifdbsmLicenseModel #-}

-- | The database name for the restored DB instance. This parameter doesn't
-- apply to the MySQL engine.
rdbifdbsmDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmDBName f x =
    f (_rdbifdbsmDBName x)
        <&> \y -> x { _rdbifdbsmDBName = y }
{-# INLINE rdbifdbsmDBName #-}

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbifdbsmEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmEngine f x =
    f (_rdbifdbsmEngine x)
        <&> \y -> x { _rdbifdbsmEngine = y }
{-# INLINE rdbifdbsmEngine #-}

-- | The name of the option group to be used for the restored DB instance.
-- cannot be removed from an option group while DB instances are associated
-- with the option group. --> Permanent options, such as the TDE option for
-- Oracle Advanced Security TDE, cannot be removed from an option group, and
-- that option group cannot be removed from a DB instance once it is
-- associated with a DB instance.
rdbifdbsmOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsmOptionGroupName f x =
    f (_rdbifdbsmOptionGroupName x)
        <&> \y -> x { _rdbifdbsmOptionGroupName = y }
{-# INLINE rdbifdbsmOptionGroupName #-}

-- | A list of tags.
rdbifdbsmTags :: Lens' RestoreDBInstanceFromDBSnapshot ([Tag])
rdbifdbsmTags f x =
    f (_rdbifdbsmTags x)
        <&> \y -> x { _rdbifdbsmTags = y }
{-# INLINE rdbifdbsmTags #-}

instance ToQuery RestoreDBInstanceFromDBSnapshot where
    toQuery = genericQuery def

data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _dbidtDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
dbidtDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
dbidtDBInstance f x =
    f (_dbidtDBInstance x)
        <&> \y -> x { _dbidtDBInstance = y }
{-# INLINE dbidtDBInstance #-}

instance FromXML RestoreDBInstanceFromDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreDBInstanceFromDBSnapshot where
    type Sv RestoreDBInstanceFromDBSnapshot = RDS
    type Rs RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshotResponse

    request = post "RestoreDBInstanceFromDBSnapshot"
    response _ = xmlResponse
