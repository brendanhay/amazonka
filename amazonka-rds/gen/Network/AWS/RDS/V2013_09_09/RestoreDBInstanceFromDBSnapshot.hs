{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RestoreDBInstanceFromDBSnapshot' request.
restoreDBInstanceFromDBSnapshot :: Text -- ^ '_rdbifdbsmDBSnapshotIdentifier'
                                -> Text -- ^ '_rdbifdbsmDBInstanceIdentifier'
                                -> RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot p1 p2 = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsmDBSnapshotIdentifier = p1
    , _rdbifdbsmDBInstanceIdentifier = p2
    , _rdbifdbsmPubliclyAccessible = Nothing
    , _rdbifdbsmAutoMinorVersionUpgrade = Nothing
    , _rdbifdbsmMultiAZ = Nothing
    , _rdbifdbsmIops = Nothing
    , _rdbifdbsmPort = Nothing
    , _rdbifdbsmDBSubnetGroupName = Nothing
    , _rdbifdbsmEngine = Nothing
    , _rdbifdbsmDBInstanceClass = Nothing
    , _rdbifdbsmLicenseModel = Nothing
    , _rdbifdbsmAvailabilityZone = Nothing
    , _rdbifdbsmOptionGroupName = Nothing
    , _rdbifdbsmDBName = Nothing
    , _rdbifdbsmTags = mempty
    }

data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsmDBSnapshotIdentifier :: Text
      -- ^ Name of the DB instance to create from the DB snapshot. This
      -- parameter isn't case sensitive. Constraints: Must contain from 1
      -- to 255 alphanumeric characters or hyphens First character must be
      -- a letter Cannot end with a hyphen or contain two consecutive
      -- hyphens Example: my-snapshot-id.
    , _rdbifdbsmDBInstanceIdentifier :: Text
      -- ^ The identifier for the DB snapshot to restore from. Constraints:
      -- Must contain from 1 to 63 alphanumeric characters or hyphens
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens.
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
    , _rdbifdbsmMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true.
    , _rdbifdbsmIops :: Maybe Integer
      -- ^ Specifies the amount of provisioned IOPS for the DB instance,
      -- expressed in I/O operations per second. If this parameter is not
      -- specified, the IOPS value will be taken from the backup. If this
      -- parameter is set to 0, the new instance will be converted to a
      -- non-PIOPS instance, which will take additional time, though your
      -- DB instance will be available for connections before the
      -- conversion starts. Constraints: Must be an integer greater than
      -- 1000.
    , _rdbifdbsmPort :: Maybe Integer
      -- ^ The port number on which the database accepts connections.
      -- Default: The same port as the original DB instance Constraints:
      -- Value must be 1150-65535.
    , _rdbifdbsmDBSubnetGroupName :: Maybe Text
      -- ^ The DB subnet group name to use for the new instance.
    , _rdbifdbsmEngine :: Maybe Text
      -- ^ The database engine to use for the new instance. Default: The
      -- same as source Constraint: Must be compatible with the engine of
      -- the source Example: oracle-ee.
    , _rdbifdbsmDBInstanceClass :: Maybe Text
      -- ^ The compute and memory capacity of the Amazon RDS DB instance.
      -- Valid Values: db.t1.micro | db.m1.small | db.m1.medium |
      -- db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge.
    , _rdbifdbsmLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance. Default:
      -- Same as source. Valid values: license-included |
      -- bring-your-own-license | general-public-license.
    , _rdbifdbsmAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be
      -- created in. Default: A random, system-chosen Availability Zone.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true. Example: us-east-1a.
    , _rdbifdbsmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to be used for the restored DB
      -- instance. cannot be removed from an option group while DB
      -- instances are associated with the option group. --> Permanent
      -- options, such as the TDE option for Oracle Advanced Security TDE,
      -- cannot be removed from an option group, and that option group
      -- cannot be removed from a DB instance once it is associated with a
      -- DB instance.
    , _rdbifdbsmDBName :: Maybe Text
      -- ^ The database name for the restored DB instance. This parameter
      -- doesn't apply to the MySQL engine.
    , _rdbifdbsmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

makeLenses ''RestoreDBInstanceFromDBSnapshot

instance ToQuery RestoreDBInstanceFromDBSnapshot where
    toQuery = genericQuery def

data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _dbiyDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

makeLenses ''RestoreDBInstanceFromDBSnapshotResponse

instance FromXML RestoreDBInstanceFromDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreDBInstanceFromDBSnapshot where
    type Sv RestoreDBInstanceFromDBSnapshot = RDS
    type Rs RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshotResponse

    request = post "RestoreDBInstanceFromDBSnapshot"
    response _ = xmlResponse
