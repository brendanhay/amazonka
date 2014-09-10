{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      RestoreDBInstanceFromDBSnapshot
    -- ** Request constructor
    , mkRestoreDBInstanceFromDBSnapshot
    -- ** Request lenses
    , rdbifdbsDBInstanceIdentifier
    , rdbifdbsDBSnapshotIdentifier
    , rdbifdbsDBInstanceClass
    , rdbifdbsPort
    , rdbifdbsAvailabilityZone
    , rdbifdbsDBSubnetGroupName
    , rdbifdbsMultiAZ
    , rdbifdbsPubliclyAccessible
    , rdbifdbsAutoMinorVersionUpgrade
    , rdbifdbsLicenseModel
    , rdbifdbsDBName
    , rdbifdbsEngine
    , rdbifdbsIops
    , rdbifdbsOptionGroupName
    , rdbifdbsTags

    -- * Response
    , RestoreDBInstanceFromDBSnapshotResponse
    -- ** Response constructor
    , mkRestoreDBInstanceFromDBSnapshotResponse
    -- ** Response lenses
    , rdbifdbsrDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsDBInstanceIdentifier :: !Text
    , _rdbifdbsDBSnapshotIdentifier :: !Text
    , _rdbifdbsDBInstanceClass :: !(Maybe Text)
    , _rdbifdbsPort :: !(Maybe Integer)
    , _rdbifdbsAvailabilityZone :: !(Maybe Text)
    , _rdbifdbsDBSubnetGroupName :: !(Maybe Text)
    , _rdbifdbsMultiAZ :: !(Maybe Bool)
    , _rdbifdbsPubliclyAccessible :: !(Maybe Bool)
    , _rdbifdbsAutoMinorVersionUpgrade :: !(Maybe Bool)
    , _rdbifdbsLicenseModel :: !(Maybe Text)
    , _rdbifdbsDBName :: !(Maybe Text)
    , _rdbifdbsEngine :: !(Maybe Text)
    , _rdbifdbsIops :: !(Maybe Integer)
    , _rdbifdbsOptionGroupName :: !(Maybe Text)
    , _rdbifdbsTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreDBInstanceFromDBSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Text@
--
-- * @DBSnapshotIdentifier ::@ @Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @DBSubnetGroupName ::@ @Maybe Text@
--
-- * @MultiAZ ::@ @Maybe Bool@
--
-- * @PubliclyAccessible ::@ @Maybe Bool@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @LicenseModel ::@ @Maybe Text@
--
-- * @DBName ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @Tags ::@ @[Tag]@
--
mkRestoreDBInstanceFromDBSnapshot :: Text -- ^ 'rdbifdbsDBInstanceIdentifier'
                                  -> Text -- ^ 'rdbifdbsDBSnapshotIdentifier'
                                  -> RestoreDBInstanceFromDBSnapshot
mkRestoreDBInstanceFromDBSnapshot p1 p2 = RestoreDBInstanceFromDBSnapshot
    { _rdbifdbsDBInstanceIdentifier = p1
    , _rdbifdbsDBSnapshotIdentifier = p2
    , _rdbifdbsDBInstanceClass = Nothing
    , _rdbifdbsPort = Nothing
    , _rdbifdbsAvailabilityZone = Nothing
    , _rdbifdbsDBSubnetGroupName = Nothing
    , _rdbifdbsMultiAZ = Nothing
    , _rdbifdbsPubliclyAccessible = Nothing
    , _rdbifdbsAutoMinorVersionUpgrade = Nothing
    , _rdbifdbsLicenseModel = Nothing
    , _rdbifdbsDBName = Nothing
    , _rdbifdbsEngine = Nothing
    , _rdbifdbsIops = Nothing
    , _rdbifdbsOptionGroupName = Nothing
    , _rdbifdbsTags = mempty
    }

-- | The identifier for the DB snapshot to restore from. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbifdbsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdbifdbsDBInstanceIdentifier =
    lens _rdbifdbsDBInstanceIdentifier
         (\s a -> s { _rdbifdbsDBInstanceIdentifier = a })

-- | Name of the DB instance to create from the DB snapshot. This parameter
-- isn't case sensitive. Constraints: Must contain from 1 to 255 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens Example: my-snapshot-id.
rdbifdbsDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdbifdbsDBSnapshotIdentifier =
    lens _rdbifdbsDBSnapshotIdentifier
         (\s a -> s { _rdbifdbsDBSnapshotIdentifier = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge.
rdbifdbsDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBInstanceClass =
    lens _rdbifdbsDBInstanceClass
         (\s a -> s { _rdbifdbsDBInstanceClass = a })

-- | The port number on which the database accepts connections. Default: The
-- same port as the original DB instance Constraints: Value must be
-- 1150-65535.
rdbifdbsPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Integer)
rdbifdbsPort = lens _rdbifdbsPort (\s a -> s { _rdbifdbsPort = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You cannot
-- specify the AvailabilityZone parameter if the MultiAZ parameter is set to
-- true. Example: us-east-1a.
rdbifdbsAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsAvailabilityZone =
    lens _rdbifdbsAvailabilityZone
         (\s a -> s { _rdbifdbsAvailabilityZone = a })

-- | The DB subnet group name to use for the new instance.
rdbifdbsDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBSubnetGroupName =
    lens _rdbifdbsDBSubnetGroupName
         (\s a -> s { _rdbifdbsDBSubnetGroupName = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbifdbsMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsMultiAZ = lens _rdbifdbsMultiAZ (\s a -> s { _rdbifdbsMultiAZ = a })

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
rdbifdbsPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsPubliclyAccessible =
    lens _rdbifdbsPubliclyAccessible
         (\s a -> s { _rdbifdbsPubliclyAccessible = a })

-- | Indicates that minor version upgrades will be applied automatically to the
-- DB instance during the maintenance window.
rdbifdbsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdbifdbsAutoMinorVersionUpgrade =
    lens _rdbifdbsAutoMinorVersionUpgrade
         (\s a -> s { _rdbifdbsAutoMinorVersionUpgrade = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbifdbsLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsLicenseModel =
    lens _rdbifdbsLicenseModel (\s a -> s { _rdbifdbsLicenseModel = a })

-- | The database name for the restored DB instance. This parameter doesn't
-- apply to the MySQL engine.
rdbifdbsDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsDBName = lens _rdbifdbsDBName (\s a -> s { _rdbifdbsDBName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbifdbsEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsEngine = lens _rdbifdbsEngine (\s a -> s { _rdbifdbsEngine = a })

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in
-- I/O operations per second. If this parameter is not specified, the IOPS
-- value will be taken from the backup. If this parameter is set to 0, the new
-- instance will be converted to a non-PIOPS instance, which will take
-- additional time, though your DB instance will be available for connections
-- before the conversion starts. Constraints: Must be an integer greater than
-- 1000.
rdbifdbsIops :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Integer)
rdbifdbsIops = lens _rdbifdbsIops (\s a -> s { _rdbifdbsIops = a })

-- | The name of the option group to be used for the restored DB instance.
-- cannot be removed from an option group while DB instances are associated
-- with the option group. --> Permanent options, such as the TDE option for
-- Oracle Advanced Security TDE, cannot be removed from an option group, and
-- that option group cannot be removed from a DB instance once it is
-- associated with a DB instance.
rdbifdbsOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdbifdbsOptionGroupName =
    lens _rdbifdbsOptionGroupName
         (\s a -> s { _rdbifdbsOptionGroupName = a })

-- | A list of tags.
rdbifdbsTags :: Lens' RestoreDBInstanceFromDBSnapshot [Tag]
rdbifdbsTags = lens _rdbifdbsTags (\s a -> s { _rdbifdbsTags = a })

instance ToQuery RestoreDBInstanceFromDBSnapshot where
    toQuery = genericQuery def

newtype RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _rdbifdbsrDBInstance :: Maybe DBInstance
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreDBInstanceFromDBSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstance ::@ @Maybe DBInstance@
--
mkRestoreDBInstanceFromDBSnapshotResponse :: RestoreDBInstanceFromDBSnapshotResponse
mkRestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse
    { _rdbifdbsrDBInstance = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
rdbifdbsrDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
rdbifdbsrDBInstance =
    lens _rdbifdbsrDBInstance (\s a -> s { _rdbifdbsrDBInstance = a })

instance FromXML RestoreDBInstanceFromDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreDBInstanceFromDBSnapshot where
    type Sv RestoreDBInstanceFromDBSnapshot = RDS
    type Rs RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshotResponse

    request = post "RestoreDBInstanceFromDBSnapshot"
    response _ = xmlResponse
