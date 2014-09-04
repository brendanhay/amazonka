{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RestoreDBInstanceToPointInTime
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
-- https://rds.amazon.com/ ?Action=RestoreDBInstanceToPointInTime
-- &TargetDBInstanceIdentifier=restored-db
-- &SourceDBInstanceIdentifier=simcoprod01 &UseLatestRestorableTime=true
-- &Version=2013-05-15 &Timestamp=2011-05-23T07%3A06%3A02.313Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= mysql 1 false general-public-license creating 5.1.50
-- restored-db in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 10 db.m1.large master
-- 1ef546bc-850b-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.RestoreDBInstanceToPointInTime
    (
    -- * Request
      RestoreDBInstanceToPointInTime
    -- ** Request constructor
    , mkRestoreDBInstanceToPointInTimeMessage
    -- ** Request lenses
    , rdbitpitmSourceDBInstanceIdentifier
    , rdbitpitmTargetDBInstanceIdentifier
    , rdbitpitmRestoreTime
    , rdbitpitmUseLatestRestorableTime
    , rdbitpitmDBInstanceClass
    , rdbitpitmPort
    , rdbitpitmAvailabilityZone
    , rdbitpitmDBSubnetGroupName
    , rdbitpitmMultiAZ
    , rdbitpitmPubliclyAccessible
    , rdbitpitmAutoMinorVersionUpgrade
    , rdbitpitmLicenseModel
    , rdbitpitmDBName
    , rdbitpitmEngine
    , rdbitpitmIops
    , rdbitpitmOptionGroupName
    , rdbitpitmTags

    -- * Response
    , RestoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , dbiduDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreDBInstanceToPointInTime' request.
mkRestoreDBInstanceToPointInTimeMessage :: Text -- ^ 'rdbitpitmSourceDBInstanceIdentifier'
                                        -> Text -- ^ 'rdbitpitmTargetDBInstanceIdentifier'
                                        -> RestoreDBInstanceToPointInTime
mkRestoreDBInstanceToPointInTimeMessage p1 p2 = RestoreDBInstanceToPointInTime
    { _rdbitpitmSourceDBInstanceIdentifier = p1
    , _rdbitpitmTargetDBInstanceIdentifier = p2
    , _rdbitpitmRestoreTime = Nothing
    , _rdbitpitmUseLatestRestorableTime = Nothing
    , _rdbitpitmDBInstanceClass = Nothing
    , _rdbitpitmPort = Nothing
    , _rdbitpitmAvailabilityZone = Nothing
    , _rdbitpitmDBSubnetGroupName = Nothing
    , _rdbitpitmMultiAZ = Nothing
    , _rdbitpitmPubliclyAccessible = Nothing
    , _rdbitpitmAutoMinorVersionUpgrade = Nothing
    , _rdbitpitmLicenseModel = Nothing
    , _rdbitpitmDBName = Nothing
    , _rdbitpitmEngine = Nothing
    , _rdbitpitmIops = Nothing
    , _rdbitpitmOptionGroupName = Nothing
    , _rdbitpitmTags = mempty
    }
{-# INLINE mkRestoreDBInstanceToPointInTimeMessage #-}

data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime
    { _rdbitpitmSourceDBInstanceIdentifier :: Text
      -- ^ The identifier of the source DB instance from which to restore.
      -- Constraints: Must be the identifier of an existing database
      -- instance Must contain from 1 to 63 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _rdbitpitmTargetDBInstanceIdentifier :: Text
      -- ^ The name of the new database instance to be created. Constraints:
      -- Must contain from 1 to 63 alphanumeric characters or hyphens
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    , _rdbitpitmRestoreTime :: Maybe ISO8601
      -- ^ The date and time to restore from. Valid Values: Value must be a
      -- UTC time Constraints: Must be before the latest restorable time
      -- for the DB instance Cannot be specified if
      -- UseLatestRestorableTime parameter is true Example:
      -- 2009-09-07T23:45:00Z.
    , _rdbitpitmUseLatestRestorableTime :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) the DB instance is
      -- restored from the latest backup time. Default: false Constraints:
      -- Cannot be specified if RestoreTime parameter is provided.
    , _rdbitpitmDBInstanceClass :: Maybe Text
      -- ^ The compute and memory capacity of the Amazon RDS DB instance.
      -- Valid Values: db.t1.micro | db.m1.small | db.m1.medium |
      -- db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge
      -- Default: The same DBInstanceClass as the original DB instance.
    , _rdbitpitmPort :: Maybe Integer
      -- ^ The port number on which the database accepts connections.
      -- Constraints: Value must be 1150-65535 Default: The same port as
      -- the original DB instance.
    , _rdbitpitmAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be
      -- created in. Default: A random, system-chosen Availability Zone.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true. Example: us-east-1a.
    , _rdbitpitmDBSubnetGroupName :: Maybe Text
      -- ^ The DB subnet group name to use for the new instance.
    , _rdbitpitmMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
      -- Constraint: You cannot specify the AvailabilityZone parameter if
      -- the MultiAZ parameter is set to true.
    , _rdbitpitmPubliclyAccessible :: Maybe Bool
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
    , _rdbitpitmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version upgrades will be applied
      -- automatically to the DB instance during the maintenance window.
    , _rdbitpitmLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance. Default:
      -- Same as source. Valid values: license-included |
      -- bring-your-own-license | general-public-license.
    , _rdbitpitmDBName :: Maybe Text
      -- ^ The database name for the restored DB instance. This parameter is
      -- not used for the MySQL engine.
    , _rdbitpitmEngine :: Maybe Text
      -- ^ The database engine to use for the new instance. Default: The
      -- same as source Constraint: Must be compatible with the engine of
      -- the source Example: oracle-ee.
    , _rdbitpitmIops :: Maybe Integer
      -- ^ The amount of Provisioned IOPS (input/output operations per
      -- second) to be initially allocated for the DB instance.
      -- Constraints: Must be an integer greater than 1000.
    , _rdbitpitmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to be used for the restored DB
      -- instance. cannot be removed from an option group while DB
      -- instances are associated with the option group. --> Permanent
      -- options, such as the TDE option for Oracle Advanced Security TDE,
      -- cannot be removed from an option group, and that option group
      -- cannot be removed from a DB instance once it is associated with a
      -- DB instance.
    , _rdbitpitmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

-- | The identifier of the source DB instance from which to restore.
-- Constraints: Must be the identifier of an existing database instance Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitmSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime (Text)
rdbitpitmSourceDBInstanceIdentifier = lens _rdbitpitmSourceDBInstanceIdentifier (\s a -> s { _rdbitpitmSourceDBInstanceIdentifier = a })
{-# INLINE rdbitpitmSourceDBInstanceIdentifier #-}

-- | The name of the new database instance to be created. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitmTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime (Text)
rdbitpitmTargetDBInstanceIdentifier = lens _rdbitpitmTargetDBInstanceIdentifier (\s a -> s { _rdbitpitmTargetDBInstanceIdentifier = a })
{-# INLINE rdbitpitmTargetDBInstanceIdentifier #-}

-- | The date and time to restore from. Valid Values: Value must be a UTC time
-- Constraints: Must be before the latest restorable time for the DB instance
-- Cannot be specified if UseLatestRestorableTime parameter is true Example:
-- 2009-09-07T23:45:00Z.
rdbitpitmRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe ISO8601)
rdbitpitmRestoreTime = lens _rdbitpitmRestoreTime (\s a -> s { _rdbitpitmRestoreTime = a })
{-# INLINE rdbitpitmRestoreTime #-}

-- | Specifies whether (true) or not (false) the DB instance is restored from
-- the latest backup time. Default: false Constraints: Cannot be specified if
-- RestoreTime parameter is provided.
rdbitpitmUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitmUseLatestRestorableTime = lens _rdbitpitmUseLatestRestorableTime (\s a -> s { _rdbitpitmUseLatestRestorableTime = a })
{-# INLINE rdbitpitmUseLatestRestorableTime #-}

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge Default: The same
-- DBInstanceClass as the original DB instance.
rdbitpitmDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmDBInstanceClass = lens _rdbitpitmDBInstanceClass (\s a -> s { _rdbitpitmDBInstanceClass = a })
{-# INLINE rdbitpitmDBInstanceClass #-}

-- | The port number on which the database accepts connections. Constraints:
-- Value must be 1150-65535 Default: The same port as the original DB
-- instance.
rdbitpitmPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Integer)
rdbitpitmPort = lens _rdbitpitmPort (\s a -> s { _rdbitpitmPort = a })
{-# INLINE rdbitpitmPort #-}

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You cannot
-- specify the AvailabilityZone parameter if the MultiAZ parameter is set to
-- true. Example: us-east-1a.
rdbitpitmAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmAvailabilityZone = lens _rdbitpitmAvailabilityZone (\s a -> s { _rdbitpitmAvailabilityZone = a })
{-# INLINE rdbitpitmAvailabilityZone #-}

-- | The DB subnet group name to use for the new instance.
rdbitpitmDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmDBSubnetGroupName = lens _rdbitpitmDBSubnetGroupName (\s a -> s { _rdbitpitmDBSubnetGroupName = a })
{-# INLINE rdbitpitmDBSubnetGroupName #-}

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbitpitmMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitmMultiAZ = lens _rdbitpitmMultiAZ (\s a -> s { _rdbitpitmMultiAZ = a })
{-# INLINE rdbitpitmMultiAZ #-}

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
rdbitpitmPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitmPubliclyAccessible = lens _rdbitpitmPubliclyAccessible (\s a -> s { _rdbitpitmPubliclyAccessible = a })
{-# INLINE rdbitpitmPubliclyAccessible #-}

-- | Indicates that minor version upgrades will be applied automatically to the
-- DB instance during the maintenance window.
rdbitpitmAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitmAutoMinorVersionUpgrade = lens _rdbitpitmAutoMinorVersionUpgrade (\s a -> s { _rdbitpitmAutoMinorVersionUpgrade = a })
{-# INLINE rdbitpitmAutoMinorVersionUpgrade #-}

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbitpitmLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmLicenseModel = lens _rdbitpitmLicenseModel (\s a -> s { _rdbitpitmLicenseModel = a })
{-# INLINE rdbitpitmLicenseModel #-}

-- | The database name for the restored DB instance. This parameter is not used
-- for the MySQL engine.
rdbitpitmDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmDBName = lens _rdbitpitmDBName (\s a -> s { _rdbitpitmDBName = a })
{-# INLINE rdbitpitmDBName #-}

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbitpitmEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmEngine = lens _rdbitpitmEngine (\s a -> s { _rdbitpitmEngine = a })
{-# INLINE rdbitpitmEngine #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000.
rdbitpitmIops :: Lens' RestoreDBInstanceToPointInTime (Maybe Integer)
rdbitpitmIops = lens _rdbitpitmIops (\s a -> s { _rdbitpitmIops = a })
{-# INLINE rdbitpitmIops #-}

-- | The name of the option group to be used for the restored DB instance.
-- cannot be removed from an option group while DB instances are associated
-- with the option group. --> Permanent options, such as the TDE option for
-- Oracle Advanced Security TDE, cannot be removed from an option group, and
-- that option group cannot be removed from a DB instance once it is
-- associated with a DB instance.
rdbitpitmOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitmOptionGroupName = lens _rdbitpitmOptionGroupName (\s a -> s { _rdbitpitmOptionGroupName = a })
{-# INLINE rdbitpitmOptionGroupName #-}

-- | A list of tags.
rdbitpitmTags :: Lens' RestoreDBInstanceToPointInTime ([Tag])
rdbitpitmTags = lens _rdbitpitmTags (\s a -> s { _rdbitpitmTags = a })
{-# INLINE rdbitpitmTags #-}

instance ToQuery RestoreDBInstanceToPointInTime where
    toQuery = genericQuery def

newtype RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { _dbiduDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
dbiduDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
dbiduDBInstance = lens _dbiduDBInstance (\s a -> s { _dbiduDBInstance = a })
{-# INLINE dbiduDBInstance #-}

instance FromXML RestoreDBInstanceToPointInTimeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreDBInstanceToPointInTime where
    type Sv RestoreDBInstanceToPointInTime = RDS
    type Rs RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTimeResponse

    request = post "RestoreDBInstanceToPointInTime"
    response _ = xmlResponse
