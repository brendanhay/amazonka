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
    , mkRestoreDBInstanceToPointInTime
    -- ** Request lenses
    , rdbitpitSourceDBInstanceIdentifier
    , rdbitpitTargetDBInstanceIdentifier
    , rdbitpitRestoreTime
    , rdbitpitUseLatestRestorableTime
    , rdbitpitDBInstanceClass
    , rdbitpitPort
    , rdbitpitAvailabilityZone
    , rdbitpitDBSubnetGroupName
    , rdbitpitMultiAZ
    , rdbitpitPubliclyAccessible
    , rdbitpitAutoMinorVersionUpgrade
    , rdbitpitLicenseModel
    , rdbitpitDBName
    , rdbitpitEngine
    , rdbitpitIops
    , rdbitpitOptionGroupName
    , rdbitpitTags

    -- * Response
    , RestoreDBInstanceToPointInTimeResponse
    -- ** Response constructor
    , mkRestoreDBInstanceToPointInTimeResponse
    -- ** Response lenses
    , rdbitpitrDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime
    { _rdbitpitSourceDBInstanceIdentifier :: Text
    , _rdbitpitTargetDBInstanceIdentifier :: Text
    , _rdbitpitRestoreTime :: Maybe ISO8601
    , _rdbitpitUseLatestRestorableTime :: Maybe Bool
    , _rdbitpitDBInstanceClass :: Maybe Text
    , _rdbitpitPort :: Maybe Integer
    , _rdbitpitAvailabilityZone :: Maybe Text
    , _rdbitpitDBSubnetGroupName :: Maybe Text
    , _rdbitpitMultiAZ :: Maybe Bool
    , _rdbitpitPubliclyAccessible :: Maybe Bool
    , _rdbitpitAutoMinorVersionUpgrade :: Maybe Bool
    , _rdbitpitLicenseModel :: Maybe Text
    , _rdbitpitDBName :: Maybe Text
    , _rdbitpitEngine :: Maybe Text
    , _rdbitpitIops :: Maybe Integer
    , _rdbitpitOptionGroupName :: Maybe Text
    , _rdbitpitTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreDBInstanceToPointInTime' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceDBInstanceIdentifier ::@ @Text@
--
-- * @TargetDBInstanceIdentifier ::@ @Text@
--
-- * @RestoreTime ::@ @Maybe ISO8601@
--
-- * @UseLatestRestorableTime ::@ @Maybe Bool@
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
mkRestoreDBInstanceToPointInTime :: Text -- ^ 'rdbitpitSourceDBInstanceIdentifier'
                                 -> Text -- ^ 'rdbitpitTargetDBInstanceIdentifier'
                                 -> RestoreDBInstanceToPointInTime
mkRestoreDBInstanceToPointInTime p1 p2 = RestoreDBInstanceToPointInTime
    { _rdbitpitSourceDBInstanceIdentifier = p1
    , _rdbitpitTargetDBInstanceIdentifier = p2
    , _rdbitpitRestoreTime = Nothing
    , _rdbitpitUseLatestRestorableTime = Nothing
    , _rdbitpitDBInstanceClass = Nothing
    , _rdbitpitPort = Nothing
    , _rdbitpitAvailabilityZone = Nothing
    , _rdbitpitDBSubnetGroupName = Nothing
    , _rdbitpitMultiAZ = Nothing
    , _rdbitpitPubliclyAccessible = Nothing
    , _rdbitpitAutoMinorVersionUpgrade = Nothing
    , _rdbitpitLicenseModel = Nothing
    , _rdbitpitDBName = Nothing
    , _rdbitpitEngine = Nothing
    , _rdbitpitIops = Nothing
    , _rdbitpitOptionGroupName = Nothing
    , _rdbitpitTags = mempty
    }

-- | The identifier of the source DB instance from which to restore.
-- Constraints: Must be the identifier of an existing database instance Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rdbitpitSourceDBInstanceIdentifier =
    lens _rdbitpitSourceDBInstanceIdentifier
         (\s a -> s { _rdbitpitSourceDBInstanceIdentifier = a })

-- | The name of the new database instance to be created. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rdbitpitTargetDBInstanceIdentifier =
    lens _rdbitpitTargetDBInstanceIdentifier
         (\s a -> s { _rdbitpitTargetDBInstanceIdentifier = a })

-- | The date and time to restore from. Valid Values: Value must be a UTC time
-- Constraints: Must be before the latest restorable time for the DB instance
-- Cannot be specified if UseLatestRestorableTime parameter is true Example:
-- 2009-09-07T23:45:00Z.
rdbitpitRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe ISO8601)
rdbitpitRestoreTime =
    lens _rdbitpitRestoreTime (\s a -> s { _rdbitpitRestoreTime = a })

-- | Specifies whether (true) or not (false) the DB instance is restored from
-- the latest backup time. Default: false Constraints: Cannot be specified if
-- RestoreTime parameter is provided.
rdbitpitUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitUseLatestRestorableTime =
    lens _rdbitpitUseLatestRestorableTime
         (\s a -> s { _rdbitpitUseLatestRestorableTime = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge Default: The same
-- DBInstanceClass as the original DB instance.
rdbitpitDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBInstanceClass =
    lens _rdbitpitDBInstanceClass
         (\s a -> s { _rdbitpitDBInstanceClass = a })

-- | The port number on which the database accepts connections. Constraints:
-- Value must be 1150-65535 Default: The same port as the original DB
-- instance.
rdbitpitPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Integer)
rdbitpitPort = lens _rdbitpitPort (\s a -> s { _rdbitpitPort = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You cannot
-- specify the AvailabilityZone parameter if the MultiAZ parameter is set to
-- true. Example: us-east-1a.
rdbitpitAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitAvailabilityZone =
    lens _rdbitpitAvailabilityZone
         (\s a -> s { _rdbitpitAvailabilityZone = a })

-- | The DB subnet group name to use for the new instance.
rdbitpitDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBSubnetGroupName =
    lens _rdbitpitDBSubnetGroupName
         (\s a -> s { _rdbitpitDBSubnetGroupName = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbitpitMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitMultiAZ = lens _rdbitpitMultiAZ (\s a -> s { _rdbitpitMultiAZ = a })

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
rdbitpitPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitPubliclyAccessible =
    lens _rdbitpitPubliclyAccessible
         (\s a -> s { _rdbitpitPubliclyAccessible = a })

-- | Indicates that minor version upgrades will be applied automatically to the
-- DB instance during the maintenance window.
rdbitpitAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rdbitpitAutoMinorVersionUpgrade =
    lens _rdbitpitAutoMinorVersionUpgrade
         (\s a -> s { _rdbitpitAutoMinorVersionUpgrade = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbitpitLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitLicenseModel =
    lens _rdbitpitLicenseModel (\s a -> s { _rdbitpitLicenseModel = a })

-- | The database name for the restored DB instance. This parameter is not used
-- for the MySQL engine.
rdbitpitDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitDBName = lens _rdbitpitDBName (\s a -> s { _rdbitpitDBName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbitpitEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitEngine = lens _rdbitpitEngine (\s a -> s { _rdbitpitEngine = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000.
rdbitpitIops :: Lens' RestoreDBInstanceToPointInTime (Maybe Integer)
rdbitpitIops = lens _rdbitpitIops (\s a -> s { _rdbitpitIops = a })

-- | The name of the option group to be used for the restored DB instance.
-- cannot be removed from an option group while DB instances are associated
-- with the option group. --> Permanent options, such as the TDE option for
-- Oracle Advanced Security TDE, cannot be removed from an option group, and
-- that option group cannot be removed from a DB instance once it is
-- associated with a DB instance.
rdbitpitOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rdbitpitOptionGroupName =
    lens _rdbitpitOptionGroupName
         (\s a -> s { _rdbitpitOptionGroupName = a })

-- | A list of tags.
rdbitpitTags :: Lens' RestoreDBInstanceToPointInTime [Tag]
rdbitpitTags = lens _rdbitpitTags (\s a -> s { _rdbitpitTags = a })

instance ToQuery RestoreDBInstanceToPointInTime where
    toQuery = genericQuery def

newtype RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { _rdbitpitrDBInstance :: Maybe DBInstance
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreDBInstanceToPointInTimeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstance ::@ @Maybe DBInstance@
--
mkRestoreDBInstanceToPointInTimeResponse :: RestoreDBInstanceToPointInTimeResponse
mkRestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { _rdbitpitrDBInstance = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
rdbitpitrDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
rdbitpitrDBInstance =
    lens _rdbitpitrDBInstance (\s a -> s { _rdbitpitrDBInstance = a })

instance FromXML RestoreDBInstanceToPointInTimeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RestoreDBInstanceToPointInTime where
    type Sv RestoreDBInstanceToPointInTime = RDS
    type Rs RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTimeResponse

    request = post "RestoreDBInstanceToPointInTime"
    response _ = xmlResponse
