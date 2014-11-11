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
      RestoreDBInstanceToPointInTimeMessage
    -- ** Request constructor
    , restoreDBInstanceToPointInTimeMessage
    -- ** Request lenses
    , rdbitpitmAutoMinorVersionUpgrade
    , rdbitpitmAvailabilityZone
    , rdbitpitmDBInstanceClass
    , rdbitpitmDBName
    , rdbitpitmDBSubnetGroupName
    , rdbitpitmEngine
    , rdbitpitmIops
    , rdbitpitmLicenseModel
    , rdbitpitmMultiAZ
    , rdbitpitmOptionGroupName
    , rdbitpitmPort
    , rdbitpitmPubliclyAccessible
    , rdbitpitmRestoreTime
    , rdbitpitmSourceDBInstanceIdentifier
    , rdbitpitmStorageType
    , rdbitpitmTags
    , rdbitpitmTargetDBInstanceIdentifier
    , rdbitpitmTdeCredentialArn
    , rdbitpitmTdeCredentialPassword
    , rdbitpitmUseLatestRestorableTime

    -- * Response
    , RestoreDBInstanceToPointInTimeResult
    -- ** Response constructor
    , restoreDBInstanceToPointInTimeResult
    -- ** Response lenses
    , rdbitpitrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RestoreDBInstanceToPointInTimeMessage = RestoreDBInstanceToPointInTimeMessage
    { _rdbitpitmAutoMinorVersionUpgrade    :: Maybe Bool
    , _rdbitpitmAvailabilityZone           :: Maybe Text
    , _rdbitpitmDBInstanceClass            :: Maybe Text
    , _rdbitpitmDBName                     :: Maybe Text
    , _rdbitpitmDBSubnetGroupName          :: Maybe Text
    , _rdbitpitmEngine                     :: Maybe Text
    , _rdbitpitmIops                       :: Maybe Int
    , _rdbitpitmLicenseModel               :: Maybe Text
    , _rdbitpitmMultiAZ                    :: Maybe Bool
    , _rdbitpitmOptionGroupName            :: Maybe Text
    , _rdbitpitmPort                       :: Maybe Int
    , _rdbitpitmPubliclyAccessible         :: Maybe Bool
    , _rdbitpitmRestoreTime                :: Maybe RFC822
    , _rdbitpitmSourceDBInstanceIdentifier :: Text
    , _rdbitpitmStorageType                :: Maybe Text
    , _rdbitpitmTags                       :: [Tag]
    , _rdbitpitmTargetDBInstanceIdentifier :: Text
    , _rdbitpitmTdeCredentialArn           :: Maybe Text
    , _rdbitpitmTdeCredentialPassword      :: Maybe Text
    , _rdbitpitmUseLatestRestorableTime    :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceToPointInTimeMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbitpitmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitmAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmDBName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmEngine' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmIops' @::@ 'Maybe' 'Int'
--
-- * 'rdbitpitmLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmMultiAZ' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitmOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmPort' @::@ 'Maybe' 'Int'
--
-- * 'rdbitpitmPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'rdbitpitmRestoreTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'rdbitpitmSourceDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbitpitmStorageType' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmTags' @::@ ['Tag']
--
-- * 'rdbitpitmTargetDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbitpitmTdeCredentialArn' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmTdeCredentialPassword' @::@ 'Maybe' 'Text'
--
-- * 'rdbitpitmUseLatestRestorableTime' @::@ 'Maybe' 'Bool'
--
restoreDBInstanceToPointInTimeMessage :: Text -- ^ 'rdbitpitmSourceDBInstanceIdentifier'
                                      -> Text -- ^ 'rdbitpitmTargetDBInstanceIdentifier'
                                      -> RestoreDBInstanceToPointInTimeMessage
restoreDBInstanceToPointInTimeMessage p1 p2 = RestoreDBInstanceToPointInTimeMessage
    { _rdbitpitmSourceDBInstanceIdentifier = p1
    , _rdbitpitmTargetDBInstanceIdentifier = p2
    , _rdbitpitmRestoreTime                = Nothing
    , _rdbitpitmUseLatestRestorableTime    = Nothing
    , _rdbitpitmDBInstanceClass            = Nothing
    , _rdbitpitmPort                       = Nothing
    , _rdbitpitmAvailabilityZone           = Nothing
    , _rdbitpitmDBSubnetGroupName          = Nothing
    , _rdbitpitmMultiAZ                    = Nothing
    , _rdbitpitmPubliclyAccessible         = Nothing
    , _rdbitpitmAutoMinorVersionUpgrade    = Nothing
    , _rdbitpitmLicenseModel               = Nothing
    , _rdbitpitmDBName                     = Nothing
    , _rdbitpitmEngine                     = Nothing
    , _rdbitpitmIops                       = Nothing
    , _rdbitpitmOptionGroupName            = Nothing
    , _rdbitpitmTags                       = mempty
    , _rdbitpitmStorageType                = Nothing
    , _rdbitpitmTdeCredentialArn           = Nothing
    , _rdbitpitmTdeCredentialPassword      = Nothing
    }

-- | Indicates that minor version upgrades will be applied automatically to
-- the DB instance during the maintenance window.
rdbitpitmAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Bool)
rdbitpitmAutoMinorVersionUpgrade =
    lens _rdbitpitmAutoMinorVersionUpgrade
        (\s a -> s { _rdbitpitmAutoMinorVersionUpgrade = a })

-- | The EC2 Availability Zone that the database instance will be created in.
-- Default: A random, system-chosen Availability Zone. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true. Example: us-east-1a.
rdbitpitmAvailabilityZone :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmAvailabilityZone =
    lens _rdbitpitmAvailabilityZone
        (\s a -> s { _rdbitpitmAvailabilityZone = a })

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid
-- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
-- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large
-- | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge |
-- db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small
-- | db.t2.medium Default: The same DBInstanceClass as the original DB
-- instance.
rdbitpitmDBInstanceClass :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmDBInstanceClass =
    lens _rdbitpitmDBInstanceClass
        (\s a -> s { _rdbitpitmDBInstanceClass = a })

-- | The database name for the restored DB instance.
rdbitpitmDBName :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmDBName = lens _rdbitpitmDBName (\s a -> s { _rdbitpitmDBName = a })

-- | The DB subnet group name to use for the new instance.
rdbitpitmDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmDBSubnetGroupName =
    lens _rdbitpitmDBSubnetGroupName
        (\s a -> s { _rdbitpitmDBSubnetGroupName = a })

-- | The database engine to use for the new instance. Default: The same as
-- source Constraint: Must be compatible with the engine of the source
-- Example: oracle-ee.
rdbitpitmEngine :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmEngine = lens _rdbitpitmEngine (\s a -> s { _rdbitpitmEngine = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance. Constraints: Must be an integer
-- greater than 1000. SQL Server Setting the IOPS value for the SQL Server
-- database engine is not supported.
rdbitpitmIops :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Int)
rdbitpitmIops = lens _rdbitpitmIops (\s a -> s { _rdbitpitmIops = a })

-- | License model information for the restored DB instance. Default: Same as
-- source. Valid values: license-included | bring-your-own-license |
-- general-public-license.
rdbitpitmLicenseModel :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmLicenseModel =
    lens _rdbitpitmLicenseModel (\s a -> s { _rdbitpitmLicenseModel = a })

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
-- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
-- set to true.
rdbitpitmMultiAZ :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Bool)
rdbitpitmMultiAZ = lens _rdbitpitmMultiAZ (\s a -> s { _rdbitpitmMultiAZ = a })

-- | The name of the option group to be used for the restored DB instance.
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group cannot
-- be removed from a DB instance once it is associated with a DB instance.
rdbitpitmOptionGroupName :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmOptionGroupName =
    lens _rdbitpitmOptionGroupName
        (\s a -> s { _rdbitpitmOptionGroupName = a })

-- | The port number on which the database accepts connections. Constraints:
-- Value must be 1150-65535 Default: The same port as the original DB
-- instance.
rdbitpitmPort :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Int)
rdbitpitmPort = lens _rdbitpitmPort (\s a -> s { _rdbitpitmPort = a })

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
rdbitpitmPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Bool)
rdbitpitmPubliclyAccessible =
    lens _rdbitpitmPubliclyAccessible
        (\s a -> s { _rdbitpitmPubliclyAccessible = a })

-- | The date and time to restore from. Valid Values: Value must be a UTC time
-- Constraints: Must be before the latest restorable time for the DB
-- instance Cannot be specified if UseLatestRestorableTime parameter is true
-- Example: 2009-09-07T23:45:00Z.
rdbitpitmRestoreTime :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe UTCTime)
rdbitpitmRestoreTime =
    lens _rdbitpitmRestoreTime (\s a -> s { _rdbitpitmRestoreTime = a })
        . mapping _Time

-- | The identifier of the source DB instance from which to restore.
-- Constraints: Must be the identifier of an existing database instance Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitmSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTimeMessage Text
rdbitpitmSourceDBInstanceIdentifier =
    lens _rdbitpitmSourceDBInstanceIdentifier
        (\s a -> s { _rdbitpitmSourceDBInstanceIdentifier = a })

-- | Specifies storage type to be associated with the DB Instance. Valid
-- values: standard | gp2 | io1 If you specify io1, you must also include a
-- value for the Iops parameter.
rdbitpitmStorageType :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmStorageType =
    lens _rdbitpitmStorageType (\s a -> s { _rdbitpitmStorageType = a })

rdbitpitmTags :: Lens' RestoreDBInstanceToPointInTimeMessage [Tag]
rdbitpitmTags = lens _rdbitpitmTags (\s a -> s { _rdbitpitmTags = a })

-- | The name of the new database instance to be created. Constraints: Must
-- contain from 1 to 63 alphanumeric characters or hyphens First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
rdbitpitmTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTimeMessage Text
rdbitpitmTargetDBInstanceIdentifier =
    lens _rdbitpitmTargetDBInstanceIdentifier
        (\s a -> s { _rdbitpitmTargetDBInstanceIdentifier = a })

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
rdbitpitmTdeCredentialArn :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmTdeCredentialArn =
    lens _rdbitpitmTdeCredentialArn
        (\s a -> s { _rdbitpitmTdeCredentialArn = a })

-- | The password for the given ARN from the Key Store in order to access the
-- device.
rdbitpitmTdeCredentialPassword :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Text)
rdbitpitmTdeCredentialPassword =
    lens _rdbitpitmTdeCredentialPassword
        (\s a -> s { _rdbitpitmTdeCredentialPassword = a })

-- | Specifies whether (true) or not (false) the DB instance is restored from
-- the latest backup time. Default: false Constraints: Cannot be specified
-- if RestoreTime parameter is provided.
rdbitpitmUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTimeMessage (Maybe Bool)
rdbitpitmUseLatestRestorableTime =
    lens _rdbitpitmUseLatestRestorableTime
        (\s a -> s { _rdbitpitmUseLatestRestorableTime = a })
instance ToQuery RestoreDBInstanceToPointInTimeMessage

instance ToPath RestoreDBInstanceToPointInTimeMessage where
    toPath = const "/"

newtype RestoreDBInstanceToPointInTimeResult = RestoreDBInstanceToPointInTimeResult
    { _rdbitpitrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RestoreDBInstanceToPointInTimeResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbitpitrDBInstance' @::@ 'Maybe' 'DBInstance'
--
restoreDBInstanceToPointInTimeResult :: RestoreDBInstanceToPointInTimeResult
restoreDBInstanceToPointInTimeResult = RestoreDBInstanceToPointInTimeResult
    { _rdbitpitrDBInstance = Nothing
    }

rdbitpitrDBInstance :: Lens' RestoreDBInstanceToPointInTimeResult (Maybe DBInstance)
rdbitpitrDBInstance =
    lens _rdbitpitrDBInstance (\s a -> s { _rdbitpitrDBInstance = a })
instance FromXML RestoreDBInstanceToPointInTimeResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreDBInstanceToPointInTimeResult"

instance AWSRequest RestoreDBInstanceToPointInTimeMessage where
    type Sv RestoreDBInstanceToPointInTimeMessage = RDS
    type Rs RestoreDBInstanceToPointInTimeMessage = RestoreDBInstanceToPointInTimeResult

    request  = post "RestoreDBInstanceToPointInTime"
    response = xmlResponse $ \h x -> RestoreDBInstanceToPointInTimeResult
        <$> x %| "DBInstance"
