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

-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a DB instance that acts as a read replica of a source DB instance.
-- All read replica DB instances are created as Single-AZ deployments with
-- backups disabled. All other DB instance attributes (including DB security
-- groups and DB parameter groups) are inherited from the source DB instance,
-- except as specified below. The source DB instance must have backup
-- retention enabled.
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Request
      CreateDBInstanceReadReplicaMessage
    -- ** Request constructor
    , createDBInstanceReadReplica
    -- ** Request lenses
    , cdbirrmAutoMinorVersionUpgrade
    , cdbirrmAvailabilityZone
    , cdbirrmDBInstanceClass
    , cdbirrmDBInstanceIdentifier
    , cdbirrmDBSubnetGroupName
    , cdbirrmIops
    , cdbirrmOptionGroupName
    , cdbirrmPort
    , cdbirrmPubliclyAccessible
    , cdbirrmSourceDBInstanceIdentifier
    , cdbirrmStorageType
    , cdbirrmTags

    -- * Response
    , CreateDBInstanceReadReplicaResult
    -- ** Response constructor
    , createDBInstanceReadReplicaResponse
    -- ** Response lenses
    , cdbirrrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBInstanceReadReplicaMessage = CreateDBInstanceReadReplicaMessage
    { _cdbirrmAutoMinorVersionUpgrade    :: Maybe Bool
    , _cdbirrmAvailabilityZone           :: Maybe Text
    , _cdbirrmDBInstanceClass            :: Maybe Text
    , _cdbirrmDBInstanceIdentifier       :: Text
    , _cdbirrmDBSubnetGroupName          :: Maybe Text
    , _cdbirrmIops                       :: Maybe Int
    , _cdbirrmOptionGroupName            :: Maybe Text
    , _cdbirrmPort                       :: Maybe Int
    , _cdbirrmPubliclyAccessible         :: Maybe Bool
    , _cdbirrmSourceDBInstanceIdentifier :: Text
    , _cdbirrmStorageType                :: Maybe Text
    , _cdbirrmTags                       :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceReadReplicaMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbirrmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cdbirrmAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrmDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrmDBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbirrmDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrmIops' @::@ 'Maybe' 'Int'
--
-- * 'cdbirrmOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrmPort' @::@ 'Maybe' 'Int'
--
-- * 'cdbirrmPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'cdbirrmSourceDBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbirrmStorageType' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrmTags' @::@ ['Tag']
--
createDBInstanceReadReplica :: Text -- ^ 'cdbirrmDBInstanceIdentifier'
                            -> Text -- ^ 'cdbirrmSourceDBInstanceIdentifier'
                            -> CreateDBInstanceReadReplicaMessage
createDBInstanceReadReplica p1 p2 = CreateDBInstanceReadReplicaMessage
    { _cdbirrmDBInstanceIdentifier       = p1
    , _cdbirrmSourceDBInstanceIdentifier = p2
    , _cdbirrmDBInstanceClass            = Nothing
    , _cdbirrmAvailabilityZone           = Nothing
    , _cdbirrmPort                       = Nothing
    , _cdbirrmAutoMinorVersionUpgrade    = Nothing
    , _cdbirrmIops                       = Nothing
    , _cdbirrmOptionGroupName            = Nothing
    , _cdbirrmPubliclyAccessible         = Nothing
    , _cdbirrmTags                       = mempty
    , _cdbirrmDBSubnetGroupName          = Nothing
    , _cdbirrmStorageType                = Nothing
    }

-- | Indicates that minor engine upgrades will be applied automatically to the
-- read replica during the maintenance window. Default: Inherits from the
-- source DB instance.
cdbirrmAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Bool)
cdbirrmAutoMinorVersionUpgrade =
    lens _cdbirrmAutoMinorVersionUpgrade
        (\s a -> s { _cdbirrmAutoMinorVersionUpgrade = a })

-- | The Amazon EC2 Availability Zone that the read replica will be created
-- in. Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d.
cdbirrmAvailabilityZone :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Text)
cdbirrmAvailabilityZone =
    lens _cdbirrmAvailabilityZone (\s a -> s { _cdbirrmAvailabilityZone = a })

-- | The compute and memory capacity of the read replica. Valid Values:
-- db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge
-- |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large |
-- db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge
-- | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small |
-- db.t2.medium Default: Inherits from the source DB instance.
cdbirrmDBInstanceClass :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Text)
cdbirrmDBInstanceClass =
    lens _cdbirrmDBInstanceClass (\s a -> s { _cdbirrmDBInstanceClass = a })

-- | The DB instance identifier of the read replica. This is the unique key
-- that identifies a DB instance. This parameter is stored as a lowercase
-- string.
cdbirrmDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplicaMessage Text
cdbirrmDBInstanceIdentifier =
    lens _cdbirrmDBInstanceIdentifier
        (\s a -> s { _cdbirrmDBInstanceIdentifier = a })

-- | Specifies a DB subnet group for the DB instance. The new DB instance will
-- be created in the VPC associated with the DB subnet group. If no DB
-- subnet group is specified, then the new DB instance is not created in a
-- VPC. Constraints: Can only be specified if the source DB instance
-- identifier specifies a DB instance in another region. The specified DB
-- subnet group must be in the same region in which the operation is
-- running. All read replicas in one region that are created from the same
-- source DB instance must either: Specify DB subnet groups from the same
-- VPC. All these read replicas will be created in the same VPC. Not specify
-- a DB subnet group. All these read replicas will be created outside of any
-- VPC.
cdbirrmDBSubnetGroupName :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Text)
cdbirrmDBSubnetGroupName =
    lens _cdbirrmDBSubnetGroupName
        (\s a -> s { _cdbirrmDBSubnetGroupName = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance.
cdbirrmIops :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Int)
cdbirrmIops = lens _cdbirrmIops (\s a -> s { _cdbirrmIops = a })

-- | The option group the DB instance will be associated with. If omitted, the
-- default option group for the engine specified will be used.
cdbirrmOptionGroupName :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Text)
cdbirrmOptionGroupName =
    lens _cdbirrmOptionGroupName (\s a -> s { _cdbirrmOptionGroupName = a })

-- | The port number that the DB instance uses for connections. Default:
-- Inherits from the source DB instance Valid Values: 1150-65535.
cdbirrmPort :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Int)
cdbirrmPort = lens _cdbirrmPort (\s a -> s { _cdbirrmPort = a })

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
cdbirrmPubliclyAccessible :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Bool)
cdbirrmPubliclyAccessible =
    lens _cdbirrmPubliclyAccessible
        (\s a -> s { _cdbirrmPubliclyAccessible = a })

-- | The identifier of the DB instance that will act as the source for the
-- read replica. Each DB instance can have up to five read replicas.
-- Constraints: Must be the identifier of an existing DB instance. Can
-- specify a DB instance that is a read replica only if the source is
-- running MySQL 5.6. The specified DB instance must have automatic backups
-- enabled, its backup retention period must be greater than 0. If the
-- source DB instance is in the same region as the read replica, specify a
-- valid DB instance identifier. If the source DB instance is in a different
-- region than the read replica, specify a valid DB instance ARN. For more
-- information, go to Constructing a Amazon RDS Amazon Resource Name (ARN).
cdbirrmSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplicaMessage Text
cdbirrmSourceDBInstanceIdentifier =
    lens _cdbirrmSourceDBInstanceIdentifier
        (\s a -> s { _cdbirrmSourceDBInstanceIdentifier = a })

-- | Specifies storage type to be associated with the DB Instance read
-- replica. Valid values: standard | gp2 | io1 If you specify io1, you must
-- also include a value for the Iops parameter.
cdbirrmStorageType :: Lens' CreateDBInstanceReadReplicaMessage (Maybe Text)
cdbirrmStorageType =
    lens _cdbirrmStorageType (\s a -> s { _cdbirrmStorageType = a })

cdbirrmTags :: Lens' CreateDBInstanceReadReplicaMessage [Tag]
cdbirrmTags = lens _cdbirrmTags (\s a -> s { _cdbirrmTags = a })

instance ToPath CreateDBInstanceReadReplicaMessage where
    toPath = const "/"

instance ToQuery CreateDBInstanceReadReplicaMessage

newtype CreateDBInstanceReadReplicaResult = CreateDBInstanceReadReplicaResult
    { _cdbirrrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceReadReplicaResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbirrrDBInstance' @::@ 'Maybe' 'DBInstance'
--
createDBInstanceReadReplicaResponse :: CreateDBInstanceReadReplicaResult
createDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResult
    { _cdbirrrDBInstance = Nothing
    }

cdbirrrDBInstance :: Lens' CreateDBInstanceReadReplicaResult (Maybe DBInstance)
cdbirrrDBInstance =
    lens _cdbirrrDBInstance (\s a -> s { _cdbirrrDBInstance = a })

instance AWSRequest CreateDBInstanceReadReplicaMessage where
    type Sv CreateDBInstanceReadReplicaMessage = RDS
    type Rs CreateDBInstanceReadReplicaMessage = CreateDBInstanceReadReplicaResult

    request  = post "CreateDBInstanceReadReplica"
    response = xmlResponse $ \h x -> CreateDBInstanceReadReplicaResult
        <$> x %| "DBInstance"
