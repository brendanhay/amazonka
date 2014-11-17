{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBInstanceReadReplica.html>
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Request
      CreateDBInstanceReadReplica
    -- ** Request constructor
    , createDBInstanceReadReplica
    -- ** Request lenses
    , cdbirrAutoMinorVersionUpgrade
    , cdbirrAvailabilityZone
    , cdbirrDBInstanceClass
    , cdbirrDBInstanceIdentifier
    , cdbirrDBSubnetGroupName
    , cdbirrIops
    , cdbirrOptionGroupName
    , cdbirrPort
    , cdbirrPubliclyAccessible
    , cdbirrSourceDBInstanceIdentifier
    , cdbirrStorageType
    , cdbirrTags

    -- * Response
    , CreateDBInstanceReadReplicaResponse
    -- ** Response constructor
    , createDBInstanceReadReplicaResponse
    -- ** Response lenses
    , cdbirrrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica
    { _cdbirrAutoMinorVersionUpgrade    :: Maybe Bool
    , _cdbirrAvailabilityZone           :: Maybe Text
    , _cdbirrDBInstanceClass            :: Maybe Text
    , _cdbirrDBInstanceIdentifier       :: Text
    , _cdbirrDBSubnetGroupName          :: Maybe Text
    , _cdbirrIops                       :: Maybe Int
    , _cdbirrOptionGroupName            :: Maybe Text
    , _cdbirrPort                       :: Maybe Int
    , _cdbirrPubliclyAccessible         :: Maybe Bool
    , _cdbirrSourceDBInstanceIdentifier :: Text
    , _cdbirrStorageType                :: Maybe Text
    , _cdbirrTags                       :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceReadReplica' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbirrAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cdbirrAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrDBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbirrDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrIops' @::@ 'Maybe' 'Int'
--
-- * 'cdbirrOptionGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrPort' @::@ 'Maybe' 'Int'
--
-- * 'cdbirrPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'cdbirrSourceDBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbirrStorageType' @::@ 'Maybe' 'Text'
--
-- * 'cdbirrTags' @::@ ['Tag']
--
createDBInstanceReadReplica :: Text -- ^ 'cdbirrDBInstanceIdentifier'
                            -> Text -- ^ 'cdbirrSourceDBInstanceIdentifier'
                            -> CreateDBInstanceReadReplica
createDBInstanceReadReplica p1 p2 = CreateDBInstanceReadReplica
    { _cdbirrDBInstanceIdentifier       = p1
    , _cdbirrSourceDBInstanceIdentifier = p2
    , _cdbirrDBInstanceClass            = Nothing
    , _cdbirrAvailabilityZone           = Nothing
    , _cdbirrPort                       = Nothing
    , _cdbirrAutoMinorVersionUpgrade    = Nothing
    , _cdbirrIops                       = Nothing
    , _cdbirrOptionGroupName            = Nothing
    , _cdbirrPubliclyAccessible         = Nothing
    , _cdbirrTags                       = mempty
    , _cdbirrDBSubnetGroupName          = Nothing
    , _cdbirrStorageType                = Nothing
    }

-- | Indicates that minor engine upgrades will be applied automatically to the
-- read replica during the maintenance window. Default: Inherits from the
-- source DB instance.
cdbirrAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdbirrAutoMinorVersionUpgrade =
    lens _cdbirrAutoMinorVersionUpgrade
        (\s a -> s { _cdbirrAutoMinorVersionUpgrade = a })

-- | The Amazon EC2 Availability Zone that the read replica will be created
-- in. Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d.
cdbirrAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrAvailabilityZone =
    lens _cdbirrAvailabilityZone (\s a -> s { _cdbirrAvailabilityZone = a })

-- | The compute and memory capacity of the read replica. Valid Values:
-- db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge
-- |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large |
-- db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge
-- | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small |
-- db.t2.medium Default: Inherits from the source DB instance.
cdbirrDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrDBInstanceClass =
    lens _cdbirrDBInstanceClass (\s a -> s { _cdbirrDBInstanceClass = a })

-- | The DB instance identifier of the read replica. This is the unique key
-- that identifies a DB instance. This parameter is stored as a lowercase
-- string.
cdbirrDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdbirrDBInstanceIdentifier =
    lens _cdbirrDBInstanceIdentifier
        (\s a -> s { _cdbirrDBInstanceIdentifier = a })

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
cdbirrDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrDBSubnetGroupName =
    lens _cdbirrDBSubnetGroupName (\s a -> s { _cdbirrDBSubnetGroupName = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance.
cdbirrIops :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdbirrIops = lens _cdbirrIops (\s a -> s { _cdbirrIops = a })

-- | The option group the DB instance will be associated with. If omitted, the
-- default option group for the engine specified will be used.
cdbirrOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrOptionGroupName =
    lens _cdbirrOptionGroupName (\s a -> s { _cdbirrOptionGroupName = a })

-- | The port number that the DB instance uses for connections. Default:
-- Inherits from the source DB instance Valid Values: 1150-65535.
cdbirrPort :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdbirrPort = lens _cdbirrPort (\s a -> s { _cdbirrPort = a })

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
cdbirrPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdbirrPubliclyAccessible =
    lens _cdbirrPubliclyAccessible
        (\s a -> s { _cdbirrPubliclyAccessible = a })

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
cdbirrSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdbirrSourceDBInstanceIdentifier =
    lens _cdbirrSourceDBInstanceIdentifier
        (\s a -> s { _cdbirrSourceDBInstanceIdentifier = a })

-- | Specifies storage type to be associated with the DB Instance read
-- replica. Valid values: standard | gp2 | io1 If you specify io1, you must
-- also include a value for the Iops parameter.
cdbirrStorageType :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrStorageType =
    lens _cdbirrStorageType (\s a -> s { _cdbirrStorageType = a })

cdbirrTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdbirrTags = lens _cdbirrTags (\s a -> s { _cdbirrTags = a })

newtype CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse
    { _cdbirrrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'CreateDBInstanceReadReplicaResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbirrrDBInstance' @::@ 'Maybe' 'DBInstance'
--
createDBInstanceReadReplicaResponse :: CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse
    { _cdbirrrDBInstance = Nothing
    }

cdbirrrDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdbirrrDBInstance =
    lens _cdbirrrDBInstance (\s a -> s { _cdbirrrDBInstance = a })

instance ToPath CreateDBInstanceReadReplica where
    toPath = const "/"

instance ToQuery CreateDBInstanceReadReplica

instance ToHeaders CreateDBInstanceReadReplica

instance AWSRequest CreateDBInstanceReadReplica where
    type Sv CreateDBInstanceReadReplica = RDS
    type Rs CreateDBInstanceReadReplica = CreateDBInstanceReadReplicaResponse

    request  = post "CreateDBInstanceReadReplica"
    response = xmlResponse

instance FromXML CreateDBInstanceReadReplicaResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateDBInstanceReadReplicaResponse"
