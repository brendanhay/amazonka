{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateDBInstanceReadReplica
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
-- retention enabled. https://rds.amazonaws.com/
-- ?Action=CreateDBInstanceReadReplica &DBInstanceIdentifier=myreadreplica
-- &SourceDBInstanceIdentifier=mydbinstance &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-15T23%3A35%3A07.325Z &AWSAccessKeyId= &Signature= mysql
-- 0 false general-public-license creating 5.1.50 myreadreplica in-sync
-- default.mysql5.1 mydbinstance active default 23:00-01:00 true
-- sun:05:00-sun:09:00 10 db.m1.small master
-- 3e24c5cd-c6e2-11df-8463-4f0c49644cb7.
module Network.AWS.RDS.V2013_09_09.CreateDBInstanceReadReplica where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDBInstanceReadReplica' request.
createDBInstanceReadReplica :: Text -- ^ '_cdbirrmSourceDBInstanceIdentifier'
                            -> Text -- ^ '_cdbirrmDBInstanceIdentifier'
                            -> CreateDBInstanceReadReplica
createDBInstanceReadReplica p1 p2 = CreateDBInstanceReadReplica
    { _cdbirrmSourceDBInstanceIdentifier = p1
    , _cdbirrmDBInstanceIdentifier = p2
    , _cdbirrmPubliclyAccessible = Nothing
    , _cdbirrmAutoMinorVersionUpgrade = Nothing
    , _cdbirrmIops = Nothing
    , _cdbirrmPort = Nothing
    , _cdbirrmDBSubnetGroupName = Nothing
    , _cdbirrmDBInstanceClass = Nothing
    , _cdbirrmAvailabilityZone = Nothing
    , _cdbirrmOptionGroupName = Nothing
    , _cdbirrmTags = mempty
    }

data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica
    { _cdbirrmSourceDBInstanceIdentifier :: Text
      -- ^ The identifier of the DB instance that will act as the source for
      -- the read replica. Each DB instance can have up to five read
      -- replicas. Constraints: Must be the identifier of an existing DB
      -- instance. Can specify a DB instance that is a read replica only
      -- if the source is running MySQL 5.6. The specified DB instance
      -- must have automatic backups enabled, its backup retention period
      -- must be greater than 0.
    , _cdbirrmDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier of the read replica. This is the
      -- unique key that identifies a DB instance. This parameter is
      -- stored as a lowercase string.
    , _cdbirrmPubliclyAccessible :: Maybe Bool
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
    , _cdbirrmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor engine upgrades will be applied
      -- automatically to the read replica during the maintenance window.
      -- Default: Inherits from the source DB instance.
    , _cdbirrmIops :: Maybe Integer
      -- ^ The amount of Provisioned IOPS (input/output operations per
      -- second) to be initially allocated for the DB instance.
    , _cdbirrmPort :: Maybe Integer
      -- ^ The port number that the DB instance uses for connections.
      -- Default: Inherits from the source DB instance Valid Values:
      -- 1150-65535.
    , _cdbirrmDBSubnetGroupName :: Maybe Text
      -- ^ A DB Subnet Group to associate with this DB Instance in case of a
      -- cross region read replica. If there is no DB Subnet Group, then
      -- it is a non-VPC DB instance. Constraints: All the cross region
      -- read replicas that share the source instance should lie within
      -- the same VPC.
    , _cdbirrmDBInstanceClass :: Maybe Text
      -- ^ The compute and memory capacity of the read replica. Valid
      -- Values: db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge |
      -- db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge Default: Inherits
      -- from the source DB instance.
    , _cdbirrmAvailabilityZone :: Maybe Text
      -- ^ The Amazon EC2 Availability Zone that the read replica will be
      -- created in. Default: A random, system-chosen Availability Zone in
      -- the endpoint's region. Example: us-east-1d.
    , _cdbirrmOptionGroupName :: Maybe Text
      -- ^ The option group the DB instance will be associated with. If
      -- omitted, the default option group for the engine specified will
      -- be used.
    , _cdbirrmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

makeLenses ''CreateDBInstanceReadReplica

instance ToQuery CreateDBInstanceReadReplica where
    toQuery = genericQuery def

data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse
    { _dbiduDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

makeLenses ''CreateDBInstanceReadReplicaResponse

instance FromXML CreateDBInstanceReadReplicaResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBInstanceReadReplica where
    type Sv CreateDBInstanceReadReplica = RDS
    type Rs CreateDBInstanceReadReplica = CreateDBInstanceReadReplicaResponse

    request = post "CreateDBInstanceReadReplica"
    response _ = xmlResponse
