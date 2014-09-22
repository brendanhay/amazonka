{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- retention enabled. https://rds.amazonaws.com/
-- ?Action=CreateDBInstanceReadReplica &DBInstanceIdentifier=myreadreplica
-- &SourceDBInstanceIdentifier=mydbinstance &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-15T23%3A35%3A07.325Z &AWSAccessKeyId= &Signature= mysql
-- 0 false general-public-license creating 5.1.50 myreadreplica in-sync
-- default.mysql5.1 mydbinstance active default 23:00-01:00 true
-- sun:05:00-sun:09:00 10 db.m1.small master
-- 3e24c5cd-c6e2-11df-8463-4f0c49644cb7.
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Request
      CreateDBInstanceReadReplica
    -- ** Request constructor
    , createDBInstanceReadReplica
    -- ** Request lenses
    , cdbirrDBInstanceIdentifier
    , cdbirrSourceDBInstanceIdentifier
    , cdbirrDBInstanceClass
    , cdbirrAvailabilityZone
    , cdbirrPort
    , cdbirrAutoMinorVersionUpgrade
    , cdbirrIops
    , cdbirrOptionGroupName
    , cdbirrPubliclyAccessible
    , cdbirrTags
    , cdbirrDBSubnetGroupName

    -- * Response
    , CreateDBInstanceReadReplicaResponse
    -- ** Response constructor
    , createDBInstanceReadReplicaResponse
    -- ** Response lenses
    , cdbirrrDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica
    { _cdbirrDBInstanceIdentifier :: Text
    , _cdbirrSourceDBInstanceIdentifier :: Text
    , _cdbirrDBInstanceClass :: Maybe Text
    , _cdbirrAvailabilityZone :: Maybe Text
    , _cdbirrPort :: Maybe Integer
    , _cdbirrAutoMinorVersionUpgrade :: Maybe Bool
    , _cdbirrIops :: Maybe Integer
    , _cdbirrOptionGroupName :: Maybe Text
    , _cdbirrPubliclyAccessible :: Maybe Bool
    , _cdbirrTags :: [Tag]
    , _cdbirrDBSubnetGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBInstanceReadReplica' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Text@
--
-- * @SourceDBInstanceIdentifier ::@ @Text@
--
-- * @DBInstanceClass ::@ @Maybe Text@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @Iops ::@ @Maybe Integer@
--
-- * @OptionGroupName ::@ @Maybe Text@
--
-- * @PubliclyAccessible ::@ @Maybe Bool@
--
-- * @Tags ::@ @[Tag]@
--
-- * @DBSubnetGroupName ::@ @Maybe Text@
--
createDBInstanceReadReplica :: Text -- ^ 'cdbirrDBInstanceIdentifier'
                            -> Text -- ^ 'cdbirrSourceDBInstanceIdentifier'
                            -> CreateDBInstanceReadReplica
createDBInstanceReadReplica p1 p2 = CreateDBInstanceReadReplica
    { _cdbirrDBInstanceIdentifier = p1
    , _cdbirrSourceDBInstanceIdentifier = p2
    , _cdbirrDBInstanceClass = Nothing
    , _cdbirrAvailabilityZone = Nothing
    , _cdbirrPort = Nothing
    , _cdbirrAutoMinorVersionUpgrade = Nothing
    , _cdbirrIops = Nothing
    , _cdbirrOptionGroupName = Nothing
    , _cdbirrPubliclyAccessible = Nothing
    , _cdbirrTags = mempty
    , _cdbirrDBSubnetGroupName = Nothing
    }

-- | The DB instance identifier of the read replica. This is the unique key that
-- identifies a DB instance. This parameter is stored as a lowercase string.
cdbirrDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdbirrDBInstanceIdentifier =
    lens _cdbirrDBInstanceIdentifier
         (\s a -> s { _cdbirrDBInstanceIdentifier = a })

-- | The identifier of the DB instance that will act as the source for the read
-- replica. Each DB instance can have up to five read replicas. Constraints:
-- Must be the identifier of an existing DB instance. Can specify a DB
-- instance that is a read replica only if the source is running MySQL 5.6.
-- The specified DB instance must have automatic backups enabled, its backup
-- retention period must be greater than 0.
cdbirrSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdbirrSourceDBInstanceIdentifier =
    lens _cdbirrSourceDBInstanceIdentifier
         (\s a -> s { _cdbirrSourceDBInstanceIdentifier = a })

-- | The compute and memory capacity of the read replica. Valid Values:
-- db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge
-- |db.m2.2xlarge | db.m2.4xlarge Default: Inherits from the source DB
-- instance.
cdbirrDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrDBInstanceClass =
    lens _cdbirrDBInstanceClass (\s a -> s { _cdbirrDBInstanceClass = a })

-- | The Amazon EC2 Availability Zone that the read replica will be created in.
-- Default: A random, system-chosen Availability Zone in the endpoint's
-- region. Example: us-east-1d.
cdbirrAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrAvailabilityZone =
    lens _cdbirrAvailabilityZone (\s a -> s { _cdbirrAvailabilityZone = a })

-- | The port number that the DB instance uses for connections. Default:
-- Inherits from the source DB instance Valid Values: 1150-65535.
cdbirrPort :: Lens' CreateDBInstanceReadReplica (Maybe Integer)
cdbirrPort = lens _cdbirrPort (\s a -> s { _cdbirrPort = a })

-- | Indicates that minor engine upgrades will be applied automatically to the
-- read replica during the maintenance window. Default: Inherits from the
-- source DB instance.
cdbirrAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdbirrAutoMinorVersionUpgrade =
    lens _cdbirrAutoMinorVersionUpgrade
         (\s a -> s { _cdbirrAutoMinorVersionUpgrade = a })

-- | The amount of Provisioned IOPS (input/output operations per second) to be
-- initially allocated for the DB instance.
cdbirrIops :: Lens' CreateDBInstanceReadReplica (Maybe Integer)
cdbirrIops = lens _cdbirrIops (\s a -> s { _cdbirrIops = a })

-- | The option group the DB instance will be associated with. If omitted, the
-- default option group for the engine specified will be used.
cdbirrOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrOptionGroupName =
    lens _cdbirrOptionGroupName (\s a -> s { _cdbirrOptionGroupName = a })

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
cdbirrPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdbirrPubliclyAccessible =
    lens _cdbirrPubliclyAccessible
         (\s a -> s { _cdbirrPubliclyAccessible = a })

-- | A list of tags.
cdbirrTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdbirrTags = lens _cdbirrTags (\s a -> s { _cdbirrTags = a })

-- | A DB Subnet Group to associate with this DB Instance in case of a cross
-- region read replica. If there is no DB Subnet Group, then it is a non-VPC
-- DB instance. Constraints: All the cross region read replicas that share the
-- source instance should lie within the same VPC.
cdbirrDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdbirrDBSubnetGroupName =
    lens _cdbirrDBSubnetGroupName
         (\s a -> s { _cdbirrDBSubnetGroupName = a })

instance ToQuery CreateDBInstanceReadReplica where
    toQuery = genericQuery def

newtype CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse
    { _cdbirrrDBInstance :: Maybe DBInstance
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBInstanceReadReplicaResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstance ::@ @Maybe DBInstance@
--
createDBInstanceReadReplicaResponse :: CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse
    { _cdbirrrDBInstance = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
cdbirrrDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdbirrrDBInstance =
    lens _cdbirrrDBInstance (\s a -> s { _cdbirrrDBInstance = a })

instance FromXML CreateDBInstanceReadReplicaResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBInstanceReadReplica where
    type Sv CreateDBInstanceReadReplica = RDS
    type Rs CreateDBInstanceReadReplica = CreateDBInstanceReadReplicaResponse

    request = post "CreateDBInstanceReadReplica"
    response _ = xmlResponse
