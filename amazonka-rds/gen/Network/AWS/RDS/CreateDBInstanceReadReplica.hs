{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a DB instance that acts as a Read Replica of a source DB
-- instance.
--
-- All Read Replica DB instances are created as Single-AZ deployments with
-- backups disabled. All other DB instance attributes (including DB
-- security groups and DB parameter groups) are inherited from the source
-- DB instance, except as specified below.
--
-- The source DB instance must have backup retention enabled.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBInstanceReadReplica.html>
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Request
      CreateDBInstanceReadReplica
    -- ** Request constructor
    , createDBInstanceReadReplica
    -- ** Request lenses
    , cdirrrqAutoMinorVersionUpgrade
    , cdirrrqPubliclyAccessible
    , cdirrrqDBSubnetGroupName
    , cdirrrqIOPS
    , cdirrrqDBInstanceClass
    , cdirrrqAvailabilityZone
    , cdirrrqOptionGroupName
    , cdirrrqTags
    , cdirrrqPort
    , cdirrrqStorageType
    , cdirrrqDBInstanceIdentifier
    , cdirrrqSourceDBInstanceIdentifier

    -- * Response
    , CreateDBInstanceReadReplicaResponse
    -- ** Response constructor
    , createDBInstanceReadReplicaResponse
    -- ** Response lenses
    , cdirrrsDBInstance
    , cdirrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDBInstanceReadReplica' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdirrrqAutoMinorVersionUpgrade'
--
-- * 'cdirrrqPubliclyAccessible'
--
-- * 'cdirrrqDBSubnetGroupName'
--
-- * 'cdirrrqIOPS'
--
-- * 'cdirrrqDBInstanceClass'
--
-- * 'cdirrrqAvailabilityZone'
--
-- * 'cdirrrqOptionGroupName'
--
-- * 'cdirrrqTags'
--
-- * 'cdirrrqPort'
--
-- * 'cdirrrqStorageType'
--
-- * 'cdirrrqDBInstanceIdentifier'
--
-- * 'cdirrrqSourceDBInstanceIdentifier'
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
    { _cdirrrqAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cdirrrqPubliclyAccessible         :: !(Maybe Bool)
    , _cdirrrqDBSubnetGroupName          :: !(Maybe Text)
    , _cdirrrqIOPS                       :: !(Maybe Int)
    , _cdirrrqDBInstanceClass            :: !(Maybe Text)
    , _cdirrrqAvailabilityZone           :: !(Maybe Text)
    , _cdirrrqOptionGroupName            :: !(Maybe Text)
    , _cdirrrqTags                       :: !(Maybe [Tag])
    , _cdirrrqPort                       :: !(Maybe Int)
    , _cdirrrqStorageType                :: !(Maybe Text)
    , _cdirrrqDBInstanceIdentifier       :: !Text
    , _cdirrrqSourceDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBInstanceReadReplica' smart constructor.
createDBInstanceReadReplica :: Text -> Text -> CreateDBInstanceReadReplica
createDBInstanceReadReplica pDBInstanceIdentifier pSourceDBInstanceIdentifier =
    CreateDBInstanceReadReplica'
    { _cdirrrqAutoMinorVersionUpgrade = Nothing
    , _cdirrrqPubliclyAccessible = Nothing
    , _cdirrrqDBSubnetGroupName = Nothing
    , _cdirrrqIOPS = Nothing
    , _cdirrrqDBInstanceClass = Nothing
    , _cdirrrqAvailabilityZone = Nothing
    , _cdirrrqOptionGroupName = Nothing
    , _cdirrrqTags = Nothing
    , _cdirrrqPort = Nothing
    , _cdirrrqStorageType = Nothing
    , _cdirrrqDBInstanceIdentifier = pDBInstanceIdentifier
    , _cdirrrqSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier
    }

-- | Indicates that minor engine upgrades will be applied automatically to
-- the Read Replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
cdirrrqAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrrqAutoMinorVersionUpgrade = lens _cdirrrqAutoMinorVersionUpgrade (\ s a -> s{_cdirrrqAutoMinorVersionUpgrade = a});

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS
-- name, which resolves to a public IP address. A value of false specifies
-- an internal instance with a DNS name that resolves to a private IP
-- address.
--
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case.
--
-- -   __Default VPC:__true
-- -   __VPC:__false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
cdirrrqPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrrqPubliclyAccessible = lens _cdirrrqPubliclyAccessible (\ s a -> s{_cdirrrqPubliclyAccessible = a});

-- | Specifies a DB subnet group for the DB instance. The new DB instance
-- will be created in the VPC associated with the DB subnet group. If no DB
-- subnet group is specified, then the new DB instance is not created in a
-- VPC.
--
-- Constraints:
--
-- -   Can only be specified if the source DB instance identifier specifies
--     a DB instance in another region.
-- -   The specified DB subnet group must be in the same region in which
--     the operation is running.
-- -   All Read Replicas in one region that are created from the same
--     source DB instance must either:
--     -   Specify DB subnet groups from the same VPC. All these Read
--         Replicas will be created in the same VPC.
--     -   Not specify a DB subnet group. All these Read Replicas will be
--         created outside of any VPC.
cdirrrqDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrrqDBSubnetGroupName = lens _cdirrrqDBSubnetGroupName (\ s a -> s{_cdirrrqDBSubnetGroupName = a});

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
cdirrrqIOPS :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrrqIOPS = lens _cdirrrqIOPS (\ s a -> s{_cdirrrqIOPS = a});

-- | The compute and memory capacity of the Read Replica.
--
-- Valid Values:
-- @db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
--
-- Default: Inherits from the source DB instance.
cdirrrqDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrrqDBInstanceClass = lens _cdirrrqDBInstanceClass (\ s a -> s{_cdirrrqDBInstanceClass = a});

-- | The Amazon EC2 Availability Zone that the Read Replica will be created
-- in.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- region.
--
-- Example: @us-east-1d@
cdirrrqAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrrqAvailabilityZone = lens _cdirrrqAvailabilityZone (\ s a -> s{_cdirrrqAvailabilityZone = a});

-- | The option group the DB instance will be associated with. If omitted,
-- the default option group for the engine specified will be used.
cdirrrqOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrrqOptionGroupName = lens _cdirrrqOptionGroupName (\ s a -> s{_cdirrrqOptionGroupName = a});

-- | FIXME: Undocumented member.
cdirrrqTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdirrrqTags = lens _cdirrrqTags (\ s a -> s{_cdirrrqTags = a}) . _Default;

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
cdirrrqPort :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrrqPort = lens _cdirrrqPort (\ s a -> s{_cdirrrqPort = a});

-- | Specifies the storage type to be associated with the Read Replica.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
cdirrrqStorageType :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrrqStorageType = lens _cdirrrqStorageType (\ s a -> s{_cdirrrqStorageType = a});

-- | The DB instance identifier of the Read Replica. This is the unique key
-- that identifies a DB instance. This parameter is stored as a lowercase
-- string.
cdirrrqDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrrqDBInstanceIdentifier = lens _cdirrrqDBInstanceIdentifier (\ s a -> s{_cdirrrqDBInstanceIdentifier = a});

-- | The identifier of the DB instance that will act as the source for the
-- Read Replica. Each DB instance can have up to five Read Replicas.
--
-- Constraints:
--
-- -   Must be the identifier of an existing DB instance.
-- -   Can specify a DB instance that is a MySQL Read Replica only if the
--     source is running MySQL 5.6.
-- -   Can specify a DB instance that is a PostgreSQL Read Replica only if
--     the source is running PostgreSQL 9.3.5.
-- -   The specified DB instance must have automatic backups enabled, its
--     backup retention period must be greater than 0.
-- -   If the source DB instance is in the same region as the Read Replica,
--     specify a valid DB instance identifier.
-- -   If the source DB instance is in a different region than the Read
--     Replica, specify a valid DB instance ARN. For more information, go
--     to
--     <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing a Amazon RDS Amazon Resource Name (ARN)>.
cdirrrqSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrrqSourceDBInstanceIdentifier = lens _cdirrrqSourceDBInstanceIdentifier (\ s a -> s{_cdirrrqSourceDBInstanceIdentifier = a});

instance AWSRequest CreateDBInstanceReadReplica where
        type Sv CreateDBInstanceReadReplica = RDS
        type Rs CreateDBInstanceReadReplica =
             CreateDBInstanceReadReplicaResponse
        request = post
        response
          = receiveXMLWrapper
              "CreateDBInstanceReadReplicaResult"
              (\ s h x ->
                 CreateDBInstanceReadReplicaResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBInstanceReadReplica where
        toHeaders = const mempty

instance ToPath CreateDBInstanceReadReplica where
        toPath = const "/"

instance ToQuery CreateDBInstanceReadReplica where
        toQuery CreateDBInstanceReadReplica'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBInstanceReadReplica" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "AutoMinorVersionUpgrade" =:
                 _cdirrrqAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _cdirrrqPubliclyAccessible,
               "DBSubnetGroupName" =: _cdirrrqDBSubnetGroupName,
               "Iops" =: _cdirrrqIOPS,
               "DBInstanceClass" =: _cdirrrqDBInstanceClass,
               "AvailabilityZone" =: _cdirrrqAvailabilityZone,
               "OptionGroupName" =: _cdirrrqOptionGroupName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdirrrqTags),
               "Port" =: _cdirrrqPort,
               "StorageType" =: _cdirrrqStorageType,
               "DBInstanceIdentifier" =:
                 _cdirrrqDBInstanceIdentifier,
               "SourceDBInstanceIdentifier" =:
                 _cdirrrqSourceDBInstanceIdentifier]

-- | /See:/ 'createDBInstanceReadReplicaResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdirrrsDBInstance'
--
-- * 'cdirrrsStatus'
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
    { _cdirrrsDBInstance :: !(Maybe DBInstance)
    , _cdirrrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBInstanceReadReplicaResponse' smart constructor.
createDBInstanceReadReplicaResponse :: Int -> CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse pStatus =
    CreateDBInstanceReadReplicaResponse'
    { _cdirrrsDBInstance = Nothing
    , _cdirrrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdirrrsDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdirrrsDBInstance = lens _cdirrrsDBInstance (\ s a -> s{_cdirrrsDBInstance = a});

-- | FIXME: Undocumented member.
cdirrrsStatus :: Lens' CreateDBInstanceReadReplicaResponse Int
cdirrrsStatus = lens _cdirrrsStatus (\ s a -> s{_cdirrrsStatus = a});
