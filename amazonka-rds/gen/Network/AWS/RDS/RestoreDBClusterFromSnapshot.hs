{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB cluster snapshot. The target DB
-- cluster is created from the source DB cluster restore point with the
-- same configuration as the original source DB cluster, except that the
-- new DB cluster is created with the default security group.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RestoreDBClusterFromSnapshot.html AWS API Reference> for RestoreDBClusterFromSnapshot.
module Network.AWS.RDS.RestoreDBClusterFromSnapshot
    (
    -- * Creating a Request
      restoreDBClusterFromSnapshot
    , RestoreDBClusterFromSnapshot
    -- * Request Lenses
    , rdcfsEngineVersion
    , rdcfsDBSubnetGroupName
    , rdcfsAvailabilityZones
    , rdcfsDatabaseName
    , rdcfsVPCSecurityGroupIds
    , rdcfsOptionGroupName
    , rdcfsTags
    , rdcfsPort
    , rdcfsDBClusterIdentifier
    , rdcfsSnapshotIdentifier
    , rdcfsEngine

    -- * Destructuring the Response
    , restoreDBClusterFromSnapshotResponse
    , RestoreDBClusterFromSnapshotResponse
    -- * Response Lenses
    , rdcfsrsDBCluster
    , rdcfsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'restoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
    { _rdcfsEngineVersion       :: !(Maybe Text)
    , _rdcfsDBSubnetGroupName   :: !(Maybe Text)
    , _rdcfsAvailabilityZones   :: !(Maybe [Text])
    , _rdcfsDatabaseName        :: !(Maybe Text)
    , _rdcfsVPCSecurityGroupIds :: !(Maybe [Text])
    , _rdcfsOptionGroupName     :: !(Maybe Text)
    , _rdcfsTags                :: !(Maybe [Tag])
    , _rdcfsPort                :: !(Maybe Int)
    , _rdcfsDBClusterIdentifier :: !Text
    , _rdcfsSnapshotIdentifier  :: !Text
    , _rdcfsEngine              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestoreDBClusterFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsEngineVersion'
--
-- * 'rdcfsDBSubnetGroupName'
--
-- * 'rdcfsAvailabilityZones'
--
-- * 'rdcfsDatabaseName'
--
-- * 'rdcfsVPCSecurityGroupIds'
--
-- * 'rdcfsOptionGroupName'
--
-- * 'rdcfsTags'
--
-- * 'rdcfsPort'
--
-- * 'rdcfsDBClusterIdentifier'
--
-- * 'rdcfsSnapshotIdentifier'
--
-- * 'rdcfsEngine'
restoreDBClusterFromSnapshot
    :: Text -- ^ 'rdcfsDBClusterIdentifier'
    -> Text -- ^ 'rdcfsSnapshotIdentifier'
    -> Text -- ^ 'rdcfsEngine'
    -> RestoreDBClusterFromSnapshot
restoreDBClusterFromSnapshot pDBClusterIdentifier_ pSnapshotIdentifier_ pEngine_ =
    RestoreDBClusterFromSnapshot'
    { _rdcfsEngineVersion = Nothing
    , _rdcfsDBSubnetGroupName = Nothing
    , _rdcfsAvailabilityZones = Nothing
    , _rdcfsDatabaseName = Nothing
    , _rdcfsVPCSecurityGroupIds = Nothing
    , _rdcfsOptionGroupName = Nothing
    , _rdcfsTags = Nothing
    , _rdcfsPort = Nothing
    , _rdcfsDBClusterIdentifier = pDBClusterIdentifier_
    , _rdcfsSnapshotIdentifier = pSnapshotIdentifier_
    , _rdcfsEngine = pEngine_
    }

-- | The version of the database engine to use for the new DB cluster.
rdcfsEngineVersion :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsEngineVersion = lens _rdcfsEngineVersion (\ s a -> s{_rdcfsEngineVersion = a});

-- | The name of the DB subnet group to use for the new DB cluster.
rdcfsDBSubnetGroupName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsDBSubnetGroupName = lens _rdcfsDBSubnetGroupName (\ s a -> s{_rdcfsDBSubnetGroupName = a});

-- | Provides the list of EC2 Availability Zones that instances in the
-- restored DB cluster can be created in.
rdcfsAvailabilityZones :: Lens' RestoreDBClusterFromSnapshot [Text]
rdcfsAvailabilityZones = lens _rdcfsAvailabilityZones (\ s a -> s{_rdcfsAvailabilityZones = a}) . _Default . _Coerce;

-- | The database name for the restored DB cluster.
rdcfsDatabaseName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsDatabaseName = lens _rdcfsDatabaseName (\ s a -> s{_rdcfsDatabaseName = a});

-- | A list of VPC security groups that the new DB cluster will belong to.
rdcfsVPCSecurityGroupIds :: Lens' RestoreDBClusterFromSnapshot [Text]
rdcfsVPCSecurityGroupIds = lens _rdcfsVPCSecurityGroupIds (\ s a -> s{_rdcfsVPCSecurityGroupIds = a}) . _Default . _Coerce;

-- | The name of the option group to use for the restored DB cluster.
rdcfsOptionGroupName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdcfsOptionGroupName = lens _rdcfsOptionGroupName (\ s a -> s{_rdcfsOptionGroupName = a});

-- | The tags to be assigned to the restored DB cluster.
rdcfsTags :: Lens' RestoreDBClusterFromSnapshot [Tag]
rdcfsTags = lens _rdcfsTags (\ s a -> s{_rdcfsTags = a}) . _Default . _Coerce;

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: Value must be '1150-65535'
--
-- Default: The same port as the original DB cluster.
rdcfsPort :: Lens' RestoreDBClusterFromSnapshot (Maybe Int)
rdcfsPort = lens _rdcfsPort (\ s a -> s{_rdcfsPort = a});

-- | The name of the DB cluster to create from the DB cluster snapshot. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: 'my-snapshot-id'
rdcfsDBClusterIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsDBClusterIdentifier = lens _rdcfsDBClusterIdentifier (\ s a -> s{_rdcfsDBClusterIdentifier = a});

-- | The identifier for the DB cluster snapshot to restore from.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rdcfsSnapshotIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsSnapshotIdentifier = lens _rdcfsSnapshotIdentifier (\ s a -> s{_rdcfsSnapshotIdentifier = a});

-- | The database engine to use for the new DB cluster.
--
-- Default: The same as source
--
-- Constraint: Must be compatible with the engine of the source
rdcfsEngine :: Lens' RestoreDBClusterFromSnapshot Text
rdcfsEngine = lens _rdcfsEngine (\ s a -> s{_rdcfsEngine = a});

instance AWSRequest RestoreDBClusterFromSnapshot
         where
        type Rs RestoreDBClusterFromSnapshot =
             RestoreDBClusterFromSnapshotResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper
              "RestoreDBClusterFromSnapshotResult"
              (\ s h x ->
                 RestoreDBClusterFromSnapshotResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance ToHeaders RestoreDBClusterFromSnapshot where
        toHeaders = const mempty

instance ToPath RestoreDBClusterFromSnapshot where
        toPath = const "/"

instance ToQuery RestoreDBClusterFromSnapshot where
        toQuery RestoreDBClusterFromSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBClusterFromSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _rdcfsEngineVersion,
               "DBSubnetGroupName" =: _rdcfsDBSubnetGroupName,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rdcfsAvailabilityZones),
               "DatabaseName" =: _rdcfsDatabaseName,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rdcfsVPCSecurityGroupIds),
               "OptionGroupName" =: _rdcfsOptionGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _rdcfsTags),
               "Port" =: _rdcfsPort,
               "DBClusterIdentifier" =: _rdcfsDBClusterIdentifier,
               "SnapshotIdentifier" =: _rdcfsSnapshotIdentifier,
               "Engine" =: _rdcfsEngine]

-- | /See:/ 'restoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
    { _rdcfsrsDBCluster :: !(Maybe DBCluster)
    , _rdcfsrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestoreDBClusterFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsrsDBCluster'
--
-- * 'rdcfsrsStatus'
restoreDBClusterFromSnapshotResponse
    :: Int -- ^ 'rdcfsrsStatus'
    -> RestoreDBClusterFromSnapshotResponse
restoreDBClusterFromSnapshotResponse pStatus_ =
    RestoreDBClusterFromSnapshotResponse'
    { _rdcfsrsDBCluster = Nothing
    , _rdcfsrsStatus = pStatus_
    }

-- | Undocumented member.
rdcfsrsDBCluster :: Lens' RestoreDBClusterFromSnapshotResponse (Maybe DBCluster)
rdcfsrsDBCluster = lens _rdcfsrsDBCluster (\ s a -> s{_rdcfsrsDBCluster = a});

-- | The response status code.
rdcfsrsStatus :: Lens' RestoreDBClusterFromSnapshotResponse Int
rdcfsrsStatus = lens _rdcfsrsStatus (\ s a -> s{_rdcfsrsStatus = a});
