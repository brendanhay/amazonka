{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster.
-- A successful response from the web service indicates the request was
-- received correctly. When you delete a DB cluster, all automated backups
-- for that DB cluster are deleted and cannot be recovered. Manual DB
-- cluster snapshots of the DB cluster to be deleted are not deleted.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBCluster.html>
module Network.AWS.RDS.DeleteDBCluster
    (
    -- * Request
      DeleteDBCluster
    -- ** Request constructor
    , deleteDBCluster
    -- ** Request lenses
    , ddbcDBClusterIdentifier
    , ddbcFinalDBSnapshotIdentifier
    , ddbcSkipFinalSnapshot

    -- * Response
    , DeleteDBClusterResponse
    -- ** Response constructor
    , deleteDBClusterResponse
    -- ** Response lenses
    , ddbcrsDBCluster
    , ddbcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbcDBClusterIdentifier'
--
-- * 'ddbcFinalDBSnapshotIdentifier'
--
-- * 'ddbcSkipFinalSnapshot'
data DeleteDBCluster = DeleteDBCluster'
    { _ddbcDBClusterIdentifier       :: !(Maybe Text)
    , _ddbcFinalDBSnapshotIdentifier :: !(Maybe Text)
    , _ddbcSkipFinalSnapshot         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBCluster' smart constructor.
deleteDBCluster :: DeleteDBCluster
deleteDBCluster =
    DeleteDBCluster'
    { _ddbcDBClusterIdentifier = Nothing
    , _ddbcFinalDBSnapshotIdentifier = Nothing
    , _ddbcSkipFinalSnapshot = Nothing
    }

-- | The DB cluster identifier for the DB cluster to be deleted. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbcDBClusterIdentifier :: Lens' DeleteDBCluster (Maybe Text)
ddbcDBClusterIdentifier = lens _ddbcDBClusterIdentifier (\ s a -> s{_ddbcDBClusterIdentifier = a});

-- | The DB cluster snapshot identifier of the new DB cluster snapshot
-- created when @SkipFinalSnapshot@ is set to @false@.
--
-- Specifying this parameter and also setting the @SkipFinalShapshot@
-- parameter to true results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbcFinalDBSnapshotIdentifier :: Lens' DeleteDBCluster (Maybe Text)
ddbcFinalDBSnapshotIdentifier = lens _ddbcFinalDBSnapshotIdentifier (\ s a -> s{_ddbcFinalDBSnapshotIdentifier = a});

-- | Determines whether a final DB cluster snapshot is created before the DB
-- cluster is deleted. If @true@ is specified, no DB cluster snapshot is
-- created. If @false@ is specified, a DB cluster snapshot is created
-- before the DB cluster is deleted.
--
-- You must specify a @FinalDBSnapshotIdentifier@ parameter if
-- @SkipFinalSnapshot@ is @false@.
--
-- Default: @false@
ddbcSkipFinalSnapshot :: Lens' DeleteDBCluster (Maybe Bool)
ddbcSkipFinalSnapshot = lens _ddbcSkipFinalSnapshot (\ s a -> s{_ddbcSkipFinalSnapshot = a});

instance AWSRequest DeleteDBCluster where
        type Sv DeleteDBCluster = RDS
        type Rs DeleteDBCluster = DeleteDBClusterResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteDBClusterResult"
              (\ s h x ->
                 DeleteDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDBCluster where
        toHeaders = const mempty

instance ToPath DeleteDBCluster where
        toPath = const "/"

instance ToQuery DeleteDBCluster where
        toQuery DeleteDBCluster'{..}
          = mconcat
              ["Action" =: ("DeleteDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _ddbcDBClusterIdentifier,
               "FinalDBSnapshotIdentifier" =:
                 _ddbcFinalDBSnapshotIdentifier,
               "SkipFinalSnapshot" =: _ddbcSkipFinalSnapshot]

-- | /See:/ 'deleteDBClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbcrsDBCluster'
--
-- * 'ddbcrsStatus'
data DeleteDBClusterResponse = DeleteDBClusterResponse'
    { _ddbcrsDBCluster :: !(Maybe DBCluster)
    , _ddbcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBClusterResponse' smart constructor.
deleteDBClusterResponse :: Int -> DeleteDBClusterResponse
deleteDBClusterResponse pStatus_ =
    DeleteDBClusterResponse'
    { _ddbcrsDBCluster = Nothing
    , _ddbcrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ddbcrsDBCluster :: Lens' DeleteDBClusterResponse (Maybe DBCluster)
ddbcrsDBCluster = lens _ddbcrsDBCluster (\ s a -> s{_ddbcrsDBCluster = a});

-- | FIXME: Undocumented member.
ddbcrsStatus :: Lens' DeleteDBClusterResponse Int
ddbcrsStatus = lens _ddbcrsStatus (\ s a -> s{_ddbcrsStatus = a});
