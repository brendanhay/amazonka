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
-- Module      : Network.AWS.RDS.DeleteDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster. When you delete a DB cluster, all automated backups for that DB cluster are deleted and can't be recovered. Manual DB cluster snapshots of the specified DB cluster are not deleted.
--
--
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.DeleteDBCluster
    (
    -- * Creating a Request
      deleteDBCluster
    , DeleteDBCluster
    -- * Request Lenses
    , ddbcFinalDBSnapshotIdentifier
    , ddbcSkipFinalSnapshot
    , ddbcDBClusterIdentifier

    -- * Destructuring the Response
    , deleteDBClusterResponse
    , DeleteDBClusterResponse
    -- * Response Lenses
    , ddbcrsDBCluster
    , ddbcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteDBCluster' smart constructor.
data DeleteDBCluster = DeleteDBCluster'
  { _ddbcFinalDBSnapshotIdentifier :: !(Maybe Text)
  , _ddbcSkipFinalSnapshot         :: !(Maybe Bool)
  , _ddbcDBClusterIdentifier       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcFinalDBSnapshotIdentifier' - The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is set to @false@ .  Constraints:     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
--
-- * 'ddbcSkipFinalSnapshot' - Determines whether a final DB cluster snapshot is created before the DB cluster is deleted. If @true@ is specified, no DB cluster snapshot is created. If @false@ is specified, a DB cluster snapshot is created before the DB cluster is deleted.  Default: @false@
--
-- * 'ddbcDBClusterIdentifier' - The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive. Constraints:     * Must match an existing DBClusterIdentifier.
deleteDBCluster
    :: Text -- ^ 'ddbcDBClusterIdentifier'
    -> DeleteDBCluster
deleteDBCluster pDBClusterIdentifier_ =
  DeleteDBCluster'
    { _ddbcFinalDBSnapshotIdentifier = Nothing
    , _ddbcSkipFinalSnapshot = Nothing
    , _ddbcDBClusterIdentifier = pDBClusterIdentifier_
    }


-- | The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is set to @false@ .  Constraints:     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
ddbcFinalDBSnapshotIdentifier :: Lens' DeleteDBCluster (Maybe Text)
ddbcFinalDBSnapshotIdentifier = lens _ddbcFinalDBSnapshotIdentifier (\ s a -> s{_ddbcFinalDBSnapshotIdentifier = a})

-- | Determines whether a final DB cluster snapshot is created before the DB cluster is deleted. If @true@ is specified, no DB cluster snapshot is created. If @false@ is specified, a DB cluster snapshot is created before the DB cluster is deleted.  Default: @false@
ddbcSkipFinalSnapshot :: Lens' DeleteDBCluster (Maybe Bool)
ddbcSkipFinalSnapshot = lens _ddbcSkipFinalSnapshot (\ s a -> s{_ddbcSkipFinalSnapshot = a})

-- | The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive. Constraints:     * Must match an existing DBClusterIdentifier.
ddbcDBClusterIdentifier :: Lens' DeleteDBCluster Text
ddbcDBClusterIdentifier = lens _ddbcDBClusterIdentifier (\ s a -> s{_ddbcDBClusterIdentifier = a})

instance AWSRequest DeleteDBCluster where
        type Rs DeleteDBCluster = DeleteDBClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DeleteDBClusterResult"
              (\ s h x ->
                 DeleteDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable DeleteDBCluster where

instance NFData DeleteDBCluster where

instance ToHeaders DeleteDBCluster where
        toHeaders = const mempty

instance ToPath DeleteDBCluster where
        toPath = const "/"

instance ToQuery DeleteDBCluster where
        toQuery DeleteDBCluster'{..}
          = mconcat
              ["Action" =: ("DeleteDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "FinalDBSnapshotIdentifier" =:
                 _ddbcFinalDBSnapshotIdentifier,
               "SkipFinalSnapshot" =: _ddbcSkipFinalSnapshot,
               "DBClusterIdentifier" =: _ddbcDBClusterIdentifier]

-- | /See:/ 'deleteDBClusterResponse' smart constructor.
data DeleteDBClusterResponse = DeleteDBClusterResponse'
  { _ddbcrsDBCluster      :: !(Maybe DBCluster)
  , _ddbcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcrsDBCluster' - Undocumented member.
--
-- * 'ddbcrsResponseStatus' - -- | The response status code.
deleteDBClusterResponse
    :: Int -- ^ 'ddbcrsResponseStatus'
    -> DeleteDBClusterResponse
deleteDBClusterResponse pResponseStatus_ =
  DeleteDBClusterResponse'
    {_ddbcrsDBCluster = Nothing, _ddbcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ddbcrsDBCluster :: Lens' DeleteDBClusterResponse (Maybe DBCluster)
ddbcrsDBCluster = lens _ddbcrsDBCluster (\ s a -> s{_ddbcrsDBCluster = a})

-- | -- | The response status code.
ddbcrsResponseStatus :: Lens' DeleteDBClusterResponse Int
ddbcrsResponseStatus = lens _ddbcrsResponseStatus (\ s a -> s{_ddbcrsResponseStatus = a})

instance NFData DeleteDBClusterResponse where
