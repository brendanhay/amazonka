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
-- Module      : Network.AWS.RDS.DeleteDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBInstance action deletes a previously provisioned DB instance. When you delete a DB instance, all automated backups for that instance are deleted and can't be recovered. Manual DB snapshots of the DB instance to be deleted by @DeleteDBInstance@ are not deleted.
--
--
-- If you request a final DB snapshot the status of the Amazon RDS DB instance is @deleting@ until the DB snapshot is created. The API action @DescribeDBInstance@ is used to monitor the status of this operation. The action can't be canceled or reverted once submitted.
--
-- Note that when a DB instance is in a failure state and has a status of @failed@ , @incompatible-restore@ , or @incompatible-network@ , you can only delete it when the @SkipFinalSnapshot@ parameter is set to @true@ .
--
-- If the specified DB instance is part of an Amazon Aurora DB cluster, you can't delete the DB instance if both of the following conditions are true:
--
--     * The DB cluster is a Read Replica of another Amazon Aurora DB cluster.
--
--     * The DB instance is the only instance in the DB cluster.
--
--
--
-- To delete a DB instance in this case, first call the 'PromoteReadReplicaDBCluster' API action to promote the DB cluster so it's no longer a Read Replica. After the promotion completes, then call the @DeleteDBInstance@ API action to delete the final instance in the DB cluster.
--
module Network.AWS.RDS.DeleteDBInstance
    (
    -- * Creating a Request
      deleteDBInstance
    , DeleteDBInstance
    -- * Request Lenses
    , ddiFinalDBSnapshotIdentifier
    , ddiSkipFinalSnapshot
    , ddiDBInstanceIdentifier

    -- * Destructuring the Response
    , deleteDBInstanceResponse
    , DeleteDBInstanceResponse
    -- * Response Lenses
    , ddirsDBInstance
    , ddirsResponseStatus
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
-- /See:/ 'deleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { _ddiFinalDBSnapshotIdentifier :: !(Maybe Text)
  , _ddiSkipFinalSnapshot         :: !(Maybe Bool)
  , _ddiDBInstanceIdentifier      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddiFinalDBSnapshotIdentifier' - The DBSnapshotIdentifier of the new DBSnapshot created when SkipFinalSnapshot is set to @false@ .  Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens     * Cannot be specified when deleting a Read Replica.
--
-- * 'ddiSkipFinalSnapshot' - Determines whether a final DB snapshot is created before the DB instance is deleted. If @true@ is specified, no DBSnapshot is created. If @false@ is specified, a DB snapshot is created before the DB instance is deleted.  Note that when a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when the SkipFinalSnapshot parameter is set to "true". Specify @true@ when deleting a Read Replica. Default: @false@
--
-- * 'ddiDBInstanceIdentifier' - The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive. Constraints:     * Must match the name of an existing DB instance.
deleteDBInstance
    :: Text -- ^ 'ddiDBInstanceIdentifier'
    -> DeleteDBInstance
deleteDBInstance pDBInstanceIdentifier_ =
  DeleteDBInstance'
    { _ddiFinalDBSnapshotIdentifier = Nothing
    , _ddiSkipFinalSnapshot = Nothing
    , _ddiDBInstanceIdentifier = pDBInstanceIdentifier_
    }


-- | The DBSnapshotIdentifier of the new DBSnapshot created when SkipFinalSnapshot is set to @false@ .  Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens     * Cannot be specified when deleting a Read Replica.
ddiFinalDBSnapshotIdentifier :: Lens' DeleteDBInstance (Maybe Text)
ddiFinalDBSnapshotIdentifier = lens _ddiFinalDBSnapshotIdentifier (\ s a -> s{_ddiFinalDBSnapshotIdentifier = a})

-- | Determines whether a final DB snapshot is created before the DB instance is deleted. If @true@ is specified, no DBSnapshot is created. If @false@ is specified, a DB snapshot is created before the DB instance is deleted.  Note that when a DB instance is in a failure state and has a status of 'failed', 'incompatible-restore', or 'incompatible-network', it can only be deleted when the SkipFinalSnapshot parameter is set to "true". Specify @true@ when deleting a Read Replica. Default: @false@
ddiSkipFinalSnapshot :: Lens' DeleteDBInstance (Maybe Bool)
ddiSkipFinalSnapshot = lens _ddiSkipFinalSnapshot (\ s a -> s{_ddiSkipFinalSnapshot = a})

-- | The DB instance identifier for the DB instance to be deleted. This parameter isn't case-sensitive. Constraints:     * Must match the name of an existing DB instance.
ddiDBInstanceIdentifier :: Lens' DeleteDBInstance Text
ddiDBInstanceIdentifier = lens _ddiDBInstanceIdentifier (\ s a -> s{_ddiDBInstanceIdentifier = a})

instance AWSRequest DeleteDBInstance where
        type Rs DeleteDBInstance = DeleteDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DeleteDBInstanceResult"
              (\ s h x ->
                 DeleteDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable DeleteDBInstance where

instance NFData DeleteDBInstance where

instance ToHeaders DeleteDBInstance where
        toHeaders = const mempty

instance ToPath DeleteDBInstance where
        toPath = const "/"

instance ToQuery DeleteDBInstance where
        toQuery DeleteDBInstance'{..}
          = mconcat
              ["Action" =: ("DeleteDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "FinalDBSnapshotIdentifier" =:
                 _ddiFinalDBSnapshotIdentifier,
               "SkipFinalSnapshot" =: _ddiSkipFinalSnapshot,
               "DBInstanceIdentifier" =: _ddiDBInstanceIdentifier]

-- | /See:/ 'deleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { _ddirsDBInstance     :: !(Maybe DBInstance)
  , _ddirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddirsDBInstance' - Undocumented member.
--
-- * 'ddirsResponseStatus' - -- | The response status code.
deleteDBInstanceResponse
    :: Int -- ^ 'ddirsResponseStatus'
    -> DeleteDBInstanceResponse
deleteDBInstanceResponse pResponseStatus_ =
  DeleteDBInstanceResponse'
    {_ddirsDBInstance = Nothing, _ddirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ddirsDBInstance :: Lens' DeleteDBInstanceResponse (Maybe DBInstance)
ddirsDBInstance = lens _ddirsDBInstance (\ s a -> s{_ddirsDBInstance = a})

-- | -- | The response status code.
ddirsResponseStatus :: Lens' DeleteDBInstanceResponse Int
ddirsResponseStatus = lens _ddirsResponseStatus (\ s a -> s{_ddirsResponseStatus = a})

instance NFData DeleteDBInstanceResponse where
