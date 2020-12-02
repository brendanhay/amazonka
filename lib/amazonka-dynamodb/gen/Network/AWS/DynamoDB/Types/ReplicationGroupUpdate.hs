{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicationGroupUpdate where

import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents one of the following:
--
--
--     * A new replica to be added to an existing regional table or global table. This request invokes the @CreateTableReplica@ action in the destination Region.
--
--     * New parameters for an existing replica. This request invokes the @UpdateTable@ action in the destination Region.
--
--     * An existing replica to be deleted. The request invokes the @DeleteTableReplica@ action in the destination Region, deleting the replica and all if its items in the destination Region.
--
--
--
--
-- /See:/ 'replicationGroupUpdate' smart constructor.
data ReplicationGroupUpdate = ReplicationGroupUpdate'
  { _rguCreate ::
      !(Maybe CreateReplicationGroupMemberAction),
    _rguDelete ::
      !(Maybe DeleteReplicationGroupMemberAction),
    _rguUpdate ::
      !(Maybe UpdateReplicationGroupMemberAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationGroupUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rguCreate' - The parameters required for creating a replica for the table.
--
-- * 'rguDelete' - The parameters required for deleting a replica for the table.
--
-- * 'rguUpdate' - The parameters required for updating a replica for the table.
replicationGroupUpdate ::
  ReplicationGroupUpdate
replicationGroupUpdate =
  ReplicationGroupUpdate'
    { _rguCreate = Nothing,
      _rguDelete = Nothing,
      _rguUpdate = Nothing
    }

-- | The parameters required for creating a replica for the table.
rguCreate :: Lens' ReplicationGroupUpdate (Maybe CreateReplicationGroupMemberAction)
rguCreate = lens _rguCreate (\s a -> s {_rguCreate = a})

-- | The parameters required for deleting a replica for the table.
rguDelete :: Lens' ReplicationGroupUpdate (Maybe DeleteReplicationGroupMemberAction)
rguDelete = lens _rguDelete (\s a -> s {_rguDelete = a})

-- | The parameters required for updating a replica for the table.
rguUpdate :: Lens' ReplicationGroupUpdate (Maybe UpdateReplicationGroupMemberAction)
rguUpdate = lens _rguUpdate (\s a -> s {_rguUpdate = a})

instance Hashable ReplicationGroupUpdate

instance NFData ReplicationGroupUpdate

instance ToJSON ReplicationGroupUpdate where
  toJSON ReplicationGroupUpdate' {..} =
    object
      ( catMaybes
          [ ("Create" .=) <$> _rguCreate,
            ("Delete" .=) <$> _rguDelete,
            ("Update" .=) <$> _rguUpdate
          ]
      )
