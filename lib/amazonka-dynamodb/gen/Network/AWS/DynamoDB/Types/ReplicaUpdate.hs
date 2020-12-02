{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaUpdate where

import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents one of the following:
--
--
--     * A new replica to be added to an existing global table.
--
--     * New parameters for an existing replica.
--
--     * An existing replica to be removed from an existing global table.
--
--
--
--
-- /See:/ 'replicaUpdate' smart constructor.
data ReplicaUpdate = ReplicaUpdate'
  { _ruCreate ::
      !(Maybe CreateReplicaAction),
    _ruDelete :: !(Maybe DeleteReplicaAction)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicaUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruCreate' - The parameters required for creating a replica on an existing global table.
--
-- * 'ruDelete' - The name of the existing replica to be removed.
replicaUpdate ::
  ReplicaUpdate
replicaUpdate =
  ReplicaUpdate' {_ruCreate = Nothing, _ruDelete = Nothing}

-- | The parameters required for creating a replica on an existing global table.
ruCreate :: Lens' ReplicaUpdate (Maybe CreateReplicaAction)
ruCreate = lens _ruCreate (\s a -> s {_ruCreate = a})

-- | The name of the existing replica to be removed.
ruDelete :: Lens' ReplicaUpdate (Maybe DeleteReplicaAction)
ruDelete = lens _ruDelete (\s a -> s {_ruDelete = a})

instance Hashable ReplicaUpdate

instance NFData ReplicaUpdate

instance ToJSON ReplicaUpdate where
  toJSON ReplicaUpdate' {..} =
    object
      ( catMaybes
          [("Create" .=) <$> _ruCreate, ("Delete" .=) <$> _ruDelete]
      )
