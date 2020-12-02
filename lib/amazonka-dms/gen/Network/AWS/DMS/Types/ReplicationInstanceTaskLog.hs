{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationInstanceTaskLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationInstanceTaskLog where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains metadata for a replication instance task log.
--
--
--
-- /See:/ 'replicationInstanceTaskLog' smart constructor.
data ReplicationInstanceTaskLog = ReplicationInstanceTaskLog'
  { _ritlReplicationTaskName ::
      !(Maybe Text),
    _ritlReplicationTaskARN ::
      !(Maybe Text),
    _ritlReplicationInstanceTaskLogSize ::
      !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationInstanceTaskLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ritlReplicationTaskName' - The name of the replication task.
--
-- * 'ritlReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'ritlReplicationInstanceTaskLogSize' - The size, in bytes, of the replication task log.
replicationInstanceTaskLog ::
  ReplicationInstanceTaskLog
replicationInstanceTaskLog =
  ReplicationInstanceTaskLog'
    { _ritlReplicationTaskName = Nothing,
      _ritlReplicationTaskARN = Nothing,
      _ritlReplicationInstanceTaskLogSize = Nothing
    }

-- | The name of the replication task.
ritlReplicationTaskName :: Lens' ReplicationInstanceTaskLog (Maybe Text)
ritlReplicationTaskName = lens _ritlReplicationTaskName (\s a -> s {_ritlReplicationTaskName = a})

-- | The Amazon Resource Name (ARN) of the replication task.
ritlReplicationTaskARN :: Lens' ReplicationInstanceTaskLog (Maybe Text)
ritlReplicationTaskARN = lens _ritlReplicationTaskARN (\s a -> s {_ritlReplicationTaskARN = a})

-- | The size, in bytes, of the replication task log.
ritlReplicationInstanceTaskLogSize :: Lens' ReplicationInstanceTaskLog (Maybe Integer)
ritlReplicationInstanceTaskLogSize = lens _ritlReplicationInstanceTaskLogSize (\s a -> s {_ritlReplicationInstanceTaskLogSize = a})

instance FromJSON ReplicationInstanceTaskLog where
  parseJSON =
    withObject
      "ReplicationInstanceTaskLog"
      ( \x ->
          ReplicationInstanceTaskLog'
            <$> (x .:? "ReplicationTaskName")
            <*> (x .:? "ReplicationTaskArn")
            <*> (x .:? "ReplicationInstanceTaskLogSize")
      )

instance Hashable ReplicationInstanceTaskLog

instance NFData ReplicationInstanceTaskLog
