{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTable where

import Network.AWS.DynamoDB.Types.Replica
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a global table.
--
--
--
-- /See:/ 'globalTable' smart constructor.
data GlobalTable = GlobalTable'
  { _gtGlobalTableName ::
      !(Maybe Text),
    _gtReplicationGroup :: !(Maybe [Replica])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtGlobalTableName' - The global table name.
--
-- * 'gtReplicationGroup' - The Regions where the global table has replicas.
globalTable ::
  GlobalTable
globalTable =
  GlobalTable'
    { _gtGlobalTableName = Nothing,
      _gtReplicationGroup = Nothing
    }

-- | The global table name.
gtGlobalTableName :: Lens' GlobalTable (Maybe Text)
gtGlobalTableName = lens _gtGlobalTableName (\s a -> s {_gtGlobalTableName = a})

-- | The Regions where the global table has replicas.
gtReplicationGroup :: Lens' GlobalTable [Replica]
gtReplicationGroup = lens _gtReplicationGroup (\s a -> s {_gtReplicationGroup = a}) . _Default . _Coerce

instance FromJSON GlobalTable where
  parseJSON =
    withObject
      "GlobalTable"
      ( \x ->
          GlobalTable'
            <$> (x .:? "GlobalTableName")
            <*> (x .:? "ReplicationGroup" .!= mempty)
      )

instance Hashable GlobalTable

instance NFData GlobalTable
